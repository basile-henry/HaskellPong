module Main where

import System.Environment
import Graphics.UI.GLUT hiding (normalize)
import Linear
import Safe
import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Data.IORef
import Data.Maybe
import Data.Char (toUpper, toLower)
import Data.Fixed
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Fix
import Text.Read (readMaybe)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as Mix


type Vec2 = V2 Float
type Vec3 = V3 Float

data Collider = WallT | WallB | WallL | WallR | PlayerT | PlayerB
    deriving (Eq, Show)

data WorldSate t = WorldState {
    ballPos :: Behavior t Vec2,
    player1Pos :: Behavior t Vec2
}

data Side = Top | Bottom deriving (Eq, Show)

data PlayState = Play | PlaceT | PlaceB
    deriving (Eq, Show)

data Game t = Game {
    tickE :: Event t Float,
    timeB :: Behavior t Float,
    sounds :: GameSounds
}

data GameSounds = GameSounds {
    goal :: IO (),
    playerBounce :: IO (),
    wallBounce :: IO ()
}

integrateBall :: forall t. Frameworks t => Game t -> Vec2 -> Behavior t Vec2 -> Behavior t (Maybe Vec2) -> Behavior t Vec2
integrateBall (Game{ tickE = tickE }) initialPos velB resetPosB = accumB initialPos $ step <$> velB <*> resetPosB <@> tickE
    where
        step vel Nothing dt oldPos = oldPos + (vel ^* dt)
        step _ (Just pos) _ _ = pos

integratePlayer :: forall t. Frameworks t => Game t -> Vec2 -> Behavior t Vec2 -> Behavior t Vec2
integratePlayer (Game{ tickE = tickE }) initialPos velB = accumB initialPos $ (\vel dt oldPos -> oldPos + (vel ^* dt)) <$> velB <@> tickE

createFrpNetwork :: forall t. Frameworks t => Int -> GameSounds -> AddHandler (Float, Float) -> AddHandler Key -> AddHandler Key -> SettableStateVar [GraphicsPrimitive] -> Moment t ()
createFrpNetwork numHP gameSounds frameAddHandler keyDownAddHandler keyUpAddhandler prims = do
    tickE    <- fromAddHandler frameAddHandler
    keyDownE  <- fromAddHandler keyDownAddHandler
    keyUpE    <- fromAddHandler keyUpAddhandler

    let

        game = Game {
            tickE = fst <$> tickE,
            timeB = stepper 0 (snd <$> tickE),
            sounds = gameSounds
        }

        keysPressedB = accumB Set.empty $
            (Set.delete <$> keyUpE) `union`
            (Set.insert <$> keyDownE)

        playStateB = stepper PlaceB $
            (toPlayer <$> goalE) `union`
            (Play  <$ startPlayE)
            where
                toPlayer WallT = PlaceT
                toPlayer WallB = PlaceB

        getKeyE (Char char) = filterE (`elem` [Char lower, Char upper]) keyDownE where
            lower = toLower char
            upper = toUpper lower
        getKeyE key = filterE (== key) keyDownE

        startPlayE = fireTE `union` fireBE

        --
        -- Players
        --


        playerSpeed = 2
        playerRadius = 0.3
        playerWidth = 0.3
        playerOffset = playerRadius * cos (asin (playerWidth / (2 * playerRadius)))

        createHumanInput leftKey rightKey fireKey = (desiredXVelB, tryFireE)
            where

                tryFireE = () <$ getKeyE fireKey

                desiredXVelB = toTotVel <$> keysPressedB where
                    toTotVel keys = sum $ mapMaybe toVel (Set.toList keys) where
                        toVel (Char char) = toVel' (Char (toLower char))
                        toVel key = toVel' key
                        toVel' key = lookup key [
                                  (leftKey,   -1),
                                  (rightKey,  1)
                              ]

        createAI side posB = (desiredXVelB, tryFireE)
            where

                tryFireE = () <$ tickE

                awayYDir = case side of
                    Top -> -1
                    Bottom -> 1

                desiredXVelB = getVel <$> posB <*> posBallB <*> velBallB where
                    getVel (V2 px _) (V2 bx by) (V2 _ bvy)
                        | abs (targetX - px) < playerWidth/4 = 0
                        | otherwise                     = targetX - px
                        where
                            targetX
                                | bvy * awayYDir >= 0 = (by-awayYDir)/6
                                | otherwise          = bx

        playerFromDesiredVelB side (desiredXVelB, tryFireE) = (posB, velB, fireE)
            where

                fireGameState = case side of
                    Top -> PlaceT
                    Bottom -> PlaceB

                fireE = whenE ((== fireGameState) <$> playStateB) tryFireE

                startPos = case side of
                    Top -> V2 0 (1 + playerOffset)
                    Bottom -> V2 0 (-1 - playerOffset)

                posB = integratePlayer game startPos velB

                velB = toTotVel <$> posB <*> desiredXVelB where
                    toTotVel (V2 x _) desiredXVel = (playerSpeed *^) $ normalize $ constrainedVel where
                        constrainedVel
                            | x < -1 + (playerRadius/2) && desiredXVel < 0 ||
                              x > 1 - (playerRadius/2) && desiredXVel > 0     = zero
                            | otherwise                                       = V2 desiredXVel 0


        (posPlayerT, velPlayerT, fireTE) = playerFromDesiredVelB Top (if numHP > 1
            then
                createHumanInput (Char 'a') (Char 'd') (Char 's')
            else
                createAI Top posPlayerT)

        (posPlayerB, velPlayerB, fireBE) = playerFromDesiredVelB Bottom (if numHP > 0
            then
                createHumanInput (SpecialKey KeyLeft) (SpecialKey KeyRight) (SpecialKey KeyUp)
            else
                (createAI Bottom posPlayerB))

        --
        -- Ball
        --

        ballRadius = 0.05
        ballSpeed = 2.5

        colBallE = fst <$> (accumE ([], []) $ (\new (_, last) -> (new List.\\ last, new)) <$> (filterE (not . null) (checkCollision <$> posBallB <*> posPlayerB <*> posPlayerT <@ tickE)))
        checkCollision posBall@(V2 x y) posB posT =
            [WallT    | y > 1] ++
            [WallB    | (-1) > y] ++
            [WallL    | ballRadius - 1 > x] ++
            [WallR    | x > 1 - ballRadius] ++
            [PlayerT  | (ballRadius + playerRadius) > (norm $ posBall - posT)] ++
            [PlayerB  | (ballRadius + playerRadius) > (norm $ posBall - posB)]



        goalE = head <$> (filterE (/= []) $ (List.intersect [WallT, WallB]) <$> colBallE)
        placeBallPosB = ((\playState posT posB -> case playState of
                    PlaceT -> Just $ posT - (V2 0 (playerRadius + ballRadius))
                    PlaceB -> Just $ posB + (V2 0 (playerRadius + ballRadius))
                    _      -> Nothing
                ) <$> playStateB <*> posPlayerT <*> posPlayerB)


        velBallB = stepper zero $
            (newVel <$> posPlayerT <*> posPlayerB <*> velPlayerT <*> velPlayerB <*> posBallB <*> velBallB <@> colBallE) `union`
            (startVel <$> velPlayerT <*> velPlayerB <*> playStateB <@ startPlayE)
            where
                startVel velT _ PlaceT = ballSpeed *^ (normalize ((normalize velT) + (V2 0 (-1))))
                startVel _ velB PlaceB = ballSpeed *^ (normalize ((normalize velB) + (V2 0 1)))
                startVel _ _ _ = error "Unexpected state!"

        newVel posT posB velT velB posBall (V2 h v) (WallL:other) = newVel posT posB velT velB posBall (V2 (abs h) v) other
        newVel posT posB velT velB posBall (V2 h v) (WallR:other) = newVel posT posB velT velB posBall (V2 (-(abs h)) v) other
        newVel posT posB velT velB posBall velBall (PlayerT:other) = newVel posT posB velT velB posBall (newVelPlayerBounce posT velT posBall velBall) other
        newVel posT posB velT velB posBall velBall (PlayerB:other) = newVel posT posB velT velB posBall (newVelPlayerBounce posB velB posBall velBall) other
        newVel posT posB velT velB posBall vel (x:xs) = newVel posT posB velT velB posBall vel xs
        newVel _ _ _ _ _ vel [] = vel

        newVelPlayerBounce playerPos playerVel ballPos ballVel = ballVel - (2 * (ballVel `dot` n) *^ n) + ((normalize playerVel) ^* (playerVel `dot` n)) where
            n = normalize $ ballPos - playerPos

        posBallB = integrateBall game zero velBallB placeBallPosB

    --
    -- Audio
    --

    let
        ss = sounds game
        collisionSounds = sequence_ . (map collisionSound)
        collisionSound WallL = wallBounce ss
        collisionSound WallR = wallBounce ss
        collisionSound PlayerT = playerBounce ss
        collisionSound PlayerB = playerBounce ss
        collisionSound _ = return ()
    reactimate $
        ((goal ss) <$ goalE) `union`
        (collisionSounds <$>  colBallE) `union`
        (playerBounce ss <$ startPlayE)

    --
    -- Score
    --
    let
        tallyRadius = 0.01
        scoreB = accumB (0, 0) ((score <$> goalE) `union`
                                ((\_ -> (0,0)) <$ (getKeyE $ Char 'r'))) where
            score WallT (x, y) = (x, y+1)
            score WallB (x, y) = (x+1, y)

        scoreTallyPositionsT = scoreTallyPositions (2 * tallyRadius)
        scoreTallyPositionsB = scoreTallyPositions (-2 * tallyRadius)
        scoreTallyPositions yOffset score = [V2 ((ix * 4 * tallyRadius) - hWidth) yOffset | ix <- [0..score-1] ] where
            hWidth = 2 * (score - 1) * tallyRadius


    --
    -- Graphics prims
    --

    let
        colorT = V3 0 1 0
        colorB = V3 1 0 1

    reactimate $ (\ballPos@(V2 x y) posB posT (scoreT, scoreB) -> prims $=
            ColoredShape (V3 0.2 0.2 0.21) (Rectangle (V2 (-1) (-1)) (V2 1 1)) :
            [ColoredShape colorT (Circle tallyPos tallyRadius) | tallyPos <- scoreTallyPositionsT scoreT]
            ++
            [ColoredShape colorB (Circle tallyPos tallyRadius) | tallyPos <- scoreTallyPositionsB scoreB]
            ++
            [
                ColoredShape (fromHue $ (x + y + 2) / 4) (Circle ballPos ballRadius),
                ColoredShape colorB (Circle posB playerRadius),
                ColoredShape colorT (Circle posT playerRadius)
            ]
        ) <$> posBallB <*> posPlayerB <*> posPlayerT <*> scoreB <@ tickE

--
-- Graphics
--

main :: IO ()
main = do
    -- Command line arguments (number of human players)
    args <- getArgs
    let
        hm = headMay args
        numHumanPlayersM = maybe Nothing readMaybe hm
        numHumanPlayers :: Int
        numHumanPlayers = maybe 2 id numHumanPlayersM

    -- Create mutable graphics primitive list
    refPrims <- newIORef ([] :: [GraphicsPrimitive])

    -- Init the audio
    let
        audioRate     = 22050
        audioFormat   = Mix.AudioS16LSB
        audioChannels = 2
        audioBuffers  = 1024
        anyChannel    = (-1)

    SDL.init [SDL.InitAudio]
    Mix.openAudio audioRate audioFormat audioChannels audioBuffers

    let
        -- String -> IO (IO ())
        getSound s = do
            sound <- Mix.loadWAV s
            return $ void $ Mix.playChannel anyChannel sound 0

    goalSound <- getSound "res/goal.wav"
    playerBounceSound <- getSound "res/player_bounce.wav"
    wallBounceSound <- getSound "res/wall_bounce.wav"

    let
        gameSounds = GameSounds {
            goal = goalSound,
            playerBounce = playerBounceSound,
            wallBounce = wallBounceSound
        }

    -- Init the graphics
    (_progName, _args) <- getArgsAndInitialize

    initialWindowSize $= Size 1000 1000
    initialWindowPosition $= Position 800 10
    initialDisplayMode $= [
            RGBAMode,
            DoubleBuffered,
            Multisampling
        ]
    window <- createWindow "Haskell Pong"
    clearColor $= Color4 0 0 0 1

    -- Callbacks
    idleCallback $= (return $ postRedisplay Nothing)
    displayCallback $= display window refPrims
    (keyDownAddHandler, keyDownHandler) <- newAddHandler
    (keyUpAddHandler, keyUpHandler) <- newAddHandler
    keyboardMouseCallback $= Just (keyboardMouse keyDownHandler keyUpHandler)

    -- Start FRP network
    (frameAddHandler, frameHandler) <- newAddHandler
    startTime <- getPOSIXTime
    let
        loop lastTime = do
            time <- getPOSIXTime
            let dt = realToFrac (time - lastTime)
            frameHandler $ (dt, realToFrac (time - startTime))
            threadDelay (15000 - round (dt * 1000000))
            loop time
    forkIO $ loop startTime
    network <- compile $ createFrpNetwork
        numHumanPlayers
        gameSounds
        frameAddHandler
        keyDownAddHandler
        keyUpAddHandler
        (makeSettableStateVar (refPrims $=))
    actuate network

    -- Main loop
    mainLoop

    -- Cleanup
    Mix.closeAudio
    SDL.quit

display :: Window -> IORef [GraphicsPrimitive] ->  DisplayCallback
display window refPrims = do
    -- Clear the color buffer
    clear [ ColorBuffer ]

    -- Get the current scene primitives
    prims <- readIORef refPrims

    -- Draw all the primitives.
    mapM_ renderGraphicsPrimitive prims

    swapBuffers
    flush
    postRedisplay (Just window)


data ShapePrimitive =
      Rectangle Vec2 Vec2
    | Circle Vec2 Float
data GraphicsPrimitive = ColoredShape Vec3 ShapePrimitive

defaultColor = V3 1 1 1

renderGraphicsPrimitive :: GraphicsPrimitive -> IO ()
renderGraphicsPrimitive (ColoredShape color (Rectangle (V2 left top) (V2 right bottom))) = renderPrimitive Quads $ do
    color3f color
    vertex3f left top 0
    vertex3f right top 0
    vertex3f right bottom 0
    vertex3f left bottom 0
renderGraphicsPrimitive (ColoredShape color (Circle center@(V2 x y) radius)) = renderPrimitive Polygon $ do
    color3f color
    mapM_ vertexV2 [ center + (V2 (radius * sin (2*pi*k/n)) (radius * cos (2*pi*k/n))) | k <- [1..n] ]
    where
        n= 1000

fromHue :: Float -> Vec3
fromHue hue
    | h < 60       = V3 1 l 0
    | h < 120      = V3 l 1 0
    | h < 180      = V3 0 1 l
    | h < 240      = V3 0 l 1
    | h < 300      = V3 l 0 1
    | otherwise    = V3 1 0 l
    where
        h = 360 * hue
        l' = 1 - (abs (((h / 60) `mod'` 2) - 1))
        l = min (max 0 l') 1

color3f :: Vec3 -> IO ()
color3f (V3 r g b) = color $ Color3 (realToFrac r) (realToFrac g) (realToFrac b :: GLfloat)

vertexV2 :: Vec2 -> IO ()
vertexV2 (V2 x y) = vertex3f x y 0

vertex3f :: Float -> Float -> Float -> IO ()
vertex3f x y z = vertex $ Vertex3 (realToFrac x) (realToFrac y) (realToFrac z :: GLfloat)

keyboardMouse :: Handler Key -> Handler Key -> KeyboardMouseCallback
keyboardMouse keyDownHandler _ key Down _ _ = keyDownHandler key
keyboardMouse _ keyUpHandler key Up _ _ = keyUpHandler key
keyboardMouse _ _ _ _ _ _ = return ()