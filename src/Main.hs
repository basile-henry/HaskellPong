module Main where

import Graphics.UI.GLUT hiding (normalize)
import Linear
import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Monad.Reader


type Vec2 = V2 Float

data Collider = WallT | WallB | WallL | WallR | PlayerT | PlayerB
    deriving (Eq, Show)

data WorldSate t = WorldState {
    ballPos :: Behavior t Vec2,
    player1Pos :: Behavior t Vec2
}

data PlayState = Play | PlaceT | PlaceB
    deriving (Eq, Show)

data Game t = Game {
    tickE :: Event t Float,
    timeB :: Behavior t Float
}

integrateBall :: forall t. Frameworks t => Game t -> Vec2 -> Behavior t Vec2 -> Behavior t (Maybe Vec2) -> Behavior t Vec2
integrateBall (Game{ tickE = tickE }) initialPos velB resetPosB = accumB initialPos $ step <$> velB <*> resetPosB <@> tickE
    where
        step vel Nothing dt oldPos = oldPos + (vel ^* dt)
        step _ (Just pos) _ _ = pos

integratePlayer :: forall t. Frameworks t => Game t -> Vec2 -> Behavior t Vec2 -> Behavior t Vec2
integratePlayer (Game{ tickE = tickE }) initialPos velB = accumB initialPos $ (\vel dt oldPos -> oldPos + (vel ^* dt)) <$> velB <@> tickE

createFrpNetwork :: forall t. Frameworks t => AddHandler (Float, Float) -> AddHandler Key -> AddHandler Key -> SettableStateVar [GraphicsPrimitive] -> Moment t ()
createFrpNetwork frameAddHandler keyDownAddHandler keyUpAddhandler prims = do
    frameE    <- fromAddHandler frameAddHandler
    keyDownE  <- fromAddHandler keyDownAddHandler
    keyUpE    <- fromAddHandler keyUpAddhandler

    let

        game = Game {
            tickE = fst <$> frameE,
            timeB = stepper 0 (snd <$> frameE)
        }

        keysPressedB = accumB Set.empty $
            (Set.delete <$> keyUpE) `union`
            (Set.insert <$> keyDownE)

        playStateB = stepper PlaceT $
            (toPlayer <$> goalE) `union`
            (Play  <$ startPlayE)
            where
                toPlayer WallT = PlaceT
                toPlayer WallB = PlaceB

        startPlayE = (whenE ((== PlaceT) <$> playStateB) (filterE (== Char 's') keyDownE)) `union`
                     (whenE ((== PlaceB) <$> playStateB) (filterE (== SpecialKey KeyUp) keyDownE))

        --
        -- Players
        --


        playerSpeed = 1.8
        playerRadius = 0.3
        playerWidth = 0.3
        playerOffset = playerRadius * cos (asin (playerWidth / (2 * playerRadius)))

        createPlayer startPos (leftKey, rightKey) = (posB, velB)
            where

                posB = integratePlayer game startPos velB

                velB = toTotVel <$> posB <*> keysPressedB where
                    toTotVel (V2 px _) keys = (playerSpeed *^) $ normalize $ sum $ mapMaybe toVel (Set.toList keys) where
                        toVel key = constrain <$> lookup key [
                                  (leftKey,   V2 (-1) 0),
                                  (rightKey,  V2   1  0)
                              ]
                        constrain (V2 vx vy)
                            | px < -1 + (playerRadius/2) && vx < 0 || px > 1 - (playerRadius/2) && vx > 0  = zero
                            | otherwise                              = V2 (max ((playerWidth/2) - 1) (min vx (1 - (playerWidth/2)))) vy

        (posPlayerT, velPlayerT) = createPlayer (V2 0 (1 + playerOffset)) (Char 'a', Char 'd')
        (posPlayerB, velPlayerB) = createPlayer (V2 0 (-1 - playerOffset)) (SpecialKey KeyLeft, SpecialKey KeyRight)

        --
        -- Ball
        --

        ballRadius = 0.05
        ballSpeed = 0.5

        colBallE = fst <$> (accumE ([], []) $ (\new (_, last) -> (new List.\\ last, new)) <$> (filterE (not . null) (checkCollision <$> posBallB <*> posPlayerB <*> posPlayerT <@ frameE)))
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
                startVel _ _ _ = error "WTF Mate!"

--        newVel (V2 h v) (WallT:other) = newVel (V2 h (-(abs v))) other
--        newVel (V2 h v) (WallB:other) = newVel (V2 h (abs v)) other
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
    -- Graphics prims
    --

    reactimate $ print <$> goalE

    reactimate $ (\ballPos pos1 pos2-> prims $= [
            Circle ballPos ballRadius,
            Circle pos1 playerRadius,
            Circle pos2 playerRadius
        ]) <$> posBallB <*> posPlayerB <*> posPlayerT <@ frameE


--
-- Graphics
--

data GraphicsPrimitive =
      Rectangle Vec2 Vec2
    | Circle Vec2 Float

main :: IO ()
main = do
    -- Create mutable graphics primitive list
    refPrims <- newIORef ([] :: [GraphicsPrimitive])

    -- Init the graphics
    (_progName, _args) <- getArgsAndInitialize

    initialWindowSize $= Size 1000 1000
    initialWindowPosition $= Position 2000 300
    initialDisplayMode $= [
            RGBAMode,
            DoubleBuffered
        ]
    window <- createWindow "Crazy bouncy ball"
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
            frameHandler $ (realToFrac (time - lastTime), realToFrac (time - startTime))
            threadDelay 10000
            loop time
    forkIO $ loop startTime
    network <- compile $ createFrpNetwork
            frameAddHandler
            keyDownAddHandler
            keyUpAddHandler
            (makeSettableStateVar (refPrims $=))
    actuate network

    -- Main loop
    mainLoop

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

renderGraphicsPrimitive :: GraphicsPrimitive -> IO ()
renderGraphicsPrimitive (Rectangle (V2 left top) (V2 right bottom)) = renderPrimitive Quads $ do
    color3f 1 1 1
    vertex3f left top 0
    vertex3f right top 0
    vertex3f right bottom 0
    vertex3f left bottom 0
renderGraphicsPrimitive (Circle center radius) = renderPrimitive Polygon $ mapM_ vertexV2
    [ center + (V2 (radius * sin (2*pi*k/n)) (radius * cos (2*pi*k/n))) | k <- [1..n] ]
    where
        n= 1000

color3f :: Float -> Float -> Float -> IO ()
color3f r g b = color $ Color3 (realToFrac r) (realToFrac g) ((realToFrac b) :: GLfloat)

vertexV2 :: Vec2 -> IO ()
vertexV2 (V2 x y) = vertex3f x y 0

vertex3f :: Float -> Float -> Float -> IO ()
vertex3f x y z = vertex $ Vertex3 (realToFrac x) (realToFrac y) ((realToFrac z) :: GLfloat)

keyboardMouse :: Handler Key -> Handler Key -> KeyboardMouseCallback
keyboardMouse keyDownHandler _ key Down _ _ = keyDownHandler key
keyboardMouse _ keyUpHandler key Up _ _ = keyUpHandler key
keyboardMouse _ _ _ _ _ _ = return ()