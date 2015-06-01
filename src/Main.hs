module Main where

import Graphics.UI.GLUT hiding (normalize)
import Linear
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Control.Concurrent


type Vec2 = V2 Float

data Wall = WallT | WallB | WallL | WallR

data WorldSate = WorldState {
    pos1 :: Vec2
}

velToPos :: forall t. Frameworks t => Vec2 -> Behavior t Vec2 -> Event t Float -> Behavior t Vec2
velToPos initialPos velB frameE = position <$> changesTimePosVel <*> (stepper 0 frameE)
    where
        initialTimePosVel = (0, initialPos, zero)
        position (time, pos, vel) currentTime = pos + ((currentTime - time) *^ vel)
        -- (time, pos, vel)
        changesTimePosVel = accumB initialTimePosVel (step <$> velB <@> frameE)
        step newVel newTime tpv = (newTime, position tpv newTime, newVel)

createFrpNetwork :: forall t. Frameworks t => AddHandler Float -> AddHandler Key -> AddHandler Key -> SettableStateVar [GraphicsPrimitive] -> Moment t ()
createFrpNetwork frameAddHandler keyDownAddHandler keyUpAddhandler prims = do
    frameE    <- fromAddHandler frameAddHandler
    keyDownE  <- fromAddHandler keyDownAddHandler
    keyUpE    <- fromAddHandler keyUpAddhandler

    let

        radius = 0.05

        timeB = stepper 0 frameE
        {-
        keysPressedB = accumB Set.empty $
            (Set.delete <$> keyUpE) `union`
            (Set.insert <$> keyDownE)

        vel1 = toTotVel <$> keysPressedB where
            toTotVel keys = (0.3 *^) $ normalize $ sum $ map toVel (Set.toList keys)
            toVel (SpecialKey KeyLeft)   = V2 (-1) 0
            toVel (SpecialKey KeyRight)  = V2 1 0
            toVel (SpecialKey KeyUp)     = V2 0 1
            toVel (SpecialKey KeyDown)   = V2 0 (-1)
            toVel _                      = V2 0 0

        pos1B = velToPos zero vel1 frameE
        -}

        -- Event Wall
        colBallE = filterE (not . null) (checkCollision <$> posBallB <@ frameE)
        checkCollision (V2 x y) =
            [WallT    | y > 1 - radius] ++
            [WallB    | radius - 1 > y] ++
            [WallL    | radius - 1 > x] ++
            [WallR    | x > 1 - radius]

        velBallB = stepper (V2 4 10) (newVel <$> velBallB <@> colBallE)
        newVel (V2 h v) (WallT:other) = newVel (V2 h (-(abs v))) other
        newVel (V2 h v) (WallB:other) = newVel (V2 h (abs v)) other
        newVel (V2 h v) (WallL:other) = newVel (V2 (abs h) v) other
        newVel (V2 h v) (WallR:other) = newVel (V2 (-(abs h)) v) other
        newVel vel [] = vel

        posBallB = velToPos zero velBallB frameE



        worldStateB = (\pos1 -> WorldState { pos1 = pos1 }) <$> posBallB

    --reactimate $ (putStrLn "Hello!") <$ frameE
    reactimate $ (\worldState -> do
            prims $= [
                    --Rectangle (V2 (x-0.1) (y-0.1)) (V2 (x+0.1) (y+0.1))
                    Circle (pos1 worldState) radius
                ]
        ) <$> (worldStateB <@ frameE)





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
            frameHandler $ realToFrac (time - startTime)
            threadDelay 17
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
    let
        center = (left + right) / 2
        r = (center + 1) / 2
        g = (center + 1) / 2
        b = (center + 1) / 2
    color3f r g b
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