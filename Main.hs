
-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import Data.ByteString (ByteString)
import Codec.Picture
import Graphics.Gloss.Juicy(fromImageRGBA8)
import Data.Maybe
import Codec.Picture.RGBA8
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State.Lazy
import System.Random
import Data.Traversable

xSize = 1024
ySize = 800
tileSize = 32
xTiles = xSize `div` tileSize
yTiles = ySize `div` tileSize
xTrans = (fromIntegral $ (- (xSize `div` 2))::Float) + 16
yTrans = (fromIntegral $ ySize `div` 2 ::Float) - 16
characterY :: Float
characterY = fromIntegral $ (ySize - 4 * tileSize)
floorY :: Float
floorY = fromIntegral $ ySize - (3*tileSize)
momentumLoss :: Float
momentumLoss = 3

data Tiles = Tiles {
    floorTile :: Picture
} deriving (Show)

data Particle = Particle {
    pos :: Float,
    momentum :: Float,
    accel :: Float
} deriving (Show)

data Bubble = Bubble {
    particleX :: Particle,
    particleY :: Particle,
    explode :: Particle,
    bcolor :: Color,
    bounces :: Int,
    size :: Int
} deriving (Show)

data World = World {
    allTiles :: Tiles,
    mainCharacter :: Particle,
    bubbles :: [Bubble],
    lastUpdate :: Float
} deriving (Show)

main    = do
    bubbles <- traverse (\x -> newBubble) [0..10]
    blocks <- readImage "graphics.bmp"
    either (putStrLn . show)
           (playGame bubbles . setupTiles)
           blocks
    return ()

setupTiles bmp = Tiles { floorTile = trimit (0,7) }
    where trimit = trimBlock $ fromDynamicImage bmp
-- tileAll [(x,7)|x <- [0..2]] $

trimBlock image (x,y) = fromImageRGBA8 $ trimImage image (tileSize,tileSize) (x*tileSize,y*tileSize)

tileAll :: [(Int,Int)] -> Image PixelRGBA8 -> [Picture]
tileAll tiles image = fmap (trimBlock image) tiles

playGame bubbles tiles = play
           (InWindow "Twin Terrors" (xSize, ySize)(300, 300)) black 50
           (World { allTiles = tiles, bubbles = bubbles, mainCharacter = mainStart, lastUpdate = 0 })
           render
           (execState . events)
           (execState . stepper)

mainStart = Particle 0 0 0
newBubble = do
    bounces <- fmap ((*5) . abs) randomIO :: IO Float
    colorI <- fmap (\x -> rem x 7) randomIO :: IO Int
    let c = case colorI of
            0 -> red
            1 -> orange
            2 -> yellow
            3 -> green
            4 -> blue
            5 -> white
            _ -> violet
    momentum <- fmap ((*3) . abs) randomIO :: IO Float
    size <- fmap (*32) randomIO :: IO Float
    pos <- fmap (* (fromIntegral xSize)) randomIO :: IO Float
    return Bubble { explode = Particle 0 0 0 , bcolor = c, bounces = floor bounces, particleX = Particle pos 0 0, particleY = Particle size (1+momentum+size/15) 50, size = floor size }


physics :: Float -> State Particle ()
physics time = do
    x <- gets pos
    m <- gets momentum
    a <- gets accel
    let a2 = time * a
    let newMomentum = case (a,m) of
          (0,m) | (abs m) < 0.1 -> 0
                | m > 0 -> m - 0.1
                | m < 0 -> m + 0.1
          _ -> m + a2
    modify (\w -> w { pos = x + m , momentum = newMomentum } )

stepper :: Float -> State World ()
stepper time = do
    newChar <- gets (execState (physics time) . mainCharacter)
    newBubbles <- gets (fmap (execState (bubblePhysics time)) . bubbles)
    modify (\w -> w { mainCharacter = newChar, lastUpdate = time, bubbles = newBubbles })

events :: Event -> State World ()
events (EventKey k s m _) = do
    newChar <- gets (execState (handleKey k s m) . mainCharacter)
    modify (\w -> w { mainCharacter = newChar })
events _ = return ()

handleKey :: Key -> KeyState -> Modifiers -> State Particle ()
handleKey (SpecialKey KeyLeft) s _ = accelChange s (-5)
handleKey (SpecialKey KeyRight) s _ = accelChange s 5
handleKey _ _ _ = return ()

render :: World -> Picture
render w = Pictures [
           (Translate xTrans yTrans
           $ Pictures ([
           (drawFloor tile1), (drawCharacter $ mainCharacter w)] ++ fmap drawBubble (bubbles w))),
           Text $ show $ lastUpdate w ]

    where tile1 = floorTile $ allTiles w


drawCharacter :: Particle -> Picture
drawCharacter w = Translate (pos w) (-characterY) $ Color white $ Circle 32

bubbleCollision :: Bubble -> Bool
bubbleCollision b = pos y > floorHeight && momentum y > 0
    where
        y = particleY b
        floorHeight = floorY - (fromIntegral (size b) * 2)

reverseMomentum :: Particle -> Particle
reverseMomentum p = p { momentum = -(momentum p) + momentumLoss }

startExplosion :: Particle -> Particle
startExplosion p = p { accel = 50 }

keepStill :: Particle -> Particle
keepStill p = p { momentum = 0, accel = 0 }

bubblePhysics :: Float -> State Bubble ()
bubblePhysics ticks = do
    bubbleX <- gets particleX
    bubbleY <- gets particleY
    exploder <- gets explode
    exploding <- gets ((/= 0) . pos . explode)
    bl <- gets bounces
    collided <- gets bubbleCollision
    let (explodeF,yF, bleft) = case (collided, bl) of
                            (True, 0) -> (startExplosion, keepStill, 0)
                            (True, l) -> (id, reverseMomentum, l-1)
                            _ -> (id, id, bl)
    let newExplode = explodeF exploder
    let newY = yF bubbleY
    modify (\b -> b { bounces = bleft, explode = doPhysics newExplode, particleX = doPhysics bubbleX, particleY = doPhysics newY })
    where
        doPhysics = execState (physics ticks)

drawBubble :: Bubble -> Picture
drawBubble b = Translate posx (-posy)
                $ Color (bcolor b)
                $ Circle $ (fromIntegral $ size b) * factor
    where
        (x,y) = (particleX b, particleY b)
        (posx,posy) = (pos x, pos y)
        factor = 1 + ((pos . explode) b)

drawLedgeAt :: (Int,Int) -> Picture -> Int -> Picture
drawLedgeAt (x,y) tile size = Translate (fromIntegral $ x*tileSize) (-(fromIntegral $ y*tileSize))
                                $ drawLedge tile size

drawLedge tile size = Pictures [ Translate (fromIntegral $ x*tileSize) 0 tile | x <- [0..size-1]]

drawMultiple (x,ys) (xsz,ysz) tile = Pictures [ drawLedgeAt (x,y) tile xsz | y <- [ys..ys+ysz-1] ]

drawFloor = drawMultiple (0,yTiles-3) (xTiles,3)

accelChange :: KeyState -> Float -> State Particle ()
accelChange key amount = do
            xa <- gets accel
            let newAcc = case key of
                        Up    | xa == amount -> 0
                        Down  -> amount
                        _     -> xa
            modify (\w -> w { accel = newAcc })

