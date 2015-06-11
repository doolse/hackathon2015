
-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import Data.ByteString (ByteString)
import Codec.Picture
import Graphics.Gloss.Juicy(fromImageRGBA8)
import Data.Maybe
import Codec.Picture.RGBA8
import Graphics.Gloss.Data.ViewPort

xSize = 640
ySize = 480
tileSize = 32
xTiles = xSize `div` tileSize
yTiles = ySize `div` tileSize

data OurModel = TranslateModel(Float,Float)

main    = do
    blocks <- readImage "/home/jolz/haskell/jolsehackathon/graphics.bmp"
    either (putStrLn . show)
           (\bmp ->
             simulate
             (InWindow "Twin Terrors" (xSize, ySize)(10, 10))
             black
             15
             (TranslateModel (0,0))
             (picture $ tileAll [(x,7)|x <- [0..2]] $ fromDynamicImage bmp)
             stepper
             )
             -- $ )
           blocks
    return ()

stepper :: ViewPort -> Float -> OurModel -> OurModel
stepper vp _ model = TranslateModel $ viewPortTranslate vp

tileAll :: [(Int,Int)] -> Image PixelRGBA8 -> [Picture]
tileAll tiles image = fmap (fromImageRGBA8 . trimBlock image) tiles

trimBlock image (x,y) = trimImage image (tileSize,tileSize) (x*tileSize,y*tileSize)

picture :: [Picture] -> OurModel -> Picture
picture	(tile1:tile2:_) (TranslateModel(x,y)) = Translate x y $ drawFloor tile1

drawLedgeAt :: (Int,Int) -> Picture -> Int -> Picture
drawLedgeAt (x,y) tile size = Translate (fromIntegral $ x*tileSize) (-(fromIntegral $ y*tileSize))
                                $ drawLedge tile size

drawLedge tile size = Pictures [ Translate (fromIntegral $ x*tileSize) 0 tile | x <- [0..size-1]]

drawMultiple (x,ys) (xsz,ysz) tile = Pictures [ drawLedgeAt (x,y) tile xsz | y <- [ys..ys+ysz-1] ]

drawFloor = drawMultiple (0,yTiles-3) (xTiles,3)
