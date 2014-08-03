module Picture (Picture (Region, Over, EmptyPic),
                Color (Black, Blue, Green, Cyan,
                       Red, Magenta, Yello, White),
                regionToGRegion, shapeToGRegion,
                drawRegionInWindow, drawPic, draw,
                module Region) where

import Draw
import Region
import SOE hiding (Region)
import qualified SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
   deriving Show

data NewRegion = Rect Side Side

drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r) = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = do drawPic w p2; drawPic w p1
drawPic w EmptyPic = return ()

regionToGRegion :: Region -> G.Region

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r =
   drawInWindow w
      (withColor c
          (drawRegion (regionToGRegion r)))

regToNReg :: Region -> NewRegion
regToNReg r = rToNR (1,1) r

rToNR :: (Float,Float) -> Region -> NewRegion
rToNR (x1, y1) (Shape (Rectangle sx sy))
    = Rec (x1 * sx) (y1 * sy)

rToNR (x1, y1) (Scale (x2, y2) r) =
   rToNR (x1 * x2, y1 * y2) r

regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0,0) (1,1) r



type Vector = (Float, Float)
regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s) = shapeToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r) =
   regToGReg loc (sx * u, sy * v) r

regToGReg (lx, ly) (sx, sy) (Translate (u, v) r) =
   regToGReg (lx + u * xs, ly + v * sy) (sx, sy) r

regToGReg loc sca Empty =
   createRectangle (0,0)(0,0)

regToGReg loc sca (r1 `Union` r2) =
   primGReg loc sca r1 r2 orRegion

regToGReg loc sca (r1 `Intersect` r2) =
   primGReg loc sca r1 r2 andRegion

regToGReg loc sca (Complement r) =
   primGReg loc sca r winRect diffRegion

winRect :: Region
winRect = Whap (Rectangle (pixelToInch xWin) (pixelToInch yWin))

primGReg loc sca r1 r2 op =
   let gr1 = regToGReg loc sca r1
       gr2 = regToGReg loc sca r2
   in op gr1 gr2


shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx, ly) (sx, sy) (Rectangle s1 s2) =
   createRectangle (trans (-s1/2, -s2/2)) (trans (s1/2, s2/2))
   where trans (x,y) = (xWin2 + inchToPixel ((x + lx) * sx),
                        yWin2 - inchToPixel ((y + ly) * sy))
shapeToGRegion (lx, ly) (sx, sy) (Ellipse r1 r2) =
   createEllipse (trans (-r1, -r2)) (trans (s1/2, s2/2))
   where trans (x,y) = (xWin2 + inchToPixel ((x + lx) * sx),
                        yWin2 - inchToPixel ((y + ly) * sy))

xWin2 = xWin `div` 2
yWin2 = yWin `div` 2
