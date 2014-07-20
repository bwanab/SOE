import SOE

fillTri :: Window -> Int -> Int -> Int -> IO()
fillTri w x y size =
    drawInWindow w (withColor Blue
                 (polygon [(x,y),(x + size,y),(x,y - size),(x,y)]))

minSize :: Int
minSize = 8

sierpinskiTri :: Window -> Int -> Int -> Int -> IO()
sierpinskiTri w x y size =
     if size <= minSize
     then fillTri w x y size
     else let size2 = size `div` 2
          in do sierpinskiTri w x y size2
                sierpinskiTri w x (y - size2) size2
                sierpinskiTri w (x + size2) y size2

main =
  runGraphics(
    do w <- openWindow "Sierpinski's Triangle" (400, 400)
       sierpinskiTri w 50 300 256
       k <- getKey w
       closeWindow w
    )
