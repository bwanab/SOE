import SOE

minSize :: Int
minSize = 4

equiTri :: Int -> Int -> Int -> Int -> [(Int, Int)]
equiTri x y size dir =
   let s :: Double
       s = fromIntegral size
       c = round (sqrt (2 * s ^ 2 * 1.5))
       t = round (s * 0.5)
       y1 = y - round (s / 3)
       p1 = (x, y1 - (dir * size))
       p2 = (x - c `div` 2, (y1 + (dir * t)))
       p3 = (x + c `div` 2, (y1 + (dir * t)))
   in [p1, p2, p3]

drawTri :: Window -> [(Int, Int)] -> [(Int, Int)] -> IO()
drawTri w p1 p2 =
    do
        (drawInWindow w (withColor Blue (polygon p1)))
        (drawInWindow w (withColor Blue (polygon p2)))


fillTri :: Window -> Int -> Int -> Int -> IO()
fillTri w x y size =
  let
      p1 = equiTri x y size 1
      p2 = equiTri x y size (-1)
  in
      if size > minSize
      then let
                s = size `div` 3
                (x1, y1) = p1 !! 0
                (x2, y2) = p1 !! 1
                (x3, y3) = p1 !! 2
                (x4, y4) = p2 !! 0
                (x5, y5) = p2 !! 1
                (x6, y6) = p2 !! 2
           in do
                drawTri w p1 p2
                fillTri w x1 y1 s
                fillTri w x2 y2 s
                fillTri w x3 y3 s
                fillTri w x4 y4 s
                fillTri w x5 y5 s
                fillTri w x6 y6 s
      else return ()

snowflake :: Window -> Int -> Int -> Int -> IO()
snowflake w x y size =
     fillTri w x y size

main =
  runGraphics(
    do w <- openWindow "snowflake" (400, 400)
       snowflake w 200 200 100
       k <- getKey w
       closeWindow w
    )
