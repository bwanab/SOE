import SOE

minSize :: Int
minSize = 2

equiTri :: Int -> Int -> Int -> Int -> [(Int, Int)]
equiTri x y size dir =
   let s :: Double
       s = fromIntegral size
       c = round (sqrt (2 * s ^ 2 * 1.5))    -- 1 - cos(pi * 2/3) => 1.5
       t = round (s * 0.5)                   -- cos(pi / 3) => 0.5
       p1 = (x, y - (dir * size))
       p2 = (x - c `div` 2, (y + (dir * t)))
       p3 = (x + c `div` 2, (y + (dir * t)))
   in [p1, p2, p3]

drawTri :: Window -> Color -> [(Int, Int)] -> [(Int, Int)] -> IO()
drawTri w c p1 p2 =
    do
        (drawInWindow w (withColor c (polygon p1)))
        (drawInWindow w (withColor c (polygon p2)))


fillTri :: Window -> [Color] -> Int -> Int -> Int -> IO()
fillTri w (c:cs) x y size =
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
                drawTri w c p1 p2
                fillTri w cs x1 y1 s
                fillTri w cs x2 y2 s
                fillTri w cs x3 y3 s
                fillTri w cs x4 y4 s
                fillTri w cs x5 y5 s
                fillTri w cs x6 y6 s
      else return ()

snowflake :: Window -> Int -> Int -> Int -> IO()
snowflake w x y size =
     let colors = [Blue, Green, Cyan, Red, Magenta, Yellow]
     in fillTri w colors x y size

main =
  runGraphics(
    do w <- openWindow "snowflake" (800, 800)
       snowflake w 400 400 200
       k <- getKey w
       closeWindow w
    )
