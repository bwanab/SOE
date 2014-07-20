import SOE

equiTri :: Int -> Int -> Int -> Int -> [(Int, Int)]
equiTri x y size dir =
   let c = size * round (sqrt (1 - (cos (0.6667 * pi))))
       t = size - round (cos (0.3333 * pi))
       y1 = y - (dir * t `div` 3)
       p1 = (x, y1 - (dir * size))
       p2 = (x - c, (y1 + (dir * t)))
       p3 = (x + c, (y1 + (dir * t)))
   in [p1, p2, p3]

fillTri :: Window -> Int -> Int -> Int -> IO()
fillTri w x y size =
  do
    (drawInWindow w (withColor Blue
                 (polygon (equiTri x y size 1))))
    (drawInWindow w (withColor Blue
                  (polygon (equiTri x y size (-1)))))

minSize :: Int
minSize = 8

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
