module Shape where

type Side = Float
type Radius = Float
type Vertex = (Float, Float)

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show

square :: Float -> Shape
square s = Rectangle s s

circle :: Float -> Shape
circle r = Ellipse r r

-- E2.1 Define rectangle and rtTriangle in terms of Polygon

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = let v1 = (0,0)
                      v2 = (s1, 0)
                      v3 = (s1, s2)
                      v4 = (0, s2)
                  in Polygon [v1, v2, v3, v4]

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = let v1 = (0,0)
                       v2 = (s1, 0)
                       v3 = (0, s2)
                   in Polygon [v1, v2, v3]

-- E2.2

regularPolygon :: Int -> Side -> Shape
--regularPolygon :: Int -> Side -> [Vertex]
regularPolygon n s =
   let theta = (2 * pi) / fromIntegral n
       theta1 = theta / 2
       --r = s / sqrt (2 * (1 - (cos theta)))
       r = s / (2 * sin theta1)
       v :: Int -> Vertex
       v k = let k1 = fromIntegral k
             in (r * (cos (k1 * theta)), r * (sin (k1 * theta)))
   in Polygon $ map v [0..n]

testVerticesConcave :: [Vertex]
testVerticesConcave = [(-20, 20),(-10,40),(20,30),(15,20),(10,10),(-10,10)]
testPolygonConcave = Polygon testVerticesConcave

testVerticesConvex :: [Vertex]
testVerticesConvex = [(-20, 20),(-10,40),(20,30),(25,20),(10,10),(-10,10)]
testPolygonConvex = Polygon testVerticesConvex

-- special trapezoid determined by two vertices and the y == 0 vertex equivalents.
trapArea :: Vertex -> Vertex -> Float
trapArea (x1,y1) (x2,y2)  =
    (x2 - x1) * (y2 + y1) / 2

area :: Shape -> Float
area (Rectangle s1 s2) = s1 * s2
area (RtTriangle s1 s2) = s1 * s2 / 2
area (Ellipse r1 r2) = pi * r1 * r2
-- this is the exercise 2.5 method
area (Polygon vs) =
    let ps = zip (init vs) (tail vs)
    in sum (map (\(x,y) -> trapArea x y) ps)

-- recursive version
-- area (Polygon vs) = polyArea vs
--    where polyArea :: [Vertex] -> Float
--          polyArea (v1:v2:vs') = (trapArea v1 v2) + polyArea (v2:vs')
--          polyArea _ = 0

slope :: Vertex -> Vertex -> Float
slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)


angles :: [Vertex] -> [Float]
angles vs = angles' (vs ++ take 2 vs) where
       angles' (v1:v2:v3:vs) = (angle v1 v2 v3):(angles' (v2:v3:vs))
       angles' _ = []

subtractVertices :: Vertex -> Vertex -> Vertex
subtractVertices (x1,y1) (x2, y2) = (x1 - x2, y1 - y2)

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

angle :: Vertex -> Vertex -> Vertex -> Float
angle v1 v2 v3 =
  let a = distBetween v1 v2
      b = distBetween v3 v2
      c = distBetween v1 v3
  in  acos ((a ^ 2 + b ^ 2 - c ^ 2) / (2 * a * b))

crossProduct :: Vertex -> Vertex -> Vertex -> Float
crossProduct (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y2)
                                          - (y2 - y1) * (x3 - x2)

crossProducts :: [Vertex] -> [Float]
crossProducts (v1:v2:vs) =
    let z = zip3 (v1:v2:vs) ((v2:vs) ++ [v1]) (vs ++ [v1, v2])
    in  map (\(v1, v2, v3) -> crossProduct v1 v2 v3) z

-- exercise 2.4
-- the cross products of all the angles should be the same sign
convex :: Shape -> Bool
convex (Polygon vs) =
    let z = crossProducts vs
    in (and $ map (< 0) z) || (and $ map (> 0) z)

sides :: [Vertex] -> [Side]
sides vs = zipWith distBetween vs (tail vs ++ [head vs])
