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
testVerticesConcave = [(-20, 20),(-10,40),(20,30),(15,20),(10,10),(-10,10),(-20,20)]
testPolygonConcave = Polygon testVerticesConcave

testVerticesConvex :: [Vertex]
testVerticesConvex = [(-20, 20),(-10,40),(20,30),(25,20),(10,10),(-10,10),(-20,20)]
testPolygonConvex = Polygon testVerticesConvex

trapArea :: Vertex -> Vertex -> Float
trapArea (x1,y1) (x2,y2)  =
    (x2 - x1) * (y2 + y1) / 2

area :: Shape -> Float
area (Polygon vs) = polyArea vs
   where polyArea :: [Vertex] -> Float
         polyArea (v1:v2:vs') = (trapArea v1 v2) + polyArea (v2:vs')
         polyArea _ = 0

slope :: Vertex -> Vertex -> Float
slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)


angles :: [Vertex] -> [Float]
angles (v1:v2:v3:vs) = (angle v1 v2 v3):(angles (v2:v3:vs))
angles _ = []

subtractVertices :: Vertex -> Vertex -> Vertex
subtractVertices (x1,y1) (x2, y2) = (x1 - x2, y1 - y2)

distance :: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

angle :: Vertex -> Vertex -> Vertex -> Float
angle v1 v2 v3 =
  let a = distance v1 v2
      b = distance v3 v2
      c = distance v1 v3
  in  acos ((a ^ 2 + b ^ 2 - c ^ 2) / (2 * a * b))
