module Region (Region (Shape, Translate, Scale, Complement,
                       Union, Intersect, Halfplane, Empty),
               Coordinate, containsS, containsR,
               module Shape) where
import Shape

infixr 5 `Union`
infixr 6 `Intersect`

data Region = Shape Shape
            | Translate Vector Region
            | Scale Vector Region
            | Complement Region
            | Union Region Region
            | Intersect Region Region
            | Halfplane Point Point
            | Empty  deriving Show

type Point = (Float, Float)
type Vector = (Float, Float)
type Coordinate = (Float, Float)
type Ray = (Coordinate, Coordinate)

containsS :: Shape -> Coordinate -> Bool
containsS (Rectangle s1 s2) (x, y) =
   let t1 = s1 / 2
       t2 = s2 / 2
   in  -t1 <= x && x <= t1 && -t2 <= y && y <= t2

containsS (Ellipse r1 r2) (x, y) =
   (x / r1) ^ 2 + (y / r2) ^ 2 <= 1

containsS (Polygon points) p =
    let pts = if isClockwise points then points else reverse points
        leftOfList = map leftOfp (zip pts (tail pts ++ [head pts]))
        leftOfp r = isLeftOf p r
    in and leftOfList
containsS (RtTriangle s1 s2) p =
    containsS (Polygon [(0, 0),(s1, 0),(0,s2)]) p


isLeftOf :: Point -> Ray -> Bool
isLeftOf (px, py) ((ax, ay),(bx, by)) =
    let (s, t) = (px - ax, py - ay)
        (u, v) = (px - bx, py - by)
    in s * v < u * t

containsR :: Region -> Coordinate -> Bool
containsR (Shape s) p = containsS s p
containsR (Translate (u, v) r) (x,y) = containsR r (x - u, y - v)
containsR (Scale (u,v) r) (x,y) = containsR r (x / u, y / v)
containsR (Complement r) p = not (containsR r p)
containsR Empty p = False
containsR (Union r1 r2) p = containsR r1 p || containsR r2 p
containsR (Intersect r1 r2) p = containsR r1 p && containsR r2 p
containsR (Halfplane p1 p2) p = isLeftOf p (p1, p2)


-- ex 8.3
annulus :: Radius -> Radius -> Region
annulus r1 r2 =
   let (r1', r2') = if (r1 > r2) then (r1, r2) else (r2, r1)
       a r1 r2 =  let c1 = Ellipse r1 r1
                      c2 = Ellipse r2 r2
                  in Intersect (Shape c1) (Shape c2)
   in a r1' r2'

-- ex 8.7
flipX :: Region -> Region
flipX r = Scale (-1, 1) r

flipY :: Region -> Region
flipY r = Scale (1, -1) r
