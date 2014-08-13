module Konigsberg where

type Place = Int

data Bridge = Bridge Int Place Place deriving (Show,Eq,Ord)
data Places = Places {place :: Place
                     ,connections :: [Bridge]
                     } deriving (Show)
b1 = Bridge 1 1 2
b2 = Bridge 2 1 2
b3 = Bridge 3 2 4
b4 = Bridge 4 1 3
b5 = Bridge 5 1 3
b6 = Bridge 6 1 4
b7 = Bridge 7 3 4
{- there's no solution for Konigsberg with 7 bridges, but if we add an eighth
   which makes the degree even (either 0 or 2 places having and odd number of
   bridges), then there are many solutions
   -}

b8 = Bridge 8 3 4
bridges = [b1,b2,b3,b4,b5,b6,b7]
--bridges8 = [b1,b2,b3,b4,b5,b6,b7,b8]

find :: Bridge -> [Bridge] -> Bool
find _ [] = False
find a (b:bs) = if a == b then True else find a bs

bridgesOnPlace :: [Bridge] -> Place -> [Bridge]
bridgesOnPlace bs p = [(Bridge bn p1 p2) | (Bridge bn p1 p2) <- bs, p == p1 || p == p2]

places :: [[Bridge]]
places = map (bridgesOnPlace bridges) [1..4]

data BridgeTree = Leaf [Bridge] | Branch [BridgeTree] deriving (Show)

{- first solution effort was to start at a bridge and traverse. Doesn't work
   since you lose directionality in your path.
   -}

-- buildTree2 :: [Bridge] -> Bridge -> BridgeTree
-- buildTree2 bs (Bridge b p1 p2) =
--     Branch [Branch [buildTree1 bs nb | nb <- (places !! (p1 - 1))]
--            ,Branch [buildTree1 bs nb | nb <- (places !! (p2 - 1))]]

-- buildTree1 :: [Bridge] -> Bridge -> BridgeTree
-- buildTree1 bs b
--     | find b bs = Leaf bs
--     | otherwise = buildTree2 (b:bs) b


-- buildTree :: Bridge -> BridgeTree
-- buildTree b = buildTree1 [] b

-- buildTrees = map buildTree bridges

{- second and working solution is to start at a place, create a path for each of the
   bridges that connect to that place, then starting at the place on the other side
   and recursing through.
   -}

buildTreeX2 :: [Bridge] -> Bridge -> Place -> BridgeTree
buildTreeX2 bs (Bridge b p1 p2) p = buildTreeX bs $ if p == p1 then p2 else p1

buildTreeX1 :: [Bridge] -> Bridge -> Place -> BridgeTree
buildTreeX1 bs b p
    | find b bs = Leaf bs
    | otherwise = buildTreeX2 (b:bs) b p

buildTreeX bs p = Branch (map (\(p1,b) -> buildTreeX1 bs b p1) $ zip (repeat p) (places !! (p - 1)))

buildTree p = buildTreeX [] p

bridgeFringe1 :: [BridgeTree] -> [[Bridge]]
bridgeFringe1 [] = []
bridgeFringe1 (bt:bts) = bridgeFringe bt ++ bridgeFringe1 bts

bridgeFringe :: BridgeTree -> [[Bridge]]
bridgeFringe (Leaf x) = [x]
bridgeFringe (Branch x) = bridgeFringe1 x

test = let a = concat $ map (\p -> (bridgeFringe . buildTree) p) [1..4]
           nb = length bridges
           n = length $ filter (\x -> length x == nb) a
       in if n > 0 then putStrLn ("there are " ++ show n ++ " paths")
                   else putStrLn "there are no valid paths"
