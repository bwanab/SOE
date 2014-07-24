module Tree where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

v :: Tree Int
v = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4))

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2

treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

treeHeight :: Tree a -> Int
treeHeight (Leaf _) = 0
treeHeight (Branch t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)


-- ex 7.1

treeFold :: Tree a -> (a -> b -> b) -> (b -> b -> b) -> b -> b
treeFold (Leaf x) op _ i = op x i
treeFold (Branch t1 t2) iop top i = (treeFold t1 iop top i) `top` (treeFold t2 iop top i)

fringe1 :: Tree a -> [a]
fringe1 t = treeFold t (:) (++) []

treeSize1 :: Tree a -> Int
treeSize1 t = treeFold t (\_ i -> i) (+) 1

treeHeight1 :: Tree a -> Int
treeHeight1 t = treeFold t (\_ i -> i) (\t1 t2 -> 1 + max t1 t2) 0

-- ex 7.2

-- ex 7.3

-- ex 7.4

-- ex 7.5
