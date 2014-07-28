module Tree where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

v :: Tree Int
v = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4))

n :: Tree Char
n = Branch (Branch (Leaf 'a') (Leaf 'b')) (Branch (Leaf 'c') (Leaf 'd'))

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

data ITree a = ILeaf | IBranch a (ITree a) (ITree a) deriving Show

takeTree :: Int -> ITree a -> ITree a
takeTree 0 _ = ILeaf
takeTree n ILeaf = ILeaf
takeTree n (IBranch a t1 t2) = IBranch a (takeTree (n-1) t1) (takeTree (n-1) t2)

-- ex 7.3

iTreeFold :: (a -> b -> b) -> b -> ITree a -> b
iTreeFold _ b ILeaf = b
iTreeFold  f i (IBranch a t1 t2) =
    iTreeFold f (f a (iTreeFold f i t2)) t1

iTreeRepeat :: a -> ITree a
iTreeRepeat a = t
    where t = IBranch a (t) (t)

-- ex 7.4

treeZip :: Tree a -> Tree b -> Tree (a,b)
treeZip t1 t2 = treeZipWith (\a b -> (a,b)) t1 t2

treeZipWith :: (a -> b -> c ) -> Tree a -> Tree b -> Tree c
treeZipWith f (Leaf a) (Leaf b) = Leaf $ f a b
treeZipWith f (Branch t11 t12) (Branch t21 t22) = Branch (treeZipWith f t11 t21) (treeZipWith f t12 t22)

-- ex 7.5

data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr |
            Let String Expr Expr | V String deriving Show

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
evaluate (Let s val (C x)) = x
evaluate (Let s val (e1 :+ e2)) = (evaluate (Let s val e1)) + (evaluate (Let s val e2))
evaluate (Let s val (e1 :- e2)) = (evaluate (Let s val e1)) - (evaluate (Let s val e2))
evaluate (Let s val (e1 :* e2)) = (evaluate (Let s val e1)) * (evaluate (Let s val e2))
evaluate (Let s val (e1 :/ e2)) = (evaluate (Let s val e1)) / (evaluate (Let s val e2))
evaluate (Let s val (V s2)) | s == s2 = evaluate val
                            | otherwise = error "undefined variable"

testExpr1 = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)
testExpr2 = Let "x" (C 5) (V "x" :+ (V "x"))
testExpr3 = Let "x" (V "y") (Let "y" (C 6.0) (V "x"))
