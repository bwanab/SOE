module Chap5 where

-- ex5.3
mylength :: [a] -> Int
mylength s = foldr (\_ x -> x + 1) 0 s

-- ex5.4
f1 = (\fs x -> map (\f -> f x ) fs)
f2 = (map (*) [1,2,3,4])

-- ex5.5.1
doubleEach :: [Int] -> [Int]
doubleEach xs = map (* 2) xs

-- ex5.5.2
pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne xs = map (\n -> (n, n + 1)) xs

-- ex5.5.3 - non-recursive
addEachPair1 :: [(Int, Int)] -> [Int]
addEachPair1 xs = map (\(x, y) -> x + y) xs

-- ex5.5.3 - recursive
addEachPair2 :: [(Int, Int)] -> [Int]
addEachPair2 [] = []
addEachPair2 (x:xs) =
   let (x1,y1) = x
       a = x1 + y1
   in a:addEachPair2 xs

-- ex5.6 - non-recursive
mList1 :: (Int -> Int -> Bool) -> Int -> [Int] -> Int
mList1 op dir xs = foldr (\x a -> if x `op` a then x else a) (dir * 9999999) xs

maxList1 :: [Int] -> Int
maxList1 xs = mList1 (>) (-1) xs

minList1 :: [Int] -> Int
minList1 xs = mList1 (<) 1 xs


-- ex5.6 - recursive
mList2 :: (Int -> Int -> Bool) -> Int -> [Int] -> Int
mList2 op dir xs = mx2 (dir * 999999) xs
   where mx2 acc [] = acc
         mx2 acc (x:xs') =
             let z = if x `op` acc then x else acc
             in mx2 z xs'

maxList2 :: [Int] -> Int
maxList2 xs = mList2 (>) (-1) xs

minList2 :: [Int] -> Int
minList2 xs = mList2 (<) 1 xs

-- ex5.7 - non-recursive
addPairsPointwise1 :: [(Int,Int)] -> (Int, Int)
addPairsPointwise1 xs = foldr (\(x1,x2) (a1,a2) -> (x1 + a1, x2 + a2)) (0,0) xs

-- ex5.7 - recursive

addPairsPointwise2 :: [(Int,Int)] -> (Int, Int)
addPairsPointwise2 xs = addPPW (0,0) xs
    where addPPW acc [] = acc
          addPPW (a1,a2) ((x1,x2):xs) = addPPW (a1 + x1, a2 + x2) xs

-- ex5.8

encrypt :: [Char] -> [Char]
encrypt cs = map (\c -> toEnum ((fromEnum c) + 1)) cs

decrypt :: [Char] -> [Char]
decrypt cs = map (\c -> toEnum ((fromEnum c) - 1)) cs

-- ex5.9
makeChange :: Int -> [Int] -> [Int]
makeChange val units = mkChange val [] units
    where mkChange _ acc [] = reverse acc
          mkChange val acc (u:us) =
              let n = val `div` u
                  r = val `rem` u
              in mkChange r (n:acc) us
