module Chap9 where

-- ex 9.4
applyEach :: [(a -> a)] -> a -> [a]
applyEach fs v = map (\f -> f v) fs

applyAll :: [(a -> a)] -> a -> a
applyAll fs v = foldr (\f x -> f x) v fs

-- ex 9.7

twice :: (a -> a) -> a -> a
twice f v = f (f v)

-- ex 9.8

power :: (a -> a) -> Int -> a -> a
power _ 0 v = v
power f n v = f (power f (n - 1) v)

power1 :: (a -> a) -> Int -> a -> a
power1 f n v = foldr (\_ x -> f x) v [1..n]

-- ex 9.9
fix :: (a -> a) -> a
fix f = f (fix f)

remainder' :: Integer -> Integer -> Integer
remainder' a b = fix (r b) a
    where r b = (\f x -> if x < b then x else f (x - b))

-- ex 9.10

r1 xs = map (\x -> (x + 1) / 2) xs
r2 = map ((+1).(/2))

-- ex 9.11

r3 f g xs = map f (map g xs)
r4 f g = map (f.g)

r5 xs = map (+1) (map (/2) xs)
