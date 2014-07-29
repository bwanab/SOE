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
remainder' = fix (\f a b -> if a < b then a else f (a - b) b)
