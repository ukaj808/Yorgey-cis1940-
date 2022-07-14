module Yorgey.Hw04 where

fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (subtract 2) . filter even

fun1' :: [Integer] -> Integer
fun1' [] = 1 
fun1' (x:xs)
  | even x = (x - 2) * fun1' xs
  | otherwise = fun1' xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 x | even x = x + fun2 (x `div` 2)
       | otherwise = fun2 (3 * x + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate g
    where g = (\x -> if even x then x `div` 2 else 3 * x + 1)


