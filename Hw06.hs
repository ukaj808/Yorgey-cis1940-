module Yorgey.Hw06 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = [f i | i <- [0 ..]]
  where
    f i
      | i <= 0 = 0
      | i <= 1 = 1
      | otherwise = fibs2 !! (i - 1) + fibs2 !! (i - 2)
