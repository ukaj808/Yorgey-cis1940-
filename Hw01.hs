module Yorgey.Hw01 where

import Data.Char (digitToInt)

digitToInteger :: Char -> Integer
digitToInteger x = toInteger (digitToInt x)

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = map digitToInteger (show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = snd (unzip (map (\(x, y) -> if x /= 0 && x `mod` 2 /= 0 then (x, 2 * y) else (x, y)) (zip [0 ..] xs)))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum (toDigits (read (concat (map show xs))))

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 start temp end = []
hanoi 1 start temp end = [(start, end)]
hanoi n start temp end = concat [(hanoi (n - 1) start end temp), (hanoi 1 start temp end), (hanoi (n - 1) temp start end)]

hanoiCalc :: Integer -> Int
hanoiCalc n = length (hanoi n "a" "b" "c")

main :: IO ()
main = do
  putStrLn "How many disc's?"
  numDiscStr <- getLine
  let result = show (hanoiCalc (read numDiscStr))
  putStrLn result
