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

data MyList a = NilList | ConsList a (MyList a)
  deriving (Show)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x (xs)) = x : streamToList xs

listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x (xs)) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons y (streamFromSeed f y)
  where
    y = f x

nats :: Stream Integer
nats = listToStream [0..] 

instance Show a => Show (Stream a) where
  show (Cons x (xs)) = take 20 ("Cons x " ++ (show xs))

