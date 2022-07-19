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

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = (-1)
height (Node h _ _ _) = h

leftTree :: Tree a -> Tree a
leftTree (Node h lt x rt) = lt

foldTree' :: [a] -> Tree a
foldTree' [] = Leaf
foldTree' [x] = Node 0 Leaf x Leaf
foldTree' (x:xs) = Node height (foldTree' lt) x (foldTree' rt)
    where lt = fst split
          rt = snd split
          split = splitAt n xs 
          n = (length xs) `div` 2
          height = ceiling $ log $ fromIntegral $ length (x:xs)

-- recursion with fold?
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h lt y rt)
  | height lt < height rt =  Node h (insert x lt) y rt
  | height lt > height rt = Node h lt y (insert x rt)
  | otherwise = Node ((height tree + 1)) (insert x lt) y rt
  
      where tree = (insert x lt)

xor :: [Bool] -> Bool
xor = foldr (\x acc -> x /= acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

--sieveSundaram :: Integer -> [Integer]
sieveSundaram = filter isOddPrime . genTo . (+2) . (*2)

    where genTo n = [1..n] 
          isOddPrime n = odd n && 
              (foldr (\x acc -> if acc == False 
                                then False
                                else x `mod` n /= 0) True [2..n]) 
