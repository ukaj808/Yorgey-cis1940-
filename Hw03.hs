module Yorgey.Hw03 where

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n xs
  | n <= 0 = xs
  | otherwise = snd unzipped
  where unzipped = unzip filtered
        filtered = filter (\(x,_) -> x `mod` n == 0) zipped
        zipped = zip [1..] xs

skips :: [a] -> [[a]]
skips [] = []
skips xs = zipWith everyNth [1..length xs] $ repeat xs

snd' :: (a,a,a) -> a
snd' (x,y,z) = y

hasMaxima :: Ord a => (a,a,a) -> Bool
hasMaxima (x,y,z) = y > x && y > z

getAllTriples :: [a] -> [(a,a,a)]
getAllTriples (x:rest@(y:z:xs)) = (x,y,z):getAllTriples rest 
getAllTriples _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = snd' (unzip3 (filter hasMaxima (getAllTriples xs)))

countOccur :: Eq a => [a] -> a -> Integer
countOccur [] _ = 0
countOccur (x:xs) y = if y == x
                      then (countOccur xs y) + 1
                      else countOccur xs y
mapStar :: Eq a => a -> a -> Char
mapStar x y = if x == y then '*' else ' '

histoTally :: [Integer] -> String
histoTally [] = "          \n" 
--histoTally xs = zipWith (mapStar $ countOccur) [0..9] xs 
histoTally xs = zipWith mapStar (map (countOccur xs) [0..9]) [0..9]

histogram :: [Integer] -> String
histogram [] = []
histogram xs = (fst tup) ++ "\n" ++ (snd tup)
    where tup = unzip (zip (repeat '=') ['0'..'9'])
