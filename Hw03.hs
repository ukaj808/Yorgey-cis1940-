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

mapStar :: Integer -> Row -> Char
mapStar numOccurs row = if numOccurs >= row then '*' else ' '

type Row = Integer
type HistogramInput = [Integer]

rowMap :: HistogramInput -> Row -> String
rowMap _ (-1)     = "0123456789"
rowMap _ 0        = "==========\n"
rowMap [] _       = "          "
rowMap input row  =
    mapStar (countOccur input 0) row :
    mapStar (countOccur input 1) row :
    mapStar (countOccur input 2) row :
    mapStar (countOccur input 3) row :
    mapStar (countOccur input 4) row :
    mapStar (countOccur input 5) row :
    mapStar (countOccur input 6) row :
    mapStar (countOccur input 7) row :
    mapStar (countOccur input 8) row :
    mapStar (countOccur input 9) row :
    ['\n']

histogram :: [Integer] -> String 
histogram input =
    concat $ zipWith rowMap (repeat input) [maxOccur, maxOccur - 1..(-1)]
    where maxOccur = maximum (map (countOccur input) [0..9]) 
