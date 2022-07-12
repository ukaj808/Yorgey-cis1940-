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

type RowNumber = Integer
type HistogramInput = [Integer]
type Row = String

countOccur :: Eq a => [a] -> a -> Integer
countOccur [] _ = 0
countOccur (x:xs) y = if y == x
                      then (countOccur xs y) + 1
                      else countOccur xs y

plot :: Integer -> RowNumber -> Char
plot numOccurs row = if numOccurs >= row then '*' else ' '

mapRow :: HistogramInput -> RowNumber -> Row
mapRow _ (-1)     = "0123456789"
mapRow _ 0        = "==========\n"
mapRow [] _       = "          "
mapRow input row  =
    [plot (countOccur input i) row | i <- [0..9]] ++ "\n" 

histogram :: HistogramInput -> String 
histogram input =
    concat $ rows 
        where rows = zipWith mapRow (repeat input) countdown   
              countdown = [maxOccur, maxOccur - 1..(-1)]
              maxOccur = maximum (map (countOccur input) [0..9])
