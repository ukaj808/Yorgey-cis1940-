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
