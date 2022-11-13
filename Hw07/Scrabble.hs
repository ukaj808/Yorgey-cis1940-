module Scrabble where

newtype Score =
  Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score 'a' = Score 1
score 'A' = Score 1
score 'b' = Score 3
score 'B' = Score 3
score 'c' = Score 3
score 'C' = Score 3
score 'd' = Score 2
score 'D' = Score 2
score 'e' = Score 1
score 'E' = Score 1
score 'f' = Score 4
score 'F' = Score 4
score 'g' = Score 2
score 'G' = Score 2
score 'h' = Score 4
score 'H' = Score 4
score 'i' = Score 1
score 'I' = Score 1
score 'j' = Score 8
score 'J' = Score 8
score 'k' = Score 5
score 'K' = Score 5
score 'l' = Score 1
score 'L' = Score 1
score 'm' = Score 3
score 'M' = Score 3
score 'n' = Score 1
score 'N' = Score 1
score 'o' = Score 1
score 'O' = Score 1
score 'p' = Score 3
score 'P' = Score 3
score 'q' = Score 10
score 'Q' = Score 10
score 'r' = Score 1
score 'R' = Score 1
score 's' = Score 1
score 'S' = Score 1
score 't' = Score 1
score 'T' = Score 1
score 'u' = Score 1
score 'U' = Score 1
score 'v' = Score 4
score 'V' = Score 4
score 'w' = Score 4
score 'W' = Score 4
score 'x' = Score 8
score 'X' = Score 8
score 'y' = Score 4
score 'Y' = Score 4
score 'z' = Score 10
score 'Z' = Score 10
score _ = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
