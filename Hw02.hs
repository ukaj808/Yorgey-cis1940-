{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Char
import GHC.Base
import Log

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

isValidNum :: String -> Bool
isValidNum [] = False
isValidNum x = all isNumber x

isValidErrorNum :: String -> Bool
isValidErrorNum [] = False
isValidErrorNum x
  | length x == 1 && not (eqString "0" x) = isValidNum x
  | length x == 2 = isValidNum x
  | length x == 3 && eqString "100" x = True
  | otherwise = False

hasValidErrorMessageStruct :: [String] -> Bool
hasValidErrorMessageStruct [] = False
hasValidErrorMessageStruct xs
  | length xs < 4 = False
  | not (isValidErrorNum (xs !! 1)) = False
  | not (isValidNum (xs !! 2)) = False
  | otherwise = head xs == "E"

hasValidWarningMessageStruct :: [String] -> Bool
hasValidWarningMessageStruct [] = False
hasValidWarningMessageStruct xs
  | length xs < 3 = False
  | not (isValidNum (xs !! 1)) = False
  | otherwise = head xs == "W"

hasValidInfoMessageStruct :: [String] -> Bool
hasValidInfoMessageStruct [] = False
hasValidInfoMessageStruct xs
  | length xs < 3 = False
  | not (isValidNum (xs !! 1)) = False
  | otherwise = head xs == "I"

parseErrorMessageArr :: [String] -> LogMessage
parseErrorMessageArr xs =
  if hasValidErrorMessageStruct xs
    then LogMessage (Error (read (xs !! 1) :: Int)) (read (xs !! 2) :: Int) (unwords (slice 3 (length xs) xs))
    else Unknown (unwords xs)

parseWarningMessageArr :: [String] -> LogMessage
parseWarningMessageArr xs =
  if hasValidWarningMessageStruct xs
    then LogMessage Warning (read (xs !! 1) :: Int) (unwords (slice 2 (length xs) xs))
    else Unknown (unwords xs)

parseInfoMessageArr :: [String] -> LogMessage
parseInfoMessageArr xs =
  if hasValidInfoMessageStruct xs
    then LogMessage Info (read (xs !! 1) :: Int) (unwords (slice 2 (length xs) xs))
    else Unknown (unwords xs)

parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty message!"
parseMessage x
  | eqString "E" (head xs) = parseErrorMessageArr xs
  | eqString "W" (head xs) = parseWarningMessageArr xs
  | eqString "I" (head xs) = parseInfoMessageArr xs
  | otherwise = Unknown x
  where
    xs = words x

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

getTimestamp :: LogMessage -> Maybe Int
getTimestamp (LogMessage _ t _) = Just t
getTimestamp (Unknown _) = Nothing

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lg Leaf = Node Leaf lg Leaf
insert lg (Node lt lg2 rt)
  | getTimestamp lg < getTimestamp lg2 = Node (insert lg lt) lg2 rt
  | getTimestamp lg > getTimestamp lg2 = Node lt lg2 (insert lg rt)
  | otherwise = Node Leaf lg Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder mt =
  case mt of
    Leaf -> []
    Node Leaf lg Leaf -> lg : []
    Node Leaf lg rt -> [lg] ++ inOrder rt
    Node lt lg rt -> inOrder lt ++ [lg] ++ inOrder rt

isSevere :: LogMessage -> Bool
isSevere (Unknown _) = False
isSevere (LogMessage t _ _) =
  case t of
    Error n -> n > 50
    _ -> False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = map (\(LogMessage _ _ msg) -> msg) (filter isSevere (inOrder (Prelude.foldr (\x -> insert x) (Leaf) logs)))
