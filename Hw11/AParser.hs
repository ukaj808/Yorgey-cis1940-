{-# LANGUAGE InstanceSigs #-}

{- CIS 194 HW 10
   due Monday, 1 April
-}
module AParser where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a =
  Parser
    { runParser :: String -> Maybe (a, String)
    }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x:xs) -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}
-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser runParserF) = Parser runParserG
    where
      runParserG xs = fmap (first f) (runParserF xs)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser runParserF
    where
      runParserF _ = Just (a, "")
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser runParserP1) (Parser runParserP2) = Parser runParserZ
    where
      runParserZ xs = runParserOnRemains p1Results runParserP2
        where
          p1Results = runParserP1 xs
          runParserOnRemains Nothing _ = Nothing
          runParserOnRemains (Just (fn, remains)) parser =
            fmap (first fn) (parser remains)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser runParserF) (Parser runParserG) =
    Parser
      (\xs ->
         case runParserF xs of
           Nothing -> runParserG xs
           Just x -> runParserF xs)

type Name = String

type Phone = String

data Employee =
  Emp
    { name :: !Name
    , phone :: !Phone
    }
  deriving (Show)

parseName :: Parser Name
parseName = Parser f
  where
    f [] = Nothing
    f xs = Just (name, restOfString)
      where
        (name, restOfString) = span isLetter xs

parsePhone :: Parser Phone
parsePhone = Parser f
  where
    f [] = Nothing
    f xs = Just (phone, restOfString)
      where
        (phone, restOfString) = span isDigit xs

employeeParser = Emp <$> parseName <*> parsePhone

parseLowerA :: Parser Char
parseLowerA = char 'a'

parseLowerB :: Parser Char
parseLowerB = char 'b'

parseSpace :: Parser Char
parseSpace = char ' '

erase2 :: a -> b -> ()
erase2 a b = ()

erase :: a -> ()
erase a = ()

abParser :: Parser (Char, Char)
abParser = (,) <$> parseLowerA <*> parseLowerB

abParser_ :: Parser ()
abParser_ = erase2 <$> parseLowerA <*> parseLowerB

intPair :: Parser [Integer]
intPair = intPairCons <$> posInt <*> parseSpace <*> posInt
  where
    intPairCons x _ y = [x, y]

intOrUppercase :: Parser ()
intOrUppercase = (erase <$> posInt) <|> (erase <$> satisfy isUpper)

parseAorB :: Parser Char
parseAorB = char 'A' <|> char 'B'
