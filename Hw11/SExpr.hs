{- CIS 194 HW 11
   due Monday, 8 April
-}
module SExpr where

import AParserFisnik
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- (:) :: a -> [a] -> [a]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p <|> pure [])

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace 

ident :: Parser Ident
ident = 
       pure (++) 
  <*> (oneOrMore $ satisfy isAlpha) 
  <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------
-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom
  = N Integer
  | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)


parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr =
  spaces *> 
  (A <$> parseAtom 
      <|> 
   satisfy (== '(') *> (Comb <$> oneOrMore parseSExpr) <* satisfy (== ')'))
  <* spaces

  
