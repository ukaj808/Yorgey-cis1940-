{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Data.Map.Strict qualified as M
import Data.Time.Format.ISO8601 (yearFormat)
import ExprT
import Parser

eval :: ExprT -> Integer
eval exp = case exp of
  Lit x -> x
  Add exp' exp'' -> (eval exp') + (eval exp'')
  Mul exp' exp'' -> (eval exp') * (eval exp'')

evalStr :: String -> Maybe Integer
evalStr = check . parseExp Lit Add Mul
  where
    check x = case x of
      Just y -> Just $ eval y
      Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: (a -> a -> a)
  mul :: (a -> a -> a)

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = if x <= 0 then False else True
  add x y = x && y
  mul x y = x || y

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

class HasVars a where
  var :: String -> a

data VarExprT
  = VarLit Integer
  | VarAdd VarExprT VarExprT
  | VarMul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x = VarLit x
  add x y = VarAdd x y
  mul x y = VarMul x y

instance HasVars VarExprT where
  var str = Var str

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

varCalc :: Num a => (Maybe a) -> (Maybe a) -> (a -> a -> a) -> (Maybe a)
varCalc (Just x) (Just y) f = Just (x `f` y)
varCalc _ _ _ = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \m -> Just x
  add fx fy mp = varCalc (fx mp) (fy mp) (+)
  mul fx fy mp = varCalc (fx mp) (fy mp) (*)
