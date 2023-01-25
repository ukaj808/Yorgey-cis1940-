module Scratch where

type Name = String

data Employee =
  Employee
    { name :: !Name
    , phone :: !String
    }
  deriving (Show)

fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
fmap2 = undefined
