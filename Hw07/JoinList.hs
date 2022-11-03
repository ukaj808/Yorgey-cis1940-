module Hw07 where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m jl1 jl2) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty x@(Single m a) = Append m Empty x
(+++) x@(Single m a) empty = Append m x Empty
(+++) Empty x@(Append m jl1 jl2) = Append m Empty x
(+++) x@(Append m jl1 jl2) Empty = Append m x Empty
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i jl
  | i < 0 = Nothing
  | i == 0 =
    case jl of
      Single m a -> Just a
      Append m jlx jly -> Nothing
  | otherwise =
    case jl of
      Single m a -> undefined
      Append m jlx jly -> undefined
