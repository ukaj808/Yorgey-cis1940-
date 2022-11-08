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
  | i >= rootValue = Nothing
  where
    rootValue = getSize (size (tag jl))

indexJ i (Single m a) =
  if i == 0
    then Just a
    else Nothing

indexJ i (Append m jl1 jl2) =
  if i < ltSize
    then case jl1 of Empty -> indexJ (i - ltSize) jl2
                     _ -> indexJ i jl1
    else indexJ (i - ltSize) jl2
  where
    ltSize = case jl1 of Empty -> 0 
                         _ -> getSize (size (tag jl1))

test =
  Append
    (Size 7)
    (Append
       (Size 6)
       (Append
          (Size 2)
          (Append (Size 1) Empty (Single (Size 1) 'e'))
          (Append (Size 1) (Single (Size 1) 'a') Empty))
       (Append
          (Size 4)
          (Append (Size 2) (Single (Size 1) ' ') (Single (Size 1) 'b'))
          (Append (Size 2) (Single (Size 1) 'u') (Single (Size 1) 'd'))))
    (Append (Size 1) (Single (Size 1) 'd') Empty)
