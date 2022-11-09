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

joinListSize :: (Sized m, Monoid m) => JoinList m a -> Int
joinListSize Empty = 0
joinListSize (Single m a) = getSize (size m)
joinListSize (Append m jl1 jl2) = getSize (size m)

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
  if i < joinListSize jl1
    then case jl1 of Empty -> indexJ (i - joinListSize jl1) jl2
                     _ -> indexJ i jl1
    else indexJ (i - joinListSize jl1) jl2

joinListFold :: b -> (b -> a -> b -> b) -> JoinList m a -> b
joinListFold e _ Empty = e 
joinListFold e f (Single m a) = undefined
joinListFold e f (Append m jl1 jl2) = undefined

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl | i <= 0 = jl
           | i >= joinListSize jl = Empty
dropJ i Empty = Empty
dropJ i (Append m jl1 jl2) | joinListSize jl1 == i = Append m Empty jl2
                           | joinListSize jl1 > i = Append m (dropJ i jl1) jl2
test1 =
  Append
    (Size 10)
    (Append
       (Size 8)
       (Append
          (Size 4)
          (Append (Size 2) (Single (Size 1) 'y') (Single (Size 1) 'e'))
          (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'h')))
       (Append
          (Size 4)
          (Append (Size 2) (Single (Size 1) ' ') (Single (Size 1) 'b'))
          (Append (Size 2) (Single (Size 1) 'u') (Single (Size 1) 'd'))))
    (Append (Size 2) (Single (Size 1) 'd') (Single (Size 1) 'y'))

test2 =
  Append
    (Size 10)
    (Append
       (Size 8)
       (Append
          (Size 4)
          (Append (Size 2) (Single (Size 1) 'i') (Single (Size 1) 't'))
          (Append (Size 2) (Single (Size 1) 's') (Single (Size 1) ' ')))
       (Append
          (Size 4)
          (Append (Size 2) (Single (Size 1) 't') (Single (Size 1) 'i'))
          (Append (Size 2) (Single (Size 1) 'm') (Single (Size 1) 'e'))))
    (Append (Size 2) (Single (Size 1) 'r') (Single (Size 1) 'y'))

