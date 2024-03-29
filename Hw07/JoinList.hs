module JoinList where

import Buffer
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single m a) = a ++ ['\n']
  toString (Append m jl1 jl2) = toString jl1 ++ toString jl2
  fromString xs =
    foldl
      (\jl line -> jl +++ Single (scoreString line, Size 1) line)
      Empty
      (lines xs)
  line = indexJ
  replaceLine _ _ Empty = Empty
  replaceLine i _ jl
    | i < 0 = jl
    | i > getSize (snd (tag jl)) = jl
  replaceLine i newLine jl =
    (takeJ i jl +++ Single (scoreString newLine, Size 1) newLine) +++
    dropJ (i + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m jl1 jl2) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty x@(Single m a) = Append m Empty x
(+++) x@(Single m a) Empty = Append m x Empty
(+++) Empty x@(Append m jl1 jl2) = Append m Empty x
(+++) x@(Append m jl1 jl2) Empty = Append m x Empty
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

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
    then case jl1 of
           Empty -> indexJ (i - joinListSize jl1) jl2
           _ -> indexJ i jl1
    else indexJ (i - joinListSize jl1) jl2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
  | i <= 0 = jl
  | i >= joinListSize jl = Empty
dropJ _ Empty = Empty
dropJ i (Append m jl1 jl2)
  | i == jl1Size = Append (tag Empty <> tag jl2) Empty jl2
  | i > jl1Size = Append (tag Empty <> jl2'm) Empty jl2'
  | i < jl1Size = Append (jl1'm <> jl2'm) jl1' jl2
  where
    jl1Size = joinListSize jl1
    jl1' = dropJ i jl1
    jl2' = dropJ (i - joinListSize jl1) jl2
    jl1'm = tag jl1'
    jl2'm = tag jl2'

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl
  | i <= 0 = Empty
  | i >= joinListSize jl = jl
takeJ _ Empty = Empty
takeJ i (Append m jl1 jl2)
  | i == jl1Size = Append (tag jl1 <> tag Empty) jl1 Empty
  | i < jl1Size = Append (jl1'm <> tag Empty) jl1' Empty
  | i > jl1Size = Append (tag jl1 <> jl2'm) jl1 jl2'
  where
    jl1Size = joinListSize jl1
    jl1' = takeJ i jl1
    jl1'm = tag jl1'
    jl2' = takeJ (i - joinListSize jl1) jl2
    jl2'm = tag jl2'

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine xs = Single (scoreString xs) xs

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
