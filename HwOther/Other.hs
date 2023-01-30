module Other where


data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' $ f x


instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _ = Nothing'
  Just' f <*> m = fmap f m


instance Monad Maybe' where
  return :: a -> Maybe' a
  return = pure

  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Nothing' >>= _ = Nothing'
  Just' x >>= k = k x


data Either' e a = Left' e | Right' a 

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _ (Left' e) = Left' e
  fmap f (Right' x) = Right' $ f x

instance Applicative (Either' e) where
  pure :: a -> Either' e a
  pure = Right'

  (<*>) :: Either' e (a -> b) -> Either' e a -> Either' e b
  Left' e <*> _ = Left' e
  Right' f <*> m = fmap f m 


instance Monad (Either' e) where
  return :: a -> Either' e a
  return = pure

  (>>=) :: Either' e a -> (a -> Either' e b) -> Either' e b
  Left' e >>= _ = Left' e
  Right' x >>= k = k x

newtype Reader' e a = Reader' { runReader :: e -> a }

instance Functor (Reader' e) where
  fmap :: (a -> b) -> Reader' e a -> Reader' e b
  fmap f (Reader' runReader) = Reader' $ f . runReader
  
instance Applicative (Reader' e) where
  pure :: a -> Reader' e a
  pure x = Reader' $ const x

  (<*>) :: Reader' e (a -> b) -> Reader' e a -> Reader' e b
  Reader' f <*> Reader' g = Reader' $ \e -> f e (g e)

instance Monad (Reader' e) where
  return :: a -> Reader' e a
  return = pure

  (>>=) :: Reader' e a -> (a -> Reader' e b) -> Reader' e b
  Reader' f >>= k = Reader' $ \e -> runReader (k (f e)) e


newtype State' s a = State' { runState :: s -> (a, s) } 

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f st = State' runState' 
    where 
      runState' s = (f $ fst newState, snd newState) 
        where newState = runState st s

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure = undefined

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  stf <*> st = State' runState'
    where
      runState' s = 
