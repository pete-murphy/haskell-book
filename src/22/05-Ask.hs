module Ask where

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

-- fmap :: (a -> b) -> Reader r a -> Reader r b
-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

ask :: Reader a a
ask = Reader id
