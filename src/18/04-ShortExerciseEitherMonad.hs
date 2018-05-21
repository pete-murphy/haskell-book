module ShortExerciseEitherMonad where

import           Control.Monad (join)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  Second f <*> Second x = Second (f x)
  First x <*> _ = First x
  _ <*> First x = First x

instance Monad (Sum a) where
  return = pure
  First x >>= _ = First x
  Second x >>= f = join $ Second (f x)
