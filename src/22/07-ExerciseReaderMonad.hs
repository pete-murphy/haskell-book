{-# LANGUAGE InstanceSigs #-}

module ExerciseReaderMonad where

data Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (fmap f r)

instance Applicative (Reader r) where
  pure a = Reader (const a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader rab <*> Reader ra = Reader (rab <*> ra)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= aRb = Reader ra >>= aRb
