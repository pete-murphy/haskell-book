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

-- 1
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= aRb = Reader ra >>= aRb

-- 2
data Person = Person
  { humanName :: String
  , dogName   :: String
  , address   :: String
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName    :: String
  , dogsAddress :: String
  } deriving (Eq, Show)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' =
  dogName >>= \name -> address >>= \address -> return $ Dog name address
