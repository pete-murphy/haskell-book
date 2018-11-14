module CommonFunctionsAsTypes where

import Control.Applicative
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity
  { runIdentity :: a
  } deriving (Eq, Show)

newtype Compose f g a = Compose
  { runCompose :: f (g a)
  } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ (pure . pure) a
  (Compose f) <*> (Compose a) = Compose $ ((<*>) $ fmap (<*>) f) a

instance Eq (f (g a)) => EqProp (Compose f g a) where
  (=-=) = eq

instance Arbitrary (f (g a)) => Arbitrary (Compose f g a) where
  arbitrary = do
    fga <- arbitrary
    return $ Compose fga

type III = (Int, Int, Int)

main :: IO ()
main = do
  quickBatch $ applicative $ (undefined :: Compose [] Maybe III)
