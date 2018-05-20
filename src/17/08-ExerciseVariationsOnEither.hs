module ExerciseVariationsOnEither where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success x) = Success (f x)
  (<*>) (Failure x) (Failure y) = Failure (x <> y)
  (<*>) _ (Failure x)           = Failure x
  (<*>) (Failure x) _           = Failure x

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return $ Failure x, return $ Success y]
-- main :: IO ()
-- main =
--  quickBatch $ applicative $ Failure ("poop", 2 :: Product Int, 2 :: Sum Int)
-- Also tried this:
-- main =
--   quickBatch $
--   applicative $ undefined :: Validation
--                              ([String], Product Int, Sum Int)
--                              ([String], Product Int, Sum Int)
