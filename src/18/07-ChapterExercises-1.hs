module ChapterExercises1 where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- 2
data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' x)  = Left' (f x)
  fmap _ (Right' x) = Right' x

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Left' f <*> Left' x = Left' (f x)
  Right' x <*> _ = Right' x
  _ <*> Right' x = Right' x

instance Monad (PhhhbbtttEither b) where
  return = pure
  Left' x >>= f = f x
  Right' x >>= _ = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (undefined :: Nope (Int, String, [Int]))
  quickBatch $
    monad
      (undefined :: PhhhbbtttEither (Int, String, [Int]) (Int, String, [Int]))
