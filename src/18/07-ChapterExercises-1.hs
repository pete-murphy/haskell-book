module ChapterExercises1 where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1
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

-- 3
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x _ >>= f = f x

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (undefined :: Nope (Int, String, [Int]))
  quickBatch $
    monad
      (undefined :: PhhhbbtttEither (Int, String, [Int]) (Int, String, [Int]))
  quickBatch $ monad (undefined :: Identity (Int, String, [Int]))
  quickBatch $ monad (undefined :: List (Int, String, [Int]))
