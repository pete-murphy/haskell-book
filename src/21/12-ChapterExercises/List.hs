module List where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: List (Sum Int, Product Int, [Int]))
