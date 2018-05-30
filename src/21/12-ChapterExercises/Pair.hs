module Pair where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Pair a b =
  Pair a
       b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  Pair x f <*> Pair x' y = Pair (x <> x') (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ x) = f x

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    traversable
      (undefined :: Pair (Sum Int, Product Int, [Int]) ( Sum Int
                                                       , Product Int
                                                       , [Int]))
