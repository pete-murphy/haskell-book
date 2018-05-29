module Constant where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x <> y)

instance Foldable (Constant a) where
  foldr _ x (Constant _) = x

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    traversable
      (undefined :: Constant (Sum Int, Product Int, [Int]) ( Sum Int
                                                           , Product Int
                                                           , [Int]))
