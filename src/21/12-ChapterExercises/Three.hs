module Three where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three x y f <*> Three x' y' z = Three (x <> x') (y <> y') (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    traversable
      (undefined :: Three (Sum Int, Product Int, [Int]) ( Sum Int
                                                        , Product Int
                                                        , [Int]) ( Sum Int
                                                                 , Product Int
                                                                 , [Int]))
