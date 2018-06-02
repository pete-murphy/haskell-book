module Bigger where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Bigger a b =
  Bigger a
         b
         b
         b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a x y z) = Bigger a (f x) (f y) (f z)

instance Monoid a => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  Bigger a f g h <*> Bigger b x y z = Bigger (a <> b) (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = (f x) <> (f y) <> (f z)

instance Traversable (Bigger a) where
  traverse f (Bigger a x y z) = Bigger a <$> f x <*> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

type SIPIIs = (Sum Int, Product Int, [Int])

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Bigger SIPIIs SIPIIs)
