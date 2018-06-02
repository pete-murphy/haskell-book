module Big where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Big a b =
  Big a
      b
      b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  Big a f g <*> Big b x y = Big (a <> b) (f x) (g y)

instance Foldable (Big a) where
  foldMap f (Big _ x y) = f x <> f y

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

type SIPIIs = (Sum Int, Product Int, [Int])

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Big SIPIIs SIPIIs)
