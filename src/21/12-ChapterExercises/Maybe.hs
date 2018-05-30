module Optional where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep (f x)
  fmap _ Nada    = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Yep x `mappend` Yep y = Yep (x <> y)
  x `mappend` Nada = x
  Nada `mappend` x = x
  Nada `mappend` Nada = Nada

instance Applicative Optional where
  pure = Yep
  Yep f <*> Yep x = Yep (f x)
  _ <*> Nada = Nada
  Nada <*> _ = Nada

instance Foldable Optional where
  foldr f x (Yep y) = f y x
  foldr _ x Nada    = x

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ Nada    = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [Yep <$> arbitrary, return Nada]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Optional (Sum Int, Product Int, [Int]))
