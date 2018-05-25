module Maybe where

import           Data.Foldable
import           Data.Monoid

data Optional a
  = Nada
  | Yep a

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a
