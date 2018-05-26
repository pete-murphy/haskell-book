module ChapterExercises where

import           Data.Foldable
import           Data.Monoid

-- 1
data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f z (Constant x) = z

-- 2
data Two a b =
  Two a
      b

instance Foldable (Two a) where
  foldr f z (Two x y) = f y z

-- 3
data Three a b c =
  Three a
        b
        c

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z

-- 4
data Three' a b =
  Three' a
         b
         b

instance Foldable (Three' a) where
  foldr f z (Three' _ _ x) = f x z

-- 5
data Four' a b =
  Four' a
        b
        b
        b

instance Foldable (Four' a) where
  foldr f z (Four' _ _ _ x) = f x z

-- 6?
filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f =
  foldMap
    (\x ->
       case (f x) of
         True  -> pure x
         False -> mempty)
