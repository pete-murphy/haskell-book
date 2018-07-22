{-# LANGUAGE InstanceSigs #-}

module ComposeInstances where

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = fmap (fmap f) Compose fga

-- 1. Write the `Compose Foldable` instance
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap h (Compose fga) = foldMap (foldMap h) fga

-- 2. Write the `Compose Traversable` instance
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse ::
       Applicative f' => (a -> f' b) -> Compose f g a -> f' (Compose f g b)
  traverse f (Compose fga) = fmap Compose $ (traverse (traverse f)) fga
