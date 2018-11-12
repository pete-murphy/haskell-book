{-# LANGUAGE InstanceSigs #-}

module MaybeT where

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure $ Just x
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT mab <*> MaybeT ma = MaybeT $ (fmap (<*>) mab) <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f =
    MaybeT $ do
      a <- ma
      case a of
        Nothing -> pure Nothing
        Just x  -> runMaybeT $ f x
