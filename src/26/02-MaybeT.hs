{-# LANGUAGE InstanceSigs #-}

module MaybeT where

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure $ pure x
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT fab <*> MaybeT ma = MaybeT $ (<*>) <$> fab <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y  -> runMaybeT (f y)
