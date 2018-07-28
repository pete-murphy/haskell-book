{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT fmas) = StateT $ (fmap . fmap) (\(x, y) -> (f x, y)) fmas

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ (\y -> pure (x, y))
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT f <*> StateT sma =
    StateT $ \s -> do
      (g, s') <- f s
      (x, s'') <- sma s'
      return $ (g x, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  StateT sma >>= f =
    StateT $ \s -> do
      (v, s') <- sma s
      runStateT (f v) s'
