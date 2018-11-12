{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (fmap . fmap) (\(x, y) -> (f x, y)) smas

instance Monad m => Applicative (StateT s m) where
  pure x = StateT (\y -> pure (x, y))
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma =
    StateT $ \s -> do
      (ab, s') <- smab s
      (a, s'') <- sma s'
      pure $ (ab a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f =
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'
