{-# LANGUAGE InstanceSigs #-}

module ExercisesSomeInstances where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           MaybeT
import           ReaderT
import           StateT

-- 1
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- 2
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

-- 3
instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma =
    StateT $ \s -> do
      a <- ma
      return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
