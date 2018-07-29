{-# LANGUAGE InstanceSigs #-}

module ExercisesLiftMore where

import           Control.Monad
import           Control.Monad.Trans.Class
import           EitherT
import           StateT

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma =
    StateT $ \s -> do
      a <- ma
      return (a, s)
