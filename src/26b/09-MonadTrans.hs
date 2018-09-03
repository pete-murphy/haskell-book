module MonadTrans where

class MonadTrans t
  -- Lift a computation from
  -- the argument monad to
  -- the constructed monad.
  where
  lift :: (Monad m) => m a -> t m a
