module WrapItUp where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ const $ pure (Right (Just 1))

embedded' :: ReaderT () IO (Either String (Maybe Int))
embedded' = runExceptT $ runMaybeT embedded
