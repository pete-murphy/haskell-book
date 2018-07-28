module OuterInner where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

-- We only need to use return once
-- because it's one big Monad
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

readerRewrap :: ReaderT () IO (Either String (Maybe Int))
readerRewrap = ReaderT $ runReaderT eitherUnwrap

eitherRewrap :: ExceptT String (ReaderT () IO) (Maybe Int)
eitherRewrap = ExceptT readerRewrap

reEmbedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
reEmbedded = MaybeT eitherRewrap

reEmbedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
reEmbedded' = MaybeT $ ExceptT $ ReaderT $ runReaderT eitherUnwrap

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ fmap return (const (Right (Just 1)))
