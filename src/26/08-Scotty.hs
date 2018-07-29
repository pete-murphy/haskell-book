{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy hiding (get)
import           Data.Monoid                    (mconcat)
import           Web.Scotty
import           Web.Scotty.Internal.Types      (ActionT (..))

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      (lift :: (Monad m, MonadTrans t) =>
                 m a -> t m a) $
        putStrLn "hello"
      (lift :: MonadTrans t =>
                 IO a -> t IO a) $
        putStrLn "hello"
      (lift :: IO a -> ActionM a) $ putStrLn "hello"
      (lift :: IO () -> ActionM ()) $ putStrLn "hello"
      (ActionT . lift . lift . lift) (putStrLn "Hello!")
      (ActionT . (ExceptT . fmap Right) . liftReaderT . lift) (putStrLn "YELLO")
      (ActionT .
       (ExceptT . fmap Right) .
       ReaderT .
       const . \m ->
         StateT
           (\s -> do
              a <- m
              return (a, s)))
        (putStrLn "YELLLLLOOOOOO!")
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
