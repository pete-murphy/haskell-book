{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (pack)
import           Data.Text.Lazy            (Text)
import           Web.Scotty

param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k) (const (return Nothing))

param'' :: Parsable a => Text -> MaybeT ActionM a
param'' k = MaybeT $ rescue (Just <$> param k) (const (return Nothing))

type Reco = (Integer, Integer, Integer, Integer)

main =
  scotty 3000 $ do
    get "/:word" $ do
      beam' <- param' "word"
      let beam = fromMaybe "" beam'
      i <- param' "num"
      liftIO $ print (i :: Maybe Integer)
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

main' =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      reco <-
        runMaybeT $ do
          a <- param'' "1"
          liftIO $ print a
          b <- param'' "2"
          c <- param'' "3"
          d <- param'' "4"
          (lift . lift) $ print b
          return ((a, b, c, d) :: Reco)
      liftIO $ print reco
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

param''' :: Parsable a => Text -> ActionM (Either String a)
param''' k =
  rescue
    (Right <$> param k)
    (const (return (Left $ "The key: " ++ show k ++ " was missing!")))

main'' =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      a <- param''' "1"
      let a' = either (const 0) id a
      liftIO $ print (a :: Either String Int)
      liftIO $ print (a' :: Int)
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
