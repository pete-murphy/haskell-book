{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import           Control.Monad.Trans.Class (lift)
import           Data.Monoid               ((<>))
import           Web.Scotty

main =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      lift $ putStrLn $ show beam
      html $ "<h1>Scotty, " <> beam <> " me up!</h1>"
