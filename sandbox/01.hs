{-# LANGUAGE QuasiQuotes #-}

module Throwaway where

import           Text.RawString.QQ

sample :: String
sample =
  [r|1 abc
2 def
3 poo|]
