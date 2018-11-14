{-# LANGUAGE BangPatterns #-}

module ManualBang where

data DoesntForce =
  TisLazy Int
          String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

data BangBang =
  SheShotMeDown !Int
                !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s
