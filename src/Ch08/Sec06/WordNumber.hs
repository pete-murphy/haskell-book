{-# LANGUAGE LambdaCase #-}

module WordNumber where

import           Data.List  (intersperse, unfoldr)
import           Data.Tuple (swap)

digitToWord :: Int -> String
digitToWord = concat . intersperse "-" . map wordNumber . digits

-- There's got to be a nicer way of writing this
digits :: Int -> [Int]
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- unfoldr :: (Int -> Maybe (Int, Int)) -> Int -> [Int]
digits =
  reverse .
  unfoldr
    (\n ->
       if n > 0
         then Just (swap $ divMod n 10)
         else Nothing)

wordNumber :: Int -> String
wordNumber =
  \case
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    6 -> "Six"
    7 -> "Seven"
    8 -> "Eight"
    9 -> "Nine"
