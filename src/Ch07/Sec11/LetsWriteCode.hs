{-# LANGUAGE LambdaCase #-}

module Ch07.Sec11.LetsWriteCode where

import           Control.Arrow (first)
import           Data.Tuple    (swap)

-- 1.
-- tensDigit :: Integral a => a -> a
-- tensDigit x = d
--  where xLast = x `div` 10
--        d = xLast `mod` 10
-- a) Rewrite using `divMod`
tensDigit :: Integral a => a -> a
tensDigit = snd . flip divMod 10 . (fst . flip divMod 10)

-- c)
hunsD :: Integral a => a -> a
hunsD = snd . flip divMod 100 . (fst . flip divMod 100)

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y =
  \case
    True -> x
    _ -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b = x
  | otherwise = y

-- 3.
g, g' :: (a -> b) -> (a, c) -> (b, c)
g f = swap . fmap f . swap

g' = first
-- 4.
