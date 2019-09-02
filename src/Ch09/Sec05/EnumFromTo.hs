{-# LANGUAGE ScopedTypeVariables #-}

module Ch09.Sec05.EnumFromTo where

import           Control.Exception
import           System.IO.Unsafe  (unsafePerformIO)

eftBool :: Bool -> Bool -> [Bool]
eftBool a b = [a | not a] ++ [b | b]

succSafe :: Enum a => a -> Maybe a
succSafe x =
  unsafePerformIO $
  (pure <$> evaluate (succ x)) `catch`
  (pure (pure Nothing) :: SomeException -> IO (Maybe a))

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a >= b = []
  | otherwise = a : eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a >= b = []
  | otherwise = a : eftInt (succ a) b

eftChar :: Char -> Char -> String
eftChar a b
  | a >= b = []
  | otherwise = a : eftChar (succ a) b
