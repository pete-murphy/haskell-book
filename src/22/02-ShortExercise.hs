module ShortExercise where

import           Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- rev
  b <- cap
  return (a, b)

-- Damn, I don't get this syntax
tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= \x -> cap >>= \y -> return (x, y)
