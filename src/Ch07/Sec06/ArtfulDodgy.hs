module Ch07.Sec06.ArtfulDodgy where

import           Control.Exception (assert)
import           Data.Foldable     (fold)
import           Data.Monoid       (Endo (..))

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = flip dodgy 2

answers =
  [ dodgy 1 0 == 1
  , dodgy 1 1 == 11
  , dodgy 2 2 == 22
  , dodgy 2 1 == 12
  , oneIsOne 1 == 11
  , oneIsOne 2 == 21
  , oneIsTwo 1 == 21
  , oneIsTwo 2 == 22
  , oneIsOne 3 == 31
  , oneIsTwo 3 == 23
  ]

-- Goofing around
main' :: IO ()
main' = mconcat (assert <$> answers) putStrLn "You were right!"

main :: IO ()
main = appEndo (fold (Endo . assert <$> answers)) putStrLn "You were right!"
