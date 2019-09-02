module Ch09.Sec06.ThyFearfulSymmetry where

import           Control.Applicative (liftA2)

splitOn :: Char -> String -> [String]
-- splitOn c str = takeWhile (/= c) str : splitOn c (drop 1 $ dropWhile (/= c) str)
splitOn c =
  takeWhile (/= "") .
  liftA2 (:) (takeWhile (/= c)) (splitOn c . drop 1 . dropWhile (/= c))

myWords :: String -> [String]
myWords = splitOn ' '
