module Combinations where

import           Control.Applicative (liftA3)
import           Data.List           (intercalate)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

collapse :: (a, a, a) -> [a]
collapse (x, y, z) = [x, y, z]

main :: IO ()
main = putStrLn $ intercalate "\n" $ map collapse $ combos vowels stops vowels
