module FizzBuzzDifferently where

import           Control.Monad
import           Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

generateListInt :: Integer -> Integer -> [Integer]
generateListInt x y = go x y []
  where
    go l h acc
      | l == h = acc
      | otherwise = go (l + 1) h (l : acc)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo x y = fizzbuzzList $ generateListInt x y

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100
