module Main where

import           Control.Monad.State
import           System.Random

main = do
  answer <- getStdRandom (randomR (1, 100))
  putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
  guesses <- execStateT (guessSession answer) 0
  putStrLn $ "Success in " ++ show guesses ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer = do
  gs <- lift getLine
  let g = read gs
  modify (+ 1)
  case compare g answer of
    LT -> do
      lift $ putStrLn "Too low"
      guessSession answer
    GT -> do
      lift $ putStrLn "Too high"
      guessSession answer
    EQ -> lift $ putStrLn "Got it!"
