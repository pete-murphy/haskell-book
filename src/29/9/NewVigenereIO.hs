module Main where

import           NewVigenere        (unVigenere, vigenere)
import           System.Environment (getArgs)
import           System.IO          (hGetLine, hPutStrLn, stdin, stdout)

maybeGetArgs :: IO (Maybe [String])
maybeGetArgs = do
  args <- getArgs
  let result =
        case args of
          [flag, key] -> Just [flag, key]
          _           -> Nothing
  pure result

main :: IO ()
main = do
  res <- maybeGetArgs
  let fn =
        case res of
          Nothing -> const "Invalid input"
          Just [flag, key]
            | flag == "-d" -> unVigenere key
            | flag == "-e" -> vigenere key
            | otherwise -> const "Invalid flag"
  str <- hGetLine stdin
  hPutStrLn stdout $ fn str
