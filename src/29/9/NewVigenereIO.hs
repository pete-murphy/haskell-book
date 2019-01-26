module Main where

import           NewVigenere        (unVigenere, vigenere)
import           System.Environment (getArgs)
import           System.IO          (hGetLine, hPutStrLn, stdin, stdout)

main :: IO ()
main = do
  [opt, key] <- getArgs
  str <- hGetLine stdin
  let out
        | opt == "-e" = vigenere key str
        | opt == "-d" = unVigenere key str
        | otherwise = "Need to provide valid flag"
  hPutStrLn stdout out
