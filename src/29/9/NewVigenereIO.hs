module NewVigenereIO where

import           NewVigenere        (unVigenere, vigenere)
import           System.Environment (getArgs)
import           System.IO          (hGetChar, hPutChar, hPutStr, stdin, stdout)

main :: IO ()
main = hGetChar stdin >>= hPutChar stdout
