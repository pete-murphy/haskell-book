module OldVigenere where

import           Data.Char

caesar :: Int -> Char -> Char
caesar n c = chr $ ord 'a' + mod (ord c - ord 'a' + n) 26

caesar' :: Char -> Char -> Char
caesar' k c = chr $ ord 'a' + abs (ord c - ord k)

vigenere :: String -> String -> String
vigenere _ [] = []
vigenere [] xs = xs
vigenere ks xs = go ks xs [] 0
  where
    go ks (x:xs) acc count
      | xs == [] =
        reverse
          ((:) (caesar (ord ((!!) ks (mod count (length ks))) - ord 'a') x) acc)
      | x == ' ' = go ks xs ((:) ' ' acc) count
      | otherwise =
        go
          ks
          xs
          ((:) (caesar (ord ((!!) ks (mod count (length ks))) - ord 'a') x) acc)
          (count + 1)

main :: IO String
main = do
  putStrLn "What is the keyword?"
  keyword <- getLine
  putStrLn "What is the secret?"
  secret <- getLine
  return $ vigenere keyword secret
