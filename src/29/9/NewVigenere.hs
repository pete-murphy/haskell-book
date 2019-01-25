module NewVigenere where

import           Data.Char
import           Test.QuickCheck

caesar :: Int -> Char -> Char
caesar n c = chr $ ord 'a' + mod (ord c - ord 'a' + n) 26

unCaesar :: Int -> Char -> Char
unCaesar n c = chr $ ord 'a' + mod (ord c - ord 'a' - n) 26

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

unVigenere :: String -> String -> String
unVigenere _ [] = []
unVigenere [] xs = xs
unVigenere ks xs = go ks xs [] 0
  where
    go ks (x:xs) acc count
      | xs == [] =
        reverse
          ((:)
             (unCaesar (ord ((!!) ks (mod count (length ks))) - ord 'a') x)
             acc)
      | x == ' ' = go ks xs ((:) ' ' acc) count
      | otherwise =
        go
          ks
          xs
          ((:)
             (unCaesar (ord ((!!) ks (mod count (length ks))) - ord 'a') x)
             acc)
          (count + 1)

prop_caesarRoundTrip :: Property
prop_caesarRoundTrip =
  forAll (arbitrary `suchThat` (> 0) :: Gen Int) $ \n ->
    forAll validCharGen $ \c -> unCaesar n (caesar n c) == c

validStringGen :: Gen String
validStringGen = arbitrary `suchThat` (all $ flip elem ['a' .. 'z'])

validCharGen :: Gen Char
validCharGen = arbitrary `suchThat` (flip elem ['a' .. 'z'])

prop_vigenereRoundTrip :: Property
prop_vigenereRoundTrip =
  forAll validStringGen $ \ks ->
    forAll validStringGen $ \xs -> unVigenere ks (vigenere ks xs) == xs

main :: IO ()
main = do
  quickCheck prop_caesarRoundTrip
  quickCheck prop_vigenereRoundTrip
