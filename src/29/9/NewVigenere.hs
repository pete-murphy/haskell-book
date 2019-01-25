module NewVigenere where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

caesar :: Int -> Char -> Char
caesar n c = chr $ ordAlpha c + mod (ord c - ordAlpha c + n) 26

unCaesar :: Int -> Char -> Char
unCaesar n c = chr $ ordAlpha c + mod (ord c - ordAlpha c - n) 26

ordAlpha :: Char -> Int
ordAlpha c
  | isUpper c = ord 'A'
  | otherwise = ord 'a'

vigenere :: String -> String -> String
vigenere _ _ = "HEllo"

unVigenere :: String -> String -> String
unVigenere _ _ = "Foo"

prop_caesarRoundTrip :: Property
prop_caesarRoundTrip =
  forAll (arbitrary `suchThat` (> 0) :: Gen Int) $ \n ->
    forAll validCharGen $ \c -> unCaesar n (caesar n c) == c

validStringGen :: Gen String
validStringGen = arbitrary `suchThat` (all $ flip elem ['A' .. 'Z'])

validCharGen :: Gen Char
validCharGen = arbitrary `suchThat` (flip elem ['A' .. 'Z'])

spec :: Spec
spec = do
  describe "Spec test" $ do
    it "sample input 1" $ do
      vigenere "ALLY" "MEET AT DAWN" `shouldBe` "MPPR AE OYWY"
    it "sample input 2" $ do
      unVigenere "ALLY" "MPPR AE OYWY" `shouldBe` "MEET AT DAWN"

prop_vigenereRoundTrip :: Property
prop_vigenereRoundTrip =
  forAll validStringGen $ \ks ->
    forAll validStringGen $ \xs -> unVigenere ks (vigenere ks xs) == xs

main :: IO ()
main = do
  hspec spec
  quickCheck prop_caesarRoundTrip
  quickCheck prop_vigenereRoundTrip
