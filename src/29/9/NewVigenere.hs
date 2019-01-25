module NewVigenere where

import           Data.Char

-- import           Test.Hspec
import           Test.QuickCheck

caesar :: Int -> Char -> Char
caesar _ ' ' = ' '
caesar n c   = chr $ ordAlpha c + mod (ord c - ordAlpha c + n) 26

unCaesar :: Int -> Char -> Char
unCaesar _ ' ' = ' '
unCaesar n c   = chr $ ordAlpha c + mod (ord c - ordAlpha c - n) 26

ordAlpha :: Char -> Int
ordAlpha c
  | isUpper c = ord 'A'
  | otherwise = ord 'a'

vigenere :: String -> String -> String
vigenere key raw =
  zipWith (\c c' -> caesar (ord c - ordAlpha c) c') (cycle key) raw

unVigenere :: String -> String -> String
unVigenere key encoded =
  zipWith (\c c' -> unCaesar (ord c - ordAlpha c) c') (cycle key) encoded

prop_caesarRoundTrip :: Property
prop_caesarRoundTrip =
  forAll (arbitrary `suchThat` (> 0) :: Gen Int) $ \n ->
    forAll validCharGen $ \c -> unCaesar n (caesar n c) == c

validStringGen :: Gen String
validStringGen = arbitrary `suchThat` all ((&&) <$> isLetter <*> isAscii)

validCharGen :: Gen Char
validCharGen = arbitrary `suchThat` ((&&) <$> isLetter <*> isAscii)

-- spec :: Spec
-- spec = do
--   describe "Spec test" $ do
--     it "sample input 1" $ do
--       vigenere "ALLY" "MEET AT DAWN" `shouldBe` "MPPR AE OYWY"
--     it "sample input 2" $ do
--       unVigenere "ALLY" "MPPR AE OYWY" `shouldBe` "MEET AT DAWN"
prop_vigenereRoundTrip :: Property
prop_vigenereRoundTrip =
  forAll validStringGen $ \ks ->
    forAll validStringGen $ \xs -> unVigenere ks (vigenere ks xs) == xs

main :: IO ()
main
  -- hspec spec
 = do
  quickCheck prop_caesarRoundTrip
  quickCheck prop_vigenereRoundTrip
