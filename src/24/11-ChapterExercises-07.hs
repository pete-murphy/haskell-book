module ChapterExercises07 where

import           Control.Applicative
import           Data.Char           (digitToInt)
import           Data.Word
import           Text.Trifecta

data IPAddress6 =
  IPAddress6 Word64
             Word64
  deriving (Eq, Ord, Show)

type Base16 = Char

type Byte = [Base16]

-- rewrite both these as folds?
byteToInt :: Byte -> Int
byteToInt []     = 0
byteToInt (h:hs) = (digitToInt h) * (2 ^ length hs) + byteToInt hs

blockToW64 :: [Byte] -> Word64
blockToW64 []     = 0
blockToW64 (b:bs) = (fromIntegral $ byteToInt b) * (16 ^ 4) ^ 2

-- parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 :: Parser [[Char]]
parseIPAddress6 = do
  block1 <- count 4 $ (many hexDigit) <* char ':'
  block2 <-
    many $ try ((many hexDigit) <* char ':') <|> ((some hexDigit) <* eof)
  return $ (mkFull block1) ++ (mkFull block2)

parseP :: Parser [[Char]]
parseP = many $ try $ (many hexDigit) <* char ':' <|> (many hexDigit) <* eof

mkFull :: [Byte] -> [Byte]
mkFull xs
  | length xs == 4 = xs
  | otherwise = go xs []
  where
    go (y:ys) acc
      | ys == [] = (acc ++ [y] ++ ys)
      | length y == 0 = mkFull (acc ++ [y] ++ [[]] ++ ys)
      | otherwise = go ys (y : acc)

ipEx1 :: String
ipEx1 = "0:0:0:0:0:ffff:ac10:fe01"

ipEx2 :: String
ipEx2 = "0:0:0:0:0:ffff:cc78:f"

ipEx3 :: String
ipEx3 = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

ipEx3' :: String
ipEx3' = "FE80::0202:B3FF:FE1E:8329"

ipEx4 :: String
ipEx4 = "2001:DB8::8:800:200C:417A"
