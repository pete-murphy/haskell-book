module ChapterExercises09 where

import           Control.Applicative
import           Data.Char           (digitToInt, intToDigit)
import           Data.List           (intercalate)
import           Data.Word
import           Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress x) = intercalate "." $ map show $ intToList $ fromIntegral x

data IPAddress6 =
  IPAddress6 Word64
             Word64
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show = show . intercalate ":" . ipAddr6ToByteList

parseIPList :: Parser [Integer]
parseIPList = many $ try (integer <* char '.') <|> (integer <* eof)

parseIPAddress :: Parser IPAddress
parseIPAddress =
  (IPAddress .
   fromIntegral . foldl (+) 0 . zipWith (*) (((2 ^ 8) ^) <$> [0 ..]) . reverse) <$>
  parseIPList

-- shortcuts!
fromSuccess :: Result a -> a
fromSuccess (Success x) = x

fromSndIPAddr6 :: IPAddress6 -> Word64
fromSndIPAddr6 (IPAddress6 _ w) = w

ip4ToIP6 :: IPAddress -> IPAddress6
ip4ToIP6 (IPAddress x) = IPAddress6 0 (fromIntegral x + w)
  where
    w =
      (fromIntegral $
       fromSndIPAddr6 $
       fromSuccess $ parseString parseIPAddress6 mempty "::ffff:0:0")

ip6ToIP4 :: IPAddress6 -> Maybe IPAddress
ip6ToIP4 (IPAddress6 w w'')
  | w > 0 = Nothing
  | w' >= (2 ^ 32) = Nothing
  | otherwise = Just $ IPAddress (fromIntegral w')
  where
    w' =
      w'' -
      (fromIntegral $
       fromSndIPAddr6 $
       fromSuccess $ parseString parseIPAddress6 mempty "::ffff:0:0")

-- pretty sure this is unfold pattern
intToList :: Integer -> [Integer]
intToList n = go n [] 3
  where
    go x acc p
      | p == 0 = acc ++ [x]
      | otherwise =
        let num = ((2 ^ 8) ^ p)
         in go (mod x num) (acc ++ [(div x num)]) (p - 1)

ipAddr6ToByteList :: IPAddress6 -> [Byte]
ipAddr6ToByteList (IPAddress6 w w') = w64ToBlock w ++ w64ToBlock w'

w64ToBlock :: Word64 -> [Byte]
w64ToBlock w = go w [] 3
  where
    go x acc p
      | p == 0 = acc ++ [intToByte $ fromIntegral x]
      | otherwise =
        let num = (16 ^ 4) ^ p
         in go
              (mod x num)
              (acc ++ [intToByte $ fromIntegral (div x num)])
              (p - 1)

intToByte :: Integer -> Byte
intToByte n = go n [] 3
  where
    go x acc p
      | p == 0 = acc ++ [intToDigit $ fromIntegral x]
      | otherwise =
        let num = 16 ^ p
         in go
              (mod x num)
              (acc ++ [intToDigit $ fromIntegral (div x num)])
              (p - 1)

type Byte = [Char]

-- rewrite both these as folds?
byteToInt :: Byte -> Int
byteToInt []     = 0
byteToInt (h:hs) = (digitToInt h) * 16 ^ (length hs) + byteToInt hs

blockToInteger :: [Byte] -> Integer
blockToInteger [] = 0
blockToInteger (b:bs) =
  (fromIntegral $ byteToInt b) * (16 ^ 4) ^ (length bs) + blockToInteger bs

blockToW64 :: [Byte] -> Word64
blockToW64 [] = 0
blockToW64 (b:bs) =
  (fromIntegral $ byteToInt b) * (16 ^ 4) ^ (length bs) + blockToW64 bs

splitBlocks :: [Byte] -> ([Byte], [Byte])
splitBlocks = splitAt 4

blocksToIP6 :: ([Byte], [Byte]) -> IPAddress6
blocksToIP6 (b1, b2) = IPAddress6 (blockToW64 b1) (blockToW64 b2)

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  bytes <- many $ try ((many hexDigit) <* char ':') <|> ((some hexDigit) <* eof)
  return $ blocksToIP6 $ splitBlocks $ mkFull bytes

mkFull :: [Byte] -> [Byte]
mkFull xs
  | length xs == 8 = xs
  | otherwise = go xs []
  where
    go (y:ys) acc
      | length y == 0 = mkFull (acc ++ (y : "" : ys))
      | otherwise = go ys (acc ++ [y])

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
