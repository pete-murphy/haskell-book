module ChapterExercises06 where

import           Control.Applicative
import           Data.Word
import           Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseIPList :: Parser [Integer]
parseIPList = many $ try (integer <* char '.') <|> (integer <* eof)

parseIPAddress :: Parser IPAddress
parseIPAddress =
  (IPAddress .
   fromIntegral . foldl (+) 0 . zipWith (*) (((2 ^ 8) ^) <$> [0 ..]) . reverse) <$>
  parseIPList
