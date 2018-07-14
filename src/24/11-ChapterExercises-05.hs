{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ChapterExercises05 where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta

type LogDay = Map Day LogEntries

type LogEntries = Map Time Activity

data Date =
  D Year
    Month
    Day
  deriving (Eq, Ord, Show)

type Year = Int

type Month = Int

type Day = Int

data Time =
  T Hour
    Minute
  deriving (Eq, Ord, Show)

type Hour = Int

type Minute = Int

type Activity = String

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany
    (do _ <- count 2 $ char '-'
        skipMany (noneOf "\n")
        skipEOL)

parseDate :: Parser Date
parseDate = do
  year <- count 4 $ digit
  _ <- char '-'
  month <- count 2 $ digit
  _ <- char '-'
  date <- count 2 $ digit
  return $ D (read year) (read month) (read date)

parseTime :: Parser Time
parseTime = do
  hour <- many digit
  _ <- char ':'
  mins <- many digit
  return $ T (read hour) (read mins)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing

main :: IO ()
main =
  hspec $ do
    describe "Time parsing" $
      it "can parse a time into hours and minutes" $ do
        let m = parseByteString parseTime mempty "8:00"
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just (T 8 0)
