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

type Log = [LogDay]

data LogDay =
  LD Date
     LogEntries
  deriving (Eq, Ord, Show)

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

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany $ char ' ' <|> char '\n'

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

parseLogEntry :: Parser (Time, Activity)
parseLogEntry = do
  time <- parseTime
  _ <- char ' '
  activity <- many (noneOf "--") <* skipComments <* (noneOf "\n")
  skipEOL
  return (time, activity)

parseLogDay :: Parser LogDay
parseLogDay = do
  skipWhiteSpace
  skipComments
  date <- string "# " >> parseDate
  entries <- many parseLogEntry
  return $ LD date (M.fromList entries)

parseLog :: Parser Log
parseLog = undefined

twoLineEx :: ByteString
twoLineEx =
  [r|08:00 Breakfast --skip bfast?
09:00 Sanitizing moisture collector
|]

logEx :: ByteString
logEx =
  [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

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
    describe "Comment parsing" $
      it "ignores comments before date" $ do
        let t = "--comment\n \n# 2025-02-07"
            m =
              parseByteString
                (skipComments >> skipWhiteSpace >> char '#' >> char ' ' >>
                 parseDate)
                mempty
                t
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just (D 2025 2 7)
    describe "Log entry parsing" $
      it "parses a log entry to a tuple" $ do
        let t = "08:00 Breakfast"
            m = parseByteString parseLogEntry mempty t
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just ((T 8 0), "Breakfast")
    describe "Log parsing" $
      it "parses an entire log" $ do
        let m = parseByteString parseLog mempty logEx
            r' = maybeSuccess m
        print m
        r' `shouldNotBe` Nothing
