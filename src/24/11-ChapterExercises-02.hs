module ChapterExercises02 where

import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf $ concatMap show [0 .. 9]

base10Integer :: Parser Integer
base10Integer = try (read <$> some parseDigit) <?> "integer"
