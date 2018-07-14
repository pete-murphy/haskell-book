module ChapterExercises03 where

import           Control.Applicative
import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf $ concatMap show [0 .. 9]

base10Integer :: Parser Integer
base10Integer = try (read <$> some parseDigit) <?> "integer"

base10Integer' :: Parser Integer
base10Integer' =
  try
    (do _ <- char '-'
        nums <- base10Integer
        return $ negate nums) <|>
  base10Integer
