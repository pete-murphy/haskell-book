module ChapterExercises04 where

import           Control.Applicative
import           Text.Trifecta

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

parseDigit :: Parser Char
parseDigit = oneOf $ concatMap show [0 .. 9]

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- skipMany $ try $ char '1' >> char '-'
  _ <- skipMany $ char '('
  area <- token $ many parseDigit
  _ <- skipMany $ char ')' <|> char '-' <|> char ' '
  exch <- token $ many parseDigit
  _ <- skipMany $ char '-'
  line' <- token $ many parseDigit
  return $
    case length area of
      10 ->
        let a = take 3 area
            b = take 3 $ drop 3 area
            c = drop 6 area
            [a', b', c'] = read <$> [a, b, c]
         in PhoneNumber a' b' c'
      _ ->
        let [a, b, c] = read <$> [area, exch, line']
         in PhoneNumber a b c
