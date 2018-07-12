module ChapterExercises01 where

import           Control.Applicative
import           Text.Trifecta

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show, Ord)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer Major
         Minor
         Patch
         Release
         Metadata
  deriving (Eq, Show, Ord)

skipDots :: Parser ()
skipDots = skipMany $ char '.'

parseNos :: Parser NumberOrString
parseNos = do
  skipDots
  x <- (NOSI <$> integer) <|> (NOSS <$> some letter)
  return x

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  skipDots
  minor <- integer
  skipDots
  patch <- integer
  _ <- skipMany $ char '-'
  release' <- many parseNos
  _ <- skipMany $ char '+'
  metadata <- many parseNos
  return $ SemVer major minor patch release' metadata
