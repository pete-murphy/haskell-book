{-# LANGUAGE OverloadedStrings #-}

module TryTry where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

type IntOrRat = Either Integer Rational

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseIntOrRat :: Parser IntOrRat
parseIntOrRat = (Left <$> try integer) <|> (Right <$> parseFraction)
