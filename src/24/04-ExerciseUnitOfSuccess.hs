module ExerciseUnitOfSuccess where

import           Text.Trifecta

allInt :: Parser Integer
allInt = do
  num <- decimal
  eof
  return num
