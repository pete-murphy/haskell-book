module TryExcept where

import           Control.Exception

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try (print (div 5 denom))

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e  -> print e
    Right _ -> pure ()
