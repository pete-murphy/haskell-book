module FixTheCode where

import           Control.Monad
import           Control.Monad.Trans.Maybe

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite =
  MaybeT $ do
    v <- getLine
    pure $
      if isValid v
        then Just v
        else Nothing

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn $ "Good, was very excite: " ++ e
