module Main where

import           Control.Exception
import           Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was: " ++ show e)

foo n =
  case n of
    1 -> Nothing
    2 ->
      case n + 1 of
        3 -> Nothing
        _ -> Just True

main = writeFile "zzz" "hi" `catch` handler
