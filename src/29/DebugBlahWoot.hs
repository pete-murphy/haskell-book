module DebugBlahWoot where

import           Debug.Trace

blah :: IO String
blah = pure "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = pure $ trace "inner trace" "woot"

main :: IO ()
main = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w
