module Main where

import           Criterion.Main
import           Debug.Trace

myList :: [Int]
myList = trace "myList was evaluated" ([1 .. 9999] ++ [undefined])

infixl 9 !?

{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      (\x r k ->
         case k of
           0 -> Just x
           _ -> r (k - 1))
      (const Nothing)
      xs
      n

-- _ !? n
--   | n < 0 = Nothing
-- [] !? _ = Nothing
-- (x:_) !? 0 = Just x
-- (_:xs) !? n = xs !? (n - 1)
main :: IO ()
main =
  defaultMain
    [ bench "index list 9999" $ whnf (myList !!) 9998
    , bench "index list maybe index 9999" $ nf (myList !?) 9999
    ]
