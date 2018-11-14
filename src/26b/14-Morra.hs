module Morra where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Random

play :: IO Int
play = getStdRandom $ randomR (1, 2)

turn :: StateT (Int, Int) IO Int
turn =
  StateT $ \(p, c) -> do
    p' <- lift $ read <$> getLine
    c' <- lift play
    pure $
      if even (c' + p')
        then (1, (p, c + 1))
        else (2, (p + 1, c))

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT $ \x -> do
    putStrLn $ "Hi: " ++ show x
    pure $ (show x, x + 1)

example1 :: State (Integer, Integer) Integer
example1 = do
  replicateM 100 $ do
    (n, m) <- get
    put (m, n + m)
  (n, _) <- get
  return n

main :: IO ()
main = mapM (runStateT sPrintIncAccum) [1 .. 5] >>= mapM_ (putStrLn . show)
