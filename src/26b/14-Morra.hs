{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Random

turns :: Int -> StateT (Int, Int) IO String
turns n = do
  void $
    replicateM n $ do
      lift $ putStr "P: "
      p' <- lift (read <$> getLine :: IO Int)
      c' <- lift (getStdRandom $ randomR (1, 2) :: IO Int)
      lift $ putStrLn $ "C: " ++ show c'
      (c, p) <- get
      if even (c' + p')
        then put (p, c + 1)
        else put (p + 1, c)
  (c'', p'') <- get
  pure $
    if c'' > p''
      then "Computer wins"
      else "You win"

main :: IO ()
main = runStateT (turns 3) (0, 0) >>= (putStrLn . show)
