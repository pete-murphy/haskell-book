module ChapterExercises where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity

-- 1
rDec' :: Num a => Reader a a
rDec' = ReaderT $ \x -> Identity x - 1

-- 2
rDec :: Num a => Reader a a
rDec = ReaderT $ subtract 1 <$> Identity

-- 3
rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ \x -> Identity $ show x

-- 4
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity <$> show

-- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \x -> do
    putStrLn $ "Hi: " ++ show x
    pure $ x + 1

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = undefined
