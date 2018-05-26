module ExercisesLibraryFunctions where

import           Data.Foldable
import           Data.Monoid

-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' n xs = foldr (||) False $ map ((==) n) xs
