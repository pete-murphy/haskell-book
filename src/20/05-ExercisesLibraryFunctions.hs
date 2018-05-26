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
elem' n = foldr ((||) . (==) n) False

-- This doesn't work:
-- elem' n xs = foldr (||) False $ map ((==) n) xs
-- 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (min . Just) Nothing

-- 5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

-- 6
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7
length' :: (Foldable t) => t a -> Int
length' = foldr ((+) . const 1) 0

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty
