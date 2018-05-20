module ChapterExercises2 where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    return $ Two a0 a1

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  Two a f <*> Two a' x = Two (a <> a') (f x)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Three a0 a1 a2

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    return $ Three' a0 a1 a1

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f f' <*> Three' a' x x' = Three' (a <> a') (f x) (f' x')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    return $ Four a0 a1 a2 a3

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    return $ Four' a0 a0 a0 a1

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a b c f <*> Four' a' b' c' x = Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

{-
runQb :: Applicative m => String -> m -> IO ()
runQb name = do
  putStrLn "Testing " ++ show name
  quickBatch $ applicative $ pure ("poop", 2 :: Sum Int, 2 :: Product Int)
-}
type P = (String, Sum Int, Product Int)

main :: IO ()
main = do
  putStr "Testing Pair:"
  quickBatch $ applicative (undefined :: Pair P)
  putStr "Testing Two:"
  quickBatch $ applicative (undefined :: Two P P)
  putStr "Testing Three:"
  quickBatch $ applicative (undefined :: Three P P P)
  putStr "Testing Three':"
  quickBatch $ applicative (undefined :: Three' P P)
  putStr "Testing Four:"
  quickBatch $ applicative (undefined :: Four P P P P)
  putStr "Testing Four':"
  quickBatch $ applicative (undefined :: Four' P P)
