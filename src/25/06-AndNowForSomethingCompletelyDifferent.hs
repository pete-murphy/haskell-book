module AndNowForSomethingCompletelyDifferent where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1
data Deux a b =
  Deux a
       b

instance Bifunctor Deux where
  first f (Deux x y) = Deux (f x) y
  second f (Deux x y) = Deux x (f y)

-- 2
data Const a b =
  Const a

instance Bifunctor Const where
  first f (Const x) = Const (f x)
  second _ (Const x) = Const x

-- 3
data Drei a b c =
  Drei a
       b
       c

instance Bifunctor (Drei a) where
  first f (Drei x y z) = Drei x (f y) z
  second f (Drei x y z) = Drei x y (f z)

-- 4
data SuperDrei a b c =
  SuperDrei a
            b

instance Bifunctor (SuperDrei a) where
  first f (SuperDrei x y) = SuperDrei x (f y)
  second _ (SuperDrei x y) = SuperDrei x y

-- 5
data SemiDrei a b c =
  SemiDrei a

instance Bifunctor (SemiDrei a) where
  first _ (SemiDrei x) = SemiDrei x
  second _ (SemiDrei x) = SemiDrei x

-- 6
data Quadriceps a b c d =
  Quadzzz a
          b
          c
          d

instance Bifunctor (Quadriceps a b) where
  first f (Quadzzz v x y z) = Quadzzz v x (f y) z
  second f (Quadzzz v x y z) = Quadzzz v x y (f z)

-- 7
data Either' a b
  = Left' a
  | Right' b

instance Bifunctor Either' where
  first f (Left' x) = Left' (f x)
  second f (Right' x) = Right' (f x)
