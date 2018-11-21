module DList where

import           Criterion.Main

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

empty :: DList a
empty = DL id

{-# INLINE empty #-}
empty' :: DList a
empty' = DL (++ [])

{-# INLINE empty' #-}
empty'' :: DList a
empty'' = DL ([] ++)

{-# INLINE empty'' #-}
empty''' :: DList a
empty''' = DL (\x -> x)

{-# INLINE empty''' #-}
singleton :: a -> DList a
singleton x = DL (x :)

{-# INLINE singleton #-}
toList :: DList a -> [a]
toList (DL f) = f []

{-# INLINE toList #-}
infixr `cons`

cons :: a -> DList a -> DList a
cons x (DL f) = DL $ (x :) . f

{-# INLINE cons #-}
infixl `snoc`

snoc :: DList a -> a -> DList a
snoc (DL f) x = DL $ f . (x :)

{-# INLINE snoc #-}
append :: DList a -> DList a -> DList a
append (DL f) (DL g) = DL $ f . g

{-# INLINE append #-}
schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDlist :: DList Int -> Int -> [Int]
constructDlist empty i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456
    , bench "concat dlist empty" $ whnf (constructDlist empty) 123456
    , bench "concat dlist empty'" $ whnf (constructDlist empty') 123456
    , bench "concat dlist empty''" $ whnf (constructDlist empty'') 123456
    , bench "concat dlist empty'''" $ whnf (constructDlist empty''') 123456
    ]
