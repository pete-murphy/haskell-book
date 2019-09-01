module Ch05.Sec08.TypeKwonDo where

import           Data.Bifunctor (bimap)

-- This is same as `Void`
data Woot

data Blah

f' :: Woot -> Blah
f' = undefined

g' :: (Blah, Woot) -> (Blah, Blah)
g' = fmap f'

-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2
data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform = bimap xz yz

-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . (g . f)
