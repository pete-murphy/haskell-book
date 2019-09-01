module Ch05.Sec08.GivenAType where

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ = fmap (yToZ . xToY)

i :: a -> a
i x = x

c :: a -> b -> a
c = const

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' _ = id

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> (a -> c)
co = (<$>)

a :: (a -> c) -> a -> a
a = c'

a' :: (a -> b) -> a -> b
a' = ($)
