module Ch06.Sec14.TypeKwonDoTwo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
-- chk f a b = f a == b
-- fmap :: (b -> b -> Bool) -> f b -> f (b -> Bool)
-- fmap :: (b -> b -> Bool) -> (a -> b) -> (a -> b -> Bool)
-- fmap (==) :: (a -> b) -> a -> b -> Bool
chk = fmap (==)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a * fromIntegral n
