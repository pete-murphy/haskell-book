module Ch07.Sec05.CasePractice where

-- 1
-- functionC x y = if (x > y) then x else y
functionC :: Ord a => a -> a -> a
functionC x y =
  case compare x y of
    GT -> x
    _  -> y

-- 2
-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
  case mod n 2 of
    0 -> n + 2
    _ -> n

-- 3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
