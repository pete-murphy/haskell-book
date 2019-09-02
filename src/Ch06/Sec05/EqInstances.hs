module Ch06.Sec05.EqInstances where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn a == TisAn b = a == b

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Two a b == Two a' b' = a == a' && b == b'

data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt n == TisAnInt n' = n == n'
  TisAString s == TisAString s' = s == s'
  _ == _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair a b == Pair a' b' = a == a' && b == b'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'
  _ == _ = False
