{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s ->
      let (x, s') = g s
       in (f x, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g =
    Moi $ \s ->
      let (h, s') = f s
          (a, s'') = g s'
       in (h a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= g =
    Moi $ \s ->
      let (a, s') = f s
          (b, s'') = runMoi (g a) s'
       in (b, s'')

-- 1
get :: Moi s s
get = Moi $ \s -> (s, s)

-- 2
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

-- 3
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

-- 4
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

-- 5
modify :: (s -> s) -> Moi s ()
modify ss = Moi $ \s -> ((), ss s)
