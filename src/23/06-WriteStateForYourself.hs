{-# LANGUAGE InstanceSigs #-}

module WriteStateForYourself where

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- I know I could refactor this to something prettier
  -- but I forget how at the moment
  -- fmap f (Moi g) = Moi (\x -> (f (fst $ g x), snd $ g x))
  -- Looking at others', this could be
  fmap f (Moi g) =
    Moi $ \s ->
      let (x, s') = g s
       in (f x, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g =
    Moi $ \s
    -- `f s` gives you back a tuple of type
    -- `(_function_, s)` ???
    -- where `_function_ :: (a -> b)` ???
     ->
      let (h, s') = f s
    -- in this case `g s'` returns a `(a, s)`
    -- so we can then plug that into our `a -> b`
    -- function -- which we called `h` above
    -- (I don't understand what's going on with
    -- the various `s`'s)
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
