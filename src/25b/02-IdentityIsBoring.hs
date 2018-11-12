{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ApplicativeDo #-}

module IdentityIsBoring where

-- import Control.Monad
newtype Identity a = Identity
  { runIdentity :: a
  } deriving (Eq, Show)

newtype IdentityT f a = IdentityT
  { runIdentityT :: f a
  } deriving (Eq, Show)

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT f) <*> (IdentityT a) = IdentityT (f <*> a)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f =
    let aimb = (runIdentityT . f) =<< ma
     in IdentityT aimb

--  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose (pure (pure x))
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ do
      f' <- f
      a' <- a
      pure $ f' <*> a'
--  (Compose f) <*> (Compose a) = Compose $ (<*>) ((<*>) <$> f) a
