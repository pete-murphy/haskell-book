{-# LANGUAGE InstanceSigs #-}

module Twonad where

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose gh) = Compose $ fmap f <$> gh

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ (pure . pure) a
  Compose f <*> Compose a = (f <$>)

instance (Monad f, Monad g) => Monad (Compose f g)
