{-# LANGUAGE InstanceSigs #-}

module EitherT where

newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT f <*> EitherT a = EitherT $ ((<*>) $ fmap (<*>) f) a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT mea >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left x  -> return $ Left x
        Right y -> runEitherT (f y)

swapEither :: Either a b -> Either b a
swapEither eab =
  case eab of
    Left x  -> Right x
    Right y -> Left y

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) = (either f g) =<< mab
