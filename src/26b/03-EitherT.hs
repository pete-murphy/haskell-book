{-# LANGUAGE InstanceSigs #-}

module EitherT where

newtype EitherT e m a =
  EitherT (m (Either e a))

runEitherT :: EitherT e m a -> m (Either e a)
runEitherT (EitherT ema) = ema

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT (fmap f <$> ema)

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (Right x)
  EitherT f <*> EitherT ema = EitherT $ (fmap (<*>) f) <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  ema >>= f =
    EitherT $ do
      v <- runEitherT ema
      case v of
        Left x  -> pure $ Left x
        Right x -> runEitherT $ f x

swapEither :: Either e a -> Either a e
swapEither x =
  case x of
    Right x -> Left x
    Left x  -> Right x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT amb) = do
  v <- amb
  case v of
    Right x -> bmc x
    Left x  -> amc x
