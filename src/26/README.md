## 26.8 Lexically inner is structurally outer

> One of the trickier parts of monad transformers is that the lexical representationof the types will violate your intuitions with respect to the relationship it has with the structure of your values. Let us note something in the definitions of the following types:
  ```
  newtype ExceptT e m a =
    ExceptT { runExceptT :: m (Either e a) }

  newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

  newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }
  ```

  A necessary byproduct of how transformers work is that the additional structure _m_ is always wrapped around our value. One thing to note is that it's only wrapped around things we can have, not things we need, such as with `ReaderT`.

???

> A terminological point to keep in mind when reading about monad transformers is that when Haskellers say base monad they usually mean what is structurally outermost.

  ```
  type MyType a = IO [Maybe a]
  ```
  In `MyType`, the base monad is `IO`.

## 26.9 MonadTrans

> We often want to lift functions into a larger context. We've been doing this for  a while with `Functor`, which lifts a function into a context and applies it to the value inside. The facility to do this also undergirds `Applicative`, `Monad`, and `Traversable`. However, `fmap` isn't always enough, so we have some functions that are essentially `fmap` for different contexts:
  ```
  fmap  :: Functor f
        => (a -> b) -> f a -> f b

  liftA :: Applicative f
        => (a -> b) -> f a -> f b

  liftM :: Monad m
        => (a -> r) -> m a -> m r
  ```

### The typeclass that lifts
  `MonadTrans` is a typeclass with one core method: `lift`. Speaking generally, it is about lifting actions in some `Monad` over a transformer type which wraps itself in the original `Monad`. Fancy!

???

> 
  ```haskell
  class MonadTrans t where
      -- / Lift a computation from
      --   the argument monad to
      --   the constructed monad.
      lift :: (Monad m) => m a -> t m a
  ```

### MonadTrans instances

> Now you see why we have `MonadTrans` and have a picture of what `lift`, the only method of `MonadTrans`, does.
  Here are some examples of `MonadTrans` instances.

  1. `IdentityT`
  ```haskell
  instance MonadTrans IdentityT where
      lift = IdentityT
  ```

  2. `MaybeT`
  ```haskell
  instance MonadTrans MaybeT where
      lift = MaybeT . liftM Just
  ```

  ```haskell
  lift
    :: (Monad m)
    => m a -> t m a
  (MaybeT . liftM Just)
    :: Monad m
    => m a -> MaybeT m a
  
  MaybeT
    :: m (Maybe a) -> (MaybeT m a)
  (liftM Just)
    :: Monad m
    => m a -> m (Maybe a)
  ```
