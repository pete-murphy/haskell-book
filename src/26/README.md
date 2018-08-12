## 26.8 Lexically inner is structurally outer

One of the trickier parts of monad transformers is that the lexical representationof the types will violate your intuitions with respect to the relationship it has with the structure of your values. Let us note something in the definitions of the following types:
```
newtype ExceptT e m a =
  ExceptT { runExceptT :: m (Either e a) }

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
```

A necessary byproduct of how transformers work is that the additional structure _m_ is always wrapped around our value. One thing to note is that it's only wrapped around things we can have, not things we need, such as with `ReaderT`.

> ???

A terminological point to keep in mind when reading about monad transformers is that when Haskellers say base monad they usually mean what is structurally outermost.

```
type MyType a = IO [Maybe a]
```
In `MyType`, the base monad is `IO`.

## 26.9 MonadTrans

We often want to lift functions into a larger context. We've been doing this for  a while with `Functor`, which lifts a function into a context and applies it to the value inside. The facility to do this also undergirds `Applicative`, `Monad`, and `Traversable`. However, `fmap` isn't always enough, so we have some functions that are essentially `fmap` for different contexts:
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

> ???
 
  ```haskell
  class MonadTrans t where
      -- / Lift a computation from
      --   the argument monad to
      --   the constructed monad.
      lift :: (Monad m) => m a -> t m a
  ```

### MonadTrans instances

Now you see why we have `MonadTrans` and have a picture of what `lift`, the only method of `MonadTrans`, does.
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
Roughly speaking, this has taken an `m a` and lifted it into a `MaybeT` context.
The general pattern with `MonadTrans` instances demonstrated by `MaybeT` is that you're usually going to lift the injection of the known structure (with `MaybeT`, the known structure is `Maybe`) over some `Monad`. Injection of structure usually means `return`, but since with `MaybeT` we know we want `Maybe` structure, we use `Just`. That transforms an `m a` into `m (T a)` where capital `T` is some concrete type you're lifting the `m a` into. Then to cap it all off, you use the data constructor for your monad transformer, and the value is now lifted into the larger context. Here's a summary of the stages the type of the value goes through.
```haskell
v :: Monad m => m a
liftM Just :: Monad m => m a -> m (Maybe a)
liftM Just v :: m (Maybe a)
MaybeT (liftM Just v) :: MaybeT m a
```

## 26.10 MonadIO aka zoom-zoom

There's more than one way to skin a cat and there's more than one way to lift and action over additional structure. `MonadIO` is a different design than `MonadTrans` because rather than lifting through one layer at a time, `MonadIO` is intended to keep lifting your `IO` action until it is lifted over all structure embedded in the outermost `IO` type. The idea here is that you'd write `liftIO` once and it would instantiate to all of the following types:

## 26.12 Monads do not commute

Remember that monads in general do not commute, and you aren't guaranteed something sensible for every possible combination of types. The kit we have for constructing and using monad transformers is useful but is not a license to not think!

> In mathematics, a binary operation is commutative if changing the order of the operands does not change the result.
