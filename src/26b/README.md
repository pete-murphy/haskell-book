# Chapter 26: Monad transformers

## Notes

### How `do` syntax desugars to bind `(>>=)`

```haskell
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f =
  MaybeT $ do
    a <- ma
    case a of
      Nothing -> pure Nothing
      Just x  -> runMaybeT $ f x
```

desugars to

```haskell
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f =
    MaybeT $
    ma >>= \a ->
      case a of
        Nothing -> pure Nothing
        Just x  -> runMaybeT $ f x
```
