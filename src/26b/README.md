# Chapter 26: Monad transformers

## Notes

### How `do` syntax desugars to bind `(>>=)`

This

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

And this

```haskell
rPrintAndInc =
  ReaderT $ \x -> do
    putStrLn $ "Hi: " ++ show x
    putStrLn "foo"
    putStrLn "bar"
    pure $ x + 1
```

desugars to

```haskell
rPrintAndInc =
  ReaderT $ \x ->
    (putStrLn $ "Hi: " ++ show x) >>= \_ ->
      putStrLn "foo" >>= \_ ->
        putStrLn "bar" >>= \_ ->
          pure $ x + 1
```

### Follow up for `StateT`

Check out [Simple StateT use](https://wiki.haskell.org/Simple_StateT_use).
