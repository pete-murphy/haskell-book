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

