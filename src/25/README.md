## 25.1 Composing types

Maybe helpful for intuition regarding Functor > Applicative > Monad trinity:

> Functors and applicatives are both closed under composition: this means that you can compose two functors (or two applicatives) and return another functor (or applicative, as the case may be). **This is not true of monads, however; when you compose two monads, the result is not necessarily another monad.** _(Emphasis mine)_

But sometimes you want to compose monads nonetheless. Enter the _monad transformer_.

## 25.2 Common functions as types

> ... a monad transformer is a type constructor that takes a monad as an argument.

## 25.3 Two little functors sittin’ in a tree, L-I-F-T-I-N-G

> As with the anonymous product `(,)` and the anonymous sum `Either`, the `Compose` type allows us to express arbitrarily nested types:
 ```haskell
 v :: Compose []
              Maybe
              (Compose Maybe [] Integer)
 v = Compose [Just (Compose $ Just [1])]
 ```
 The way to think about this is that the composition of two datatypes that have a `Functor` instance gives rise to a new `Functor` instance.

## 25.5 Twonad?

I don't understand this part, on why composing monads does not (necessarily?) give you a monad:

> The issue comes down to a lack of information. Both types `Compose`

## 25.8 IdentityT
> 
  ```haskell
  instance Monad m
        => Monad (IdentityT m) where
      return = pure
      IdentityT ma >>= f =
        IdentityT $ ma >>= runIdentityT . f
  ```
  The `Monad` instance is tricky, so we’re going to do a few things to break it down. Keep in mind that `Monad` is where we have to really use concrete type information from `IdentityT` in order to make the types fit.

### The bind breakdown
> 
  ```haskell
  instance Monad m
        => Monad (IdentityT m) where
      return = pure
      IdentityT ma >>=  f =
  --  [    1     ] [2] [3]
         IdentityT $ ma
  --        [8]      [4]
         >>= runIdentityT . f
  --     [5]    [7]        [6]
  ```
  1. First we pattern match or unpack the `m a` value of `IdentityT m a` via the data constructor. Doing this has the type `IdentityT m a -> m a` and the type of `ma` is `m a`.
  2. The type of bind we are implementing is the following:
  ```haskell
  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  ```
  This is the instance we are defining.
  3. The function we're binding over is `IdentityT m a`. It has the following type:
  ```haskell
  (a -> IdentityT m b)
  ```
  4. Here `ma` is the same one we unpacked out of the `IdentityT` data constructor and has the type `m a`. Removed from its `IdentityT` context, this is now the `m a` that this bind takes as its first argument.
  5. This is a different bind! The first bind is the bind we're trying to implement; this bind is its definition or implementation. We're now using the `Monad` we asked for in the instance declaration with the constraint `Monad m =>`. This will have the type:
  ```haskell
  (>>=) :: m a -> (a -> m b) -> m b
  ```
  This is with respect to the `m` in the type `IdentityT m a`, not the class of `Monad` instances in general. In other words, since we have already unpacked the `IdentityT` bit and, in a sense, gotten it out of the way, this bind will be the bind for the type `m` in the type `IdentityT m`. We don't know what `Monad` that is yet, and we don't need to; since it has then `Monad` typeclass constraint on that variable, we know it already has a `Monad` instance defined for it, and this second bind will be the bind defined for that type. All we're doing here is defining how to use that bind in the presence of the additional `IdentityT` structure.
  6. This is the same `f` which was an argument to the `Monad` instance we are defining, of type:
  ```haskell
  (a -> IdentityT m b)
  ```
  7. We need `runIdentityT` because `f` returns `IdentityT m b`, but the `>>=` for the `Monad m =>` has the type `m a -> (a -> m b)`. It'll end up trying to join `m` (`IdentityT m b`), which won't work because `m` and `IdentityT m` are not the same type. We use `runIdentityT` to unpack the value. Doing this has the type `IdentityT m b -> m b` and the composition `runIdentityT`. `f` in this context has the type `a -> m b`. You can use `undefined` in GHCi to demonstrate this for yourself.
  8. To satisfy the type of the outer bind we are implementing for the `Monad` of `IdentityT m`, which expects a final result of the type `IdentityT m b`, we must take the `m b` which the expression `ma >>= runIdentity . f` returns and repack it in `IdentityT`.

A maybe handy example of how this plays out at term level:

> 
  ```
  Prelude> let sumR = return . (+1)
  Prelude> IdentityT [1, 2, 3] >>= sumR
  IdentityT { runIdentityT = [2, 3, 4] }
  ```
