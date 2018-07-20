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


