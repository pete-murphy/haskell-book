## 25.1 Composing types

> Functors and applicatives are both closed under composition. ... This is not true of monads.

But there are times when we want to compose monads.

> Composing monads allows you to build up computations with multiple effects.

> A monad transformer is a variant of an ordinary type that takes an additional type argument which is assumed to have a `Monad` instance. ... The transformer variant gives us a `Monad` instance that binds over both bits of structure. This allows us to compose monads and combine their effects.

## 25.2 Common functions as types

