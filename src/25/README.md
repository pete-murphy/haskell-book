## 25.1 Composing types

Maybe helpful for intuition regarding Functor > Applicative > Monad trinity:

> Functors and applicatives are both closed under composition: this means that you can compose two functors (or two applicatives) and return another functor (or applicative, as the case may be). **This is not true of monads, however; when you compose two monads, the result is not necessarily another monad.** _(Emphasis mine)_

But sometimes you want to compose monads nonetheless. Enter the _monad transformer_.

## 25.2 Common functions as types


