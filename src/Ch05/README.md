# Types

```haskell
-- This is same as 'Data.Void.Void'
data Woot 
```

I get confused about **type constructors** as opposed to **data constructors**. Not sure a good mnemonic, but data constructors exist at the value level, and so every data constructor is either a **constant** or a **function**. For example
```haskell
-- Constant
True :: Bool

-- Constant
Nothing :: Maybe a

-- Function
Just :: a -> Maybe a

-- Function
(,) :: a -> b -> (a, b)
```
