module ExampleGlobal where

import           Control.Monad.State

data Vars = Vars
  { var1 :: Int
  , var2 :: Float
  }

type MyState a = StateT Vars IO a

type Selector a = (MyState a, a -> MyState ())

s1 :: Selector Int
s1 = undefined
-- s1 = (gets var1, \x -> modify
