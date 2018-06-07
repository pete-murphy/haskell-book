module TheStateNewtype where

newtype State s a = State
  { runState :: s -> (a, s)
  }

newtype Reader r a = Reader
  { runReader :: r -> a
  }
