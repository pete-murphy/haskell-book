module Example where

import           Control.Monad.State

code :: StateT [Integer] IO ()
code = pop >>= \x -> io (print x) >> pop >>= \y -> io (print y) >> pure ()

pop :: StateT [Integer] IO Integer
pop = do
  (x:xs) <- get
  put xs
  return x

io :: IO a -> StateT [Integer] IO a
io = liftIO

main :: IO ()
main = undefined
