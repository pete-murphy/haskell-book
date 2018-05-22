module ChapterExercises2 where

import           Control.Monad
import           Data.Functor

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- liftA2, but
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = (:) <$> (f x) <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
