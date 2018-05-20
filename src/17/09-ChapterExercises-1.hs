module ChapterExercises1 where

import           Data.Monoid

-- 1
pure' :: a -> [a]
pure' = (: [])

ap' :: [(a -> b)] -> [a] -> [b]
ap' [] _          = []
ap' _ []          = []
ap' (f:fs) (x:xs) = (f x) : (ap' fs xs)

-- 2 (Not sure about this one)
pure'' :: a -> IO a
pure'' = return

ap'' :: IO (a -> b) -> IO a -> IO b
ap'' = (<*>)

-- 3
pure''' :: Monoid a => b -> (a, b)
pure''' x = (mempty, x)

ap''' :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
ap''' (a, f) (a', x) = (a <> a', f x)

-- 4
pure'''' :: Monoid a => b -> (a -> b)
pure'''' = const

ap'''' :: a -> (b -> c) -> (a -> b) -> (a -> c)
ap'''' _ f g = f . g
