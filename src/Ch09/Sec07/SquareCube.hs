module Ch09.Sec07.SquareCube where

mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [y ^ 3 | y <- [1 .. 5]]

-- 1.
mySqrCube = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
mySqrCube' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
howMany :: Int
howMany = length mySqrCube'
