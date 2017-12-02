import Data.List

puzzle1 :: [[Int]] -> Int
puzzle1 xs = sum [(puzzle1' x) | x <- xs]


puzzle1' :: [Int] -> Int
puzzle1' (xs) =  ((maximum xs) - (minimum xs))


puzzle2 :: [[Int]] -> Int
puzzle2 xs = sum [(puzzle2' x) | x <- xs]

puzzle2' :: [Int] -> Int
puzzle2' xs = head [x `div` y | x <- xs, y <- xs, x `mod` y == 0, y /= x]
