import Data.Char

puzzle1 :: String -> Int
puzzle1 (x:xs) | x == xs!!((length xs)-1) = (+) (digitToInt x) (puzzle1' xs)
puzzle1 (x:xs) | otherwise = puzzle1' (x:xs)


puzzle1' :: String -> Int
puzzle1' [x]      = 0
puzzle1' (x:xs) | x == xs!!0 = (+) (digitToInt x)  (puzzle1' xs)
                | otherwise = puzzle1' xs


puzzle2 :: String -> Int -> Int
puzzle2 list n | n >= (length list) = 0
puzzle2 list n | list!!n == (((drop n list)++(take n list)) !!((length list) `div` 2))
                            = (+) (digitToInt (list!!n)) (puzzle2 list (n+1))
puzzle2 list n | otherwise = puzzle2 list (n+1)
