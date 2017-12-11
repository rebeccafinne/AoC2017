import Data.List

puzzle1 :: String ->  Int
puzzle1 str = puzzle1' (map read' (words str))


read' :: String -> Int
read' str = read str :: Int

puzzle1' :: [Int] -> Int
puzzle1' xs = 5
