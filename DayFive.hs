module Main where
import Data.List
import qualified Data.List as List

--port Data.Char

import Data.Map
import qualified Data.Map as Map


main = puzzle3 "InputDayFive.txt"





puzzle1 :: FilePath -> IO Int
puzzle1 file = do
  input <- readFile file
  return (puzzle1' (lines input) 0 0)


puzzle1' :: [String] -> Int -> Int -> Int
puzzle1' input steps index | index >= length input = (steps)
puzzle1' input steps index = puzzle1' (updateValue input index)
           (steps+1) (index +  (read (input!!index) :: Int))

updateValue :: [String] -> Int -> [String]
updateValue input n = List.take n input ++ [(show((read (input!!n) :: Int) + 1))] ++ List.drop (n + 1) input

puzzle2 :: FilePath -> IO Int
puzzle2 file = do
  input <- readFile file
  return (puzzle2' (lines input) 0 0)


puzzle2' :: [String] -> Int -> Int -> Int
puzzle2' input steps index | index >= length input = (steps)
puzzle2' input steps index = puzzle2' (updateValue2 input index)
           (steps+1) (index + (read (input!!index) :: Int))

updateValue2 :: [String] -> Int -> [String]
updateValue2 input n | ((read (input!!n) :: Int) >= 3) = List.take n input ++
           [(show((read (input!!n) :: Int) - 1))] ++ List.drop (n + 1) input
updateValue2 input n | otherwise = List.take n input ++
           [(show((read (input!!n) :: Int) + 1))] ++ List.drop (n + 1) input


puzzle3 :: FilePath -> IO ()
puzzle3 file = do
  input <- readFile file
  print $ show (puzzle3' (fromList (zip
   [0..((length input)-1)] (List.map  strToInt (lines input)))) 0 0 )

strToInt :: String -> Int
strToInt x = read x

puzzle3' :: Map Int Int -> Int -> Int -> Int
puzzle3' input key steps | key >= length input = steps
puzzle3' input key steps | (input!key) >= 3 = puzzle3' (update (\x -> Just((input!key) - 1)) key input)
           (key + (input!key)) (steps+1)
                         | otherwise = puzzle3' (update (\x -> Just((input!key) + 1)) key input)
                                  (key + (input!key)) (steps+1)
