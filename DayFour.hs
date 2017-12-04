import Data.List


puzzle1 :: FilePath -> IO Int
puzzle1 file = do input <- readFile file
                  return (length [ys | ys <- (lines input),
                     (validLines ys ) == True, (rearrange ys) == True])


validLines :: String ->  Bool
validLines xs = and [x /= y | l <- [1..length xs], x<-(drop l (words xs)),
                               y<-(take l (words xs))]


rearrange :: String -> Bool
rearrange (xs) =  and [not (null (x \\ y)) || not (null (y \\ x))
                | l <- [1..length xs], x<-(drop l (words xs)),
                y<-(take l (words xs))]
