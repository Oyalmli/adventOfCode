module Main where

import Data.List.Split ( splitOn )

main :: IO ()
main = interact 
    $ show
    . minimum
    . distFrom [0..]
    . map (read::String -> Int)
    . splitOn ","

distFrom :: [Int] -> [Int] -> [Int]
distFrom [] ls = []
distFrom (x:xs) ls
    | x > maximum ls = []
    | otherwise = sum (map (diff x) ls) : distFrom xs ls

diff :: Int -> Int -> Int
diff a b = abs (a-b)
