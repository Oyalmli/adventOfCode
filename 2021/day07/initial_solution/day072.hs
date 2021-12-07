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
    | otherwise = sum (map (dDiff x) ls) : distFrom xs ls

diff :: Int -> Int -> Int
diff a b = abs (a-b)

dDiff :: Int -> Int -> Int 
dDiff a b = sum [1 .. diff a b]