module Main where

import Data.List.Split ( splitOn )

main :: IO ()
main = interact 
    $ show . minimum
    . map (flip distFrom [0 .. ] . read) 
    . splitOn ","

distFrom :: Int -> [Int] -> Int
distFrom x ls
    | x > maximum ls = maximum ls
    | otherwise = sum (map (diff x) ls)
    where diff a b = abs (a-b)
