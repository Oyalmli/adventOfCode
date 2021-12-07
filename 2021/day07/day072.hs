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
    | otherwise = sum (map (gaussDiff x) ls)

gaussDiff :: Int -> Int -> Int 
gaussDiff a b = (n*(n+1)) `div` 2
    where n = abs (a-b)