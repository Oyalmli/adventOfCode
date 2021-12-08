{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.List.Split ( splitOneOf )

main :: IO ()
main = interact 
    $ show . sum
    . concatMap solve
    . everySnd . splitOneOf "|\n"

everySnd :: [String] -> [String]
everySnd [] = []
everySnd (_:x:xs) = x : everySnd xs

solve :: String -> [Int]
solve = map ((fromEnum . (`elem` [2, 3, 4, 7])) . length) . words
