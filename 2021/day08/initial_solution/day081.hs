{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Bool ( bool )
import Data.List ( unfoldr )
import Data.List.Split ( splitOneOf )

main :: IO ()
main = interact 
    $ show 
    . sum
    . concatMap solve
    . parse
    . splitOneOf "|\n"

parse :: [String] -> [String]
parse [] = []
parse (_:x:xs) = x:parse xs

solve :: String -> [Int]
solve s = map fromEnum $ map (`elem` [2,3,4,7]) $ map length $ words s