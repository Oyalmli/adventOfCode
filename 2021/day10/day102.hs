{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.List ( sort )
import Data.Map (fromList, (!))

main :: IO ()
main = interact
    $ show . getMiddle . sort
    . map score
    . filter (not . null) 
    . map (solve []) 
    . lines 

getMiddle :: [Int] -> Int 
getMiddle ls = ls !! (length ls `div` 2)  

solve :: String -> String -> [Char]
solve stc [] = map closer stc
solve stc (c:cs)
    | c `elem` "(<[{" = solve (c:stc) cs
    | closer (head stc) == c = solve (tail stc) cs
    | otherwise = []

score :: String -> Int
score =  foldl (\acc c -> acc * 5 + getVal c) 0 

getVal :: Char -> Int
getVal = (!) $ fromList [(')', 1),(']', 2),('}', 3),('>', 4)]

closer :: Char -> Char
closer = (!) $ fromList [('(',')'),('[',']'),('{','}'),('<', '>')]