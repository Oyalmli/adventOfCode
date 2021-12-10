{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Map (fromList, (!))

main :: IO ()
main = interact
    $ show . sum
    . map getVal
    . filter (not . null) 
    . map (solve [])
    . lines 

solve :: String -> String -> [Char]
solve _ [] = []
solve stc (c:cs)
    | c `elem` "(<[{" = solve (c:stc) cs
    | closer (head stc) c = solve (tail stc) cs
    | otherwise = [c]

getVal :: String -> Int
getVal = (!) $ fromList [(")", 3), ("]", 57), ("}", 1197), (">", 25137)]

closer :: Char -> Char -> Bool
closer = (!) $ fromList [('(', (== ')')), ('[',(== ']')), ('<',(== '>')), ('{',(== '}'))]