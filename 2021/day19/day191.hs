module Main where

import qualified Data.List.Split as SP 

main :: IO ()
main = interact 
    $ show
    . parseInp
    . filter (not . null)
    . SP.splitOn "---"

parseInp :: [String] -> [[Int]]
parseInp (_:str:xs) = (map read $ filter (not . null) $ SP.splitOneOf "\n," str) : parseInp xs
parseInp _ = []