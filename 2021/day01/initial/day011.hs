module Main where

main :: IO ()
main = interact 
    $ show
    . solve
    . map (read::String->Int) . words

solve :: [Int] -> Int
solve [x] = 0
solve (x:y:xs) 
    | x < y = 1 + solve (y:xs)
    | otherwise = 0 + solve (y:xs) 
