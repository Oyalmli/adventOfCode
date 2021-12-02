module Main where

main :: IO ()
main = interact 
    $ show
    . solve 100000000000
    . map (read::String->Int) . words

solve :: Int -> [Int] -> Int
solve last [] = 0
solve last xs
    | curr > last = 1 + solve curr (tail xs)
    | otherwise = 0 + solve curr (tail xs)
    where curr = sum (take 3 xs)
