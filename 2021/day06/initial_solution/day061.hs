module Main where
import Data.List.Split ( splitOn ) 

main :: IO ()
main = interact 
    $ show . solve 256
    . map read . splitOn ","

solve :: Int -> [Int] -> Int
solve 0 ls = length ls
solve n ls = solve (n-1) (concatMap step ls)

step :: Int -> [Int]
step x
    | x == 0 = [6,8]
    | otherwise = [x-1]
