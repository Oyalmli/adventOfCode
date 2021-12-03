module Main where

import Data.Char ( digitToInt )

main :: IO ()
main = interact 
    $ show . uncurry (*)
    . (\ls -> (bintodec $ solve 0 0 ls, bintodec $ solve 1 0 ls))
    . map (map digitToInt) . lines

solve :: Int -> Int -> [[Int]] -> [Int]
solve _ _ [x] = x
solve v p ns  
    | ones /= v = solve v (p+1) $ filter (\l -> l!!p /= v) ns
    | otherwise = solve v (p+1) $ filter (\l -> l!!p == v) ns
    where ones = count v p ns

count :: Int -> Int -> [[Int]] -> Int
count v p ls 
    | len - f > f = 1
    | len - f < f = 0
    | otherwise = -1
    where 
        f = length $ filter (\l -> l!!p == v) ls
        len = length ls

bintodec :: [Int] -> Int
bintodec = foldr ((. (2*)) . (+)) 0 . reverse
