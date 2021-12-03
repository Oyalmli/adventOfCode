module Main where
import Data.List
import Data.Char

main = interact 
    $ show
    . solve 0
    . map (map digitToInt)
    . lines

solve :: Int -> [[Int]] -> [Int]
solve _ [x] = x
solve p ns  
    | ones /= 1 = solve (p+1) $ filter (\l -> l!!p == 0) ns
    | otherwise = solve (p+1) $ filter (\l -> l!!p == 1) ns
    where ones = count1s p ns

-- 1 = most ones, 0 = most 0, -1 = equal
count1s :: Int -> [[Int]] -> Int
count1s p ls 
    | len - f < f = 1
    | len - f > f = 0
    | otherwise = -1
    where 
        f = length $ filter (\l -> l!!p == 0) ls
        len = length ls

-- 841 * 3384