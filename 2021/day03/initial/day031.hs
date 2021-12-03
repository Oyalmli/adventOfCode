module Main where
import Data.List
import Data.Char

main = interact 
    $ show
    . solve
    . num01
    . map (map digitToInt)
    . transpose
    . lines
    
num01 :: [[Int]] -> [(Int, Int)]
num01 = map (\ n -> (sum n,length n - sum n))

--805 * 3290

solve ls = (gamma, epsilon)
    where 
        gamma = map (\(a, b ) -> a > b) ls
        epsilon = map (\(a, b ) -> a < b) ls