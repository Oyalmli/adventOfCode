module Main where

main :: IO ()
main = interact
    $ show
    . solve (0, 0, 0)
    . map words
    . lines

solve :: (Int, Int, Int) -> [[String]] -> Int
solve (h,d,a) [] = (h*d)
solve (hori, depth, aim) ((dir:sn:_):xs)
    | dir == "forward" = solve (hori + n, depth + (n * aim), aim) xs
    | dir == "down" = solve (hori, depth, aim + n) xs
    | dir == "up" = solve (hori, depth, aim - n) xs
    | otherwise = error dir
    where n = read sn