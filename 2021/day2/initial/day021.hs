module Main where

main = interact
    $ show
    . uncurry (*)
    . solve (0, 0)
    . map words
    . lines

solve :: (Int, Int) -> [[String]] -> (Int, Int) 
solve (h,d) [] = (h,d)
solve (hori, depth) ((dir:sn:_):xs)
    | dir == "forward" = solve (hori + read sn, depth) xs
    | dir == "down" = solve (hori, depth + read sn) xs
    | dir == "up" = solve (hori, depth - read sn) xs
    | otherwise = error dir