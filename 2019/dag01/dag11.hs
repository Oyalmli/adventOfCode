main = interact ( show . sum . map (solve . read) . words )

solve :: Int -> Int 
solve n = n `div` 3 - 2