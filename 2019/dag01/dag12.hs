main = interact $ show . sum . map (cF . read) . lines

cF :: Int -> Int -- Calculate fuel
cF n 
    | n < 9 = 0
    | otherwise = let f = (div n 3) - 2 in f + cF (f)
