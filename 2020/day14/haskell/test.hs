main = print $ map (combine "X10X") combs 

pad n str = replicate (n - length str) '0' ++ str

combine [] [] = []
combine (m:ms) (c:cs)
    | m == '1' = '1':combine ms cs
    | otherwise = c:combine ms cs

combs = map (pad 2 . concatMap show . fromDecimal) [0..3]


fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = go n []
    where go 0 r = r
          go k rs = go (div k 2) (mod k 2:rs)