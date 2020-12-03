main = interact $ show . sum . map fromEnum . solve 0 . map cycle . map parseInp . lines

parseInp :: String -> [Bool]
parseInp [] = []
parseInp ('#':xs) = True:parseInp xs
parseInp ('.':xs) = False:parseInp xs

solve :: Int -> [[Bool]] -> [Bool]
solve n [] = []
solve n (inner:outer) = inner!!n:solve (n+3) outer