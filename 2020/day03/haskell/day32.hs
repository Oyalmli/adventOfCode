main = interact $ show . product 
    . map (sum . (map fromEnum)) 
    . slopes 
    . map (cycle . parseInp) . lines

parseInp :: String -> [Bool]
parseInp = map (\x -> x == '#')

slopes :: [[Bool]] -> [[Bool]]
slopes = \bools -> [solve 0 x y bools | (x,y) <- [(1,1),(3,1),(5,1),(7,1),(1,2)]]

solve :: Int -> Int -> Int -> [[Bool]] -> [Bool]
solve n xd yd [] = []
solve n xd yd (inner:outer) = inner!!n:(solve (n+xd) xd yd (drop (yd-1) outer))
