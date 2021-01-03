import Data.List.Split
main = interact $ show
    . score
    . (\(p1:p2:[]) -> solve p1 p2)
    . parseInp

score = sum . zipWith (*) [1..] . reverse

solve ls [] = ls
solve [] ls = ls
solve (x:xs) (y:ys)
 | x > y = solve (xs ++ [x,y]) ys
 | otherwise = solve xs (ys ++ [y,x])

parseInp = map (map (read::String -> Int) . drop 2 . words) . splitOn "\n\n"