import Data.List

main :: IO()
main = interact $ show 
    . solve 
    . map read . words

solve :: [Int] -> Maybe (Int, [Int])
solve = find ((==2020) . fst) 
    . \ls -> [(x+y+z, [x,y,z]) | x <- ls, y <- ls, z <- ls]
