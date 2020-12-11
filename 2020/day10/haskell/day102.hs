import Data.List

main :: IO()
main = interact $ show 
    . product . map solve 
    . group . diff . sort . addEnds
    . map read . lines

addEnds :: [Int] -> [Int]
addEnds list = (maximum list+3):0:list

diff :: [Int] -> [Int]
diff [] = []
diff [x] = []
diff (x:y:xs) = (y - x) : diff (y:xs)

solve :: [Int] -> Int
solve list
    | head list == 1 = (sum [1..(length list-1)] + 1)
    | otherwise = 1
    