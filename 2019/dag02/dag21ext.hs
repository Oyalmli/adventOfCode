import Data.List.Split
import Data.Char

main = do
    contents <- getContents
    putStrLn $ show $ head $ computer $ makePair $ (map read :: [String] -> [Int]) $ splitOn "," contents

computer :: ([Int],[Int]) -> [Int]
computer (op,list) = case op of
    (99:xs) -> list
    (1:n1:n2:pos:xs) -> let res = replaceNth pos ((list!!n1) + (list!!n2)) list in computer $ mp xs res
    (2:n1:n2:pos:xs) -> let res = replaceNth pos ((list!!n1) * (list!!n2)) list in computer $ mp xs res
    otherwise -> list

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)  
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

makePair :: [a] -> ([a],[a])
makePair list = (list,list)

mp :: [Int] -> [Int] -> ([Int], [Int])
mp xs list = (reverse (take (length xs) (reverse list)), list)