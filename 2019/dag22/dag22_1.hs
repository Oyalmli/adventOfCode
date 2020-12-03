import Data.Char
import Data.List.Split
import Data.List
import Data.Ord

main = interact $ show . elemIndex 2019 . shuffle startList . map op . map words . lines

startList = [0..10006]

data Op 
    = DealNew
    | DealInc Int
    | Cut Int
    deriving (Show)

op :: [String] -> Op
op ("deal":"into":"new":"stack":xs) = DealNew
op ("deal":"with":"increment":num:xs) = DealInc (read num)
op ("cut":num:xs) = Cut (read num)

shuffle :: [Int] -> [Op]-> [Int]
shuffle list [] = list
shuffle list (DealNew:xs) = shuffle (reverse list) xs 
shuffle list (op@(DealInc num):xs) = shuffle (dealInc op list) xs
shuffle list (op@(Cut num):xs) = shuffle (cutN op list) xs

cutN :: Op -> [Int] -> [Int]
cutN (Cut num) list
    | num < 0 = let (first,last) = splitAt ((length list) + num) list in last ++ first
    | otherwise = let (first,last) = splitAt num list in last ++ first

dealInc :: Op -> [Int] -> [Int]
dealInc (DealInc num) list 
    = map fst 
    $ sortBy (comparing snd) 
    $ zip list (map (\i -> (i * num) `mod` (length list)) [0..])