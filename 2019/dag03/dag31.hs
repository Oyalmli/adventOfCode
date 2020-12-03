import Data.List.Split
import Data.List
import Data.Tuple

type Point = (Int, Int)

data Dir = U Int | D Int | L Int | R Int deriving (Show)

main = interact $ show . head . sort . map absVal . foldl1 same . map (makePoints (0,0)) . map makeDirs . map (splitOn ",") . lines

makeDirs :: [String] -> [Dir]
makeDirs [] = []
makeDirs (x:xs) = (makeDir x):(makeDirs xs)
    where makeDir (x:xs) = case x of
                                'U' -> U (read xs)
                                'D' -> D (read xs)
                                'L' -> L (read xs)
                                'R' -> R (read xs)
                                otherwise -> error "Invalid direction"

makePoints :: Point -> [Dir] -> [Point]
makePoints sp [] = []
makePoints sp ((U n):xs) = let np = mp sp n (U 1) in np ++ (makePoints (last np) xs)
makePoints sp ((D n):xs) = let np = mp sp n (D 1) in np ++ (makePoints (last np) xs)
makePoints sp ((L n):xs) = let np = mp sp n (L 1) in np ++ (makePoints (last np) xs)
makePoints sp ((R n):xs) = let np = mp sp n (R 1) in np ++ (makePoints (last np) xs)

mp :: Point -> Int -> Dir -> [Point]
mp (x, y) 0 dir = []
mp (x, y) n dir = let np = case dir of
                                U n -> (x,y+1) 
                                D n -> (x,y-1)
                                L n -> (x-1,y)
                                R n -> (x+1,y)
                                in np:(mp np (n-1) dir) 

same :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
same [] _ = []
same _ [] = []
same (x:xs) ys = if x `elem` ys then x:same xs (delete x ys) else same xs ys

absVal :: (Int,Int) -> Int
absVal (x,y) = (abs x) + (abs y) 