import Data.List.Split
import Data.Char

data OpCode 
    = EndCode
    | PlusCode Int Int Int 
    | MultCode Int Int Int
    
    deriving (Eq, Show)

main = interact $ show . computer . makePair . chunksOf 4 . (map read :: [String] -> [Int])
    . filter (not . null) . splitOn ","


makeCode :: [Int] -> OpCode
makeCode [] = EndCode
makeCode (99:xs) = EndCode
makeCode (1:n1:n2:pos:xs) = PlusCode n1 n2 pos
makeCode (2:n1:n2:pos:xs) = MultCode n1 n2 pos
makeCode _ = error "Invalid OpCode"


makePair :: [[Int]] -> ([OpCode],[Int])
makePair list = (map makeCode list, concat list)

computer :: ([OpCode],[Int]) -> [Int]
computer ((EndCode:xs),list) = list
computer (((PlusCode n1 n2 pos):xs),list) = let newList = (replaceNth pos (n1 + n2) list) in computer (map makeCode (chunksOf 4 $ newList), newList)
computer (((MultCode n1 n2 pos):xs),list) = let newList = (replaceNth pos (n1 * n2) list) in computer (map makeCode (chunksOf 4 $ newList), newList)
computer ([], list) = list

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)  | n == 0 = newVal:xs
                            | otherwise = x:replaceNth (n-1) newVal xs

    