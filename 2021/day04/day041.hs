module Main where
import Data.List.Split ( splitOn )
import Data.List ( transpose ) 

type Board = [[(Bool, Int)]]

main :: IO ()
main = interact 
    $ show 
    . findAns
    . solve
    . parseInp
    . splitOn "\n\n"

findAns :: (Int, Board) -> Int
findAns (n, board) = n * sum (map snd $ filter (not . fst) (concat board))

solve :: ([Int], [Board]) -> (Int, Board)
solve (n:ns, boards) 
    | any checkBoard nextBoards = (n, head $ filter checkBoard nextBoards)
    | otherwise = solve (ns, nextBoards)
    where nextBoards = map (setNumBoard n) boards

setNumBoard :: Int -> Board -> Board
setNumBoard num
  = map (map (\ (b, x) -> if x == num then (True, x) else (b, x)))

parseInp :: [String] -> ([Int], [Board])
parseInp (sorder:boards) = (map read order, map readBoard boards)
    where order = splitOn "," sorder

readBoard :: String -> Board
readBoard sboard = map (zip (repeat False) . map read . words) rows
    where rows = lines sboard

checkBoard :: Board -> Bool
checkBoard board = any checkRow board || any checkRow (transpose board)
    where checkRow = all fst
