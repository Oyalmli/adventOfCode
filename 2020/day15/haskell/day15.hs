import Data.List.Split (splitOn)
import Data.List hiding (insert, map)
import Data.IntMap.Lazy hiding (map)

type Memo = IntMap Int

main :: IO()
main = interact $ show
    . (\ls -> solve (length ls) (last ls, toMap ls))
    . map (read::String -> Int) . splitOn ","

toMap :: [Int] -> Memo
toMap ls = fromList $ zip ls [1..]

solve :: Int -> (Int, Memo) -> Int
solve n (curr,map)
    | n >= 30000000 = curr
    | otherwise = solve (n+1) (solve' n curr map)

solve' :: Int -> Int -> Memo -> (Int, Memo)
solve' n curr map
    | notMember curr map = (0, insert curr n map)
    | otherwise = ((n - (map ! curr)), insert curr n map)