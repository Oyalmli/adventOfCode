import Data.Maybe (fromJust, isNothing)
import Data.Set qualified as S
import Grid

main :: IO ()
main = do
  inp <- getContents
  let grid = fromList (lines inp)
  print grid
  print $ sum $ solve grid

solve :: Grid Char -> [Int]
solve grid = [S.size (S.fromList $ walk grid '0' (x, y)) | x <- [0 .. w], y <- [0 .. h], (!?) x y grid == Just '0']
  where
    (w, h) = dims grid

walk :: Grid Char -> Char -> (Int, Int) -> [(Int, Int)]
walk grid c (x, y)
  | curr >= '9' = [(x, y)]
  | otherwise = concatMap (walk grid (succ c)) neighbours
  where
    curr = (!) x y grid
    neighbours =
      filter (\(x, y) -> isOneBigger c ((!) x y grid)) $
        filter (inBounds grid) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

isOneBigger :: Char -> Char -> Bool
isOneBigger a b = a == pred b
