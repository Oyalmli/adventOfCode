--- Day 6: Guard Gallivant ---
import Data.Foldable qualified as F
import Data.HashSet qualified as S
import Data.Maybe (isNothing)
import Grid

main :: IO ()
main = do
  inp <- getContents
  let grid = fromList (lines inp)
  let Just (x, y) = findLocation (== '^') grid
  let path = S.fromList $ walk (x, y) (0, -1) grid
  putStrLn $ "Part 1: " ++ show ((+ 1) $ F.length path)
  putStrLn $ "Part 2: " ++ show ((+ 1) $ length $ filter (not . id) $ map (walkLoop S.empty (x, y) (0, -1)) (map (disrupt grid) $ S.toList path))

disrupt :: Grid Char -> (Int, Int) -> Grid Char
disrupt grid (x, y) = (><) x y grid '#'

step :: (Int, Int) -> (Int, Int) -> Grid Char -> ((Int, Int), Char)
step (x, y) (dx, dy) grid = ((x + dx, y + dy), nextChar)
  where
    (x', y') = (x + dx, y + dy)
    nextChar = case (!?) x' y' grid of
      Just c -> c
      Nothing -> 'X'

walk :: (Int, Int) -> (Int, Int) -> Grid Char -> [(Int, Int)]
walk pos dir grid = case nextChar of
  '.' -> pos : walk nextPos dir grid
  '^' -> pos : walk nextPos dir grid
  '#' -> pos : walk pos (rotateRight dir) grid
  'X' -> []
  where
    (nextPos, nextChar) = step pos dir grid

walkLoop :: S.HashSet ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> Grid Char -> Bool
walkLoop been pos dir grid
  | S.member (pos, dir) been = False
  | otherwise = case nextChar of
      '.' -> walkLoop been nextPos dir grid
      '^' -> walkLoop been nextPos dir grid
      '#' -> walkLoop (S.insert (pos, dir) been) pos (rotateRight dir) grid
      'X' -> True
  where
    (nextPos, nextChar) = step pos dir grid

rotateRight :: (Int, Int) -> (Int, Int)
rotateRight (x, y) = (-y, x)

findLocation :: (Char -> Bool) -> Grid Char -> Maybe (Int, Int)
findLocation target grid = go 0 0
  where
    (w, h) = dims grid
    go x y
      | y == h = Nothing
      | x == w = go 0 (y + 1)
      | target ((!) x y grid) = Just (x, y)
      | otherwise = go (x + 1) y
