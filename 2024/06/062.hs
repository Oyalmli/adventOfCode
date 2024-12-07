import Control.Applicative qualified as M
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Maybe (isNothing)
import Data.HashSet qualified as S
import Grid

main :: IO ()
main = do
  inp <- getContents
  let grid = fromList (lines inp)
  let Just (x, y) = findLocation (== '^') grid
  let (w, h) = dims grid

  print $ length $ filter isNothing (map (walk S.empty (x, y) (0, -1)) (map (\(x, y) -> (><) x y grid '#') [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]) `using` parList rdeepseq)

step :: (Int, Int) -> (Int, Int) -> Grid Char -> ((Int, Int), Char)
step (x, y) (dx, dy) grid = ((x + dx, y + dy), nextChar)
  where
    (x', y') = (x + dx, y + dy)
    nextChar = case (!?) x' y' grid of
      Just c -> c
      Nothing -> 'X'

walk :: S.HashSet ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> Grid Char -> Maybe (S.HashSet ((Int, Int), (Int, Int)))
walk been pos dir grid
  | S.member (pos, dir) been = Nothing
  | otherwise = case nextChar of
      '.' -> walk newBeen nextPos dir grid
      '^' -> walk newBeen nextPos dir grid
      '#' -> walk newBeen pos (rotateRight dir) grid
      'X' -> Just newBeen
  where
    (nextPos, nextChar) = step pos dir grid
    newBeen = S.insert (pos, dir) been

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

isTree :: Char -> Bool
isTree = (== '#')
