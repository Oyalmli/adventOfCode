import Data.Foldable qualified as F
import Data.Vector qualified as V
import Data.Set qualified as S
import qualified Data.Foldable as S

main :: IO ()
main = do
  inp <- getContents
  let grid = fromList (lines inp)
  let Just (x, y) = findLocation (== '^') grid
  print grid
  print (x, y)
  let path = walk (x, y) (0, -1) grid
  print $ (+1) $ S.length $ S.fromList $ walk (x, y) (0, -1) grid

step :: (Int, Int) -> (Int, Int) -> Grid Char -> ((Int, Int), Char)
step (x, y) (dx, dy) grid = ((x + dx, y + dy), nextChar)
  where
    (x', y') = (x + dx, y + dy)
    nextChar = case (!?) x' y' grid of
      Just c -> c
      Nothing -> 'X'

--walk :: (Int, Int) -> (Int, Int) -> Grid Char -> [Char]
walk :: (Int, Int) -> (Int, Int) -> Grid Char -> [(Int, Int)]
walk pos dir grid = case nextChar of
  '.' -> pos : walk nextPos dir grid
  '^' -> pos : walk nextPos dir grid
  '#' -> pos : walk pos (rotateRight dir) grid
  'X' -> []
  where
    (nextPos, nextChar) = step pos dir grid

rotateRight :: (Int, Int) -> (Int, Int)
rotateRight (x, y) = (-y, x)

findLocation :: (a -> Bool) -> Grid a -> Maybe (Int, Int)
findLocation target grid = go 0 0
  where
    (w, h) = dims grid
    go x y
      | y == h = Nothing
      | x == w = go 0 (y + 1)
      | target ((!) x y grid) = Just (x, y)
      | otherwise = go (x + 1) y

isTree = (== '#')

type Grid a = V.Vector (V.Vector a)

dims :: Grid a -> (Int, Int)
dims = (,) <$> V.length <*> V.length

fromList :: [[a]] -> Grid a
fromList = V.fromList . map V.fromList

toList :: Grid a -> [[a]]
toList = map F.toList . F.toList

(!) :: Int -> Int -> Grid a -> a
(!) x y g = g V.! y V.! x

(!?) :: Int -> Int -> Grid a -> Maybe a
(!?) x y g = do
  row <- g V.!? y
  row V.!? x