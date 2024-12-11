import Data.Foldable (find)
import Data.HashSet qualified as HS
import Data.List (transpose)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Data.Foldable qualified as F

data Dir = U | D | L | R
  deriving (Generic)

type SimpleGrid a = V.Vector [a]

instance Show Dir where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"

main :: IO ()
main = do
  inp <- getContents
  let grid = makeGrid inp
  let inverseGrid = makeInverseGrid inp
  let loc = findLocation inp
  print loc
  putStrLn $ "g : " ++ show grid
  putStrLn $ "g': " ++ show inverseGrid


  let grid_1 = fromList (lines inp)
  let path = HS.fromList $ walk_1 loc (0, -1) grid_1
  print $ length $ filter (id) $ map (\grids -> walk grids HS.empty loc U) $ makeGridVariants (grid, inverseGrid) (HS.toList path)
  putStrLn $ unlines $ map show $ makeGridVariants (grid, inverseGrid) (HS.toList path)
  print $ walk (grid, inverseGrid) HS.empty loc U

makeGridVariants :: (SimpleGrid Int, SimpleGrid Int) -> [(Int,Int)] -> [(SimpleGrid Int, SimpleGrid Int)]
makeGridVariants (grid, inverseGrid) disruptions = map (applyDisruptions (grid, inverseGrid)) disruptions

applyDisruptions :: (SimpleGrid Int, SimpleGrid Int) -> (Int, Int) -> (SimpleGrid Int, SimpleGrid Int)
applyDisruptions (grid, inverseGrid) (x, y) = (grid', inverseGrid')
  where 
    grid' = grid V.// [(y, insert x (grid V.! y))]
    inverseGrid' = inverseGrid V.// [(x, insert y (inverseGrid V.! x))]

insert :: Int -> [Int]  -> [Int]
insert elem [] = [elem]
insert elem (x:xs)
  | elem == x = x : xs
  | elem < x = elem : x : xs
  | otherwise = x : insert elem xs

walk :: (SimpleGrid Int, SimpleGrid Int) -> HS.HashSet (Int, Int) -> (Int, Int) -> Dir -> Bool
walk (g, g') set (x, y) U = case takeWhile (< y) ((V.!) g' x) of
  [] -> False
  ls -> do
    let y' = last ls
    walk (g, g') (HS.insert (x, y) set) (x, y' + 1) (rotateRight U)
walk (g, g') set (x, y) D = case dropWhile (< y) ((V.!) g' x) of
  [] -> False
  (y' : _) -> do
    if HS.member (x, y) set
      then True
      else walk (g, g') (HS.insert (x, y) set) (x, y' - 1) (rotateRight D)
walk (g, g') set (x, y) L = case takeWhile (< x) ((V.!) g y) of
  [] -> False
  ls -> do
    if HS.member (x, y) set
      then True
      else walk (g, g') (HS.insert (x, y) set) (last ls + 1, y) (rotateRight L)
walk (g, g') set (x, y) R = case dropWhile (< x) ((V.!) g y) of
  [] -> False
  (x' : _) -> do
    if HS.member (x, y) set
      then True
      else walk (g, g') (HS.insert (x, y) set) (x' - 1, y) (rotateRight R)

rotateRight :: Dir -> Dir
rotateRight U = R
rotateRight R = D
rotateRight D = L
rotateRight L = U

distance :: ((Int, Int), Int) -> (Int, Int) -> ((Int, Int), Int)
distance ((x, y), acc) (x', y') = ((x', y'), acc + abs (x' - x) + abs (y' - y))

makeGrid :: (Num a, Enum a) => String -> SimpleGrid a
makeGrid inp =
  V.fromList
    [ [idx | (c, idx) <- zip line [0 ..], c == '#']
    | line <- lines inp
    ]

makeInverseGrid :: (Num a, Enum a) => String -> SimpleGrid a
makeInverseGrid inp =
  V.fromList
    [ [idx | (c, idx) <- zip line [0 ..], c == '#']
    | line <- transpose $ lines inp
    ]

findLocation :: (Num b, Num a, Enum b, Enum a) => String -> (a, b)
findLocation inp = loc
  where
    Just [loc] = find (not . null) [[(x, y) | (c, x) <- zip line [0 ..], c == '^'] | (line, y) <- zip (lines inp) [0 ..]]


step :: (Int, Int) -> (Int, Int) -> Grid Char -> ((Int, Int), Char)
step (x, y) (dx, dy) grid = ((x + dx, y + dy), nextChar)
  where
    (x', y') = (x + dx, y + dy)
    nextChar = case (!?) x' y' grid of
      Just c -> c
      Nothing -> 'X'

--walk :: (Int, Int) -> (Int, Int) -> Grid Char -> [Char]
walk_1 :: (Int, Int) -> (Int, Int) -> Grid Char -> [(Int, Int)]
walk_1 pos dir grid = case nextChar of
  '.' -> pos : walk_1 nextPos dir grid
  '^' -> pos : walk_1 nextPos dir grid
  '#' -> pos : walk_1 pos (rotateRight_1 dir) grid
  'X' -> []
  where
    (nextPos, nextChar) = step pos dir grid

rotateRight_1 :: (Int, Int) -> (Int, Int)
rotateRight_1 (x, y) = (-y, x)

findLocation_1 :: (a -> Bool) -> Grid a -> Maybe (Int, Int)
findLocation_1 target grid = go 0 0
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