import Control.Monad (replicateM)
import Data.Foldable qualified as S
import Data.Map qualified as M
import Data.Set qualified as S

main :: IO ()
main = do
  inp <- getContents
  let ls@(l : _) = lines inp
  let width = length l
  let height = length ls
  let m = locations M.empty 0 ls
  putStrLn $ "Part 1: " ++ (show $ setLength $ part_1 (width, height) m)
  putStrLn $ "Part 2: " ++ (show $ setLength $ part_2 (width, height) m)
  where
    setLength = S.length . S.fromList . concat

part_1 :: (Ord a1, Ord b, Num a1, Num b) => (a1, b) -> M.Map a2 [(a1, b)] -> [[(a1, b)]]
part_1 dims allLocs = map (solveFreq dims makeOutsidePoints) $ M.toList allLocs

part_2 :: (Ord a1, Ord b, Num a1, Num b) => (a1, b) -> M.Map a2 [(a1, b)] -> [[(a1, b)]]
part_2 dims allLocs = map (solveFreq dims makeLinePoints) $ M.toList allLocs

solveFreq :: t1 -> (t1 -> t2 -> t2 -> [b]) -> (a, [t2]) -> [b]
solveFreq dims makePoint (freqName, freqList) = concatMap (\(x, y) -> makePoint dims x y) $ pairs freqList

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
  where
    tails [] = []
    tails l@(_ : xs) = l : tails xs

locations :: M.Map Char [(Int, Int)] -> Int -> [String] -> M.Map Char [(Int, Int)]
locations m _ [] = m
locations m y (l : ls) = locations updatedMap (y + 1) ls
  where
    updatedMap = foldl updateMap m (zip [0 ..] l)
    updateMap acc (i, c)
      | c /= '.' = M.insertWith (++) c [(i, y)] acc
      | otherwise = acc

makeLinePoints :: (Ord a, Ord b, Num a, Num b) => (a, b) -> (a, b) -> (a, b) -> [(a, b)]
makeLinePoints dims p1 p2 = reverse backwardPoints ++ forwardPoints
  where
    (x1, y1) = p1
    (x2, y2) = p2
    (dx, dy) = (x2 - x1, y2 - y1)
    forwardPoints = takeWhile (inside dims) $ iterate (\(x, y) -> (x + dx, y + dy)) p1
    backwardPoints = takeWhile (inside dims) $ iterate (\(x, y) -> (x - dx, y - dy)) p1

makeOutsidePoints :: (Ord a2, Ord a1, Num a1, Num a2) => (a1, a2) -> (a1, a2) -> (a1, a2) -> [(a1, a2)]
makeOutsidePoints dims (x1, y1) (x2, y2) = filter (inside dims) [p3, p4]
  where
    dx = (x2 - x1)
    dy = (y2 - y1)
    p3 = (x1 - dx, y1 - dy)
    p4 = (x2 + dx, y2 + dy)

inside :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> (a1, a2) -> Bool
inside (widht, height) (x, y) = x >= 0 && x < widht && y >= 0 && y < height
