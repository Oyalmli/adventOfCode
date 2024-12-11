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
  print $ S.length $ S.fromList $ concat $ solve (width, height) m

solve :: (Ord a1, Ord b, Num a1, Num b) => (a1, b) -> M.Map a2 [(a1, b)] -> [[(a1, b)]]
solve dims allLocs = map (solveFreq dims) freqs
  where
    freqs = M.toList allLocs

solveFreq :: (Ord a1, Ord b, Num a1, Num b) => (a1, b) -> (a2, [(a1, b)]) -> [(a1, b)]
solveFreq dims (freqName, freqList) = concatMap (\(x, y) -> makePointsBidirectional dims x y) freqCombs
  where
    freqCombs = makePairs freqList

pairElement :: a -> [a] -> [] (a, a)
pairElement = \elem -> \list ->
  case list of
    [] -> []
    x : xs -> [(elem, x)] ++ pairElement elem xs

makePairs :: [] a -> [] (a, a)
makePairs = \list ->
  case list of
    [] -> []
    x : xs -> pairElement x xs ++ makePairs xs

locations :: M.Map Char [(Int, Int)] -> Int -> [String] -> M.Map Char [(Int, Int)]
locations m _ [] = m
locations m y (l : ls) =
  locations
    ( foldl
        ( \m (i, c) ->
            if c /= '.'
              then M.insertWith (++) c [(i, y)] m
              else m
        )
        m
        (zip [0 ..] l)
    )
    (y + 1)
    ls

makePointsBidirectional :: (Ord a, Ord b, Num a, Num b) => (a, b) -> (a, b) -> (a, b) -> [(a, b)]
makePointsBidirectional dims p1 p2 = reverse backwardPoints ++ forwardPoints
  where
    (x1, y1) = p1
    (x2, y2) = p2
    (dx, dy) = (x2 - x1, y2 - y1)
    forwardPoints = takeWhile (inside dims) $ iterate (\(x, y) -> (x + dx, y + dy)) p1
    backwardPoints = takeWhile (inside dims) $ iterate (\(x, y) -> (x - dx, y - dy)) p1

inside :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> (a1, a2) -> Bool
inside (widht, height) (x, y) = x >= 0 && x < widht && y >= 0 && y < height
