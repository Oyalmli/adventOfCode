import Data.Map qualified as M
import Data.Set qualified as S
import Control.Monad (replicateM)
import qualified Data.Foldable as S

main :: IO ()
main = do
  inp <- getContents
  let ls@(l : _) = lines inp
  let width = length l
  let height = length ls
  let m = locations M.empty 0 ls
  print width
  print height
  print m
  print $ S.length $ S.fromList $ concat $ solve (width,height) m 


solve dims allLocs = map (solveFreq dims) freqs
    where freqs = M.toList allLocs


solveFreq dims (freqName, freqList) = concatMap (\(x,y) -> outsidePoints dims x y) freqCombs
    where freqCombs = makePairs freqList

pairElement:: a -> [a] -> [] (a,a)
pairElement = \elem -> \list -> 
    case list of
        [] -> []
        x:xs -> [(elem,x)] ++ pairElement elem xs

-- Make all possible pairs of elements in a given list
makePairs :: [] a -> [] (a, a)
makePairs = \list ->
    case list of
        [] -> []
        x:xs -> pairElement x xs ++ makePairs xs

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

--outsidePoints :: (Num a, Num b) => (Int, Int) -> (a, b) -> (a, b) -> [(a, b)]
outsidePoints dims p1 p2 = filter (inside dims) [p3, p4]
  where dx = (x2 - x1)
        dy = (y2 - y1)
        p3 = (x1 - dx, y1 - dy)
        p4 = (x2 + dx, y2 + dy)
        ((x1, y1), (x2, y2)) = maxPair p1 p2

maxPair :: (Ord a, Ord b) => (a, b) -> (a, b) -> ((a, b), (a, b))
maxPair (x1, y1) (x2, y2) = if x1 > x2 then ((x1, y1), (x2, y2)) else ((x2, y2), (x1, y1))

--inside :: (Ord a, Ord b) => (a, b) -> Bool
inside (widht, height) (x, y) = x >= 0 && x < widht && y >= 0 && y < height

