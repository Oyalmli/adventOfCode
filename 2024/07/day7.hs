--- Day 7: Bridge Repair ---
import Control.Monad (msum, replicateM)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Prelude hiding ((||))

main :: IO ()
main = do
  inp <- getContents
  let parsed = map parse (lines inp) :: [(Int, [Int])]
  putStrLn $ "Part 1: " ++ show (solveAll [(+), (*)] parsed)
  putStrLn $ "Part 2: " ++ show (solveAll [(+), (*), (||)] parsed)
  where solveAll ops = sum . mapMaybe (solve ops)

solve :: Ord a => [a -> a -> a] -> (a, [a]) -> Maybe a
solve _ (_, []) = Nothing
solve ops (target, x:xs) = msum (map (go x xs) (replicateM (length xs) ops))
  where go acc [] []           | acc == target = Just acc
        go acc (y:ys) (op:ops) | acc <= target = go (op acc y) ys ops
        go _ _ _ = Nothing

parse :: (Read a, Read a) => String -> (a, [a])
parse str = (read num, map read $ words nums)
  where [num, nums] = splitOn ":" str

(||) :: (Integral a) => a -> a -> a
(||) x y = x * (!!) (iterate (* 10) 1) (digits y) + y
  where digits n = floor (logBase 10 (fromIntegral n)) + 1
