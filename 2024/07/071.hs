import Control.Monad (replicateM)
import Data.List.Split (splitOn)

main = interact $ show . sum . map (head . snd) . filter (fst) . map solve . map parse . lines

solve :: (Int, [Int]) -> (Bool, [Int])
solve (_, []) = (False, [])
solve (target, nums) =
  let results =
        [ foldl (\acc (op, x) -> acc `op` x) (head nums) (zip ops (tail nums))
        | ops <- operations (length nums - 1)
        ]
      matching = filter (== target) results
   in (not (null matching), matching)

parse :: String -> (Int, [Int])
parse str = (read num, map read $ words nums)
  where
    (num : nums : _) = splitOn ":" str

-- combinations :: Num a => p -> [a -> a -> a]
operations :: Int -> [[(Int -> Int -> Int)]]
operations n = replicateM n [(*), (+)]

