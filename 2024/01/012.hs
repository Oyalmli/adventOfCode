import Data.List (transpose)
import Data.Map (Map, empty, findWithDefault, insertWith)

main :: IO ()
main =
  interact $
    show
      . sum
      . (\[a, b] -> map (similarityScore $ histogram b) a)
      . transpose
      . map (map read . words)
      . lines

similarityScore :: Map Int Int -> Int -> Int
similarityScore mp a = a * cnt
  where
    cnt = findWithDefault 0 a mp

histogram :: [Int] -> Map Int Int
histogram = foldl (\m k -> insertWith (+) k 1 m) empty