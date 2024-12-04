import Data.List (sort, transpose)
import Data.Map (Map, empty, findWithDefault, insertWith)

main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ part_1 inp
    putStrLn $ "Part 2: " ++ part_2 inp

part_1 :: String -> String
part_1 = show . sum 
    . foldl1 (zipWith ((abs .) . (-)))
    . map sort . transpose
    . map (map read . words)
    . lines

part_2 :: String -> String
part_2 = show . sum
    . (\[a, b] -> map (similarityScore $ histogram b) a)
    . transpose
    . map (map read . words)
    . lines

similarityScore :: Map Int Int -> Int -> Int
similarityScore mp a = a * counts
  where counts = findWithDefault 0 a mp

histogram :: [Int] -> Map Int Int
histogram = foldl (\m k -> insertWith (+) k 1 m) empty