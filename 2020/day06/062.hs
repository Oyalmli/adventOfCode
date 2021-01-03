import Data.List.Split
import Data.List(nub, intersect)
main = interact $ show . sum
    . map (length . foldl1 intersect . words)
    . splitOn "\n\n"