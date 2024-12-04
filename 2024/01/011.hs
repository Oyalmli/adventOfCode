import Data.List (sort, transpose)

main :: IO ()
main = interact 
    $ show . sum
    . foldl1 (zipWith ((abs .) . (-)))
    . map sort . transpose
    . map (map read . words)
    . lines

