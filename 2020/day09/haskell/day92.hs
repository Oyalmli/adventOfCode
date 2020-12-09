import Data.Vector (Vector, empty, fromList, slice, (!))

target :: Int
target = 10884537

main :: IO ()
main =
  interact $
    show
      . sumSlice 0 0
      . fromList
      . map read
      . lines

sumSlice :: Int -> Int -> Vector Int -> Int
sumSlice low offset list
  | (sum s) < target = sumSlice low (offset + 1) list
  | (sum s) > target = sumSlice (low + 1) (offset -1) list
  | otherwise = (minimum s) + (maximum s)
  where
    s = slice low offset list