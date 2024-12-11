main = do
  inp <- getContents
  let config = map read $ words inp :: [Int]
  print $ length $ last $ take 26 $ iterate doSteps config

doSteps :: [Int] -> [Int]
doSteps = concatMap step

step :: Int -> [Int]
step 0 = [1]
step n
  | even $ length digits = [read front, read back]
  | otherwise = [n * 2024]
  where
    digits = show n
    len = length digits
    (front, back) = splitAt (len `div` 2) digits