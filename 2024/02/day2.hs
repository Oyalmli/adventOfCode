--- Day 2: Red-Nosed Reports ---
main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ part_1 inp
    putStrLn $ "Part 2: " ++ part_2 inp

part_1 :: String -> String
part_1 = show . length
    . filter (isSafe . deltas)
    . map (map read . words)
    . lines

part_2 :: String -> String
part_2 = show . length
    . filter (any isSafe . map deltas)
    . map (removeOne . map read . words)
    . lines

removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (x : xs) = xs : map (x :) (removeOne xs)

deltas :: (Num c) => [c] -> [c]
deltas ls = zipWith (-) ls $ drop 1 ls

isSafe :: (Num c, Ord c) => [c] -> Bool
isSafe [] = True
isSafe ls = all (ok . abs) ls && allSameSign
  where
    ok n = n >= 1 && n <= 3
    allSameSign = (allEq . map signum) ls

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (== x) xs
