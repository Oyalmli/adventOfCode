main :: IO ()
main =
  interact $
    show
      . length
      . filter (isSafe . deltas)
      . map (map read . words)
      . lines

deltas :: (Num c) => [c] -> [c]
deltas ls = zipWith (-) ls $ drop 1 ls

isSafe :: (Num c, Ord c) => [c] -> Bool
isSafe [] = False
isSafe ls = allSameSign && all ok ls
  where
    allSameSign = (allEq . map signum) ls

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (== x) xs

ok :: (Num c, Ord c) => c -> Bool
ok 0 = False
ok n = n >= -3 && n <= 3