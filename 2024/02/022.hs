main :: IO ()
main =
  interact $
    show
      . length
      . filter (any isSafe . map deltas)
      . map (removeOne . map read . words)
      . lines

removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (x : xs) = xs : map (x :) (removeOne xs)

deltas :: (Num c) => [c] -> [c]
deltas ls = zipWith (-) ls $ drop 1 ls

isSafe :: (Num c, Ord c) => [c] -> Bool
isSafe [] = False
isSafe ls = all (ok . abs) ls && allSameSign
  where
    ok n = n >= 1 && n <= 3
    allSameSign = (allEq . map signum) ls

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (== x) xs
