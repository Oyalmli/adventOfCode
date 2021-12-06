{-
    * This is a rewrite to Haskell of the solution "day06_better.py" by Oyjoh
    * https://github.com/oyjoh/advent-of-code-2021/blob/main/day06/day06_better.py
-}
module Main where

import qualified Data.Foldable as F
import qualified Data.List.Split as SP
import qualified Data.Sequence as S

main :: IO ()
main = do
  content <- getContents
  let inp = (map read . SP.splitOn ",") content
  mapM_ (print . F.sum . solve inp) [80, 256]

solve :: [Int] -> Int -> S.Seq Int
solve dat days = foldl g (foldl f table dat) (genIdx days)
  where
    table = S.replicate 9 0
    f hist i = S.update i (S.index hist i + 1) hist
    g hist i = S.update i (S.index hist i + S.index hist ((i + 2) `mod` 9)) hist

genIdx :: Int -> [Int]
genIdx n = take (n + 1) $ iterate (\i -> (i + 1) `mod` 9) 6
