{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import qualified Data.Map as M
import Data.List (sort)

main :: IO ()
main = interact
    $ show
    . (\ls -> last ls - head ls)
    . sort 
    . map snd . M.toList
    . histogram
    . solve 10
    . parseInp
    . lines

solve :: Int -> (String, M.Map String Char) -> String
solve 0 (pattern, _) = pattern
solve n (pattern, ruleMap) = solve (n-1) (concatMap (\(a,b) -> a : [ruleMap M.! (a : [b])]) pairs ++ [last pattern], ruleMap)
    where pairs = zip pattern (tail pattern)

parseInp :: [String] -> (String, M.Map String Char)
parseInp (pattern:_:rules) = (pattern, M.fromList $ map parseRules rules)

parseRules :: [Char] -> (String, Char)
parseRules str = (a,head b)
    where [a,_,b] = words str

histogram :: [Char] -> M.Map Char Int
histogram = foldl (\m k -> M.insertWith (+) k 1 m) M.empty