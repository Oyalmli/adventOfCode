{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe ( fromMaybe )

type RuleMap = M.Map (Char,Char) Char
type PHist = M.Map (Char,Char) Integer

main :: IO ()
main = do
    inp <- getContents
    let (pHist, ruleMap) = makeInitialMap $ parseInp $ lines inp
    putStrLn $ ((++) "Part 1: " . show) $ solve 10 (pHist, ruleMap)
    putStrLn $ ((++) "Part 2: " . show) $ solve 40 (pHist, ruleMap)

solve :: Integer -> (PHist, RuleMap) -> Integer
solve 0 (pHist, _) = (countChars . M.toList) pHist
solve n (pHist, ruleMap) = solve (n-1) (foldl (`mkNewPair` ruleMap) pHist (M.toList pHist), ruleMap)

mkNewPair :: PHist -> RuleMap -> ((Char,Char),Integer) -> PHist
mkNewPair countMap ruleMap ((a,b), cnt) = foldl step countMap [((a,b),-cnt),((c,b), cnt),((a,c), cnt)]
    where 
    c = ruleMap M.! (a,b)
    step mp (p,v) = M.insert p (fromMaybe 0 ((M.!?) mp p) + v) mp
    getCount mp = fromMaybe 0 . (M.!?) mp

countChars :: [((Char, Char), Integer)] -> Integer
countChars = (\mp -> maximum mp - minimum mp) 
    . foldl (\m (k,v) -> M.insertWith (+) k v m) M.empty 
    . map (\((a,b),v) -> (b,v))

makeInitialMap :: (String, RuleMap) -> (PHist, RuleMap)
makeInitialMap (pattern, ruleMap) = (histogram pairs, ruleMap)
    where 
    pairs = zip pattern (tail pattern) 
    histogram = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

parseInp :: [String] -> (String, RuleMap)
parseInp (pattern:_:rules) = (pattern, M.fromList $ map parseRules rules)
    where 
    parseRules str = let [[a,b],_,[c]] = words str in ((a,b),c)
