{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import qualified Data.Map as M
import Data.List.Split ( splitOneOf )
import Data.List ( sort, (\\) )
import Data.Char ( intToDigit )

main :: IO ()
main = interact 
    $ show . sum 
    . map (\(numsEnc, numsToFind) -> read $ map (\k -> solve numsEnc M.! sort k) numsToFind) 
    . toTup . splitOneOf "|\n"

solve :: [String] -> M.Map String Char
solve numsEnc = swapMap $ fst $
    applyF (M.fromList $ concatMap isUnique numsEnc, numsEnc) 
        [ \mp -> concatMap (minus7 (mp M.! 7))
        , \mp -> concatMap (minus4 (mp M.! 4))
        , \mp -> concatMap lastStep 
        ]

applyF :: (M.Map Int String, [String]) -> [M.Map Int String -> [String] -> [(Int, String)]] -> (M.Map Int String, [String])
applyF m [] = m
applyF (foundMap, notFound) (f:fx) = applyF (foldl (\mp (k,v) -> M.insert k v mp) foundMap (f foundMap newNotFound), newNotFound) fx
    where newNotFound = removeFound foundMap notFound
        
swapMap :: M.Map Int String -> M.Map String Char
swapMap = M.fromList . map (\(a,b) -> (sort b, intToDigit a)) . M.toList

removeFound :: M.Map Int String -> [String] -> [String]
removeFound mp = filter (`notElem` fstrs)
    where fstrs = map snd $ M.toList mp

lastStep :: String -> [(Int, String)]
lastStep str = case length str of
    5 -> [(5, str)]
    6 -> [(0, str)]

minus4 :: String -> String -> [(Int, String)]
minus4 four other = case length $ four \\ other of
    0 -> [(9, other)]
    2 -> [(2, other)]
    _ -> []

minus7 :: String -> String -> [(Int, String)]
minus7 seven other = case length $ other \\ seven of
    4 -> [(6, other)]
    2 -> [(3, other)]
    _ -> []

isUnique :: String -> [(Int, String)]
isUnique str = case length str of
    2 -> [(1, str)]
    3 -> [(7, str)]
    4 -> [(4, str)]
    7 -> [(8, str)]
    _ -> []

toTup :: [String] -> [([String],[String])]
toTup [] = []
toTup (y:x:xs) = (words y,words x):toTup xs
