{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import qualified Data.Map as M
import Data.List.Split ( splitOneOf )
import Data.List ( sort, (\\) )
import Data.Tuple (swap)
import Data.Char (intToDigit)

main :: IO ()
main = do
    inp <- getContents
    let parsed = (parse . splitOneOf "|\n") inp
    print $ sum $ map (\(numsEnc, numsToFind) -> read $ map (\k -> bain numsEnc M.! sort k) numsToFind) parsed


bain :: [String] -> M.Map String Char
bain numsEnc = do
    let foundMap1 = M.fromList $ concatMap isUnique numsEnc
    let notFound1 = removeFound foundMap1 numsEnc
    let foundMap2 = foldl (\mp (k,v) -> M.insert k v mp) foundMap1 $ concatMap (minus7 (foundMap1 M.! 7)) notFound1
    let notFound2 = removeFound foundMap2 notFound1
    let foundMap3 = foldl (\mp (k,v) -> M.insert k v mp) foundMap2 $ concatMap (find0 (foundMap2 M.! 4)) notFound2
    let notFound3 = removeFound foundMap3 notFound2
    let foundMap4 = foldl (\mp (k,v) -> M.insert k v mp) foundMap3 $ map lastStep notFound3
    swapMap foundMap4

{-
bain :: [String] -> M.Map String Char
bain numsEnc = do
    foldl (\(mp, notFound) (nmp, fo))
    let foundMap1 = M.fromList $ concatMap isUnique numsEnc
    let notFound1 = removeFound foundMap1 numsEnc
    let foundMap2 = foldl (\mp (k,v) -> M.insert k v mp) foundMap1 $ concatMap (minus7 (foundMap1 M.! 7)) notFound1
    let notFound2 = removeFound foundMap2 notFound1
    let foundMap3 = foldl (\mp (k,v) -> M.insert k v mp) foundMap2 $ find0 (foundMap2 M.! 4) notFound2
    let notFound3 = removeFound foundMap3 notFound2
    let foundMap4 = foldl (\mp (k,v) -> M.insert k v mp) foundMap3 $ map lastStep notFound3
    swapMap foundMap4
-}

swapMap :: M.Map Int String -> M.Map String Char
swapMap = M.fromList . map (\(a,b) -> (sort b, intToDigit a)) . M.toList

removeFound :: M.Map Int String -> [String] -> [String]
removeFound mp = filter (`notElem` fstrs)
    where fstrs = map snd $ M.toList mp

lastStep :: String -> (Int, String)
lastStep str = case length str of
    5 -> (5, str)
    6 -> (0, str)

find0 :: String -> String -> [(Int, String)]
find0 four other = case length $ four \\ other of
    0 -> [(9, other)]
    2 -> [(2, other)]
    _ -> []

minus7 :: String -> String -> [(Int, String)]
minus7 seven other = case length $ other \\ seven of
    4 -> [(6, other)]
    2 -> [(3, other)]
    _ -> []

isUnique :: String -> [(Int, String)]
isUnique str = 
    case length str of
        2 -> [(1, str)]
        3 -> [(7, str)]
        4 -> [(4, str)]
        7 -> [(8, str)]
        _ -> []

parse :: [String] -> [([String],[String])]
parse [] = []
parse (y:x:xs) = (words y,words x):parse xs


