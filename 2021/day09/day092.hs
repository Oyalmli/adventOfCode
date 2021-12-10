{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import qualified Grid as G
import qualified Data.Sequence as S
import Data.Char ( digitToInt )
import Data.Maybe ( isJust, fromJust, fromMaybe, catMaybes )
import Data.Set (Set, empty, member, insert)
import Data.List (sort)

main :: IO ()
main = interact 
    $ show . product . take 3 . reverse . sort
    . solve . G.fromListMat
    . map (map digitToInt)
    . lines  

solve :: G.Grid Int -> [Int]
solve grid = map (\t -> goTo grid empty [t]) 
    $ concatMap (\(y,x) -> filtr grid (y,x) ) [(y,x) | y <- [0..(S.length grid-1)], x <- [0..(S.length (G.getRow grid 0)-1)]]

filtr :: G.Grid Int -> (Int,Int) -> [(Int,Int)]
filtr grid (y,x) = [(y, x) | all (curr <) $ catMaybes ((G.?+) grid y x)]
    where curr = (G.!) grid y x

goTo :: G.Grid Int -> Set (Int, Int) -> [(Int, Int)] -> Int
goTo grid visited [] = 0
goTo grid visited ((y,x):ps)
    | member (y,x) visited = 0 + goTo grid visited ps
    | otherwise = 1 + goTo grid (insert (y, x) visited) 
        ([(oy, ox) | ((oy, ox), v) <- (G.?++) grid y x, curr < v && v < 9] ++ ps)
    where curr = (G.!) grid y x 