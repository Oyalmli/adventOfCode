module Main where

import qualified Grid as G
import qualified Data.Sequence as S
import Data.Char ( digitToInt )
import Data.Maybe ( isJust, fromJust, fromMaybe, catMaybes )


main :: IO ()
main = interact 
    $ show
    . solve
    . G.fromListMat
    . map (map digitToInt)
    . lines  

--solve :: G.Grid Int -> [Int]
solve grid = concatMap (\(x,y) -> fil grid (x,y) ) [(x,y) |y <- [0..(S.length (G.getCol grid 0)-1)], x <- [0..(S.length grid-1)]]

fil :: G.Grid Int -> (Int,Int) -> [(Int,Int)]
fil grid (x,y) = [(x, y) | all (curr <) $ catMaybes ((G.?+) grid x y)]
    where curr = (G.!) grid x y
