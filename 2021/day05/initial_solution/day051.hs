{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.List.Split ( splitOneOf )
import qualified Grid as G
import Data.List ( nub, sort, group )

main :: IO ()
main = interact
    $ show . length
    . filter (>= 2)
    . concat
    . G.toList
    . foldl placeLines (G.replicate 989 990 0)
    . filter isHorVert
    . parseLines
    . map (splitOneOf ", ")
    . lines

parseLines :: [[String]] -> [((Int,Int),(Int,Int))]
parseLines [] = []
parseLines ((l0:l1:_:r0:r1:_):ls) = 
    ((read l0, read l1),(read r0, read r1)): parseLines ls

isHorVert :: ((Int,Int),(Int,Int)) -> Bool 
isHorVert ((l0,l1),(r0,r1)) = l0 == r0 || l1 == r1

--placeLines :: G.Grid Int -> ((Int, Int), (Int, Int)) -> G.Grid Int
placeLines grid ((fl0,fl1),(fr0,fr1)) = foldl (\g (r,c) -> addG g r c) grid $ nub [(y,x) | x <- [l0..r0], y <- [l1..r1]]
    where 
        [l0,r0] = sort [fl0, fr0]
        [l1,r1] = sort [fl1, fr1]

addG :: G.Grid Int -> Int -> Int -> G.Grid Int
addG grid r c = (G..>) grid r c (atp +1)
    where atp = (G..!) grid r c
