{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.List.Split ( splitOneOf )
import qualified Grid as G
import Data.List ( nub, sort, group )
import Data.Bifunctor (bimap)

main :: IO ()
main = interact
    $ show . length
    . filter (>= 2)
    . concat . G.toList
    . (\ts -> 
        let (maxX, maxY) = getMaxXY ts 
        in foldl placeLines (G.replicate maxX maxY 0) ts)
    . map (parseLine . splitOneOf ", ") . lines

getMaxXY :: [((Int,Int),(Int,Int))] -> (Int,Int)
getMaxXY tups = bimap ((+1) . maximum) ((+1) . maximum)
    $ foldl1 (\(ta0,tb0) (ta1,tb1) -> (ta0 ++ ta1, tb0 ++ tb1) )
    $ map (\((a,b),(c,d)) -> ([a,b],[c,d])) tups

parseLine :: [String] -> ((Int,Int),(Int,Int))
parseLine (x0:y0:_:x1:y1:_) = ((read x0, read y0),(read x1, read y1))

placeLines :: G.Grid Int -> ((Int, Int), (Int, Int)) -> G.Grid Int
placeLines grid t = foldl (\g (c,r) -> addG g r c) grid $ genTups t
        
addG :: G.Grid Int -> Int -> Int -> G.Grid Int
addG grid r c = (G.><) grid r c (atp +1)
    where atp = (G.!) grid r c

genTups :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genTups ((l0,l1),(r0,r1)) 
    | r0 < l0 && r1 > l1 = zip (reverse [lx..mx]) [ly..my]
    | r1 < l1 && r0 > l0 = zip [lx..mx] (reverse [ly..my])
    | l0 == r0 = zip (repeat l0) [ly..my]
    | l1 == r1 = zip  [lx..mx] (repeat l1)
    | otherwise = zip [lx..mx] [ly..my]
    where 
        [lx, mx] = sort [l0,r0]
        [ly, my] = sort [l1,r1]
