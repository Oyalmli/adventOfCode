{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.List.Split ( splitOneOf )
import qualified Grid as G
import Data.List ( nub, sort, group )

main :: IO ()
main = interact
    $ show 
    . length
    . filter (>= 2)
    . concat
    . G.toList
    . foldl placeLines (G.replicate 989 990 0)
--    . foldl placeLines (G.replicate 10 10 0)
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
placeLines grid t = foldl (\g (c,r) -> addG g r c) grid $ genTups t
        

addG :: G.Grid Int -> Int -> Int -> G.Grid Int
addG grid r c = (G..>) grid r c (atp +1)
    where atp = (G..!) grid r c

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
