{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.List.Split ( splitOn, splitOneOf )
import qualified Grid as G
import Data.List ( nub, sort, group )

main :: IO ()
main = interact
  $ show . length
  . filter (>= 2)
  . concat . G.toList
  . foldl placeLines (G.replicate 989 990 0)
  . filter isHorVert
  . map (parseLine . splitOneOf ", ") . lines

parseLine :: [String] -> ((Int,Int),(Int,Int))
parseLine (x0:y0:_:x1:y1:_) = ((read x0, read y0),(read x1, read y1))

isHorVert :: ((Int,Int),(Int,Int)) -> Bool 
isHorVert ((l0,l1),(r0,r1)) = l0 == r0 || l1 == r1

placeLines :: G.Grid Int -> ((Int, Int), (Int, Int)) -> G.Grid Int
placeLines grid ts@((x0,y0),(x1,y1)) = 
  foldl (\g (r,c) -> addG g r c) grid $ genTups ts

genTups :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genTups ((l0,l1),(r0,r1))
    | l0 == r0 = zip (repeat l0) [ly..my]
    | l1 == r1 = zip  [lx..mx] (repeat l1)
    | otherwise = zip [lx..mx] [ly..my]
    where 
        [lx, mx] = sort [l0,r0]
        [ly, my] = sort [l1,r1]

addG :: G.Grid Int -> Int -> Int -> G.Grid Int
addG grid r c = (G.><) grid r c (atp +1)
    where atp = (G.!) grid r c
