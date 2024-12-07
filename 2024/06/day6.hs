{-# LANGUAGE InstanceSigs #-}

import Data.HashSet qualified as S
import Data.Hashable
import Data.List (transpose)
import Data.Vector qualified as V

data Fin = Count Int | Loop deriving (Show, Eq)
data Dir = U | D | L | R deriving (Show, Eq, Ord, Enum)

instance Hashable Dir where
  hashWithSalt :: Int -> Dir -> Int
  hashWithSalt = hashUsing fromEnum

type TreePosGrid = V.Vector [Int]
type PlayerData = (Int, Int, Dir)

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let (px, py) = findPlayerLocation ls
  let udTree = makeTreePosGrid ls
  let lrTree = makeTreePosGrid (transpose ls)
  let grids = 
  print $ solve 0 S.empty (px, py, U) (udTree, lrTree)

solve :: Int -> S.HashSet PlayerData -> PlayerData -> (TreePosGrid, TreePosGrid) -> Fin
solve count visited pPos@(px, py, pdir) treePos =
  if S.member pPos visited
    then Loop
    else case findCollision pPos treePos of
      Just (x, y) ->
        let steps = abs (px - x) + abs (py - y)
        in solve (count + steps) (S.insert pPos visited) (x, y, rotateRight pdir) treePos
      Nothing -> Count count

rotateRight :: Dir -> Dir
rotateRight U = R
rotateRight R = D
rotateRight D = L
rotateRight L = U

findCollision :: PlayerData -> (TreePosGrid, TreePosGrid) -> Maybe (Int, Int)
findCollision (x, y, dir) (lr_treePos, ud_treePos) = case dir of
  U -> vertCheck (x, y) dir ud_treePos
  D -> vertCheck (x, y) dir ud_treePos
  L -> horizCheck (x, y) dir lr_treePos
  R -> horizCheck (x, y) dir lr_treePos

horizCheck :: (Int, Int) -> Dir -> TreePosGrid -> Maybe (Int, Int)
horizCheck (x, y) dir ud_treePos =
  let row = ud_treePos V.! y
  in case dir of
    R -> let treesRight = filter (> x) row
         in if null treesRight then Nothing else Just (minimum treesRight - 1, y)
    L -> let treesLeft = filter (< x) row
         in if null treesLeft then Nothing else Just (maximum treesLeft + 1, y)
    _ -> Nothing

vertCheck :: (Int, Int) -> Dir -> TreePosGrid -> Maybe (Int, Int)
vertCheck (x, y) dir lr_treePos =
  let col = lr_treePos V.! x
  in case dir of
    U -> let treesAbove = filter (< y) col
         in if null treesAbove then Nothing else Just (x, maximum treesAbove + 1)
    D -> let treesBelow = filter (> y) col
         in if null treesBelow then Nothing else Just (x, minimum treesBelow - 1)
    _ -> Nothing


makeTreePosGrid :: [String] -> TreePosGrid
makeTreePosGrid ls =
  V.fromList
    [ [idx | (c, idx) <- zip line [0 ..], isTree c]
    | line <- ls
    ]

isTree :: Char -> Bool
isTree = (== '#')

isPlayer :: Char -> Bool
isPlayer = (== '^')

findPlayerLocation :: [String] -> (Int, Int)
findPlayerLocation ls = head [(x, y) | (y, line) <- zip [0 ..] ls, (x, c) <- zip [0 ..] line, isPlayer c]
