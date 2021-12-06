module Main where

import Data.List.Split ( splitOn ) 
import qualified Data.Map as M
import Control.Monad.Trans.Accum (mapAccum)
import Data.IntMap (mapAccumWithKey)

main :: IO ()
main = interact 
    $ show 
    . solve 256
    . fillMap 
    . histogram
    . map read
    . splitOn ","

fillMap :: M.Map Int Int -> M.Map Int Int
fillMap mp = M.fromList [
    (0,if M.member 0 mp then mp M.! 0 else 0),
    (1,if M.member 1 mp then mp M.! 1 else 0),
    (2,if M.member 2 mp then mp M.! 2 else 0),
    (3,if M.member 3 mp then mp M.! 3 else 0),
    (4,if M.member 4 mp then mp M.! 4 else 0),
    (5,if M.member 5 mp then mp M.! 5 else 0),
    (6,if M.member 6 mp then mp M.! 6 else 0),
    (7,if M.member 7 mp then mp M.! 7 else 0),
    (8,if M.member 8 mp then mp M.! 8 else 0)
    ]

solve :: Int -> M.Map Int Int -> Int
solve 0 mp = sum $ map snd $ M.toList mp
solve n mp = solve (n-1) (step mp)

step :: M.Map Int Int -> M.Map Int Int
step mp = M.fromList [
    (0, mp M.! 1),
    (1, mp M.! 2),
    (2, mp M.! 3),
    (3, mp M.! 4),
    (4, mp M.! 5),
    (5, mp M.! 6),
    (6, mp M.! 0 + mp M.! 7),
    (7, mp M.! 8),
    (8, mp M.! 0)
    ]
    
histogram :: [Int] -> M.Map Int Int
histogram = foldl (\m k -> M.insertWith (+) k 1 m) M.empty