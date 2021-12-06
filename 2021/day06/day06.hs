module Main where

import Data.List.Split ( splitOn ) 
import qualified Data.Map as M

main :: IO ()
main = do
    content <- getContents
    let filledHistogram = (fillMap . histogram . map read . splitOn ",") content
    mapM_ (print . flip solve filledHistogram) [80, 256]

fillMap :: M.Map Int Int -> M.Map Int Int
fillMap smp = foldl (\mp i -> snd $ M.insertLookupWithKey (\k n o -> o) i 0 mp) smp ([0..8]::[Int])

solve :: Int -> M.Map Int Int -> Int
solve 0 mp = sum $ map snd $ M.toList mp
solve n mp = solve (n-1) (step mp)

step :: M.Map Int Int -> M.Map Int Int
step mp = M.mapWithKey step' mp
    where 
        step' i v 
            | i == 6 = mp M.! 0 + mp M.! 7
            | i == 8 = mp M.! 0
            | otherwise = mp M.! (i+1)

histogram :: [Int] -> M.Map Int Int
histogram = foldl (\m k -> M.insertWith (+) k 1 m) M.empty
