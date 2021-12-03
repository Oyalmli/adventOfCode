module Main where

import Data.Char ( digitToInt )
import Data.List ( transpose )
import Control.Monad ( liftM2 )

main :: IO ()
main = interact 
  $ show . solve
  . map (liftM2 (,) sum (liftM2 (-) length sum) . map digitToInt)
  . transpose . lines

solve :: [(Int, Int)] -> Int
solve = ((*) . f (>)) <*> f (<)
  where f = (bintodec .) . map . uncurry

bintodec :: [Bool] -> Int
bintodec = foldr ((. (2*)) . (+) . fromEnum) 0 . reverse