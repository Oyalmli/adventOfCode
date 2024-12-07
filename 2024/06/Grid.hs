module Grid where

import Data.Foldable qualified as F
import Data.Vector qualified as V

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

type Grid a = V.Vector (VU.Vector a)

dims :: Grid a -> (Int, Int)
dims = (,) <$> V.length <*> V.length

fromList :: [[Char]] -> Grid Char
fromList = V.fromList . map VU.fromList

toList :: Grid Char -> [[Char]]
toList = map VU.toList . V.toList

(><) :: Int -> Int -> Grid Char -> Char -> Grid Char
(><) x y g c = g V.// [(y, (g V.! y) VU.// [(x, c)])]

(!) :: Int -> Int -> Grid Char -> Char
(!) x y g = g V.! y VU.! x

(!?) :: Int -> Int -> Grid Char -> Maybe Char
(!?) x y g = do
  row <- g V.!? y
  row VU.!? x