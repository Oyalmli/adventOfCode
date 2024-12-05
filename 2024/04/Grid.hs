module Grid where

import Data.Foldable qualified as F
import Data.Vector qualified as V

type Grid a = V.Vector (V.Vector a)

dims :: Grid a -> (Int, Int)
dims = (,) <$> V.length <*> V.length

fromList :: [[a]] -> Grid a
fromList = V.fromList . map V.fromList

toList :: Grid a -> [[a]]
toList = map F.toList . F.toList

(!) :: Int -> Int -> Grid a -> a
(!) x y g = g V.! y V.! x

(!?) :: Int -> Int -> Grid a -> Maybe a
(!?) x y g = do
  row <- g V.!? y
  row V.!? x