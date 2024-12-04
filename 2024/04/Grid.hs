module Grid where

import Data.Foldable qualified as F
import Data.Sequence qualified as S

type Grid a = S.Seq (S.Seq a)

dims :: Grid a -> (Int, Int)
dims = (,) <$> S.length <*> S.length

fromList :: [[a]] -> Grid a
fromList = S.fromList . map S.fromList

toList :: Grid a -> [[a]]
toList = map F.toList . F.toList

(!) :: Int -> Int -> Grid a -> a
(!) x y g = g `S.index` y `S.index` x

(!?) :: Int -> Int -> Grid a -> Maybe a
(!?) x y g = do
  row <- g S.!? y
  row S.!? x