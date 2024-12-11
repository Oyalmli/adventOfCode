module Grid where

import Data.Vector.Unboxed qualified as VU

data Grid a = Grid
  { width :: Int,
    height :: Int,
    arr :: VU.Vector a
  }
  deriving (Show)

fromList :: (VU.Unbox a) => [[a]] -> Grid a
fromList [] = Grid 0 0 VU.empty
fromList rows@(x : xs) = Grid w h (VU.fromList (concat rows))
  where
    h = length rows
    w = if h == 0 then 0 else length x

toList :: (VU.Unbox a) => Grid a -> [[a]]
toList (Grid w _ vec) =
  [VU.toList $ VU.slice (i * w) w vec | i <- [0 .. (VU.length vec `div` w - 1)]]

dims :: Grid a -> (Int, Int)
dims (Grid w h _) = (w, h)

(!) :: (VU.Unbox a) => Int -> Int -> Grid a -> a
(!) x y (Grid w _ vec) = vec VU.! (y * w + x)

(!?) :: (VU.Unbox a) => Int -> Int -> Grid a -> Maybe a
(!?) x y (Grid w h vec)
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Just $ vec VU.! (y * w + x)

(><) :: (VU.Unbox a) => Int -> Int -> Grid a -> a -> Grid a
(><) x y (Grid w h vec) newVal = Grid w h (vec VU.// [(idx, newVal)])
  where
    idx = y * w + x
