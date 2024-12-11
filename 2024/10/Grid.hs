module Grid where

import Data.List (intercalate)
import Data.Vector.Unboxed qualified as VU

data Grid a = Grid
  { width :: Int,
    height :: Int,
    arr :: VU.Vector a
  }

getRows :: (Show a, VU.Unbox a) => Grid a -> [[a]]
getRows grid =
  let w = width grid
      h = height grid
      v = arr grid
   in [VU.toList $ VU.slice (i * w) w v | i <- [0 .. h - 1]]

instance (Show a, VU.Unbox a) => Show (Grid a) where
  show grid =
    "[" ++ intercalate ",\n " (map show (getRows grid)) ++ "]"

instance {-# OVERLAPS #-} Show (Grid Char) where
  show grid =
    unlines $ getRows grid

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

inBounds :: (VU.Unbox a) => Grid a -> (Int, Int) -> Bool
inBounds grid (x, y) =
  let (w, h) = dims grid
   in x >= 0 && y >= 0 && x < w && y < h

-- | Get the value at a specific position
(!) :: (VU.Unbox a) => Int -> Int -> Grid a -> a
(!) x y (Grid w _ vec) = vec VU.! (y * w + x)

(!?) :: (VU.Unbox a) => Int -> Int -> Grid a -> Maybe a
(!?) x y (Grid w h vec)
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Just $ vec VU.! (y * w + x)

-- | Update the value at a specific position
(><) :: (VU.Unbox a) => Int -> Int -> Grid a -> a -> Grid a
(><) x y (Grid w h vec) newVal = Grid w h (vec VU.// [(idx, newVal)])
  where
    idx = y * w + x

-- | 4 neighbours inside the bounds of the grid
(|-) :: (VU.Unbox a) => Grid a -> (Int, Int) -> [(Int, Int)]
(|-) grid (x, y) = filter (inBounds grid) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

-- | 8 neighbours inside the bounds of the grid
(\|-) :: (VU.Unbox a) => Grid a -> (Int, Int) -> [(Int, Int)]
(\|-) grid (x, y) = filter (inBounds grid) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1)]