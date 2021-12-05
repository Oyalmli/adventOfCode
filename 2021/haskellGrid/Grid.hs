module Grid
  ( Grid,
    inGrid,
    fromList,
    Grid.replicate,
    getRow, getCol,
    (.!), (.!?),
    (.+), (.#),
    (.>),
  )
where

import qualified Data.List.Split as S
import qualified Data.Vector as V

type Grid a = V.Vector (V.Vector a)

-- |Creates a grid with a given width (not given that it is even)
fromList :: Int -> [a] -> Grid a
fromList w = V.fromList . Prelude.map V.fromList . S.chunksOf w

inGrid :: V.Vector (V.Vector a) -> Int -> Int -> Bool 
inGrid grid r c = 0 <= r && r < V.length grid && 0 <=c && c < V.length (grid V.! r) 

-- |Creates an empty grid
empty :: Grid a
empty = V.singleton V.empty

-- |Generates a Grid initialized with value a
replicate :: Int -> Int -> a -> Grid a
replicate r c a = V.replicate r (V.replicate c a)

-- |Unsafe get val at row column
(.!) :: Grid a -> Int -> Int -> a
(.!) grid r c = (grid V.! r) V.! c

-- |Safe get val at row column
(.!?) :: Grid a -> Int -> Int -> Maybe a
(.!?) grid r c = case grid V.!? r of
  Nothing -> Nothing
  Just row -> row V.!? c

-- |unsafe set value at row column
(.>) :: Grid a -> Int -> Int -> a -> Grid a
(.>) grid r c a = V.unsafeUpd grid [(r, V.unsafeUpd (grid V.! r) [(c, a)])]

-- |Takes a grid.
-- Returns the 4-neighbors of:
-- Row 'Int' Column 'Int' 
(.+) :: Grid a -> Int -> Int -> [Maybe a]
(.+) grid r c =
  Prelude.map
    (\(dy, dx) -> (.!?) grid (r + dy) (c + dx))
    [(-1, 0), (0, 1), (1, 0), (0, -1)]

-- |Takes a grid.
-- Returns the 8-neighbors of:
-- Row 'Int' Column 'Int' 
(.#) :: Grid a -> Int -> Int -> [Maybe a]
(.#) grid r c =
  Prelude.map
    (\(dy, dx) -> (.!?) grid (r + dy) (c + dx))
    [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

getRow :: Grid a -> Int -> V.Vector a
getRow grid r = grid V.! r

getCol :: Grid a -> Int -> V.Vector a
getCol grid c = V.map (V.! c) grid
