module Grid
  ( Grid,
    inGrid,
    fromList, fromListMat, toList,
    replicate,
    getRow, getCol,
    (!), (!?),
    (?+), (?++), (?#),
    (><),
  )
where

import Prelude hiding (replicate)
import qualified Data.List.Split as Split
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Maybe ( isJust ,mapMaybe, fromJust )

type Grid a = S.Seq (S.Seq a)

-- |Creates a grid with a given width (not given that it is even)
fromList :: Int -> [a] -> Grid a
fromList w = S.fromList . Prelude.map S.fromList . Split.chunksOf w

fromListMat :: [[a]] -> Grid a
fromListMat ls = S.fromList (map S.fromList ls)

toList :: Grid a -> [[a]]
toList grid = map F.toList (F.toList grid) 

inGrid :: S.Seq (S.Seq a) -> Int -> Int -> Bool 
inGrid grid r c = 0 <= r && r < S.length grid && 0 <=c && c < S.length (S.index grid r) 

-- |Creates an empty grid
empty :: Grid a
empty = S.singleton S.empty

-- |Generates a Grid initialized with value a
replicate :: Int -> Int -> a -> Grid a
replicate r c a = S.replicate r (S.replicate c a)

-- |Unsafe get val at row column
(!) :: Grid a -> Int -> Int -> a
(!) grid r = S.index (S.index grid r)

-- |Safe get val at row column
(!?) :: Grid a -> Int -> Int -> Maybe a
(!?) grid r c = case grid S.!? r of
  Nothing -> Nothing
  Just row -> row S.!? c

-- |unsafe set value at row column
(><) :: Grid a -> Int -> Int -> a -> Grid a
--(><) grid r c a = S.unsafeUpd grid [(r, V.unsafeUpd (grid V.! r) [(c, a)])]
(><) grid r c a = S.update r (S.update c a (S.index grid r)) grid

-- |Takes a grid.
-- Returns the 4-neighbors of:
-- Row 'Int' Column 'Int' 
(?+) :: Grid a -> Int -> Int -> [Maybe a]
(?+) grid r c =
  Prelude.map
    (\(dy, dx) -> (!?) grid (r + dy) (c + dx))
    [(-1, 0), (0, 1), (1, 0), (0, -1)]

(?++) :: Grid a -> Int -> Int -> [((Int,Int), a)]
(?++) grid r c = concatMap
  ((\ (a, mb) -> ([(a, fromJust mb) | isJust mb]))
     . (\ (dy, dx) -> ((r + dy,  c + dx), (!?) grid (r + dy) (c + dx))))
  [(-1, 0), (0, 1), (1, 0), (0, -1)]

-- |Takes a grid.
-- Returns the 8-neighbors of:
-- Row 'Int' Column 'Int' 
(?#) :: Grid a -> Int -> Int -> [Maybe a]
(?#) grid r c =
  Prelude.map
    (\(dy, dx) -> (!?) grid (r + dy) (c + dx))
    [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

getRow :: Grid a -> Int -> S.Seq a
getRow = S.index

getCol :: Grid a -> Int -> S.Seq a
getCol grid c = (`S.index` c) grid