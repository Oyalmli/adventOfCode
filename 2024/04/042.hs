import Data.Maybe (fromMaybe)
import Grid

main :: IO ()
main = interact 
    $ show . length 
    . filter isXmas 
    . solve 
    . fromList . lines

solve :: Grid Char -> [String]
solve grid =
  [ toString [(!?) (x + dx) (y + dy) grid | (dx, dy) <- pat]
  | x <- [0 .. cols - 1],
    y <- [0 .. rows - 1],
    pat <- starPatterns
  ]
  where
    (cols, rows) = dims grid
    starPatterns = getStarPatterns
    toString = map $ fromMaybe ' '

getStarPatterns :: [[(Int, Int)]]
getStarPatterns = [[(-1, 1), (1, 1), (0, 0), (-1, -1), (1, -1)]]

isXmas :: String -> Bool
isXmas str = str `elem` ["MMASS", "MSAMS", "SSAMM", "SMASM"]

{-
MMASS
M M
 A
S S

MSAMS
M S
 A
M S

SSAMM
S S
 A
M M

SMASM
S M
 A
S M
-}