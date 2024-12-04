import Grid
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ show . length . filter isXmas . solve . fromList . lines

solve :: Grid Char -> [String]
solve grid = 
    [ toString [((!?) (x + dx * i) (y + dy * i) grid) | i <- [0 .. 3] ]
    | x <- [0 .. cols - 1], y <- [0 .. rows - 1]
    , (dx, dy) <- directions
    ]
  where
    (cols, rows) = dims grid
    toString = map $ fromMaybe ' '
    directions = 
        [ (-1, -1), (-1,  0), (-1, 1)
        , ( 0, -1),           ( 0, 1)
        , ( 1, -1), ( 1,  0), ( 1, 1)
        ]

isXmas :: String -> Bool
isXmas str = str == "XMAS"