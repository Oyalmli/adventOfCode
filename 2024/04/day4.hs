--- Day 4: Ceres Search ---
import Data.Maybe (fromMaybe)
import Grid

main :: IO ()
main = do
    inp <- getContents
    let grid = fromList $ lines inp
    putStrLn $ "Part 1: " ++ printPart (part_1 grid)
    putStrLn $ "Part 2: " ++ printPart (part_2 grid)
    where printPart = show . length

part_1 :: Grid Char -> [String]
part_1 grid = filter isXmas
    [ toString [((!?) (x + dx * i) (y + dy * i) grid) | i <- [0 .. 3]]
    | x <- [0..cols], y <- [0..rows]
    , (dx, dy) <- directions
    ]
  where
    (cols, rows) = dims grid
    isXmas = (== "XMAS")
    directions = 
      [ (-1,-1),(-1, 0),(-1, 1)
      , ( 0,-1),        ( 0, 1)
      , ( 1,-1),( 1, 0),( 1, 1)
      ]

part_2 :: Grid Char -> [String]
part_2 grid = filter isXmas
    [ toString [(!?) (x + dx) (y + dy) grid | (dx, dy) <- points]
    | x <- [0..cols], y <- [0..rows]
    ]
  where
    (cols, rows) = dims grid
    isXmas str = str `elem` ["MMASS", "MSAMS", "SSAMM", "SMASM"]
    points = 
      [ (-1, 1),       (1, 1)
      ,         (0, 0)
      , (-1,-1),       (1, -1)
      ]

toString :: [Maybe Char] -> [Char]
toString = map $ fromMaybe ' '