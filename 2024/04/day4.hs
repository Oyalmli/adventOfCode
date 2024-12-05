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
part_1 grid 
  = filter (== "XMAS") $ map toString
  [ [(!?) x y grid | (x,y) <- points] | points <- pointsList ]
  where
    (rows, cols) = dims grid
    pointsList = 
      [ [(x + dx * i, y + dy * i) | i <- [0 .. 3]]
      |  x <- [ 0 .. rows],  y <- [ 0 .. cols]
      , dx <- [-1 ..    1], dy <- [-1 ..    1]
      ]

part_2 :: Grid Char -> [String]
part_2 grid 
  = filter (flip elem ["MMASS", "MSAMS", "SMASM", "SSAMM"]) $ map toString
  [ [(!?) (x + dx) (y + dy) grid | (dx, dy) <- points]
  | x <- [0 .. rows], y <- [0 .. cols]
  ]
  where
    (rows, cols) = dims grid
    points = [(-1, 1),(1, 1),(0, 0),(-1,-1),(1, -1)]

toString :: [Maybe Char] -> [Char]
toString = map $ fromMaybe ' '
