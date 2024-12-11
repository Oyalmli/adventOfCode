--- Day 10: Hoof It ---
import Data.HashSet qualified as S
import Grid

main :: IO ()
main = do
  grid <- fromList . lines <$> getContents
  let (w, h) = dims grid
  let paths =
        [ walk grid '0' (x, y)
        | x <- [0 .. w]
        , y <- [0 .. h]
        , (!?) x y grid == Just '0'
        ]
  putStrLn $ "Part 1: " ++ show (sum $ map (S.size . S.fromList) paths)
  putStrLn $ "Part 2: " ++ show (sum $ map length paths)

walk :: Grid Char -> Char -> (Int, Int) -> [(Int, Int)]
walk grid '9' (x, y) = [(x, y)]
walk grid chr (x, y) = concatMap (walk grid next)
  [ (x', y') 
  | (x', y') <- (|-) grid (x, y)
  , next == ((!) x' y' grid)
  ] where next = succ chr
