module Main where

main :: IO ()
main = interact
  $ show
  . solve (0, 0, 0)
  . lines

solve :: (Int, Int, Int) -> [String] -> Int
solve (hori, depth, aim) []       = hori * depth
solve (hori, depth, aim) (l : ls) = solve ( 
  case dir of
    "forward" -> (hori+n, depth + n*aim, aim    )
    "down"    -> (hori  , depth        , aim + n)
    "up"      -> (hori  , depth        , aim - n)
  ) ls
  where 
    [dir, sn] = words l
    n = read sn
