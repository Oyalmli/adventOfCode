module Main where

main :: IO ()
main = interact
  $ show
  . solve (0, 0)
  . lines

solve :: (Int, Int) -> [String] -> Int
solve (hori, depth) []        = hori * depth
solve (hori, depth) (l : ls)  = solve ( 
  case dir of
    "forward" -> (hori + read sn,           depth)
    "down"    -> (hori          , depth + read sn)
    "up"      -> (hori          , depth - read sn)
  ) ls
  where [dir, sn] = words l
