module Main where

main :: IO ()
main = interact
  $ show
  . flip solve (0, 0)
  . lines

solve :: [String] -> (Int, Int) ->  Int
solve []       (hori, depth) = hori * depth
solve (l : ls) (hori, depth) = solve ls
  ( case dir of
    "forward" -> (hori + read sn,           depth)
    "down"    -> (hori          , depth + read sn)
    "up"      -> (hori          , depth - read sn) ) 
  where [dir, sn] = words l
