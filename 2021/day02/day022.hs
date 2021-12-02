module Main where

main :: IO ()
main = interact
  $ show
  . flip solve (0, 0, 0)
  . lines

solve :: [String] -> (Int, Int, Int) ->  Int
solve []       (hori, depth, aim) = hori * depth
solve (l : ls) (hori, depth, aim) = solve ls 
  ( case dir of
    "forward" -> (hori+n, depth + n*aim, aim    )
    "down"    -> (hori  , depth        , aim + n)
    "up"      -> (hori  , depth        , aim - n) )
  where [dir, sn] = words l; n = read sn
