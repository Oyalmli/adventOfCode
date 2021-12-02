module Main where

main :: IO ()
main = interact
  $ show
  . flip solve (0, 0)
  . lines

solve :: [String] -> (Int, Int) ->  Int
solve []       (hor, dpt) = hor * dpt
solve (l : ls) (hor, dpt) = solve ls
  ( case dir of
    "forward" -> (hor+n, dpt  )
    "down"    -> (hor  , dpt+n)
    "up"      -> (hor  , dpt-n) ) 
  where [dir, sn] = words l; n = read sn
