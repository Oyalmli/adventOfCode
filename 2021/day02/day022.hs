module Main where

main :: IO ()
main = interact
  $ show
  . flip solve (0, 0, 0)
  . lines

solve :: [String] -> (Int, Int, Int) ->  Int
solve []       (hor, dpt, aim) = hor * dpt
solve (l : ls) (hor, dpt, aim) = solve ls 
  ( case dir of
    "forward" -> (hor+n, dpt + n*aim, aim  )
    "down"    -> (hor  , dpt        , aim+n)
    "up"      -> (hor  , dpt        , aim-n) )
  where [dir, sn] = words l; n = read sn
