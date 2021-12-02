module Main where

main :: IO ()
main = interact
  $ show
  . uncurry (*)
  . foldl solve (0, 0)
  . lines

solve :: (Int, Int) -> String -> (Int, Int)
solve (hor, dpt) l = 
  case dir of
    "forward" -> (hor+n, dpt  )
    "down"    -> (hor  , dpt+n)
    "up"      -> (hor  , dpt-n)
  where 
    [dir, sn] = words l
    n = read sn
