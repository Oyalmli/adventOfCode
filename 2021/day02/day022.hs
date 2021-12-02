module Main where

main :: IO ()
main = interact
  $ show
  . (\(hor, dep,_) -> hor * dep)
  . foldl solve (0, 0, 0)
  . lines

solve :: (Int, Int, Int) -> String -> (Int, Int, Int)
solve (hor, dpt, aim) l =
  case dir of
    "forward" -> (hor+n, dpt + n*aim, aim  )
    "down"    -> (hor  , dpt        , aim+n)
    "up"      -> (hor  , dpt        , aim-n)
  where 
    [dir, sn] = words l
    n = read sn
