{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens ( (&), (+~), (-~), makeLenses )

data Sub = Sub {
  _hor :: Int,
  _dpt :: Int
} deriving (Show)
makeLenses ''Sub

main :: IO ()
main = interact
  $ show
  . ((*) . _hor <*> _dpt)
  . foldl solve (Sub {_hor=0, _dpt=0})
  . lines

solve :: Sub -> String ->  Sub
solve sub l = 
  case dir of
    "forward" -> sub &hor +~ n
    "down"    -> sub &dpt +~ n
    "up"      -> sub &dpt -~ n
  where 
    [dir, sn] = words l
    n = read sn
