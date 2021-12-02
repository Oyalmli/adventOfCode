{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens

data Sub = Sub {
  _hor :: Int,
  _dpt :: Int
} deriving (Show)
makeLenses ''Sub

main :: IO ()
main = interact
  $ show
  . flip solve (Sub {_hor=0, _dpt=0})
  . lines

solve :: [String] -> Sub -> Int
solve []       sub   = _hor sub * _dpt sub
solve (l : ls) sub   = solve ls 
  ( case dir of
    "forward" -> sub &hor +~ n
    "down"    -> sub &dpt +~ n
    "up"      -> sub &dpt -~ n )
  where [dir, sn] = words l; n = read sn
