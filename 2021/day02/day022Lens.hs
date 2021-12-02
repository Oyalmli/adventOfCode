{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens (makeLenses, (&), (+~), (-~))

data Sub = Sub { 
  _hor :: Int,
  _dpt :: Int,
  _aim :: Int
} deriving (Show)
makeLenses ''Sub

main :: IO ()
main = interact 
  $ show
  . flip solve (Sub {_hor = 0, _dpt = 0, _aim = 0})
  . lines

solve :: [String] -> Sub -> Int
solve []       sub = _hor sub * _dpt sub
solve (l : ls) sub = solve ls 
  ( case dir of
    "up"      -> sub &aim -~ n
    "down"    -> sub &aim +~ n
    "forward" -> sub &hor +~ n &dpt +~ n * _aim sub )
  where [dir, sn] = words l; n = read sn
