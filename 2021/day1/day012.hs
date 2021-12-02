module Main where
import Control.Monad ( ap )

main :: IO ()
main = interact 
    $ show . sum
    . (zipWith ((fromEnum .) . (<)) <*> tail)
    . (ap (zipWith3 (((+) .) . (+))) tail `ap` drop 2)
    . map read . words
