module Main where

main :: IO ()
main = interact 
    $ show . sum
    . (zipWith ((fromEnum .) . (<)) <*> tail)
    . map (read::String->Int) . words
