{-# LANGUAGE QuasiQuotes, TypeOperators #-}
import Text.Scanf
import Data.Maybe

type RP = (Int, Int, Char, String)

main = interact $ show
    . length . filter id 
    . map (verify . toRP . fromJust . scanf [fmt|%d-%d %c: %s|])
    . lines

toRP :: Int :+  Int :+ Char :+ String :+ () -> RP
toRP (x:+y:+c:+str:+()) = (x, y, c, str)

verify :: RP -> Bool
verify (x, y, c, str) = (/=) (str !! (x-1) == c) (str !! (y-1) == c)