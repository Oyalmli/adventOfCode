{-# LANGUAGE QuasiQuotes, TypeOperators #-}
import Text.Scanf
import Data.Maybe

type RP = (Int, Int, Char, String)

main = interact $ show
    . length . filter id 
    . map (verify . toRP . fromJust . scanf [fmt|%d-%d %c: %s|])
    . lines

toRP :: Int :+  Int :+ Char :+ String :+ () -> RP
toRP (x:+ y:+ c:+ str :+ ()) = (x, y, c, str)

verify :: RP -> Bool
verify (x, y, c, str) = (\cs -> between x (length cs) y) $ filter (==c) str

between x y z --using guards
  |x <= y = y <= z
  |otherwise = False

