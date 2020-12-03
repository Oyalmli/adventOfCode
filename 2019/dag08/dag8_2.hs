import Data.List.Split
import Data.Char
import Data.List

main = interact $  
    map ( 
        (\x -> if x==0 then '█' else ' ') . 
        (foldl1 (\x y -> if x < 2 then x else y)) . 
        map digitToInt 
    ) 
    . transpose . chunksOf (25*6)







{-
main = interact $ pp . map fc 
    . map (map digitToInt) . transpose . chunksOf (25*6)

fc = foldl1 (\x y -> if x < 2 then x else y) -- get the first pixel with color

pp = map (\x -> if x==0 then '█' else ' ') -- prettyPrint
-}
