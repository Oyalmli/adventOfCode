import Data.List (find)
import Data.Maybe (fromJust)

main = interact $ show
    . product . snd . fromJust
    . find ((==2020) . fst) 
    . (\ls -> [(x+y+z, [x,y,z]) | x <- ls, y <- ls, z <- ls])
    . map read . words
