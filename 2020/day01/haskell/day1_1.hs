import Data.List (find)
import Data.Maybe (fromJust)

main = interact $ show
    . product . snd . fromJust
    . find ((==2020) . fst)
    . (\ls-> [(x+y,[x,y]) | x <- ls, y <- ls])
    . map read . words


