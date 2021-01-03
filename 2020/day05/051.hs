import Data.Char  (digitToInt)
import Data.List (foldl', sort,findIndex)
--main=interact$show.maximum.map((\(x,y)->let f=(\l->sum$zipWith(*)[1,2,4,8,16,32,64](reverse l))in(f x)*8+(f y)).splitAt 7.map(\x->if x`elem`"BR" then 1 else 0)).words

main = interact $ show . maximum 
    . map((\(x,y)->let f=(\l->sum$zipWith(*)[1,2,4,8,16,32,64](reverse l)) in (f x)*8 + (f y))
    .splitAt 7.map(\x->if x`elem`"BR" then 1 else 0)).words
