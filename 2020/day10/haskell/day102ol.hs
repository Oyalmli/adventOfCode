--main=interact$show.product.map(sum.flip take(1:[1..]).length).filter(all(==1)).group.(\ls->zipWith(-)(tail ls)ls).sort.(\list->maximum list+3:0:list).map read.lines

import Data.List
main = interact $ show 
    . product
    . map (sum . flip take (1:[1..]) . length) 
    . filter (all(==1)) . group
    . (\ls -> zipWith (-) (tail ls) ls) 
    . sort . (\list -> maximum list+3:0:list)
    . map read . lines