import Data.List (sort,findIndex,find)
--main=interact$show.(\(Just x)->snd x).find(\(x,y)->x/=y).(\x->zip x[(minimum x)..]).sort. map((\(x,y)->((\l->sum$zipWith(*)(reverse(scanl1(\x y->x*2)[1..(length l)]))l) x *8) + (\l->sum$zipWith(*)(reverse(scanl1(\x y->x*2)[1..(length l)]))l) y).splitAt 7.map(\x->if x`elem`"BR" then 1 else 0)).words
--main=interact$show.find((>0).snd).(\x->zipWith(\x y->(x-1,x-y))x[minimum x..]).sort.map((\(x,y)->let f=(\l->sum$zipWith(*)[1,2,4,8,16,32,64](reverse l))in(f x)*8+(f y)).splitAt 7.map(fromEnum.(`elem`"BR"))).words

main=interact$show.find((>0).snd)
    . (\x->zipWith(\x y->(x-1,x-y))x[minimum x..]) 
    . sort . map((\(x,y)->let f=(\l->sum$zipWith(*)[1,2,4,8,16,32,64](reverse l)) in f x*8 +f y)
    . splitAt 7 . map(fromEnum.(`elem`"BR"))).words