import Data.List.Split

type Btup = (Int, Int, Int)

main = interact $ show . (\(_,x,y) -> x * y) . foldl1 fewest0 
    . map (makeTups (0,0,0)) . chunksOf (25*6)

makeTups :: Btup -> String -> Btup
makeTups tup [] = tup
makeTups (t0,t1,t2) ('0':xs) = makeTups (t0+1,t1,t2) xs
makeTups (t0,t1,t2) ('1':xs) = makeTups (t0,t1+1,t2) xs
makeTups (t0,t1,t2) ('2':xs) = makeTups (t0,t1,t2+1) xs
makeTups (t0,t1,t2) (_:xs) = makeTups (t0,t1,t2) xs

fewest0 :: Btup -> Btup -> Btup
fewest0 tup1@(t1,_,_) tup2@(t2,_,_) = if t1 < t2 then tup1 else tup2
