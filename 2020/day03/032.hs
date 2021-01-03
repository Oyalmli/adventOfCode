--main=interact$show.product.(\b->[(\n d c b->sum[fromEnum(i!!n)|(i,n)<-zip(map fst$filter((==0).snd)$zip b c)[0,d..]])0 x(cycle c)b|(x,c)<-zip[1,3,5,7,1][[0],[0],[0],[0],[0,1]]]).map(cycle.map(=='#')).lines

main = interact $ show . product 
    . (\b ->                                                                             -- Using [[Bool]] from line 8
        [(\n xd yc b ->                                                                  -- Function that takes in variables from line 6
            sum [fromEnum (i!!n) |                                                       -- (1) The list i=[Bool] Get Nth element (2) Sum of [Int] <- (fromEnum [Bool] -> [Int])
            (i,n) <- zip (map fst $ filter((==0) . snd) $ zip b yc) [0,xd..]]) 0 x y b | -- For removing rows when delta y == 2 (zip b yc) -> [([Bool], Bool)], filter on 2nd, map fst to get first in tuple.
                (x,y) <- zip [1,3,5,7,1] (map cycle [[0],[0],[0],[0],[0,1]])])           -- (1) map cycle [[Int]] -> [Inf[Int]] (2)(x,y)=(Int, [Inf[Int]]) <- zip [Int] [Inf[Int]]
    . map (cycle . (map(\x->x=='#'))) . lines                                            -- (1) Transfrom input to [String] (2) map cycle (map #-> True)::Inf[Bool]) -> [[Bool]]
