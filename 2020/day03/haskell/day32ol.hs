--main=interact$show.product.(\b->[(\n xd yc b->sum[fromEnum (i!!n)|(i,n)<-zip(map fst$filter((==0).snd)$zip b yc)[0,xd..]])0 x y b|(x,y)<-zip[1,3,5,7,1](map cycle [[0],[0],[0],[0],[0,1]])]).map(cycle.(map(\x->x=='#'))).lines
main = interact $ show . product 
    . (\b ->                                                                             -- Tar inn [[Bool]] fra linje 8
        [(\n xd yc b ->                                                                  -- Funksjon som tar inn variabler fra linje 6
            sum [fromEnum (i!!n) |                                                       -- (1) fra listen med i=[Bool]hent elementet på n (2) Sum av [Int] <- (fromEnum [Bool] -> [Int])
            (i,n) <- zip (map fst $ filter((==0) . snd) $ zip b yc) [0,xd..]]) 0 x y b | -- For å fjerne rader nå delta y == 2 (zip b yc) -> [([Bool], Bool)], filtrerer på 2nd, map fst for å få første.
                (x,y) <- zip [1,3,5,7,1] (map cycle [[0],[0],[0],[0],[0,1]])])           -- (1) map cycle [[Int]] -> [Inf[Int]] (2)(x,y)=(Int, [Inf[Int]]) <- zip [Int] [Inf[Int]]
    . map (cycle . (map(\x->x=='#'))) . lines                                            -- (1) Gjør om input til [String] (2) map cycle (map #-> True)::Inf[Bool]) -> [[Bool]]
