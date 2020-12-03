--main=interact$show.product.(\b->[(\n xd yc b->sum[fromEnum (i!!n)|(i,n)<-zip(map fst$filter((==0).snd)$zip b yc)[0,xd..]])0 x y b|(x,y)<-zip[1,3,5,7,1](map cycle [[0],[0],[0],[0],[0,1]])]).map(cycle.(map(\x->x=='#'))).lines
main = interact $ show . product 
    . (\b -> -- Tar inn [[Bool]] fra linje 8
        [(\n xd yc b -> --funksjon som tar inn variabler fra linje 6
            sum [fromEnum (i!!n) | -- Sum av [Int] <- (fromEnum [Bool] -> [Int])
            (i,n) <- zip (map fst $ filter((==0) . snd) $ zip b yc)[0,xd..]]) 0 x y b | 
                (x,y) <- zip [1,3,5,7,1] (map cycle [[0],[0],[0],[0],[0,1]])]) --
    . map (cycle . (map(\x->x=='#'))) . lines -- 1.Gjør om input til [String] 2. map cycle (map #-> True)::Inf[Bool]) -> [[Bool]]
