import Data.List
{-}
main = interact $ show . maximum . map (length . nub) 
    . (\l -> 
        map (\x -> 
            (\p@(x1,y1) -> concatMap (\cp@(x2,y2) -> 
                if cp /= p 
                    then [atan2 (fromIntegral (y2-y1)) (fromIntegral (x2-x1))] 
                    else [])) x l) l) 
    . (concatMap (\(y, str) -> 
        (\n -> (concatMap (\(x,c) -> 
            if c == '#' 
                then [(x,n)] 
                else []) . 
            zip [0..])) y str) . 
            zip [0..] ) 
    . lines
-}

main = interact $ show . maximum . map (length . nub) . (\l -> map (\x -> (\p@(x1,y1) -> concatMap (\cp@(x2,y2) -> if cp /= p then [atan2 (fromIntegral (y2-y1)) (fromIntegral (x2-x1))] else [])) x l) l) . (concatMap (\(y, str) -> (\n -> (concatMap (\(x,c) -> if c == '#' then [(x,n)] else []) . zip [0..])) y str) . zip [0..] ) . lines
