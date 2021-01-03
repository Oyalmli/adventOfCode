import Data.List

main = interact $ show . solve . diff .  sort . (\x -> (maximum x+3):x) .(++[0]) . map (read::String -> Int) . lines

solve list = (length (filter (==3) list)) * (length (filter (==1) list))

diff [] = []
diff [x] = []
diff (x:y:xs) = (y - x): diff (y:xs)

