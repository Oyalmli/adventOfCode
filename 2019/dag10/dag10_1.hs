import Data.List

type Point = (Int,Int)

main = interact $ show . maximum . map (length . nub) . allAngles . concat . makePoints . lines

makePoints :: [String] -> [[Point]]
makePoints = map (\(y, str) -> mp y str) . zip [0..] 

mp :: Int -> String -> [Point] 
mp y = concatMap (\(x,c) -> if c == '#' then [(x,y)] else []) . zip [0..]

allAngles :: [Point] -> [[Float]]
allAngles l = map (\x -> angles x l) l

angles :: Point -> [Point] -> [Float]
angles p@(x1,y1) = concatMap (\cp@(x2,y2) -> if cp /= p then [atan2 (fromIntegral (y2-y1)) (fromIntegral (x2-x1))] else [])
