import Data.Char
import Data.List.Split
import Text.Read

type Moon = (Pos,Vel)

type Pos = (Int,Int,Int)
type Vel = (Int,Int,Int)

--main = interact $ concat . map pp . solve . map makeMoon . map parse . map (splitOneOf "=,>") . lines
main = interact $ show . calculator . solve . map makeMoon . map parse . map (splitOneOf "=,>") . lines

parse :: [String] -> [Int]
parse [] = []
parse (x:xs) = case (readMaybe x :: Maybe Int) of
    Just n -> n : parse xs
    Nothing -> parse xs

makeMoon :: [Int] -> Moon
makeMoon (x:y:z:xs) = ((x,y,z), (0,0,0)) 

calcVel :: Moon -> [Moon] -> Moon
calcVel moon [] = moon
calcVel ((x,y,z),(dx,dy,dz)) (((x2,y2,z2),_):xs) = 
    do 
        let dx2 = calc x x2
        let dy2 = calc y y2
        let dz2 = calc z z2
        calcVel ((x,y,z),((dx + dx2),(dy + dy2), (dz + dz2))) xs

calc :: Int -> Int -> Int
calc n1 n2 = 
    if n1 > n2 
        then -1 
        else if n1 == n2
            then 0
            else 1   

calcPos :: Moon -> Moon
calcPos ((x,y,z),vel@(dx,dy,dz)) = (((x+dx),(y+dy),(z+dz)),vel)

pp :: Moon -> String 
pp ((x,y,z),(dx,dy,dz)) = 
    "pos=<x= " ++ show x ++ ", y= " ++ show y ++ ", z= " ++ show z ++ ">, " ++
    "vel=<x= " ++ show dx ++ ", y= " ++ show dy ++ ", z= " ++ show dz ++ "> \n"

applyN :: (a -> a) -> Int -> a -> a
applyN f n x = iterate f x !! n

solve :: [Moon] -> [Moon]
solve moonList = applyN (map calcPos .  (\ml -> map (\m -> calcVel m ml) ml)) 1000 moonList

calculator :: [Moon] -> Int
calculator [] = 0
calculator (((x,y,z),(dx,dy,dz)):xs) = (abs x + abs y + abs z) * (abs dx + abs dy + abs dz) + calculator xs