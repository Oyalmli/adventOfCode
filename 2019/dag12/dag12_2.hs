import Data.Char
import Data.List.Split
import Text.Read
import Data.HashSet hiding (map)

type Moon = (Pos,Vel)

type Pos = (Int,Int,Int)
type Vel = (Int,Int,Int)

main = interact $ show . (\list -> solve 0 (empty) list) . map makeMoon . map parse . map (splitOneOf "=,>") . lines

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
        calcVel ((x,y,z), ((dx + (calc x x2))  ,(dy + (calc y y2))  ,(dz + (calc z z2)))) xs

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

solve :: Int -> HashSet [Moon] -> [Moon] -> Int
solve n set moonList = let entry = (map calcPos . (\ml -> map (\m -> calcVel m ml) ml)) moonList in case member entry set of
    True -> n
    False -> solve (n+1) (insert entry set) entry
