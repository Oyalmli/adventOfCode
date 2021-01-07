import Prelude hiding (concatMap)
import Data.List hiding (concatMap)
import Data.Vector (Vector, fromList, toList, (!), (!?), concatMap, (//))

type Grid = Vector (Vector Char)
type Dimention = (Int, Int)
type Pos = (Int, Int)

main = interact $ show . countOccupied
    . (\grid -> update grid $ gridDim grid) 
    . toGrid . lines

toGrid :: [String] -> Grid
toGrid strList = fromList $ map fromList strList

gridDim :: Grid -> Dimention
gridDim grid = (length (grid ! 0), length grid)

update :: Grid -> Dimention -> Grid
update grid (width, height) = if grid == newgrid then grid else update newgrid (width,height)
    where 
        newgrid = fromList $ updateVecs [
            case getPos grid (x,y) of 
                '.' -> '.'
                '#' -> if getAroundP grid (x,y) then 'L' else '#'
                'L' -> if getAroundL grid (x,y) then '#' else 'L'
                s -> error (show s ++ show (x,y))
            | y <- [0..(height-1)], x <- [0..(width-1)]] width

getAroundP :: Grid -> Pos -> Bool
getAroundP grid pos = (>=5) $ sum $ map (getDirs grid 0 pos) dirs
    where dirs = [(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1),(1,1)]::[Pos]

getAroundL :: Grid -> Pos -> Bool
getAroundL grid pos = (>=1) $ sum $ map (getDirs grid 0 pos) dirs
    where dirs = [(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1),(1,1)]::[Pos]

updateVecs:: String -> Int -> [Vector Char]
updateVecs [] _ = []
updateVecs updateString width = (fromList $ take width updateString)
    : updateVecs (drop width updateString) width

getDirs :: Grid -> Int -> Pos -> Pos -> Int
getDirs grid n (x,y) (xd, yd) = case getPos grid (x+xd, y+yd) of
    ' ' -> 0
    '#' -> 1
    'L' -> 1
    '.' -> getDirs grid n (x+xd,y+yd) (xd, yd)
    _ -> error "oopies"

getPos :: Grid -> Pos -> Char
getPos grid (x,y) = case grid !? y of
    Nothing -> ' '
    Just row -> case row !? x of
        Nothing -> ' '
        Just char -> char

printGrid :: Grid -> String
printGrid grid = (unlines $ map toList (toList grid)) ++ 
    "\n" ++ "There are " ++ (show $ countOccupied grid) ++ " occupied seats"

countOccupied :: Grid -> Int
countOccupied grid = length $ filter (=='#') $ concat $ map toList $ toList grid
