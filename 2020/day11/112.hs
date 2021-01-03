import Prelude hiding (concatMap)
import Data.List hiding (concatMap)
import Data.Vector (Vector, fromList, toList, (!), (!?), concatMap, (//))

type Grid = Vector (Vector Char)
type Dimention = (Int, Int)
type Pos = (Int, Int)

main = interact $ show . countOccupied
    . (\grid -> update grid (gridDim grid)) 
    . toGrid . lines

toGrid :: [String] -> Grid
toGrid strList = fromList $ map fromList strList

gridDim :: Grid -> Dimention
gridDim grid = (length (grid ! 0), length grid)

update :: Grid -> Dimention -> Grid
update grid (width, height) = if grid == newgrid then grid else update newgrid (width,height)
    where 
        newgrid = fromList $ updateVecs [
            case getP grid (x,y) of 
                '.' -> '.'
                '#' -> if (>=5) $ length $ filter (=='#') $ getAround grid (x,y) then 'L' else '#'
                'L' -> if (==0) $ length $ filter (=='#') $ getAround grid (x,y) then '#' else 'L'
                s -> error (show s ++ show (x,y))
            | y <- [0..(height-1)], x <- [0..(width-1)]] width

getP :: Grid -> Pos -> Char
getP grid (x, y) = case grid !? y of
    Nothing -> 'X'
    Just row -> case row !? x of
        Nothing -> 'X'
        Just char -> char

getAround :: Grid -> Pos -> [Char]
getAround grid pos = filter (/=' ') $ map (getDirs grid pos) dirs
    where dirs = [(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1),(1,1)]::[Pos]

updateVecs:: String -> Int -> [Vector Char]
updateVecs [] _ = []
updateVecs updateString width = (fromList $ take width updateString)
    : updateVecs (drop width updateString) width

getDirs :: Grid -> Pos -> Pos -> Char
getDirs grid (x,y) (xd, yd) = case getPos grid (x+xd, y+yd) of
    ' ' -> ' '
    '#' -> '#'
    'L' -> 'L'
    '.' -> getDirs grid (x+xd,y+yd) (xd, yd)
    c -> c

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

tGrid :: Grid
tGrid = toGrid [
    "123..........",
    "8L4L.#.B.H.B.",
    "765.........."
    ]