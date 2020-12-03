import Data.List.Split

data Dir = U | D | L | R deriving (Show, Eq)
type Inst = (Dir, Pos, Int);
type Pos = (Int, Int)

main = interact $ show . mkPoir . filter (fCol) . robot (U, (0,0), 10) . mkPair . splitOn ","
--main = interact $ show . length . splitOn ","

mkPair :: [String] -> [(Int,Int)]
mkPair [] = []
mkPair [x] = []
mkPair (col:rot:xs) = ((read col), (read rot)) : (mkPair xs)

robot :: Inst ->[(Int,Int)] -> [Inst]
robot _ [] = []
robot (dir, pos, _) ((col,rot):xs) = let nRob = (nDir dir rot, nPos dir pos, col) in nRob : robot nRob xs
    
nDir :: Dir -> Int -> Dir
nDir d 0 = case d of
    U -> L
    L -> D
    D -> R
    R -> U
nDir d 1 = case d of
    U -> R
    R -> D
    D -> L
    L -> U
nDir d _ = d

nPos :: Dir -> Pos -> Pos
nPos d (x,y) = case d of
    U -> (x,y+1)
    D -> (x,y-1)
    L -> (x-1,y)
    R -> (x+1,y)

fCol :: Inst -> Bool
fCol (_,_,col) = case col of
    0 -> True
    1 -> True
    otherwise -> False

oneOfPos :: []