module Main where
import qualified Grid as G
import qualified Data.ByteString as IO
import qualified Data.ByteString.Char8 as C
import Data.Maybe ( fromJust )


main :: IO ()
main = do
  inp <- IO.getContents
  let (dim, laserCol, sensorRow, grid) = parseInp $ C.words inp
  let (_, visited) = placeSensorPath grid (G.replicate dim dim False) (sensorRow - 1) (dim - 1)  (0,-1)
  C.putStrLn $ findCollision grid visited 0 (laserCol - 1) (1, 0)
--main :: IO ()
--main = IO.interact 
--  $ (\(laserCol, (grid, visited)) -> 
--      findCollision grid visited 0 (laserCol - 1) (1, 0))
--  . (\(dim, laserCol, sensorRow, grid) -> 
--      (laserCol, placeSensorPath grid (G.replicate dim dim False) (sensorRow - 1) (dim - 1)  (0,-1)))
--  . parseInp
--  . C.words

findCollision :: G.Grid Char -> G.Grid Bool -> Int -> Int -> (Int, Int) -> C.ByteString
findCollision grid visited ly lx (ld0, ld1)
  | not (G.inGrid grid ly lx) = C.pack "NO"
  | (G..!) visited ly lx && (G..!) grid ly lx == '.' = C.pack "YES"
  | (G..!) grid ly lx `elem` ['\\', '/'] = findCollision grid visited (ly + nld0) (lx + nld1) (nld0, nld1)
  | otherwise = findCollision grid visited (ly + ld0) (lx + ld1) (ld0, ld1)
  where (nld0, nld1) = getNewDir (ld0,ld1) ((G..!) grid ly lx)

placeSensorPath :: G.Grid Char -> G.Grid Bool -> Int -> Int -> (Int, Int) -> (G.Grid Char, G.Grid Bool)
placeSensorPath grid visited sy sx (sd0, sd1)
  | not (G.inGrid grid sy sx) = (grid, visited) 
  | (G..!) grid sy sx `elem` ['\\', '/'] = placeSensorPath grid ((G..>) visited sy sx True) (sy + nsd0) (sx + nsd1) (nsd0, nsd1)
  | otherwise = placeSensorPath grid ((G..>) visited sy sx True) (sy + sd0) (sx + sd1) (sd0, sd1)
  where (nsd0, nsd1) = getNewDir (sd0,sd1) ((G..!) grid sy sx)

getNewDir :: (Num a, Num b) => (b, a) -> Char -> (a, b)
getNewDir (oDirY, oDirX) mirror
  | mirror == '/' = (oDirX*(-1), oDirY*(-1)) 
  | otherwise = (oDirX, oDirY)

parseInp :: [C.ByteString] -> (Int, Int, Int, G.Grid Char)
parseInp (sdim:sLaserCol:sSensorRow:cells) = 
    (getInt sdim, getInt sLaserCol, getInt sSensorRow, G.fromList (getInt sdim) (map C.head cells))
parseInp _ = error "oopies"

getInt :: C.ByteString -> Int
getInt = fst . fromJust . C.readInt