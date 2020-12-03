import Data.List.Split

data Id = Em | Wa | Bl | Hp | Ba deriving (Show)

type Tile = ((Int,Int), Id)

main = interact $ show . length . makeTup . ((map read) :: [String] -> [Int]) . splitOn ","
--main = interact $ show . last . ((map read) :: [String] -> [Int]) . splitOn ","
makeTup :: [Int] -> [Tile]
makeTup [] = []
makeTup (x:y:id:ls) = let tileId = case id of
                                        0 -> Em
                                        1 -> Wa
                                        2 -> Bl
                                        3 -> Hp
                                        4 -> Ba
                                        otherwise -> Em
    in ((x,y),tileId) : makeTup ls
makeTup (x:xs) = []