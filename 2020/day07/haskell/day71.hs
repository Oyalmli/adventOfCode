import qualified Data.Map as DM (fromList, (!), Map, map, keys)

main :: IO()
main = interact $ show . pred . sum . map fromEnum
    . (\bagMap -> 
        map 
            (\name -> search name bagMap) 
            (DM.keys bagMap)
      )
    . DM.fromList . map (parse . words) . lines

search :: String -> DM.Map String [(Int, String)] -> Bool
search name di
    |name == "shinygold" = True
    |name == "empty" = False
    |otherwise = or 
        $ map 
            (\tup -> search (snd tup) di) 
            (di DM.! name)

parse :: [String] -> (String, [(Int, String)])
parse bag = (concat (take 2 bag), splitBags (drop 4 bag))

splitBags :: [String] -> [(Int, String)]
splitBags [] = []
splitBags (x:y:z:_:xs) = (read x, y ++ z):splitBags xs
splitBags _ = [(1, "empty")]
