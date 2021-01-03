import Data.Map (fromList, (!), Map)

main :: IO()
main = interact $ show 
    . (\bagMap -> 
        sum 
        $ map 
            (\tup -> search (fst tup) (snd tup) bagMap) 
            (bagMap ! "shinygold")
      )
    . fromList . map (parse . words) . lines
  
search :: Int -> String -> Map String [(Int, String)] -> Int
search n name di
    | name == "empty" = 0
    | otherwise = (+n) $ (*n) $ sum 
        $ map (\tup -> search (fst tup) (snd tup) di) (di ! name)

parse :: [String] -> (String, [(Int, String)])
parse bag = (concat (take 2 bag), splitBags (drop 4 bag))

splitBags :: [String] -> [(Int, String)]
splitBags [] = []
splitBags (x:y:z:_:xs) = (read x, y ++ z):splitBags xs
splitBags _ = [(1, "empty")]
