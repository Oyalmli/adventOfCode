import Data.List
import Data.Map (fromList, (!), (!?))
main = interact $ show
    . length
    . filter (odd . length)
    . group
    . sort
    . map (foldl1 (\(a,b) (p,q) -> (a+p,b+q)) . tok)
    . lines

tok [] = []
tok ls = case valid !? (take 1 ls) of
    Just dir -> dir : tok (drop 1 ls)  
    Nothing -> case valid !? (take 2 ls) of
        Just dir -> dir : tok (drop 2 ls)
        Nothing -> error "INVALID INPUT"
    where valid = fromList [("e",( 2, 0)),("w", (-2, 0)),("se",( 1,-1)),("sw",(-1,-1)),("ne",( 1, 1)),("nw",(-1, 1))]
