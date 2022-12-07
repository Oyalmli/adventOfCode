import Data.List
main :: IO ()
main = interact
    $ show
    . map (filter (/=' '))
    . map nub
    . map (\(x,y) ->
        zipWith (\a b -> if b then a else ' ') x 
        $ map (flip elem y) x)
    . map sh . lines

sh str = splitAt h str
    where h = (length str) `div` 2
