import Data.Vector (slice, fromList, empty, (!), Vector)

offset :: Int
offset = 25

main :: IO()
main = interact $ show
    . (\vec -> vec ! (makeSlices offset vec))
    . fromList
    . map (read::String->Int) . lines

makeSlices :: Int -> Vector Int -> Int
makeSlices n list 
    | n > length list = 0
    | isSS set (length set) (list ! n) = makeSlices (n + 1) list 
    | otherwise = n
    where set = slice (n-offset) offset list

isSS :: Vector Int -> Int -> Int -> Bool
isSS set n sum
    | sum == 0              = True
    | n == 0                = False
    | set ! (n - 1) > sum   = isSS set (n - 1) sum
    | otherwise             = isSS set (n-1) sum || isSS set (n-1) (sum-(set ! (n-1)))
