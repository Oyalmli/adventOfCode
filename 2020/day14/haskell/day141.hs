import Data.Char
import Data.Map (fromList, toList)
import Data.List (foldl')
main = interact $ show . map tupSum . toList .  fromList . solve . map (parse . words) . lines

tupSum (idx, snum) = toDec snum

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

solve [] = []
solve (mask:list) = map (applyMask mask) nums ++ solve nextNums
    where 
        nums = takeWhile ((/="mask") . fst) list
        nextNums = dropWhile ((/="mask") . fst) list

applyMask ("mask", mask) (idx, num) = (idx, zipWith (\m n -> if m /= 'X' then m else n) mask num) 

parse (s:_:num:[])
    | s == "mask" = ("mask", num)
    | otherwise = (parseMem s, pad $ concatMap show $ fromDecimal $ read num)

parseMem ('m':'e':'m':'[':xs) = takeWhile isDigit xs

fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = go n []
    where go 0 r = r
          go k rs = go (div k 2) (mod k 2:rs)

pad str = replicate (36 - length str) '0' ++ str