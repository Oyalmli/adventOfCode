import Data.Char
import Data.Map (fromList, toList)
import Data.List (foldl', splitAt)
import Data.Bits ((.|.), (.&.), shiftL, testBit)
import Control.Monad (foldM_, forM_, foldM)


main = interact $ show . sum . map tupSum
    . toList . fromList
    . concat . solve 
    . map (parse . words) . lines


tupSum (idx, snum) = read snum

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parse (s:_:num:[])
    | s == "mask" = ("mask", num)
    | otherwise = (parseMem s, num)

parseMem ('m':'e':'m':'[':xs) = takeWhile isDigit xs

applyMaskV2 :: String -> Integer -> [Integer]
applyMaskV2 mask val = do
  let f :: Integer -> (Char, Bool) -> [Integer]
      f acc ('0', v) = if v then [shiftL acc 1 .|. 1] else [shiftL acc 1]
      f acc ('1', _) = [shiftL acc 1 .|. 1]
      f acc ('X', _) = [shiftL acc 1, shiftL acc 1 .|. 1]
  foldM f 0 . zip mask . map (testBit val) $ [35,34..0]

adresses mask [] = []
adresses mask ((adr, num):xs) = zip (applyMaskV2 mask (read adr)) (repeat num) : adresses mask xs

solve :: [(String, String)] -> [[(Integer, String)]]
solve [] = []
solve (mask:list) = concat (adresses (snd mask) nums) : solve nextBlock
    where 
        nums = takeWhile ((/="mask") . fst) list
        nextBlock = dropWhile ((/="mask") . fst) list

parseMask :: String -> [String]
parseMask = fst . foldr determineMaskAtBit ([id], 0) . drop 2 . dropWhile (/='=')
  where
    determineMaskAtBit 'X' (mask,idx) = ([(flip setBit idx .), (flip clearBit idx .)] <*> mask, idx+1)
    determineMaskAtBit '0' (mask,idx) = (mask, idx+1)
    determineMaskAtBit '1' (mask,idx) = ((flip setBit idx .) <$> mask, idx+1)