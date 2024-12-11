import Data.Char (digitToInt)
import Data.List (unfoldr)

data Elem = Num Int | Sentinel deriving (Show, Eq)

main = do
  inp <- getContents
  let nums = map (digitToInt) inp
  --print nums
  let numsAndSpace = make 0 nums
  let f = place numsAndSpace

  print $ sum $ zipWith (*) f [0 ..]

make :: Int -> [Int] -> [Elem]
make _ [] = []
make i (num : []) = replicate num (Num i)
make i (num : space : xs) = replicate num (Num i) ++ replicate space Sentinel ++ make (i + 1) xs

place :: [Elem] -> [Int]
place [] = []
place (Num i : xs) = i : place xs
place (Sentinel : xs) = case lastElem of
  [] -> place rest
  [Num elem] -> elem : place rest
  where
    lastElem = take 1 $ filter (/= Sentinel) $ reverse xs
    rest = removeLast (/= Sentinel) xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast p xs =
  let go c [] = tail (c [])
      go c (x : xs)
        | p x = c (go (x :) xs)
        | otherwise = go (c . (x :)) xs
   in case break p xs of
        (ok, []) -> ok
        (ok, x : xs) -> ok ++ go (x :) xs