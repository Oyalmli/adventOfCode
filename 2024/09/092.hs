import Data.Char (digitToInt)
import Data.Foldable qualified as F
import Data.List (unfoldr)
import Data.Vector.Unboxed qualified as V

main :: IO ()
main = do
  inp <- getContents
  let nums = map (digitToInt) inp
  let numsAndSpace = V.fromList $ make 0 nums
  let res = place (V.length numsAndSpace - 1) $ numsAndSpace
  print $ sum $ zipWith (*) (map toValue $ V.toList res) [0 ..]

toValue :: Int -> Int
toValue (-1) = 0
toValue i = i

ppSeq :: V.Vector Int -> String
ppSeq = concatMap pp . V.toList

pp :: Int -> String
pp i = show i

make :: Int -> [Int] -> [Int]
make _ [] = []
make i (num : []) = replicate num (i)
make i (num : space : xs) = replicate num (i) ++ replicate space (-1) ++ make (i + 1) xs

place :: Int -> V.Vector Int -> V.Vector Int
place 0 tape = tape
place i tape
  | i < 0 = tape
  | otherwise = case tape V.! i of
      -1 -> place (i - 1) tape
      e ->
        case findSpace tape len i of
          Nothing -> place (i - len) tape
          Just idx ->
            place (i - len) $
              prefix V.++ elems V.++ V.drop len suffix V.++ V.replicate len (-1) V.++ right
            where
              (prefix, suffix) = V.splitAt idx restLeft
        where
          (left, right) = V.splitAt (i + 1) tape
          (elems, restLeft) = V.spanR (== e) left
          len = V.length elems

findSpace :: V.Vector Int -> Int -> Int -> Maybe Int
findSpace tape len idx =
  let indices = [0 .. idx - len]
   in case filter (\i -> V.all (== (-1)) (V.take len (V.drop i tape))) indices of
        [] -> Nothing
        (x : _) -> Just x