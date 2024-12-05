import Data.Char qualified as Char
import Data.List (isPrefixOf, sortBy, tails)
import Data.Map qualified as M

main :: IO ()
main = interact $ show . sum . map findListL . map fst . filter (\(a, b) -> a /= b) . inOrder . getInp . lines

inOrder :: (Show b, Ord b) => (M.Map (b, b) Ordering, [[b]]) -> [([b], [b])]
inOrder (pageMap, books) =
  map
    ( \book -> (sortBy (\a b -> let inMap = M.lookup (a, b) pageMap in case inMap of Just x -> x; Nothing -> error (show (a, b) ++ "not in map")) book, book)
    )
    books

getInp :: [String] -> (M.Map (Int, Int) Ordering, [[Int]])
getInp ls = (M.union pageMap inversePagemap, map (map read . splitOn ',') books)
  where
    (pageOrders, books) = readPages [] ls
    pageMap = M.fromList $ zip pageOrders (repeat LT)
    inversePagemap = M.fromList $ zip (map (\(a, b) -> (b, a)) pageOrders) (repeat GT)

readPages :: [(Int, Int)] -> [String] -> ([(Int, Int)], [String])
readPages acc ("" : xs) = (acc, xs)
readPages acc (order : xs) = readPages ((a, b) : acc) xs
  where
    [a, b] = (map (read) $ splitOn '|' order)

splitOn :: Char -> String -> [String]
splitOn p s = case dropWhile (== p) s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break (== p) s'

findListL :: [a] -> a
findListL zs = go zs zs
  where
    go (_ : _ : xs) ~(_ : ys) = go xs ys
    go _ (y : _) = y