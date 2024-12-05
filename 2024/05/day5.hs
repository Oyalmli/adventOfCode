--- Day 5: Print Queue ---
import Data.List (sortBy)
import Data.List.Split (chunksOf, splitOn, splitOneOf)
import Data.Map (Map, fromList, (!))

main :: IO ()
main = do
  inp <- getContents
  let (pageLines:bookLines:_) = splitOn [""] (splitOneOf "|\n" inp)
      orderMap    = makeOrderMap (map read pageLines)
      parsedBooks = map (map read . splitOn ",") bookLines
      ordered     = orderBooks (orderMap, parsedBooks)
  putStrLn $ "Part 1: " ++ sumMiddleValue (filter (uncurry (==)) ordered)
  putStrLn $ "Part 2: " ++ sumMiddleValue (filter (uncurry (/=)) ordered)

makeOrderMap :: [Int] -> Map (Int, Int) Ordering
makeOrderMap ps =
  fromList $
    concatMap (\[a,b] -> [((a,b), LT), ((b,a), GT)]) (chunksOf 2 ps)

orderBooks :: (Ord b, Show b) => (Map (b,b) Ordering, [[b]]) -> [([b],[b])]
orderBooks (pageMap, bs) = [(sortBy (\a b -> pageMap ! (a,b)) bk, bk) | bk <- bs]

middleElement :: [a] -> a
middleElement zs = go zs zs
  where
    go (_:_:xs) (_:ys) = go xs ys
    go _ (y:_)         = y

sumMiddleValue :: [([Int], b)] -> String
sumMiddleValue = show . sum . map (middleElement . fst)