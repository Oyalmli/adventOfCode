--- Day 11: Plutonian Pebbles ---
import Data.ByteString.Char8 qualified as C
import Data.MemoTrie (memoFix)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  config <- map read' . C.words <$> C.getContents
  putStrLn $ "Part 1: " ++ (show $ solve 25 config)
  putStrLn $ "Part 2: " ++ (show $ solve 75 config)
  where solve n = foldl' (\acc curr -> acc + (stepMemo (n, curr))) 0

type Memo f = f -> f
step :: Memo ((Int, Int) -> Int)
step step (0, _) = 1
step step (i, n)
  | n == 0    = step (i', 1)
  | even len  = step (i', (read' front)) + step (i', (read' back))
  | otherwise = step (i', (n * 2024))
  where i' = i - 1
        str = C.pack $ show n
        len = C.length $ str
        (front, back) = C.splitAt (len `div` 2) str

stepMemo :: (Int, Int) -> Int
stepMemo = memoFix step

read' :: C.ByteString -> Int
read' = fst . fromJust . C.readInt
