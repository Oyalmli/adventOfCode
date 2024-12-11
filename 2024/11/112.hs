{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 qualified as C
import Data.Foldable (foldlM)
import Data.Map qualified as M

main :: IO ()
main = do
  inp <- C.getContents
  let config = C.words inp
  let results =
        foldl'
          ( \(cnt, mp) curr -> let (x, mp') = step mp 75 curr in (cnt + x, mp')
          )
          (0, M.empty)
          config
  print $ fst $ results

step :: M.Map (C.ByteString, Integer) Integer -> Integer -> C.ByteString -> (Integer, M.Map (C.ByteString, Integer) Integer)
step mp 0 num = (1, mp)
step mp i "0" = do
  let (a, mp') = step mp (i - 1) "1"
  (a, M.insert ("0", i) a mp')
step mp i num = case M.lookup (num, i) mp of
  Just n -> do
    (n, mp)
  Nothing -> do
    case even $ len of
      True -> do
        let (a, mp') = step mp (i - 1) (front)
        let (b, mp'') = step mp' (i - 1) (backElem)
        (a + b, M.insert (num, i) (a + b) mp'')
      False -> do
        let (a, mp') = step mp (i - 1) (C.pack $ show $ n * 2024)
        (a, M.insert (num, i) a mp')
  where
    Just (n, _) = C.readInt num
    len = C.length num
    (front, back) = C.splitAt (len `div` 2) num
    backElem = C.snoc (C.dropWhile (== '0') $ C.init back) (C.last back)
