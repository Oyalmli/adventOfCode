import Data.Sequence (fromList, update, index, Seq)
main = interact $ show 
  . runProgram 0 0
  . fromList . zip (repeat False) . map parse . lines

runProgram :: Int -> Int -> Seq (Bool, (String, Int)) -> Int
runProgram acc n list
  | visited = acc
  | otherwise =
    case op_name of
      "nop" -> runProgram acc (n+1) newList
      "acc" -> runProgram (acc+num) (n+1) newList
      "jmp" -> runProgram acc (n+num) newList
  where
    (visited,(op_name, num)) = list `index` n
    newList = update n (True, (op_name, num)) list

parse :: String -> (String, Int)
parse = (\(op:snum:xs) -> (op, parseNum snum)) . words

parseNum :: String -> Int
parseNum ('+':num) = read num
parseNum ('-':num) = negate $ read num