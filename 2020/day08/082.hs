import Data.Sequence (fromList, update, index, Seq)
main = interact $ show 
    . maximum
    . (\opss -> map (\ops -> runProgram 0 0 ops) opss)
    . (\seq -> map (\x -> genOps seq x)  [0..(length seq-1)]) 
    . fromList . zip (repeat False) . map parse . lines

runProgram :: Int -> Int -> Seq (Bool, (String, Int)) -> Int
runProgram acc n list
    | n >= length list = acc
    | n < 0 = 0 
    | visited = 0
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

genOps :: Seq (Bool, (String, Int)) -> Int -> Seq (Bool, (String, Int))
genOps list n =
    case op_name of
        "acc" -> list
        "nop" -> update n (visited, ("jmp", num)) list
        "jmp" -> update n (visited, ("nop", num)) list
    where (visited,(op_name, num)) = list `index` n