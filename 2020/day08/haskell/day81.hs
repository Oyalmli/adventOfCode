import Data.Sequence (fromList, update, index, Seq)
main = interact $ show 
    . runProgram 0 0 
    . fromList . zip (repeat False) . map parse . lines

runProgram :: Int -> Int -> Seq (Bool, (String, Int)) -> Int
runProgram acc n list = let (visited,(op_name, num)) = list `index` n in
    if visited then acc 
    else let newList = update n (True, (op_name, num)) list in 
        case op_name of 
            "nop" -> runProgram acc (n+1) newList 
            "acc" -> runProgram (acc+num) (n+1) newList
            "jmp" -> runProgram acc (n+num) newList 

parse :: String -> (String, Int)
parse = (\(op:snum:xs) -> (op, parseNum snum)) . words

parseNum :: String -> Int
parseNum ('+':num) = read num
parseNum ('-':num) = negate $ read num