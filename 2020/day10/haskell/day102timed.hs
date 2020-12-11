import Data.List
import System.IO
import Text.Printf
import System.CPUTime

main :: IO()
main = do 
    start <- getCPUTime
    contents <- readFile "../megaInput.txt"
    print . mainFunc $ contents
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^6)
    printf "Computation time: %0.0fμs\n" (diff :: Double)

mainFunc :: String -> Integer
mainFunc = (\x -> solve 1 (-1) x) 
    . diff . addEnds . sort 
    . map (read::String -> Integer) . lines

addEnds :: [Integer] -> [Integer]
addEnds list = 0:list++[(last list + 3)]

diff :: [Integer] -> [Integer]
diff list = zipWith (-) (tail list) list

solve :: Integer -> Integer -> [Integer] -> Integer
solve total count [] = total
solve total count (n:xs)
    | n == 1 = solve total (count+1) xs
    | count > 0 = solve (total * (1+ (count * (count +1)) `div` 2)) (-1) xs
    | otherwise = solve total (-1) xs
    