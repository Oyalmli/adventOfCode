import Data.List.Split
import Data.List(nub)
main = interact $ show
    .map(words).splitOn "\n\n"