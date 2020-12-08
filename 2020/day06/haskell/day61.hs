import Data.List.Split
import Data.List(nub)
main = interact $ show.sum.map(length.nub.filter(/='\n')).splitOn "\n\n"