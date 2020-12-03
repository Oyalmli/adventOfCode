main = interact $ show . map (map parse). lines

data Cell = Bug | Emp deriving (Show, Eq)

parse :: Char -> Cell
parse '#' = Bug
parse '.' = Emp
