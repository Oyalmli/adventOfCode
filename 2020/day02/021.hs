import Data.List.Split;
main = interact $ show . sum . map (fromEnum . (\(x:y:(c:_):_:s:_) -> read x <= read y = y <= z)) .chunksOf 5 . splitOneOf":- \n"
