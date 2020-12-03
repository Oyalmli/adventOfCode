--{-#LANGUAGE QuasiQuotes,TypeOperators#-};import Text.Scanf;main=interact$show.sum.map(fromEnum.(\(Just(x:+y:+c:+s:+_))->(s!!(x-1)==c)/=(s!!(y-1)==c)).scanf[fmt|%d-%d %c: %s|]).lines
import Data.List.Split;main=interact$show.sum.map(fromEnum.(\(x:y:(c:_):_:s:_)->(s!!(read x-1)==c)/=(s!!(read y-1)==c))).chunksOf 5.splitOneOf":- \n"
