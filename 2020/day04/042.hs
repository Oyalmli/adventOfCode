import Data.List.Split ( splitOn )
import Data.Char ( isDigit, isHexDigit )
import Data.Map ( fromList,(!) )

main :: IO ()
main=interact$show.sum
  .map(fromEnum.(==7).sum
    .map(\(x:y:_:_:v)->fromEnum$(fromList[
      ("by",\x->x>"1919"&&x<"2003"),
      ("iy",\x->x>"2009"&&x<"2021"),
      ("ey",\x->x>"2019"&&x<"2031"),
      ("hg",\x->if 'c'`elem`x then x>"149cm"&&x<"194cm" else x>"58in"&&x<"77in"),
      ("hc",\(x:s)->all isHexDigit s&&x=='#'&&length s==6),
      ("ec",flip elem["amb","blu","brn","gry","grn","hzl","oth"]),
      ("pi",\x->all isDigit x&&length x==9)]![x,y])v)
    .filter((/='c').head).words)
  .splitOn"\n\n"
