import Data.List.Split;import Data.Char;import Data.Map(fromList,(!));
--main=interact$show.sum.map((\x->fromEnum(all(==True)x&&length x==8)).map(\(k,v)->(fromList[("by",(\x->(read x)>=1920&&(read x)<=2002)),("iy",(\x->(read x)>=2010&&(read x)<=2020)),("ey",(\x->(read x)>=2020&&(read x)<=2030)),("hg",(\x->let h=read(head(splitOneOf"ic"x))in if any(=='c')x then h>=150&&h<=193 else h>=59&&h<=76)),("hc",(\(x:s)->all(==True)(map(\c->(c>='0'&&c<='9')||(c>='a'&&c<='f'))s)&&(x=='#'&&length s==6))),("ec",(\x->elem x["amb","blu","brn","gry","grn","hzl","oth"])),("pi",(\x->(all(==True)(map(isDigit)x))&&length x==9)),("ci",(\x->True))]!k)v).(\x->if any((=="ci").fst) x then x else("ci",""):x)).map(map(\x->(take 2 x,drop 4 x)).words).splitOn"\n\n"
--main=interact$show.sum.map(fromEnum.(==7).sum.map(\(x:y:_:_:v)->fromEnum$(fromList[("by",\x->x>"1919"&&x<"2003"),("iy",\x->x>"2009"&&x<"2021"),("ey",\x->x>"2019"&&x<"2031"),("hg",\x->if any(=='c')x then x>"149cm"&&x<"194cm" else x>"58in"&&x<"77in"),("hc",\(x:s)->all id(map(isHexDigit)s)&&x=='#'&&length s==6),("ec",flip elem["amb","blu","brn","gry","grn","hzl","oth"]),("pi",\x->all id(map(isDigit)x)&&length x==9)]![x,y])v).filter((/='c').head).words).splitOn"\n\n"

main=interact$show.sum.map(fromEnum.(==7).sum.map(\(x:y:_:_:v)->fromEnum$(fromList[("by",\x->x>"1919"&&x<"2003"),("iy",\x->x>"2009"&&x<"2021"),("ey",\x->x>"2019"&&x<"2031"),("hg",\x->if any(=='c')x then x>"149cm"&&x<"194cm" else x>"58in"&&x<"77in"),("hc",\(x:s)->all id(map(isHexDigit)s)&&x=='#'&&length s==6),("ec",flip elem["amb","blu","brn","gry","grn","hzl","oth"]),("pi",\x->all id(map(isDigit)x)&&length x==9)]![x,y])v).filter((/='c').head).words).splitOn"\n\n"

{-
main=interact$show.sum
    .map(fromEnum.(==7).sum
        .map(\(x:y:_:_:v)->fromEnum$(fromList[
            ("by",\x->x>"1919"&&x<"2003"),
            ("iy",\x->x>"2009"&&x<"2021"),
            ("ey",\x->x>"2019"&&x<"2031"),
            ("hg",\x->if any(=='c')x then x>"149cm"&&x<"194cm" else x>"58in"&&x<"77in"),
            ("hc",\(x:s)->all id(map(isHexDigit)s)&&x=='#'&&length s==6),
            ("ec",flip elem["amb","blu","brn","gry","grn","hzl","oth"]),
            ("pi",\x->all id(map(isDigit)x)&&length x==9)]![x,y])v)
        .filter((/='c').head).words)
    .splitOn"\n\n"
-}



