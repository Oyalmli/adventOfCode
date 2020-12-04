import Data.List.Split
import Data.Char
main = interact $ show . sum . map(fromEnum . (all (==True))). map validate . map solve . map ((\x -> (length x, x)) .words) . splitOn "\n\n"

solve (n,list) 
    | n < 7 = (0, list)
    | n == 8 = (1, list)
    | n == 7 = (findCid list, list)

findCid [] = 1
findCid (x:str) = if (take 3 x) /= "cid" then findCid str else 0 


validate (n, []) = []
validate (0,_) = [False]
validate (n, (('b':'y':'r':':':syear):xs)) = let year = read syear in (year >= 1920 && year <= 2002):(validate (n, xs))
validate (n, (('i':'y':'r':':':syear):xs)) = let year = read syear in (year >= 2010 && year <= 2020):(validate (n, xs))
validate (n, (('e':'y':'r':':':syear):xs)) = let year = read syear in (year >= 2020 && year <= 2030):(validate (n, xs))
validate (n, (('h':'g':'t':':':sheight):xs)) = let info = splitOneOf "ic" sheight in let height = read (head info) in if last info == "m" then (height >= 150 && height <= 193):(validate (n, xs)) else (height >= 59 && height <= 76):(validate (n, xs))
validate (n, (('h':'c':'l':':':'#':scolor):xs)) = if length scolor /= 6 then False:(validate (n, xs)) else (all (==True) (map (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) scolor)):(validate (n, xs))
validate (n, (('h':'c':'l':':':_:scolor):xs)) = False:(validate (n, xs))
validate (n, (('e':'c':'l':':':scolor):xs)) = if length scolor /= 3 then False:(validate (n, xs)) else (scolor `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]):(validate (n, xs))
validate (n, (('p':'i':'d':':':pid):xs)) = if length pid /= 9 then False:(validate (n, xs)) else (all (==True) (map (isDigit) pid)):(validate (n, xs))
validate (n, (('c':'i':'d':':':cid):xs)) = True:(validate (n, xs))
validate (n, (_:xs)) = False:(validate (n, xs))
