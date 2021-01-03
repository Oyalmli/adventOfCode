import Data.List.Split (splitOn)
import Data.Char

type Range = (Int,Int)

type Rule = (Range, Range)
type Rules = [Rule]
type Ticket = [Int]
type Tickets = [Ticket]
type Model = (Rules, Ticket, Tickets)

inRange :: Int -> Range -> Bool
inRange num (low, high) = 
    num >= low && num <= high

val :: Rule -> Int -> Bool
val (r1, r2) num = inRange num r1 || inRange num r2

vals :: Rule -> [Int] -> [Bool]
vals rule = map (val rule)

main = interact $ show
    . parseTickets
    . map lines
    . splitOn "\n\n"

parseRules :: String -> Rule
parseRules ruleLine = dropRules $ splitRules ruleLine

splitRules :: String -> [String]
splitRules rules = words $ last $ splitOn ":" rules

dropRules :: [String] -> (Range, Range)
dropRules (a:_:b:[]) = (parseRange a, parseRange b)
dropRules _ = ((101,101),(101,101))

parseRange :: String -> Range
parseRange str = (\(a:b:xs) -> (read a, read b)) $ splitOn "-" str

parseTickets :: [[String]] -> Model
parseTickets (rules:yourTicket:nearbyTickets:xs) =
    ( map parseRules rules
    , parseMyTicket yourTicket
    , parseNearbyTickets nearbyTickets
    )

parseMyTicket :: [String] -> Ticket
parseMyTicket (_:snums:xs) = map read $ splitOn "," snums

parseTicket :: String -> Ticket
parseTicket snums = map read $ splitOn "," snums

parseNearbyTickets :: [String] -> [Ticket]
parseNearbyTickets (_:xs) = map parseTicket xs