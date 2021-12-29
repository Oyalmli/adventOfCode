{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import qualified Data.Map as M
import Data.List ( foldl', unfoldr ) 
import Data.Char ( digitToInt )
import GHC (NamedThing(getName))

type Type = Int
type Version = Int

-- OPacket : Operator Packet, LPacket : Literal Packet
data Packet = O Version Type [Packet] | L Version Type Int | E deriving (Show, Eq)

main :: IO ()
main = interact
    $ show
--     mkEz
--    . getAns
    . parsePacket
    . toBits
    . head . words

mkEz :: [Packet] -> String
mkEz [L _ _ v] = " " ++ show v
mkEz [O _ 0 pcts] = " +" ++ concatMap (\x-> mkEz [x]) pcts
mkEz [O _ 1 pcts] =" *" ++ concatMap (\x-> mkEz [x]) pcts
mkEz [O _ 2 pcts] = " min:" ++ concatMap (\x-> mkEz [x]) pcts
mkEz [O _ 3 pcts] = " max:" ++ concatMap (\x-> mkEz [x]) pcts
mkEz [O _ 5 (p1:p2)] = " > " ++ mkEz[p1] ++ concatMap (\x-> mkEz [x]) p2
mkEz [O _ 6 (p1:p2)] = " < " ++ mkEz [p1] ++ concatMap (\x-> mkEz [x]) p2
mkEz [O _ 7 (p1:p2)] = " == " ++ mkEz [p1] ++ " ==" ++ concatMap (\x-> mkEz [x]) p2


getAns :: [Packet] -> Int
getAns [L _ _ v] = v
getAns [O _ 0 pcts] = sum $ map (\x-> getAns [x]) pcts
getAns [O _ 1 pcts] = product $ map (\x-> getAns [x]) pcts
getAns [O _ 2 pcts] = minimum $ map (\x-> getAns [x]) pcts
getAns [O _ 3 pcts] = maximum $ map (\x-> getAns [x]) pcts
getAns [O _ 5 (p1:p2)] = fromEnum (getAns [p1] > getAns p2)
getAns [O _ 6 (p1:p2)] = fromEnum (getAns [p1] < getAns p2)
getAns [O _ 7 (p1:p2)] = fromEnum (getAns [p1] == getAns p2)


toBits :: String -> String
toBits = concatMap (M.fromList bs M.!)


pPackets :: String -> [Packet]
pPackets str 
    | all (=='0') str = []
    | otherwise = p : pPackets rest
    where (p, rest) = parsePacket str

parsePacket :: String -> (Packet, String)
parsePacket "" = (E, "")
parsePacket str 
    | length str < 11 = (E, "")
    | otherwise = case toDec t of  
    4 -> (L (toDec v) (toDec t) (toDec lit), rz)
    _ -> if null pp then (E, "") else (O (toDec v) (toDec t) pp, roz)
    where 
    (v,r0) = splitAt 3 str
    (t,rest) = splitAt 3 r0
    (lit,rz) = pLiteral "" rest
    (pp, roz) = splitPackets rest
    
pLiteral :: String -> String -> (String, String)
pLiteral acc str
    | head str == '0' = (acc++(take 4 $ drop 1 str), drop 5 str)
    | head str == '1' = pLiteral (acc++(take 4 $ drop 1 str)) (drop 5 str)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

splitPackets :: String -> ([Packet], String)
splitPackets ('0':xs) = (mpb r0, r1)
    where 
    (l, r0) = splitAt 15 xs 
    (c, r1) = splitAt (toDec l) r0

splitPackets ('1':xs) = (mpb r0, r1)
    where 
    (l, r0) = splitAt 11 xs 
    (c, r1) = splitAt (toDec l) r0

mpb :: String -> [Packet]
mpb = unfoldr (\x -> case parsePacket x of
    (E,_) -> Nothing
    (p, rest) -> Just (p, rest)
    ) 

hTb :: M.Map Char String 
hTb = M.fromList bs

bs :: [(Char,String)]
bs = [
    ('0',"0000"),
    ('1',"0001"),
    ('2',"0010"),
    ('3',"0011"),
    ('4',"0100"),
    ('5',"0101"),
    ('6',"0110"),
    ('7',"0111"),
    ('8',"1000"),
    ('9',"1001"),
    ('A',"1010"),
    ('B',"1011"),
    ('C',"1100"),
    ('D',"1101"),
    ('E',"1110"),
    ('F',"1111")
    ]

