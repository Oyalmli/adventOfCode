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
data Packet = OPacket Version Type [Packet] | LPacket Version Type Int | EPacket deriving (Show, Eq)

main :: IO ()
main = interact
    $ show
    . getAns
    . mpb
    . toBits
    . head . words

getAns :: [Packet] -> Int
getAns [LPacket v _ _] = v
getAns [OPacket v _ pcts] = v + (sum $ map (\x-> getAns [x]) pcts)

getV :: Packet -> Int
getV (LPacket v _ _) = v
getV (OPacket v _ _) = v

toBits :: String -> String
toBits = concatMap (M.fromList bs M.!)

parsePacket :: String -> (Packet, String)
parsePacket "" = (EPacket, "")
parsePacket str 
    | length str < 11 = (EPacket, "")
    | otherwise = case toDec t of  
    4 -> (LPacket (toDec v) (toDec t) (toDec lit), rz)
    _ -> (OPacket (toDec v) (toDec t) pcts, "")
    where 
    (v,r0) = splitAt 3 str
    (t,rest) = splitAt 3 r0
    (lit,rz) = pLiteral (rest, [])
    pcts = splitPackets rest
    
pLiteral :: (String, String) -> (String, String)
pLiteral ((o:bs), collect) 
    | o == '0' = (c, rest)
    | o == '1' = let (p, q) = pLiteral (rest, collect) in (c ++ p, q)
    where (c,rest) = splitAt 4 bs


toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

splitPackets :: String -> [Packet]
splitPackets ('0':xs) = mpb r0
    where 
    (l, r0) = splitAt 15 xs 
    (c, r1) = splitAt (toDec l) r0

splitPackets ('1':xs) = mpb r0
    where 
    (l, r0) = splitAt 11 xs 
    (c, r1) = splitAt (toDec l) r0

splitPackets _ = []

mpb :: String -> [Packet]
mpb = unfoldr (\x -> case parsePacket x of
    (EPacket,_) -> Nothing
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

