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
data Packet = O Version Type [Packet] | L Version Type Int | E String deriving (Show, Eq)

main :: IO ()
main = interact
    $ show
    . splitPackets
    . toBits
    . head . words

splitPackets :: String -> [Packet]
splitPackets [] = []
splitPackets str
    | all (=='0') str = []
    | typ == 4 = L ver typ (toDec lit) : splitPackets nxt
    | otherwise = O ver typ (E curr : splitPackets next) : splitPackets next
    where 
    (v, r0) = splitAt 3 str
    (t, r1) = splitAt 3 r0
    ver = toDec v
    typ = toDec t
    (lit, nxt) = pLiteral "" r1
    (curr, next) = makeChunk r1

pLiteral :: String -> String -> (String, String)
pLiteral acc str
    | head str == '0' = (acc ++ take 4 (drop 1 str), drop 5 str)
    | head str == '1' = pLiteral (acc ++ take 4 (drop 1 str)) (drop 5 str)

--makeChunk :: String -> (String, String)
makeChunk [] = ([], [])
makeChunk (i:xs) 
    | i == '0' = let numP = nsp 15 in takeWhile (< n) splitPackets 
    | i == '1' = let numP = nsp 11 in takeWhile (< n) splitPackets 
    where nsp n = toDec (take n xs)

toBits :: String -> String
toBits = concatMap (M.fromList bs M.!)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

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

