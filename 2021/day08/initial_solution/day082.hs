{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.List.Split ( splitOneOf )
import qualified Data.Map as M
import Data.List ( sort, (\\) )
import Data.Maybe ( isNothing, isJust, fromJust )
import Data.Tuple (swap)
import Data.Char (intToDigit)


main :: IO ()
main = do
    inp <- getContents
    let parsed = ((parse . splitOneOf "|\n") inp)
    mapM_ bain parsed

bain :: ([String], [[Char]]) -> IO()
bain parsed = do
    let unique = (map isUnique . fst) parsed
    let foundMap1 = M.fromList $ map (\(x,y) -> (fromJust x , y)) $ filter (\(nm, _) -> isJust nm ) unique
    let notFound1 = map snd $ filter (\(nm, _) -> isNothing nm ) unique
    let foundMap2 = foldl (\mp (k,v) -> M.insert k v mp) foundMap1 $ map (\(x,y) -> (fromJust x , y)) $ filter (\(nm, _) -> isJust nm ) $ map (minus7 (foundMap1 M.! 7)) notFound1 
    let notFound2 = filter (`notElem` ( map snd $ M.toList foundMap2 )) notFound1
    --print notFound2
    --print foundMap2
    let foundMap3 = foldl (\mp (k,v) -> M.insert k v mp) foundMap2 $ map (\(x,y) -> (fromJust x , y)) $ filter (\(nm, _) -> isJust nm ) $ find0 (foundMap2 M.! 4) notFound2
    --print foundMap3
    let notFound3 = filter (`notElem` ( map snd $ M.toList foundMap3 )) notFound2
    --print notFound3

    let foundMap4 = foldl (\mp (k,v) -> M.insert k v mp) foundMap3 $ map lastStep notFound3
    --print foundMap4

    let swappedMap = M.fromList $ map (\(a,b) -> (sort b, intToDigit a)) $ M.toList foundMap4
    --print $swappedMap
    print $ (read::String -> Int) $ map (\k -> swappedMap M.! (sort k)) $ snd parsed
    
    

lastStep str = case length str of
    5 -> (5, str)
    6 -> (0, str)
    _ -> error "OOF"

find0 four nf =  map (\n -> case length $ four \\ n of
    0 -> (Just 9, n)
    2 -> (Just 2, n)
    _ -> (Nothing, n)
    ) nf

parse :: [String] -> [([String],[String])]
parse [] = []
parse (y:x:xs) = (words y,words x):parse xs


minus7 :: String -> String -> (Maybe Int, String)
minus7 seven other = case length $ other \\ seven of
    4 -> (Just 6, other)
    2 -> (Just 3, other)
    _ -> (Nothing, other)

isUnique :: String -> (Maybe Int, String)
isUnique str = 
        case length str of
            2 -> (Just 1, str)
            3 -> (Just 7, str)
            4 -> (Just 4, str)
            7 -> (Just 8, str)
            _ -> (Nothing, str) 


{-

E == er i 1,4,7,8, men det er E og 
[
    (Just 1, " b  e  "),
    (Just 4, " bc e g"),
    (Just 7, " b de  "),
    (Just 8, "abcdefg"),

    (Nothing," bcdefg"),
    (Nothing,"ab defg"),
    (Nothing," bcdef "),
    (Nothing,"abcd f "),

    (Nothing,"a cdefg"), 2 eller 3
    (Nothing,"  cdefg"), 2 eller 3
]
[1, 4, 7, 8]


1 og 7 gir A -- Ingen bruk for
4 gir 9
8 og 4 gir 0
8 og 0 gir E


   A B C D E F G
0: x x x   x x x !
1:     x     x   !
2: x   x x x   x
3: x   x x   x x 
5: x x   x   x x
4:   x x x   x   !
6: x x   x x x x
7: x   x     x
8: x x x x x x x !
9: x x x x   x x 

masker med 7 gir 6 (eneste som har 4 igjen)

0: x x x   x x x 
7: x   x     x
=:   x     x   x 

2: x   x x x   x
7: x   x     x
=:       x x   x

3: x   x x   x x 
7: x   x     x
=:       x     x 

5: x x   x   x x
7: x   x     x
=:   x   x     x 

6: x x   x x x x
7: x   x     x
=:   x   x x   x

9: x x x x   x x
7: x   x     x
=:   x   x     x 

-----------------
0: x x x   x x x 
1:     x     x  
=: x x     x   x 

2: x   x x x   x
1:     x     x  
=: x     x x   x

3: x   x x   x x 
1:     x     x  
=: x     x     x 

5: x x   x   x x
1:     x     x  
=: x x   x     x 

6: x x   x x x x
1:     x     x  
=: x x   x x   x

9: x x x x   x x
1:     x     x  
=: x x   x     x 
-}