{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char ( isDigit, isSpace )
import Text.Read (readMaybe)
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }
data Pair = P Pair Pair | L Int deriving (Show)

{-
main :: IO ()
main = interact
    $ show
--    . foldl reducePair
    . map parsePair
    . lines
    -}
item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

pars :: Parser a -> (String -> [(a, String)])
pars (Parser f) = f

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\cs ->
  case cs of
    (c:cs') ->
      (if predicate c
        then [(c, cs')]
        else [])
    _ -> [])

char :: Char -> Parser Char
char c = satisfy (== c)

instance Functor Parser where
  fmap f (Parser p) = Parser (\cs ->
    map (\(x, cs') -> (f x, cs')) (p cs))

digit :: Parser Int
digit = fmap (read . toString) (satisfy isDigit)
  where toString c = [c] -- Char -> [Char] (aka String)

instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)])
  f <*> a = Parser (\cs ->
    concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs])

main =
  let results = parse expr $ " * 3 2 "
  in print (fst (results !! 0))

instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if not (null p') then p' else q')

space :: Parser String
space = many (satisfy isSpace)

instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs ->
    concat [parse (f a) cs' | (a, cs') <- parse p cs])

string :: String -> Parser String
string "" = return ""
string (c:cs) = (:) <$> char c <*> string cs

token :: String -> Parser String
token = string

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rest
  where rest a =  (op <*> pure a <*> (p >>= rest)) <|> return a

unaryMinus :: Parser Int -> Parser Int
unaryMinus p = char '-' *> fmap negate p <|> p

integer :: Parser Int
integer =
  let positive = fmap read (some (satisfy isDigit))
  in space *> unaryMinus positive



expr = subexpr `chainr1` pow `chainl1` mul `chainl1` add
subexpr = token "[" *> expr <* *> expr <* token "," token "]" <|> integer