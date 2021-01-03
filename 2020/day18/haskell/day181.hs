{-# LANGUAGE DeriveFunctor #-}

import           Control.Monad
import           Data.Function      (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Printf

data Expr a = Value a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            deriving (Functor)

instance Show a => Show (Expr a) where
  show (Value a) = show a
  show (Add l r) = printf "(%s + %s)" (show l) (show r)
  show (Mul l r) = printf "(%s * %s)" (show l) (show r)

{-
main = interact $ show 
  . evalSum . parseInput1
-}
main = interact $ show 
  . evalSum . parseInput2

eval :: Num a => Expr a -> a
eval (Value x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

exprP1 :: Parser (Expr Int)
exprP1 = chainl1 term binOp
  where term  = value <|> parens exprP1
        value = Value <$> intLiteral
        binOp = addOp <|> mulOp
        addOp = Add <$ symbol "+"
        mulOp = Mul <$ symbol "*"

evalP1 :: Parser Int
evalP1 = chainl1 term binOp
  where term  = value <|> parens evalP1
        value = intLiteral
        binOp = addOp <|> mulOp
        addOp = (+) <$ symbol "+"
        mulOp = (*) <$ symbol "*"

exprP2 :: Parser (Expr Int)
exprP2 = chainl1 factor mulOp
  where 
    factor = chainl1 term addOp
    term   = value <|> parens exprP2
    value  = Value <$> intLiteral
    addOp  = Add <$ symbol "+"
    mulOp  = Mul <$ symbol "*"

parseInput1 :: String -> [Expr Int]
parseInput1 = parseLinesWith exprP1

parseInput2 :: String -> [Expr Int]
parseInput2 = parseLinesWith exprP2

parseWith :: Parser a -> String -> a
parseWith = either (error . show) id ... flip parse ""

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith p s = parseWith p <$> lines s

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

nameLiteral :: Parser String
nameLiteral = lexeme $ many1 lower

intLiteral :: Parser Int
intLiteral = lexeme $ choice
  [ char '-' >> fmap negate number
  , char '+' >> number
  , number
  ]
  where number = read <$> many1 digit

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)
infixr 8 ...

tryAll :: [Parser a] -> Parser a
tryAll = choice . map try

evalSum :: [Expr Int] -> Int
evalSum = sum . map eval
