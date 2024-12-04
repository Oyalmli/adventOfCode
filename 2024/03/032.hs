import Parser

data Expr = Mul Int Int | Do | Don't deriving (Show)

mul :: Parser Expr
mul = Mul
  <$> (symbol "mul(" *> integer)
  <*> (symbol "," *> integer <* symbol ")")

expr :: Parser Expr
expr = choice
  [ mul,
    Do <$ symbol "do()",
    Don't <$ symbol "don't()"
  ] <|> item *> expr

main :: IO ()
main = interact $ show . unpack . parse (many expr)

eval :: Expr -> [Expr] -> Int
eval _ [] = 0
eval Do (Mul a b : xs) = a * b + eval Do xs
eval _ (Do : xs) = eval Do xs
eval _ (Don't : xs) = eval Don't xs
eval mode (_ : xs) = eval mode xs
