import Parser

main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ part_1 inp
    putStrLn $ "Part 2: " ++ part_2 inp

part_1 :: String -> String
part_1 = show . eval . unpack . parse (many expr)
  where
    eval [] = 0
    eval (Mul a b : xs) = a * b + eval xs
    eval (_ : xs) = eval xs

part_2 :: String -> String
part_2 = show . eval Do . unpack . parse (many expr)
  where
    eval _ [] = 0
    eval Do (Mul a b : xs) = a * b + eval Do xs
    eval _ (Do : xs) = eval Do xs
    eval _ (Don't : xs) = eval Don't xs
    eval mode (_ : xs) = eval mode xs

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
