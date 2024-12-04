import Data.Char (isDigit)

main :: IO ()
main = interact $ show . sum . map eval . parseExprs

data Expr = Mul Integer Integer deriving (Show)

eval :: Expr -> Integer
eval (Mul x y) = x * y

parseExprs :: String -> [Expr]
parseExprs [] = []
parseExprs s@(_ : cs) =
  case parseMul s of
    Just (expr, rest) -> expr : parseExprs rest
    Nothing -> parseExprs cs

-- Parses a Mul expression from the beginning of the string if possible
parseMul :: String -> Maybe (Expr, String)
parseMul s0 = do
  s1 <- matchStringNoSpace "mul(" s0
  (aStr, s2) <- parseDigits s1
  s3 <- matchCharNoSpace ',' s2
  (bStr, s4) <- parseDigits s3
  s5 <- matchCharNoSpace ')' s4
  let a = read aStr :: Integer
  let b = read bStr :: Integer
  return (Mul a b, s5)

-- Matches a string exactly, failing if any character is a space
matchStringNoSpace :: String -> String -> Maybe String
matchStringNoSpace "" s = Just s
matchStringNoSpace (c : cs) (d : ds)
  | c == d && d /= ' ' = matchStringNoSpace cs ds
  | otherwise = Nothing
matchStringNoSpace _ _ = Nothing

-- Matches a specific character, failing if it is a space
matchCharNoSpace :: Char -> String -> Maybe String
matchCharNoSpace c (d : ds)
  | c == d && d /= ' ' = Just ds
  | otherwise = Nothing
matchCharNoSpace _ _ = Nothing

-- Parses consecutive digits, ensuring no spaces are involved
parseDigits :: String -> Maybe (String, String)
parseDigits s@(c : _)
  | isDigit c = Just $ span isDigit s
  | otherwise = Nothing
parseDigits _ = Nothing