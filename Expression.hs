import Data.Char

data ArithOperator = Plus | Minus | Times | Div | Mod | Exp

arithOperator :: Char -> ArithOperator
arithOperator c
  | c == '+' = Plus
  | c == '-' = Minus
  | c == '*' = Times
  | c == '/' = Div
  | c == '%' = Mod
  | c == '^' = Exp

instance Show ArithOperator where
  show (Plus)  = "+"
  show (Minus) = "-"
  show (Times) = "*"
  show (Div)   = "/"
  show (Mod)   = "%"
  show (Exp)   = "^"

data Token = TokOp ArithOperator
           | TokParen Char
           | TokVariable String
           | TokConstant String
           | TokIntValue Int
           | TokEnd
  deriving(Show)

data Expression = BinaryNode ArithOperator Expression Expression
                | IntValue Int
                | Variable String
                | Constant String

instance Show Expression where
  show (IntValue n) = show n
  show (Variable str) = str
  show (Constant str) = str
  show (BinaryNode op lhs rhs) = pars lhs ++ (show op) ++ pars rhs
    where pars (BinaryNode op lhs rhs) = "(" ++ show (BinaryNode op lhs rhs) ++ ")"
          pars e = show e

parseExpression :: String -> (String,[String])
parseExpression str = parseE "" (lexer str)

-- E  -> T E'
parseE :: String -> [String] -> (String, [String])
parseE accepted tokens = parseE' acc rest
  where (acc, rest) = parseT accepted tokens

-- E' -> + T E'
-- E' -> - T E'
-- E' -> epsilon
parseE' :: String -> [String] -> (String,[String])
parseE' accepted ("+":tokens) =
  let (acc,rest) = parseT (accepted++"+") tokens in parseE' acc rest
parseE' accepted ("-":tokens) =
  let (acc,rest) = parseT (accepted++"-") tokens in parseE' acc rest
parseE' accepted tokens = (accepted, tokens)

-- T  -> F T'
parseT :: String -> [String] -> (String,[String])
parseT accepted tokens = parseT' acc rest
  where (acc, rest) = parseF accepted tokens

-- T' -> * F T'
-- T' -> / F T'
-- T' -> epsilon
parseT' :: String -> [String] -> (String,[String])
parseT' accepted ("*":tokens) =
  let (acc,rest) = parseT' (accepted++"*") tokens
  in parseF acc rest
parseT' accepted ("/":tokens) =
  let (acc,rest) = parseT' (accepted++"/") tokens
  in parseF acc rest
parseT' accepted tokens = (accepted, tokens)

-- F -> (E)
-- F -> <Int>
-- F -> <Var>
-- F -> <Const>
parseF :: String -> [String] -> (String, [String])
parseF accepted [] = error "Parse error...abort"
parseF accepted (tok:tokens)
 | tok == "(" = let (acc,rest) = parseE "" tokens
                in (accepted++"("++acc++")", tail rest) -- needs improvement
 | isLower (head tok) = (accepted++"["++tok++"]", tokens) -- var
 | isUpper (head tok) = (accepted++"<"++tok++">", tokens) -- const
 | isDigit (head tok) = (accepted++tok, tokens)
 | otherwise = error ("Syntax Error: " ++ tok)



lexer :: String -> [String]
lexer [] = []
lexer (c : cs)
  | elem c "+-*/%^()" = [c] : (lexer cs)
  | isLower c = (c : takeWhile isAlpha cs) : lexer (dropWhile isAlpha cs)
  | isUpper c = (c : takeWhile isAlpha cs) : lexer (dropWhile isAlpha cs)
  | isDigit c = (c : takeWhile isDigit cs) : lexer (dropWhile isDigit cs)
  | isSpace c = lexer (dropWhile isSpace cs)
  | otherwise = error $ "Invalid character " ++ [c]

-- lexer2 :: String -> [Token]
-- lexer2 [] = [TokEnd]
-- lexer2 (c : cs)
--   | elem c "+-*/%^" = (TokOp (arithOperator c)) : (lexer cs)
--   | isLower c = (TokVariable (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
--   | isUpper c = (TokConstant (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
--   | isDigit c = (TokIntValue (read (c : takeWhile isDigit cs))) : lexer (dropWhile isDigit cs)
--   | isSpace c = lexer (dropWhile isSpace cs)
--   | otherwise = error $ "Invalid character " ++ [c]


--------------------------------------------------------------------------

data Assignment = Assign String Expression

wp :: Assignment -> Expression -> Expression
wp ass (BinaryNode op lhs rhs) = (BinaryNode op (wp ass lhs) (wp ass rhs))
wp ass (IntValue n) = (IntValue n)
wp ass (Constant str) = (Constant str)
wp (Assign var e) (Variable str) = if (str==var) then e else (Variable str)


main = print $ show (
  -- 2(x+y) + 2^3 - 2 / Y 
  BinaryNode Minus
    (BinaryNode Plus (BinaryNode Times (IntValue 2) (BinaryNode Plus (Variable "x") (Variable "y"))) (BinaryNode Exp (IntValue 2) (IntValue 3)))
    (BinaryNode Div (IntValue 2) (Constant "Y")))
