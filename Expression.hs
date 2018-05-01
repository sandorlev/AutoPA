import Data.Char

data ArithOperator = Plus | Minus | Times | Div | Mod | Exp
  deriving (Eq)

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
           | TokAssign
           | TokEnd
  deriving (Eq, Show)

data Expression = BinaryNode ArithOperator Expression Expression
                | IntValue Int
                | Variable String
                | Constant String

instance Show Expression where
  show (IntValue n) = show n
  show (Variable str) = "[" ++ str ++ "]"
  show (Constant str) = "<" ++ str ++ ">"
  show (BinaryNode op lhs rhs) = parens lhs ++ (show op) ++ parens rhs
    where parens (BinaryNode op lhs rhs) = "(" ++ show (BinaryNode op lhs rhs) ++ ")"
          parens e = show e

instance Eq Expression where
  (IntValue x) == (IntValue y) = x == y
  (Variable x) == (Variable y) = x == y
  (Constant x) == (Constant y) = x == y
  (BinaryNode op1 lhs1 rhs1) == (BinaryNode op2 lhs2 rhs2) =
    (op1 == op2) && (lhs1 == lhs2) && (rhs1 == rhs2)

parseExpression :: String -> Expression
parseExpression str =
  let (exp, (tok : tokens)) = parseE (lexer str)
  in
    case tok of
    TokEnd -> exp
    _ -> error ("Unused tokens: " ++ show tokens)

-- E  -> T E
-- E -> + T E
-- E -> - T E
-- E -> epsilon
parseE :: [Token] -> (Expression, [Token])
parseE tokens =
  let (lhs, (tok : toks)) = parseT tokens
  in
    case tok of
      (TokOp op) | elem op [Plus, Minus] ->
        let (rhs, rest) = parseE toks
        in (BinaryNode op lhs rhs, rest)
      TokAssign -> error "Assignment not allowed in expressions"
      _ -> (lhs, (tok : toks))

-- T  -> F T
-- T -> * F T
-- T -> / F T
-- T -> % F T
-- T -> epsilon
parseT :: [Token] -> (Expression, [Token])
parseT tokens =
  let (lhs, (tok : toks)) = parseF tokens
  in
    case tok of
      (TokOp op) | elem op [Times, Div, Mod] ->
        let (rhs, rest) = parseT toks
        in (BinaryNode op lhs rhs, rest)
      _ -> (lhs, (tok : toks))

-- F -> <Var>
-- F -> <Const>
-- F -> <Int>
-- F -> (E)
parseF :: [Token] -> (Expression, [Token])
parseF [] = error "Token expected"
parseF (tok : tokens) =
  case tok of
    (TokVariable str) -> ((Variable str), tokens)
    (TokConstant str) -> ((Constant str), tokens)
    (TokIntValue n) -> ((IntValue n), tokens)
    (TokParen '(') ->
      let (exp, (next : rest)) = parseE tokens
      in
        if next /= (TokParen ')')
        then error "Missing right parenthesis"
        else (exp, rest)
    _ -> error ("Syntax Error: " ++ show tok)

lexer :: String -> [Token]
lexer [] = [TokEnd]
lexer (c : cs)
  | elem c "+-*/%^" = (TokOp (arithOperator c)) : lexer cs
  | elem c "()" = (TokParen c) : lexer cs
  | c == '=' = TokAssign : lexer cs
  | isLower c = (TokVariable (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isUpper c = (TokConstant (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isDigit c = (TokIntValue (read (c : takeWhile isDigit cs))) : lexer (dropWhile isDigit cs)
  | isSpace c = lexer (dropWhile isSpace cs)
  | otherwise = error ("Invalid character " ++ [c])


--------------------------------------------------------------------------

data Assignment = Assign String Expression

instance Show Assignment where
  show (Assign var exp) = "[" ++ var ++ "]=" ++ show exp

parseAssignment :: String -> Assignment
parseAssignment str = parseA (lexer str)

parseA :: [Token] -> Assignment
parseA (TokVariable var : TokAssign : tokens) =
  let (exp, (tok : rest)) = parseE tokens
  in
    case tok of
    TokEnd -> (Assign var exp)
    _ -> error ("Unused tokens: " ++ show rest)
parseA (tok : _) = error ("Syntax error: " ++ show tok)


wp :: Assignment -> Expression -> Expression
wp ass (BinaryNode op lhs rhs) = (BinaryNode op (wp ass lhs) (wp ass rhs))
wp ass (IntValue n) = (IntValue n)
wp ass (Constant str) = (Constant str)
wp (Assign var e) (Variable str) = if (str==var) then e else (Variable str)

parseWp :: String -> String -> Expression
parseWp assStr expStr = wp ass exp
  where ass = parseAssignment assStr
        exp = parseExpression expStr
