import Data.Char

--------------------------------------------------------------------------
-- Lexer                                                                --
--------------------------------------------------------------------------

data Token = TokOp ArithOperator
           | TokComp Comparator
           | TokParen Char
           | TokVariable String
           | TokConstant String
           | TokIntValue Int
           | TokAssign
           | TokEnd
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer [] = [TokEnd]
lexer (c : cs)
  | elem c "+-*/%^" = (TokOp (arithOperator c)) : lexer cs
  | elem c "()" = (TokParen c) : lexer cs
  | c == ':' && (head cs) == '=' = TokAssign : lexer (tail cs)
  | isLower c = (TokVariable (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isUpper c = (TokConstant (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isDigit c = (TokIntValue (read (c : takeWhile isDigit cs))) : lexer (dropWhile isDigit cs)
  | isSpace c = lexer (dropWhile isSpace cs)
  -- ugly relations
  -- cases <, >, =
  | elem c "<>=" && not (elem (head cs) ">=") = (TokComp (comparator [c])) : lexer cs
  -- cases <=, <>
  | c == '<' && elem (head cs) ">=" = (TokComp (comparator [c, head cs])) : lexer (tail cs)
  -- case >=
  | c == '>' && head cs == '=' = (TokComp (comparator [c, head cs])) : lexer (tail cs)
  --
  | otherwise = error ("Invalid character " ++ [c])

--------------------------------------------------------------------------
-- Expression                                                           --
--------------------------------------------------------------------------

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

data Expression = BinaryNode ArithOperator Expression Expression
                | UnaryNode ArithOperator Expression
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
  show (UnaryNode op exp) = "(" ++ show op ++ show exp ++ ")"

parseExpression :: String -> Expression
parseExpression str =
  let (exp, (tok : tokens)) = parseE (lexer str)
  in
    case tok of
      TokEnd -> exp
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

-- E -> T {E'}
parseE :: [Token] -> (Expression, [Token])
parseE tokens =
  let (lhs, rest) = parseT tokens
  in parseE' lhs rest

-- E' -> ("+" | "-") T
-- E' -> epsilon
parseE' :: Expression -> [Token] -> (Expression, [Token])
parseE' lhs (tok : tokens) =
  case tok of
    (TokOp op) | elem op [Plus, Minus] ->
      let (rhs, rest) = parseT tokens
      in parseE' (BinaryNode op lhs rhs) rest
    _ -> (lhs, (tok : tokens))

-- T -> F {T'}
parseT :: [Token] -> (Expression, [Token])
parseT tokens =
  let (lhs, rest) = parseF tokens
  in parseT' lhs rest

-- T' -> ("*" | "/" | "%") F
-- T' -> epsilon
-- (TODO: Mod)
parseT' :: Expression -> [Token] -> (Expression, [Token])
parseT' lhs (tok : tokens) =
  case tok of
    (TokOp op) | elem op [Times, Div, Mod] ->
      let (rhs, rest) = parseF tokens
      in parseT' (BinaryNode op lhs rhs) rest
    _ -> (lhs, (tok : tokens))

-- F -> P {F'}
parseF :: [Token] -> (Expression, [Token])
parseF tokens =
  let (lhs, rest) = parseP tokens
  in parseF' lhs rest

-- F' -> ["^" F]
-- F' -> epsilon
parseF' :: Expression -> [Token] -> (Expression, [Token])
parseF' lhs (tok : tokens) =
  case tok of
    (TokOp Exp) ->
      let (rhs, rest) = parseF tokens
      in ((BinaryNode Exp lhs rhs), rest)
    _ -> (lhs, (tok : tokens))

-- P -> <Var>
-- P -> <Const>
-- P -> <Int>
-- P -> "(" E ")"
-- P -> "-" T
parseP :: [Token] -> (Expression, [Token])
parseP [] = error "Token expected"
parseP (tok : tokens) =
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
    (TokOp Minus) ->
      let (exp, rest) = parseF tokens
      in ((UnaryNode Minus exp), rest)
    _ -> error ("Syntax Error: " ++ show tok)

--------------------------------------------------------------------------
-- Relation                                                             --
--------------------------------------------------------------------------

data Comparator = Less | LessOrEqual | Greater | GreaterOrEqual | Equal | NotEqual
  deriving (Eq)

comparator :: String -> Comparator
comparator str
  | str == "<"  = Less
  | str == "<=" = LessOrEqual
  | str == ">"  = Greater
  | str == ">=" = GreaterOrEqual
  | str == "="  = Equal
  | str == "<>" = NotEqual

instance Show Comparator where
  show Less = "<"
  show LessOrEqual = "<="
  show Greater = ">"
  show GreaterOrEqual = ">="
  show Equal = "="
  show NotEqual = "<>"

data Relation = Relation Comparator Expression Expression

instance Show Relation where
  show (Relation comp lhs rhs) = show lhs ++ show comp ++ show rhs

parseRelation :: String -> Relation
parseRelation str =
  let (rel, (tok : tokens)) = parseR (lexer str)
  in
    case tok of
      TokEnd -> rel
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

parseR :: [Token] -> (Relation, [Token])
parseR tokens =
  let (lhs, (tok : toks)) = parseE tokens
  in
    case tok of
      (TokComp comp) ->
        let (rhs, rest) = parseE toks
        in ((Relation comp lhs rhs), rest)
      _ -> error ("Syntax error: " ++ show tok)

--------------------------------------------------------------------------
-- Assignment                                                           --
--------------------------------------------------------------------------

data Assignment = Assign String Expression

instance Show Assignment where
  show (Assign var exp) = "[" ++ var ++ "]:=" ++ show exp

parseAssignment :: String -> Assignment
parseAssignment str =
  let (ass, (tok : tokens)) = parseA (lexer str)
  in
    case tok of
      TokEnd -> ass
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

parseA :: [Token] -> (Assignment, [Token])
parseA (TokVariable var : TokAssign : tokens) =
  let (exp, rest) = parseE tokens
  in ((Assign var exp), rest)
parseA (tok : _) = error ("Syntax error: " ++ show tok)

--------------------------------------------------------------------------
-- Weakest precondition                                                 --
--------------------------------------------------------------------------

wp :: Assignment -> Expression -> Expression
wp ass (BinaryNode op lhs rhs) = (BinaryNode op (wp ass lhs) (wp ass rhs))
wp ass (IntValue n) = (IntValue n)
wp ass (Constant str) = (Constant str)
wp (Assign var e) (Variable str) =
  if str == var
  then e
  else (Variable str)

wpRel :: Assignment -> Relation -> Relation
wpRel ass (Relation op lhs rhs) = (Relation op (wp ass lhs) (wp ass rhs))

wpRel :: Assignment -> Relation -> Relation
wpRel ass (Relation op lhs rhs) = (Relation op (wp ass lhs) (wp ass rhs))

parseWp :: String -> String -> Expression
parseWp assStr expStr =
  let ass = parseAssignment assStr
      exp = parseExpression expStr
  in wp ass exp

parseWpRel :: String -> String -> Relation
parseWpRel assStr relStr =
  let ass = parseAssignment assStr
      rel = parseRelation relStr
  in wpRel ass rel
