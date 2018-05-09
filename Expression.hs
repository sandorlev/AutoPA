import Data.Char
import Data.List
import System.Random

--------------------------------------------------------------------------
-- Lexer                                                                --
--------------------------------------------------------------------------

data Token = TokOp ArithOperator
           | TokComp Comparator
           | TokBoolOp BoolOperator
           | TokParen Char
           | TokVariable String
           | TokConstant String
           | TokIntValue Integer
           | TokAssign
           | TokSemiColon
           | TokEnd
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer [] = [TokEnd]
lexer (c : cs)
  | elem c "+-*/%^" = (TokOp (arithOperator c)) : lexer cs
  | elem c "()" = (TokParen c) : lexer cs
  | c == ';' = TokSemiColon : lexer cs
  | c == ':' && (head cs) == '=' = TokAssign : lexer (tail cs)
  | isLower c = (TokVariable (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isUpper c = (TokConstant (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isDigit c = (TokIntValue (read (c : takeWhile isDigit cs))) : lexer (dropWhile isDigit cs)
  | isSpace c = lexer (dropWhile isSpace cs)
  -- ugly comparators
  -- cases <, >, =
  | elem c "<>=" && not (elem (head cs) ">=") = (TokComp (comparator [c])) : lexer cs
  -- cases <=, <>
  | c == '<' && elem (head cs) ">=" = (TokComp (comparator [c, head cs])) : lexer (tail cs)
  -- case >=
  | c == '>' && head cs == '=' = (TokComp (comparator [c, head cs])) : lexer (tail cs)
  -- binary operators
  | elem c "&|!" = (TokBoolOp (boolOperator c)) : lexer cs
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
  show Plus  = "+"
  show Minus = "-"
  show Times = "*"
  show Div   = "/"
  show Mod   = "%"
  show Exp   = "^"

data Expression = BinaryNode ArithOperator Expression Expression
                | UnaryNode ArithOperator Expression
                | IntValue Integer
                | Variable String
                | Constant String

instance Show Expression where
  show (IntValue n) = show n
  show (Variable str) = str
  show (Constant str) = str
  show (BinaryNode op lhs rhs) = parens lhs ++ (show op) ++ parens rhs
    where parens (BinaryNode op lhs rhs) = "(" ++ show (BinaryNode op lhs rhs) ++ ")"
          parens e = show e
  show (UnaryNode op exp) = show op ++ "(" ++ show exp ++ ")"

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

-- E' -> ("+" | "-") T {E'}
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

-- F' -> "^" F
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
-- P -> <Integer>
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
      let (exp, rest) = parseT tokens
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
-- Boolean expression                                                   --
--------------------------------------------------------------------------

data BoolOperator = And | Or | Not
  deriving (Eq)

boolOperator :: Char -> BoolOperator
boolOperator c
  | c == '&' = And
  | c == '|' = Or
  | c == '!' = Not

instance Show BoolOperator where
  show And = "&"
  show Or = "|"
  show Not = "!"

data BoolExpression = BoolBinaryNode BoolOperator BoolExpression BoolExpression
                    | BoolUnaryNode BoolOperator BoolExpression
                    | BoolRelNode Relation

instance Show BoolExpression where
  show (BoolBinaryNode op lhs rhs) = "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"
  show (BoolUnaryNode op rel) = show op ++ "(" ++ show rel ++ ")"
  show (BoolRelNode rel) = show rel

parseBoolExpression :: String -> BoolExpression
parseBoolExpression str =
  let (exp, (tok : tokens)) = parseB (lexer str)
  in
    case tok of
      TokEnd -> exp
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

-- B -> C {B'}
parseB :: [Token] -> (BoolExpression, [Token])
parseB tokens =
  let (lhs, rest) = parseC tokens
  in parseB' lhs rest

-- B' -> "|" C {B'}
-- B' -> epsilon
parseB' :: BoolExpression -> [Token] -> (BoolExpression, [Token])
parseB' lhs ((TokBoolOp Or) : tokens) =
  let (rhs, rest) = parseC tokens
  in parseB' (BoolBinaryNode Or lhs rhs) rest
parseB' lhs tokens = (lhs, tokens)

-- C -> U {C'}
-- C -> epsilon
parseC :: [Token] -> (BoolExpression, [Token])
parseC tokens =
  let (lhs, rest) = parseU tokens
  in parseC' lhs rest

-- C' -> "&" U {C'}
-- C' -> epsilon
parseC' :: BoolExpression -> [Token] -> (BoolExpression, [Token])
parseC' lhs ((TokBoolOp And) : tokens) =
  let (rhs, rest) = parseU tokens
  in (parseC' (BoolBinaryNode And lhs rhs) rest)
parseC' lhs tokens = (lhs, tokens)

-- U -> "!" U
-- U -> "(" B ")"
-- U -> R
parseU :: [Token] -> (BoolExpression, [Token])
parseU (tok : tokens) =
  case tok of
    (TokBoolOp Not) ->
      let (exp, rest) = parseU tokens
      in ((BoolUnaryNode Not exp), rest)
    (TokParen '(') ->
      let (exp, (next : rest)) = parseB tokens
      in
        if next /= (TokParen ')')
        then error "Missing right parenthesis"
        else (exp, rest)
    _ ->
      let (lhs, rest) = parseR (tok : tokens)
      in ((BoolRelNode lhs), rest)

--------------------------------------------------------------------------
-- Assignment                                                           --
--------------------------------------------------------------------------

data Assignment = Assign String Expression

instance Show Assignment where
  show (Assign var exp) = var ++ ":=" ++ show exp

acceptSemicolon :: [Token] -> [Token]
acceptSemicolon (TokSemiColon:xs) = xs
acceptSemicolon (TokEnd:xs) = (TokEnd:xs)
acceptSemicolon _ = error "Error: expected semicolon"

parseAssignments :: String -> [Assignment]
parseAssignments str = parseAs (lexer str)
  where parseAs [TokEnd] = []
        parseAs tokens = ass:parseAs (acceptSemicolon toks)
          where   (ass, toks) = parseA tokens

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

wpExp :: Assignment -> Expression -> Expression
wpExp ass (BinaryNode op lhs rhs) = (BinaryNode op (wpExp ass lhs) (wpExp ass rhs))
wpExp ass (UnaryNode op exp) = (UnaryNode op (wpExp ass exp))
wpExp ass (IntValue n) = (IntValue n)
wpExp ass (Constant str) = (Constant str)
wpExp (Assign var e) (Variable str) =
  if str == var
  then e
  else (Variable str)

wpRel :: Assignment -> Relation -> Relation
wpRel ass (Relation op lhs rhs) = (Relation op (wpExp ass lhs) (wpExp ass rhs))

wp :: Assignment -> BoolExpression -> BoolExpression
wp ass (BoolBinaryNode op lhs rhs) = (BoolBinaryNode op (wp ass lhs) (wp ass rhs))
wp ass (BoolUnaryNode op exp) = (BoolUnaryNode op (wp ass exp))
wp ass (BoolRelNode rel) = (BoolRelNode (wpRel ass rel))

wps :: [Assignment] -> BoolExpression -> BoolExpression
wps [] e = e
wps (a:as) e = wp a (wps as e)

parseWp :: String -> String -> BoolExpression
parseWp assStr expStr =
  let ass = parseAssignments assStr
      exp = parseBoolExpression expStr
  in wps ass exp

--------------------------------------------------------------------------
-- Simplify                                                             --
--------------------------------------------------------------------------

pushMinus :: Expression -> Expression
pushMinus (BinaryNode Minus lhs rhs) = (BinaryNode Plus (pushMinus lhs) (pushMinus rhs))
pushMinus (BinaryNode op lhs rhs) = (BinaryNode op (pushMinus lhs) (pushMinus rhs))
pushMinus (UnaryNode Minus exp) = exp
pushMinus (IntValue n) = (UnaryNode Minus (IntValue n))
pushMinus (Variable str) = (UnaryNode Minus (Variable str))
pushMinus (Constant str) = (UnaryNode Minus (Constant str))
pushMinus exp = exp

--simplifyExp :: Expression -> Expression
--simplifyExp (BinaryNode Plus (IntValue 0) b) = simplifyExp b
--simplifyExp (BinaryNode Plus a (IntValue 0)) = simplifyExp a
--
--simplifyExp (BinaryNode op (IntValue a) (IntValue b)) = evalOperation op a b
--simplifyExp (BinaryNode op (IntValue a) (UnaryNode Minus (IntValue b))) = evalOperation op a (-b)
--simplifyExp (BinaryNode op (UnaryNode Minus (IntValue a)) (IntValue b)) = evalOperation op (-a) b
--
--simplifyExp (BinaryNode Minus lhs rhs) =
--  let slhs = simplifyExp lhs
--      srhs = simplifyExp (pushMinus rhs)
--  in (BinaryNode Plus slhs srhs)
--simplifyExp (BinaryNode op lhs rhs) =
--  let slhs = simplifyExp lhs
--      srhs = simplifyExp rhs
--  in (BinaryNode op slhs srhs)

addIfMissing :: String -> [String] -> [String]
addIfMissing str strs =
  if elem str strs then
    strs
  else
    (str : strs)

listExpIdentifiers :: Expression -> ([String], [String])
listExpIdentifiers exp =
  let (vars, consts) = listExpIdentifiers' exp [] []
  in (sort vars, sort consts)

listExpIdentifiers' :: Expression -> [String] -> [String] -> ([String], [String])
listExpIdentifiers' (UnaryNode op exp) vars consts =
  let (v, c) = (listExpIdentifiers' exp) vars consts
  in (v, c)
listExpIdentifiers' (BinaryNode op lhs rhs) vars consts =
  let (v, c) = (listExpIdentifiers' lhs vars consts)
  in listExpIdentifiers' rhs v c
listExpIdentifiers' exp vars consts =
  case exp of
    (Variable var) -> ((addIfMissing var vars), consts)
    (Constant const) -> (vars, (addIfMissing const consts))
    _ -> (vars, consts)

randomList :: Int -> Int -> Int -> [Int]
randomList length lwb upb = take length rlist
  where
    rlist = map (\x -> x `mod` (upb+1-lwb) + lwb) rl
    rl = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()

type Valuation = (String, Integer)

getValue :: String -> [Valuation] -> Maybe Integer
getValue str [] = Nothing
getValue str ((s,v):vals) = if str == s then Just v else getValue str vals

setValue :: String -> Integer -> [Valuation] -> [Valuation]
setValue str n [] = [(str,n)]
setValue str n ((s,o):vals) = if str == s then ((s,n):vals) else ((s,o):(setValue str n vals))

evaluateExpression :: Expression -> [Valuation] -> Integer
evaluateExpression (IntValue n) _ = n
evaluateExpression (Variable str) vals =
  case getValue str vals of
    Just v -> v
    Nothing -> error ("Undefined variable: " ++ str)
evaluateExpression (Constant str) vals =
  case getValue str vals of
    Just v -> v
    Nothing -> error ("Undefined constant: " ++ str)
evaluateExpression (UnaryNode Minus exp) vals = evaluateExpression (pushMinus exp) vals
evaluateExpression (BinaryNode op lhs rhs) vals =
  let (lh,rh)=(evaluateExpression lhs vals,evaluateExpression rhs vals) in
  case op of
    Plus -> lh + rh
    Minus -> lh - rh
    Times -> lh * rh
    Div -> div lh rh
    Mod -> mod lh rh
    Exp -> lh ^ rh

simulateStep :: Assignment -> [Valuation] -> [Valuation]
simulateStep (Assign var exp) vals = setValue var (evaluateExpression exp vals) vals

simulate :: [Assignment] -> [Valuation] -> [Valuation]
simulate [] vals = vals
simulate (a:as) vals = simulate as (simulateStep a vals)

runSimulation :: String -> [Valuation] -> [Valuation]
runSimulation s vals = simulate (parseAssignments s) vals

evaluateRelation :: Relation -> [Valuation] -> Bool
evaluateRelation (Relation comp lhs rhs) vals =
  let (lh, rh) = (evaluateExpression lhs vals, evaluateExpression rhs vals)
  in
    case comp of
      Less -> lh < rh
      LessOrEqual -> lh <= rh
      Greater -> lh > rh
      GreaterOrEqual -> lh >= rh
      Equal -> lh == rh
      NotEqual -> lh /= rh

evaluateBoolExpression :: BoolExpression -> [Valuation] -> Bool
evaluateBoolExpression (BoolBinaryNode op lhs rhs) vals =
  let (lh, rh) = (evaluateBoolExpression lhs vals, evaluateBoolExpression rhs vals)
  in
    case op of
      And -> lh && rh
      Or -> lh || rh
evaluateBoolExpression (BoolUnaryNode Not exp) vals = not (evaluateBoolExpression exp vals)
evaluateBoolExpression (BoolRelNode rel) vals = evaluateRelation rel vals

checkHoareTriple :: [Valuation] -> String -> String -> Bool
checkHoareTriple vals assStr expStr =
  evaluateBoolExpression (parseBoolExpression expStr) (runSimulation assStr vals)
