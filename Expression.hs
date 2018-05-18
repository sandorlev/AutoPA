import Data.Char
import Data.List
import System.Random

--------------------------------------------------------------------------
-- Lexer                                                                --
--------------------------------------------------------------------------

data Token = TokPlus | TokMinus
           | TokTimes | TokDiv | TokMod | TokExp
           | TokLT | TokLEQ | TokEQ | TokGEQ | TokGT | TokNEQ
           | TokAnd | TokOr | TokNot | TokTrue | TokFalse
           | TokEquiv | TokImplies | TokFollows
           | TokLpar | TokRpar
           | TokVariable String
           | TokConstant String
           | TokIntValue Integer
           | TokAssign
           | TokSemiColon
           | TokEnd
  deriving (Eq, Show)

matchHead :: String -> String -> Bool
matchHead [] _ = True
matchHead (x:xs) (y:ys)
  | x==y      = matchHead xs ys
  | otherwise = False

lexer :: String -> [Token]
lexer [] = [TokEnd]
lexer (c : cs)
  -- Note that the order of the following lines is impoertant!
  | isSpace c = lexer (dropWhile isSpace cs)
  | matchHead ":=" (c:cs) = TokAssign : lexer (drop 1 cs)
  | c == '(' = TokLpar:lexer cs
  | c == ')' = TokRpar:lexer cs
  | c == ';' = TokSemiColon : lexer cs
  -- boolean operators
  | matchHead "True" (c:cs) = TokTrue : lexer (drop 3 cs)
  | matchHead "False" (c:cs) = TokFalse : lexer (drop 4 cs)
  | c == '&' = TokAnd:lexer cs
  | c == '|' = TokOr:lexer cs
  | c == '~' = TokNot:lexer cs
  | matchHead "==" (c:cs) = TokEquiv : lexer (drop 1 cs)
  | matchHead "->" (c:cs) = TokImplies : lexer (drop 1 cs)
  | matchHead "<-" (c:cs) = TokFollows : lexer (drop 1 cs)
  -- arithmetic operators
  | c == '+' = TokPlus:lexer cs
  | c == '-' = TokMinus:lexer cs
  | c == '*' = TokTimes:lexer cs
  | c == '/' = TokDiv:lexer cs
  | c == '%' = TokMod:lexer cs
  | c == '^' = TokExp:lexer cs
  -- literals
  | isLower c = (TokVariable (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isUpper c = (TokConstant (c : takeWhile isAlpha cs)) : lexer (dropWhile isAlpha cs)
  | isDigit c = (TokIntValue (read (c : takeWhile isDigit cs))) : lexer (dropWhile isDigit cs)
  -- compare operators
  | matchHead "<=" (c:cs) = TokLEQ : lexer (drop 1 cs)
  | matchHead ">=" (c:cs) = TokGEQ : lexer (drop 1 cs)
  | matchHead "<>" (c:cs) = TokNEQ : lexer (drop 1 cs)
  | matchHead "!=" (c:cs) = TokNEQ : lexer (drop 1 cs)
  | c == '<' = TokLT:lexer cs
  | c == '>' = TokGT:lexer cs
  | c == '=' = TokEQ:lexer cs
  | otherwise = error ("Invalid character " ++ [c])

--------------------------------------------------------------------------
-- ArithExpression                                                           --
--------------------------------------------------------------------------

data ArithExpression = Plus ArithExpression ArithExpression
                     | Minus ArithExpression ArithExpression
                     | Times ArithExpression ArithExpression
                     | Div ArithExpression ArithExpression
                     | Mod ArithExpression ArithExpression
                     | Exp ArithExpression ArithExpression
                     | UnaryMinus ArithExpression
                     | IntValue Integer
                     | Variable String
                     | Constant String

parens :: ArithExpression -> String
parens (IntValue n) = show n
parens (Variable str) = str
parens (Constant str) = str
parens exp = "(" ++ show exp ++ ")"

instance Show ArithExpression where
  show (Plus lhs rhs) = parens lhs ++ "+" ++ parens rhs
  show (Minus lhs rhs) = parens lhs ++ "-" ++ parens rhs
  show (Times lhs rhs) = parens lhs ++ "*" ++ parens rhs
  show (Div lhs rhs) = parens lhs ++ "/" ++ parens rhs
  show (Mod lhs rhs) = parens lhs ++ "%" ++ parens rhs
  show (Exp lhs rhs) = parens lhs ++ "^" ++ parens rhs
  show (UnaryMinus exp) = "-" ++ parens exp
  show exp = parens exp

parseArithExpression :: String -> ArithExpression
parseArithExpression str =
  let (exp, (tok : tokens)) = parseE (lexer str)
  in
    case tok of
      TokEnd -> exp
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

-- E -> T {E'}
parseE :: [Token] -> (ArithExpression, [Token])
parseE tokens =
  let (lhs, rest) = parseT tokens
  in parseE' lhs rest

-- E' -> ("+" | "-") T {E'}
-- E' -> epsilon
parseE' :: ArithExpression -> [Token] -> (ArithExpression, [Token])
parseE' lhs (tok : tokens) =
  let (rhs, rest) = parseT tokens
  in
    case tok of
      TokPlus -> parseE' (Plus lhs rhs) rest
      TokMinus -> parseE' (Minus lhs rhs) rest
      _ -> (lhs, (tok : tokens))

-- T -> F {T'}
parseT :: [Token] -> (ArithExpression, [Token])
parseT tokens =
  let (lhs, rest) = parseF tokens
  in parseT' lhs rest

-- T' -> ("*" | "/" | "%") F
-- T' -> epsilon
parseT' :: ArithExpression -> [Token] -> (ArithExpression, [Token])
parseT' lhs (tok : tokens) =
  let (rhs, rest) = parseF tokens
  in
    case tok of
      TokTimes -> parseT' (Times lhs rhs) rest
      TokDiv -> parseT' (Div lhs rhs) rest
      TokMod -> parseT' (Mod lhs rhs) rest
      _ -> (lhs, (tok : tokens))

-- F -> P {F'}
parseF :: [Token] -> (ArithExpression, [Token])
parseF tokens =
  let (lhs, rest) = parseP tokens
  in parseF' lhs rest

-- F' -> "^" F
-- F' -> epsilon
parseF' :: ArithExpression -> [Token] -> (ArithExpression, [Token])
parseF' lhs (tok : tokens) =
  case tok of
    TokExp ->
      let (rhs, rest) = parseF tokens
      in ((Exp lhs rhs), rest)
    _ -> (lhs, (tok : tokens))

-- P -> <Var>
-- P -> <Const>
-- P -> <Integer>
-- P -> "(" E ")"
-- P -> "-" T
parseP :: [Token] -> (ArithExpression, [Token])
parseP [] = error "Token expected"
parseP (tok : tokens) =
  case tok of
    (TokVariable str) -> ((Variable str), tokens)
    (TokConstant str) -> ((Constant str), tokens)
    (TokIntValue n) -> ((IntValue n), tokens)
    TokLpar ->
      let (exp, (next : rest)) = parseE tokens
      in
        if next /= TokRpar
          then error "Missing right parenthesis"
          else (exp, rest)
    TokMinus ->
      let (exp, rest) = parseT tokens
      in ((UnaryMinus exp), rest)
    _ -> error ("Syntax Error: " ++ show tok)

--------------------------------------------------------------------------
-- Relation                                                             --
--------------------------------------------------------------------------

data Relation = ArithExpression :<: ArithExpression
              | ArithExpression :<=: ArithExpression
              | ArithExpression :=: ArithExpression
              | ArithExpression :>=: ArithExpression
              | ArithExpression :>: ArithExpression
              | ArithExpression :<>: ArithExpression

instance Show Relation where
  show (lhs :<: rhs)  = show lhs ++ "<" ++ show rhs
  show (lhs :<=: rhs) = show lhs ++ "<=" ++ show rhs
  show (lhs :=: rhs)  = show lhs ++ "=" ++ show rhs
  show (lhs :>=: rhs) = show lhs ++ ">=" ++ show rhs
  show (lhs :>: rhs)  = show lhs ++ ">" ++ show rhs
  show (lhs :<>: rhs) = show lhs ++ "<>" ++ show rhs

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
      (rhs, rest) = parseE toks
  in
    case tok of
      TokLT -> ((lhs :<: rhs), rest)
      TokLEQ -> ((lhs :<=: rhs), rest)
      TokEQ -> ((lhs :=: rhs), rest)
      TokGEQ -> ((lhs :>=: rhs), rest)
      TokGT -> ((lhs :>: rhs), rest)
      TokNEQ -> ((lhs :<>: rhs), rest)
      _ -> error ("Syntax error: " ++ show tok)

--------------------------------------------------------------------------
-- Boolean expression                                                   --
--------------------------------------------------------------------------

data BoolExpression = And BoolExpression BoolExpression
                    | Or BoolExpression BoolExpression
                    | Implies BoolExpression BoolExpression
                    | Follows BoolExpression BoolExpression
                    | Equiv BoolExpression BoolExpression
                    | Not BoolExpression
                    | BoolConst Bool
                    | Compare Relation

instance Show BoolExpression where
  show (And lhs rhs) = "(" ++ show lhs ++ " & " ++ show rhs ++ ")"
  show (Or lhs rhs) = "(" ++ show lhs ++ " | " ++ show rhs ++ ")"
  show (Implies lhs rhs) = "(" ++ show lhs ++ " -> " ++ show rhs ++ ")"
  show (Follows lhs rhs) = "(" ++ show lhs ++ " <- " ++ show rhs ++ ")"
  show (Equiv lhs rhs) = "(" ++ show lhs ++ " == " ++ show rhs ++ ")"
  show (Not e) = "~(" ++ show e ++ ")"
  show (BoolConst val) = show val
  show (Compare rel) = show rel

parseBoolExpression :: String -> BoolExpression
parseBoolExpression str =
  let (exp, (tok : tokens)) = parseI (lexer str)
  in
    case tok of
      TokEnd -> exp
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

-- I -> B {I'}
parseI :: [Token] -> (BoolExpression, [Token])
parseI tokens =
  let (lhs, rest) = parseB tokens
  in parseI' lhs rest

-- I' -> "->" B {I'}
-- I' -> "<-" B {I'}
-- I' -> "<->" B {I'}
-- I' -> epsilon
parseI' :: BoolExpression -> [Token] -> (BoolExpression, [Token])
parseI' lhs (tok : tokens) =
  let (rhs, rest) = parseB tokens
  in
    case tok of
      TokImplies -> parseI' (Implies lhs rhs) rest
      TokFollows -> parseI' (Follows lhs rhs) rest
      TokEquiv -> parseI' (Equiv lhs rhs) rest
      _ -> (lhs, (tok : tokens))

-- B -> C {B'}
parseB :: [Token] -> (BoolExpression, [Token])
parseB tokens =
  let (lhs, rest) = parseC tokens
  in parseB' lhs rest

-- B' -> "|" C {B'}
-- B' -> epsilon
parseB' :: BoolExpression -> [Token] -> (BoolExpression, [Token])
parseB' lhs (TokOr : tokens) =
  let (rhs, rest) = parseC tokens
  in parseB' (Or lhs rhs) rest
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
parseC' lhs (TokAnd : tokens) =
  let (rhs, rest) = parseU tokens
  in (parseC' (And lhs rhs) rest)
parseC' lhs tokens = (lhs, tokens)

-- U -> "True"
-- U -> "False"
-- U -> "~" U
-- U -> "(" I ")"
-- U -> R
parseU :: [Token] -> (BoolExpression, [Token])
parseU (tok : tokens) =
  case tok of
    TokTrue -> (BoolConst True, tokens)
    TokFalse -> (BoolConst False, tokens)
    TokNot ->
      let (exp, rest) = parseU tokens
      in (Not exp, rest)
    TokLpar ->
      let (exp, (next : rest)) = parseI tokens
      in
        if next /= TokRpar
          then error "Missing right parenthesis"
          else (exp, rest)
    _ ->
      let (lhs, rest) = parseR (tok : tokens)
      in (Compare lhs, rest)

--------------------------------------------------------------------------
-- Conjunctive Normal From
--------------------------------------------------------------------------

eliminateFollowsAndEquiv :: BoolExpression -> BoolExpression
eliminateFollowsAndEquiv (Equiv lhs rhs) =
  let (l,r)=(eliminateFollowsAndEquiv lhs, eliminateFollowsAndEquiv rhs)
  in And (Implies l r) (Implies r l)
eliminateFollowsAndEquiv (Follows lhs rhs) = Implies (eliminateFollowsAndEquiv lhs) (eliminateFollowsAndEquiv rhs)
eliminateFollowsAndEquiv (Not e) = Not (eliminateFollowsAndEquiv e)
eliminateFollowsAndEquiv e = e

eliminateImplication :: BoolExpression -> BoolExpression
eliminateImplication (Implies lhs rhs) =
  let (l,r)=(eliminateImplication lhs, eliminateImplication rhs)
  in Or (Not l) r
eliminateImplication (Not e) = Not (eliminateImplication e)
eliminateImplication e = e

pushAndEliminateNot :: BoolExpression -> BoolExpression
pushAndEliminateNot e = pushNot 0 e
  where
    pushNot 1 (BoolConst e)  = BoolConst (not e)
    pushNot 1 (Compare r) = Not (Compare r)
    pushNot 1 (Not e) = pushNot 0 e
    pushNot 1 (And lhs rhs) = Or (pushNot 1 lhs) (pushNot 1 rhs)
    pushNot 1 (Or lhs rhs) = And (pushNot 1 lhs) (pushNot 1 rhs)
    pushNot 0 (Or lhs rhs) = Or (pushNot 0 lhs) (pushNot 0 rhs)
    pushNot 0 (And lhs rhs) = And (pushNot 0 lhs) (pushNot 0 rhs)
    pushNot 0 (Not e) = pushNot 1 e
    pushNot 0 e = e

{-
distributeOrOverAnd :: BoolExpression -> BoolExpression
distributeOrOverAnd (Or (And l r) rhs) = And (Or l rhs) (Or r rhs)
distributeOrOverAnd (Or lhs (And l r)) = And (Or lhs l) (Or lhs r)
distributeOrOverAnd (And lhs rhs) = And (distributeOrOverAnd lhs) (distributeOrOverAnd rhs)
distributeOrOverAnd e = e
-}

distributeOrOverAnd :: BoolExpression -> BoolExpression
distributeOrOverAnd (Or (And l r) rhs) = And (Or l rhs) (Or r rhs)
distributeOrOverAnd (Or lhs (And l r)) = And (Or lhs l) (Or lhs r)
distributeOrOverAnd (And lhs rhs) = And (distributeOrOverAnd lhs) (distributeOrOverAnd rhs)
distributeOrOverAnd e = e



cnf :: BoolExpression -> BoolExpression
cnf  = distributeOrOverAnd.pushAndEliminateNot.eliminateImplication.eliminateFollowsAndEquiv

cnfstr :: String -> BoolExpression
cnfstr s = cnf $ parseBoolExpression s
--------------------------------------------------------------------------
-- Assignment                                                           --
--------------------------------------------------------------------------

data Assignment = Assign String ArithExpression

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

wpArithExp :: Assignment -> ArithExpression -> ArithExpression
wpArithExp ass (Plus lhs rhs) = Plus (wpArithExp ass lhs) (wpArithExp ass rhs)
wpArithExp ass (Minus lhs rhs) = Minus (wpArithExp ass lhs) (wpArithExp ass rhs)
wpArithExp ass (Times lhs rhs) = Times (wpArithExp ass lhs) (wpArithExp ass rhs)
wpArithExp ass (Div lhs rhs) = Div (wpArithExp ass lhs) (wpArithExp ass rhs)
wpArithExp ass (Mod lhs rhs) = Mod (wpArithExp ass lhs) (wpArithExp ass rhs)
wpArithExp ass (Exp lhs rhs) = Exp (wpArithExp ass lhs) (wpArithExp ass rhs)
wpArithExp ass (UnaryMinus exp) = UnaryMinus (wpArithExp ass exp)
wpArithExp (Assign var e) (Variable str) =
  if str == var
  then e
  else (Variable str)
wpArithExp ass exp = exp

wpRel :: Assignment -> Relation -> Relation
wpRel ass (lhs :<: rhs) = (wpArithExp ass lhs) :<: (wpArithExp ass rhs)
wpRel ass (lhs :<=: rhs) = (wpArithExp ass lhs) :<=: (wpArithExp ass rhs)
wpRel ass (lhs :=: rhs) = (wpArithExp ass lhs) :=: (wpArithExp ass rhs)
wpRel ass (lhs :>=: rhs) = (wpArithExp ass lhs) :>=: (wpArithExp ass rhs)
wpRel ass (lhs :>: rhs) = (wpArithExp ass lhs) :>: (wpArithExp ass rhs)
wpRel ass (lhs :<>: rhs) = (wpArithExp ass lhs) :<>: (wpArithExp ass rhs)

wp0 :: Assignment -> BoolExpression -> BoolExpression
wp0 ass (And lhs rhs) = And (wp0 ass lhs) (wp0 ass rhs)
wp0 ass (Or lhs rhs) = Or (wp0 ass lhs) (wp0 ass rhs)
wp0 ass (Implies lhs rhs) = Implies (wp0 ass lhs) (wp0 ass rhs)
wp0 ass (Follows lhs rhs) = Follows (wp0 ass lhs) (wp0 ass rhs)
wp0 ass (Equiv lhs rhs) = Equiv (wp0 ass lhs) (wp0 ass rhs)
wp0 ass (Not exp) = Not (wp0 ass exp)
wp0 ass (Compare rel) = Compare (wpRel ass rel)

wps :: [Assignment] -> BoolExpression -> BoolExpression
wps [] e = e
wps (a:as) e = wp0 a (wps as e)

wp :: String -> String -> BoolExpression
wp assStr expStr =
  let ass = parseAssignments assStr
      exp = parseBoolExpression expStr
  in wps ass exp

--------------------------------------------------------------------------
-- Simplify                                                             --
--------------------------------------------------------------------------

pushMinus :: ArithExpression -> ArithExpression
pushMinus (Plus lhs rhs) = Plus (pushMinus lhs) (pushMinus rhs)
pushMinus (Minus lhs rhs) = Plus (pushMinus lhs) (pushMinus rhs)
pushMinus (Times lhs rhs) = Times (pushMinus lhs) rhs
pushMinus (Div lhs rhs) = Div (pushMinus lhs) rhs
pushMinus (Mod lhs rhs) = Mod (pushMinus lhs) rhs
pushMinus (Exp lhs rhs) = Exp (pushMinus lhs) rhs
pushMinus (UnaryMinus exp) = exp
pushMinus (IntValue n) = UnaryMinus (IntValue n)
pushMinus (Variable str) = UnaryMinus (Variable str)
pushMinus (Constant str) = UnaryMinus (Constant str)

type Valuation = (String, Integer)

getValue :: String -> [Valuation] -> Maybe Integer
getValue str [] = Nothing
getValue str ((s,v):vals) = if str == s then Just v else getValue str vals

setValue :: String -> Integer -> [Valuation] -> [Valuation]
setValue str n [] = [(str,n)]
setValue str n ((s,o):vals) = if str == s then ((s,n):vals) else ((s,o):(setValue str n vals))

popValue :: String -> [Valuation] -> [Valuation]
popValue str [] = []
popValue str ((s,o):vals) = if str == s then vals else ((s,o):popValue str vals)

sumValuations :: [Valuation] -> [Valuation] -> [Valuation]
sumValuations [] bs = bs
sumValuations (v:as) bs = sumValuations as (sumValuations' v bs)

sumValuations' :: Valuation -> [Valuation] -> [Valuation]
sumValuations' (str,n) [] = [(str,n)]
sumValuations' (str,n) ((s,o):vals) =
  if str == s
    then ((s, n+o):vals)
    else sumValuations' (str,n) vals

evalAExpression :: ArithExpression -> [Valuation] -> Integer
evalAExpression (IntValue n) _ = n
evalAExpression (Variable str) vals =
  case getValue str vals of
    Just v -> v
    Nothing -> error ("Undefined variable: " ++ str)
evalAExpression (Constant str) vals =
  case getValue str vals of
    Just v -> v
    Nothing -> error ("Undefined constant: " ++ str)
evalAExpression (UnaryMinus exp) vals = -(evalAExpression exp vals)
evalAExpression (Plus lhs rhs) vals = (evalAExpression lhs vals) + (evalAExpression rhs vals)
evalAExpression (Minus lhs rhs) vals = (evalAExpression lhs vals) - (evalAExpression rhs vals)
evalAExpression (Times lhs rhs) vals = (evalAExpression lhs vals) * (evalAExpression rhs vals)
evalAExpression (Div lhs rhs) vals = div (evalAExpression lhs vals) (evalAExpression rhs vals)
evalAExpression (Mod lhs rhs) vals = mod (evalAExpression lhs vals) (evalAExpression rhs vals)
evalAExpression (Exp lhs rhs) vals = (evalAExpression lhs vals) ^ (evalAExpression rhs vals)

simulateStep :: Assignment -> [Valuation] -> [Valuation]
simulateStep (Assign var exp) vals = setValue var (evalAExpression exp vals) vals

simulate :: [Assignment] -> [Valuation] -> [Valuation]
simulate [] vals = vals
simulate (a:as) vals = simulate as (simulateStep a vals)

runSimulation :: String -> [Valuation] -> [Valuation]
runSimulation s vals = simulate (parseAssignments s) vals

evalRelation :: Relation -> [Valuation] -> Bool
evalRelation (lhs :<: rhs) vals = evalAExpression lhs vals < evalAExpression rhs vals
evalRelation (lhs :=: rhs) vals = evalAExpression lhs vals == evalAExpression rhs vals
evalRelation (lhs :<=: rhs) vals = evalAExpression lhs vals <= evalAExpression rhs vals
evalRelation (lhs :>=: rhs) vals = evalRelation (rhs :<=: lhs) vals
evalRelation (lhs :>: rhs) vals = evalRelation (rhs :<: lhs) vals
evalRelation (lhs :<>: rhs) vals = not (evalRelation (rhs :=: lhs) vals)

evalBoolExpression :: BoolExpression -> [Valuation] -> Bool
evalBoolExpression (And lhs rhs) vals = (evalBoolExpression lhs vals) && (evalBoolExpression rhs vals)
evalBoolExpression (Or lhs rhs) vals = (evalBoolExpression lhs vals) || (evalBoolExpression rhs vals)
evalBoolExpression (Not exp) vals = not (evalBoolExpression exp vals)
evalBoolExpression (Compare rel) vals = evalRelation rel vals

testHoareTriple :: [Valuation] -> String -> String -> Bool
testHoareTriple vals assStr expStr =
  evalBoolExpression (parseBoolExpression expStr) (runSimulation assStr vals)

-- SIMPLIFICATION

--instance Eq Expression where
--  _ = True

-- instance Eq ArithExpression where
--   (Binary op1 lhs1 rhs1) == (Binary op2 lhs2 rhs2) =
--     op1 == op2 && lhs1 == lhs2 && rhs1 == rhs2
--   (Unary op1 exp1) == (Unary op2 exp2) = op1 == op2 && exp1 == exp2
--   (IntValue a) == (IntValue b) = a == b
--   (Variable a) == (Variable b) = a == b
--   (Constant a) == (Constant b) = a == b
--   _ == _ = False
-- 
-- -- Int -> Unary -> Var -> Const -> Binary
-- instance Ord ArithExpression where
--   (Binary op lhs rhs) > _ = True
--   _ > (Binary op lhs rhs) = False
-- 
--   (Unary Minus a) > (Unary Minus b) = a > b
--   (Unary Minus a) > b = a > b
--   a > (Unary Minus b) = a > b
-- 
--   (IntValue a) > (IntValue b) = a > b
--   (IntValue a) > _ = False
--   _ > (IntValue n) = True
-- 
--   (Variable a) > (Variable b) = a > b
--   (Variable a) > _ = False
--   _ > (Variable b) = True
-- 
--   (Constant a) > (Constant b) = a > b
-- 
--   a >= b = a > b || a == b
--   a <= b = not (a > b)
--   a < b = not (a == b) && a <= b

normalize :: ArithExpression -> ArithExpression
normalize (Minus lhs rhs) =
  let (lhs', rhs') = (normalize lhs, pushMinus (normalize rhs))
  in Plus lhs' rhs'
normalize (Plus lhs rhs) = Plus (normalize lhs) (normalize rhs)
normalize (Times lhs rhs) = Times (normalize lhs) (normalize rhs)
normalize (Div lhs rhs) = Div (normalize lhs) (normalize rhs)
normalize (Mod lhs rhs) = Mod (normalize lhs) (normalize rhs)
normalize (Exp lhs rhs) = Exp (normalize lhs) (normalize rhs)
normalize (UnaryMinus exp) = pushMinus (normalize exp)
normalize exp = exp

--------------------------------------------------------------------------
-- Testing                                                              --
--------------------------------------------------------------------------

--testHoareTriple :: [Valuation] -> String -> String -> Bool
--testHoareTriple vals assStr expStr =
--  evalBoolExpression (parseBoolExpression expStr) (runSimulation assStr vals)

randomList :: Integer -> Integer -> [Integer]
randomList lwb upb = map (\x -> x `mod` (upb+1-lwb) + lwb) rl
  where
    rl = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()

splitInfiniteList :: Int -> [a] -> [[a]]
splitInfiniteList n xs = take n xs:splitInfiniteList n (drop n xs)

randomSets :: Int -> Int -> Integer -> Integer -> [[Integer]]
randomSets n len lwb upb = take n (splitInfiniteList len (randomList lwb upb))

listExpIdentifiers :: ArithExpression -> [String]
listExpIdentifiers exp = nub (sort (listExpIdentifiers' exp []))

listExpIdentifiers' :: ArithExpression -> [String] -> [String]
listExpIdentifiers' (Plus lhs rhs) ids = (listExpIdentifiers' lhs ids) ++ (listExpIdentifiers' rhs ids)
listExpIdentifiers' (Minus lhs rhs) ids = (listExpIdentifiers' lhs ids) ++ (listExpIdentifiers' rhs ids)
listExpIdentifiers' (Times lhs rhs) ids = (listExpIdentifiers' lhs ids) ++ (listExpIdentifiers' rhs ids)
listExpIdentifiers' (Div lhs rhs) ids = (listExpIdentifiers' lhs ids) ++ (listExpIdentifiers' rhs ids)
listExpIdentifiers' (Mod lhs rhs) ids = (listExpIdentifiers' lhs ids) ++ (listExpIdentifiers' rhs ids)
listExpIdentifiers' (Exp lhs rhs) ids = (listExpIdentifiers' lhs ids) ++ (listExpIdentifiers' rhs ids)
listExpIdentifiers' (UnaryMinus exp) ids = listExpIdentifiers' exp ids
listExpIdentifiers' (Variable var) ids = (var : ids)
listExpIdentifiers' (Constant const) ids = (const : ids)
listExpIdentifiers' _ ids = ids

randomValuations :: ArithExpression -> Int -> Integer -> Integer -> [[Valuation]]
randomValuations exp len lwb upb =
  let ids = listExpIdentifiers exp
      values = randomSets len (length ids) lwb upb
  in randomValuations' (replicate len ids) values

randomValuations' :: [[String]] -> [[Integer]] -> [[Valuation]]
randomValuations' (ids : []) (values : []) = [zip ids values]
randomValuations' (ids : idsList) (values : valuesList) = [zip ids values] ++ (randomValuations' idsList valuesList)
randomValuations' _ _ = error "identifier and value lists must be same length"
