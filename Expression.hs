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
-- ArithExpression                                                      --
--------------------------------------------------------------------------

data ArithExpression = ArithExpression :+: ArithExpression
                     | ArithExpression :-: ArithExpression
                     | ArithExpression :*: ArithExpression
                     | ArithExpression :/: ArithExpression
                     | ArithExpression :%: ArithExpression
                     | ArithExpression :^: ArithExpression
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
  show (lhs :+: rhs) = parens lhs ++ "+" ++ parens rhs
  show (lhs :-: rhs) = parens lhs ++ "-" ++ parens rhs
  show (lhs :*: rhs) = parens lhs ++ "*" ++ parens rhs
  show (lhs :/: rhs) = parens lhs ++ "/" ++ parens rhs
  show (lhs :%: rhs) = parens lhs ++ "%" ++ parens rhs
  show (lhs :^: rhs) = parens lhs ++ "^" ++ parens rhs
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
      TokPlus -> parseE' (lhs :+: rhs) rest
      TokMinus -> parseE' (lhs :-: rhs) rest
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
      TokTimes -> parseT' (lhs :*: rhs) rest
      TokDiv -> parseT' (lhs :/: rhs) rest
      TokMod -> parseT' (lhs :%: rhs) rest
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
      in ((lhs :^: rhs), rest)
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

data BoolExpression = BoolExpression :&: BoolExpression
                    | BoolExpression :|: BoolExpression
                    | BoolExpression :->: BoolExpression
                    | BoolExpression :<-: BoolExpression
                    | BoolExpression :==: BoolExpression
                    | Not BoolExpression
                    | BoolConst Bool
                    | Compare Relation

instance Show BoolExpression where
  show (lhs :&: rhs) = "(" ++ show lhs ++ " & " ++ show rhs ++ ")"
  show (lhs :|: rhs) = "(" ++ show lhs ++ " | " ++ show rhs ++ ")"
  show (lhs :->: rhs) = "(" ++ show lhs ++ " -> " ++ show rhs ++ ")"
  show (lhs :<-: rhs) = "(" ++ show lhs ++ " <- " ++ show rhs ++ ")"
  show (lhs :==: rhs) = "(" ++ show lhs ++ " == " ++ show rhs ++ ")"
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
      TokImplies -> parseI' (lhs :->: rhs) rest
      TokFollows -> parseI' (lhs :<-: rhs) rest
      TokEquiv -> parseI' (lhs :==: rhs) rest
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
  in parseB' (lhs :|: rhs) rest
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
  in (parseC' (lhs :&: rhs) rest)
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
-- Conjunctive Normal From                                              --
--------------------------------------------------------------------------

eliminateFollowsAndEquiv :: BoolExpression -> BoolExpression
eliminateFollowsAndEquiv (lhs :==: rhs) =
  let (l, r) = (eliminateFollowsAndEquiv lhs, eliminateFollowsAndEquiv rhs)
  in (l :->: r) :&: (r :->: l)
-- Is this intentional? Maybe rhs -> lhs? check evaluateBoolExpression too
eliminateFollowsAndEquiv (lhs :<-: rhs) = (eliminateFollowsAndEquiv lhs) :->: (eliminateFollowsAndEquiv rhs)
eliminateFollowsAndEquiv (Not e) = Not (eliminateFollowsAndEquiv e)
eliminateFollowsAndEquiv e = e

eliminateImplication :: BoolExpression -> BoolExpression
eliminateImplication (lhs :->: rhs) =
  let (l, r) = (eliminateImplication lhs, eliminateImplication rhs)
  in (Not l) :|: r
eliminateImplication (Not e) = Not (eliminateImplication e)
eliminateImplication e = e

pushAndEliminateNot :: BoolExpression -> BoolExpression
pushAndEliminateNot e = pushNot 0 e
  where
    pushNot 1 (BoolConst e)  = BoolConst (not e)
    pushNot 1 (Compare r) = Not (Compare r)
    pushNot 1 (Not e) = pushNot 0 e
    pushNot 1 (lhs :&: rhs) = (pushNot 1 lhs) :|: (pushNot 1 rhs)
    pushNot 1 (lhs :|: rhs) = (pushNot 1 lhs) :&: (pushNot 1 rhs)
    pushNot 0 (lhs :|: rhs) = (pushNot 0 lhs) :|: (pushNot 0 rhs)
    pushNot 0 (lhs :&: rhs) = (pushNot 0 lhs) :&: (pushNot 0 rhs)
    pushNot 0 (Not e) = pushNot 1 e
    pushNot 0 e = e

distributeOrOverAnd :: BoolExpression -> BoolExpression
distributeOrOverAnd ((l :&: r) :|: rhs) = (l :|: rhs) :&: (r :|: rhs)
distributeOrOverAnd (lhs :|: (l :&: r)) = (lhs :|: l) :&: (lhs :|: r)
distributeOrOverAnd (lhs :&: rhs) = (distributeOrOverAnd lhs) :&: (distributeOrOverAnd rhs)
distributeOrOverAnd e = e

cnf :: BoolExpression -> BoolExpression
cnf = distributeOrOverAnd.pushAndEliminateNot.eliminateImplication.eliminateFollowsAndEquiv

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
wpArithExp ass (lhs :+: rhs) = (wpArithExp ass lhs) :+: (wpArithExp ass rhs)
wpArithExp ass (lhs :-: rhs) = (wpArithExp ass lhs) :-: (wpArithExp ass rhs)
wpArithExp ass (lhs :*: rhs) = (wpArithExp ass lhs) :*: (wpArithExp ass rhs)
wpArithExp ass (lhs :/: rhs) = (wpArithExp ass lhs) :/: (wpArithExp ass rhs)
wpArithExp ass (lhs :%: rhs) = (wpArithExp ass lhs) :%: (wpArithExp ass rhs)
wpArithExp ass (lhs :^: rhs) = (wpArithExp ass lhs) :^: (wpArithExp ass rhs)
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
wp0 ass (lhs :&: rhs) = (wp0 ass lhs) :&: (wp0 ass rhs)
wp0 ass (lhs :|: rhs) = (wp0 ass lhs) :|: (wp0 ass rhs)
wp0 ass (lhs :->: rhs) = (wp0 ass lhs) :->: (wp0 ass rhs)
wp0 ass (lhs :<-: rhs) = (wp0 ass lhs) :<-: (wp0 ass rhs)
wp0 ass (lhs :==: rhs) = (wp0 ass lhs) :==: (wp0 ass rhs)
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
-- Simplification                                                       --
--------------------------------------------------------------------------

pushMinus :: ArithExpression -> ArithExpression
pushMinus (lhs :+: rhs) = (pushMinus lhs) :+: (pushMinus rhs)
pushMinus (lhs :-: rhs) = (pushMinus lhs) :+: (pushMinus rhs)
pushMinus (lhs :*: rhs) = (pushMinus lhs) :*: rhs
pushMinus (lhs :/: rhs) = (pushMinus lhs) :/: rhs
pushMinus (lhs :%: rhs) = (pushMinus lhs) :%: rhs
pushMinus (lhs :^: rhs) = (pushMinus lhs) :^: rhs
pushMinus (UnaryMinus exp) = exp
pushMinus (IntValue n) = UnaryMinus (IntValue n)
pushMinus (Variable str) = UnaryMinus (Variable str)
pushMinus (Constant str) = UnaryMinus (Constant str)

normalize :: ArithExpression -> ArithExpression
normalize (lhs :-: rhs) =
  let (lhs', rhs') = (normalize lhs, pushMinus (normalize rhs))
  in lhs' :+: rhs'
normalize (lhs :+: rhs) = (normalize lhs) :+: (normalize rhs)
normalize (lhs :*: rhs) = (normalize lhs) :*: (normalize rhs)
normalize (lhs :/: rhs) = (normalize lhs) :/: (normalize rhs)
normalize (lhs :%: rhs) = (normalize lhs) :%: (normalize rhs)
normalize (lhs :^: rhs) = (normalize lhs) :^: (normalize rhs)
normalize (UnaryMinus exp) = pushMinus (normalize exp)
normalize exp = exp

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


--------------------------------------------------------------------------
-- Evaluation                                                           --
--------------------------------------------------------------------------

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

evalArithExp :: ArithExpression -> [Valuation] -> Integer
evalArithExp (IntValue n) _ = n
evalArithExp (Variable str) vals =
  case getValue str vals of
    Just v -> v
    Nothing -> error ("Undefined variable: " ++ str)
evalArithExp (Constant str) vals =
  case getValue str vals of
    Just v -> v
    Nothing -> error ("Undefined constant: " ++ str)
evalArithExp (UnaryMinus exp) vals = -(evalArithExp exp vals)
evalArithExp (lhs :+: rhs) vals = (evalArithExp lhs vals) + (evalArithExp rhs vals)
evalArithExp (lhs :-: rhs) vals = (evalArithExp lhs vals) - (evalArithExp rhs vals)
evalArithExp (lhs :*: rhs) vals = (evalArithExp lhs vals) * (evalArithExp rhs vals)
evalArithExp (lhs :/: rhs) vals = (evalArithExp lhs vals) `div` (evalArithExp rhs vals)
evalArithExp (lhs :%: rhs) vals = (evalArithExp lhs vals) `mod` (evalArithExp rhs vals)
evalArithExp (lhs :^: rhs) vals = (evalArithExp lhs vals) ^ (evalArithExp rhs vals)

evalRelation :: Relation -> [Valuation] -> Bool
evalRelation (lhs :<: rhs) vals = evalArithExp lhs vals < evalArithExp rhs vals
evalRelation (lhs :=: rhs) vals = evalArithExp lhs vals == evalArithExp rhs vals
evalRelation (lhs :<=: rhs) vals = evalArithExp lhs vals <= evalArithExp rhs vals
evalRelation (lhs :>=: rhs) vals = evalRelation (rhs :<=: lhs) vals
evalRelation (lhs :>: rhs) vals = evalRelation (rhs :<: lhs) vals
evalRelation (lhs :<>: rhs) vals = not (evalRelation (rhs :=: lhs) vals)

evalBoolExpression :: BoolExpression -> [Valuation] -> Bool
evalBoolExpression (lhs :&: rhs) vals = (evalBoolExpression lhs vals) && (evalBoolExpression rhs vals)
evalBoolExpression (lhs :|: rhs) vals = (evalBoolExpression lhs vals) || (evalBoolExpression rhs vals)
evalBoolExpression (lhs :->: rhs) vals = not (evalBoolExpression lhs vals) || (evalBoolExpression rhs vals)
evalBoolExpression (lhs :<-: rhs) vals = not (evalBoolExpression rhs vals) || (evalBoolExpression lhs vals)
evalBoolExpression (lhs :==: rhs) vals = (evalBoolExpression lhs vals) == (evalBoolExpression rhs vals)
evalBoolExpression (Not exp) vals = not (evalBoolExpression exp vals)
evalBoolExpression (Compare rel) vals = evalRelation rel vals

--------------------------------------------------------------------------
-- Testing                                                              --
--------------------------------------------------------------------------

simulateStep :: Assignment -> [Valuation] -> [Valuation]
simulateStep (Assign var exp) vals = setValue var (evalArithExp exp vals) vals

simulate :: [Assignment] -> [Valuation] -> [Valuation]
simulate [] vals = vals
simulate (a:as) vals = simulate as (simulateStep a vals)

runSimulation :: [Assignment] -> [Valuation] -> [Valuation]
runSimulation assignments vals = simulate assignments vals

randomList :: Integer -> Integer -> [Integer]
randomList lwb upb = map (\x -> x `mod` (upb+1-lwb) + lwb) rl
  where
    rl = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()

splitInfiniteList :: Int -> [a] -> [[a]]
splitInfiniteList n xs = take n xs:splitInfiniteList n (drop n xs)

randomSets :: Int -> Int -> Integer -> Integer -> [[Integer]]
randomSets n len lwb upb = take n (splitInfiniteList len (randomList lwb upb))

listExpIdentifiers :: ArithExpression -> [String]
listExpIdentifiers exp = nub (listExpIdentifiers' exp [])

listExpIdentifiers' :: ArithExpression -> [String] -> [String]
listExpIdentifiers' (lhs :+: rhs) ids = listExpIdentifiers' lhs ids ++ listExpIdentifiers' rhs ids
listExpIdentifiers' (lhs :-: rhs) ids = listExpIdentifiers' lhs ids ++ listExpIdentifiers' rhs ids
listExpIdentifiers' (lhs :*: rhs) ids = listExpIdentifiers' lhs ids ++ listExpIdentifiers' rhs ids
listExpIdentifiers' (lhs :/: rhs) ids = listExpIdentifiers' lhs ids ++ listExpIdentifiers' rhs ids
listExpIdentifiers' (lhs :%: rhs) ids = listExpIdentifiers' lhs ids ++ listExpIdentifiers' rhs ids
listExpIdentifiers' (lhs :^: rhs) ids = listExpIdentifiers' lhs ids ++ listExpIdentifiers' rhs ids
listExpIdentifiers' (UnaryMinus exp) ids = listExpIdentifiers' exp ids
listExpIdentifiers' (Variable var) ids = (var : ids)
listExpIdentifiers' (Constant const) ids = (const : ids)
listExpIdentifiers' _ ids = ids

listRelIdentifiers :: Relation -> [String]
listRelIdentifiers (lhs :<: rhs) = nub (listExpIdentifiers lhs ++ listExpIdentifiers rhs)
listRelIdentifiers (lhs :<=: rhs) = nub (listExpIdentifiers lhs ++ listExpIdentifiers rhs)
listRelIdentifiers (lhs :=: rhs) = nub (listExpIdentifiers lhs ++ listExpIdentifiers rhs)
listRelIdentifiers (lhs :>=: rhs) = nub (listExpIdentifiers lhs ++ listExpIdentifiers rhs)
listRelIdentifiers (lhs :>: rhs) = nub (listExpIdentifiers lhs ++ listExpIdentifiers rhs)
listRelIdentifiers (lhs :<>: rhs) = nub (listExpIdentifiers lhs ++ listExpIdentifiers rhs)

listBoolExpIdentifiers :: BoolExpression -> [String]
listBoolExpIdentifiers (lhs :&: rhs) = nub (listBoolExpIdentifiers lhs ++ listBoolExpIdentifiers rhs)
listBoolExpIdentifiers (lhs :|: rhs) = nub (listBoolExpIdentifiers lhs ++ listBoolExpIdentifiers rhs)
listBoolExpIdentifiers (lhs :->: rhs) = nub (listBoolExpIdentifiers lhs ++ listBoolExpIdentifiers rhs)
listBoolExpIdentifiers (lhs :<-: rhs) = nub (listBoolExpIdentifiers lhs ++ listBoolExpIdentifiers rhs)
listBoolExpIdentifiers (lhs :==: rhs) = nub (listBoolExpIdentifiers lhs ++ listBoolExpIdentifiers rhs)
listBoolExpIdentifiers (Not exp) = listBoolExpIdentifiers exp
listBoolExpIdentifiers (Compare rel) = listRelIdentifiers rel
listBoolExpIdentifiers _ = []

randomValuations :: [String] -> Int -> Integer -> Integer -> [[Valuation]]
randomValuations ids len lwb upb = randomValuations' (replicate len ids) (randomSets len (length ids) lwb upb)

randomValuations' :: [[String]] -> [[Integer]] -> [[Valuation]]
randomValuations' [] [] = []
randomValuations' (ids:idsList) (values:valuesList) = [zip ids values] ++ (randomValuations' idsList valuesList)
randomValuations' _ _ = error "Identifier and value lists must be same length"

findCounterExample :: String -> String -> String -> Int -> Integer -> Integer -> Maybe [Valuation]
findCounterExample preStr assStr postStr len lwb upb =
  let pre = parseBoolExpression preStr
      assignments = parseAssignments assStr
      post = parseBoolExpression postStr
      ids = nub (listBoolExpIdentifiers pre ++ listBoolExpIdentifiers post)
      values = randomValuations ids len lwb upb
  in findCounterExample' pre assignments post values

findCounterExample' :: BoolExpression -> [Assignment] -> BoolExpression -> [[Valuation]] -> Maybe [Valuation]
findCounterExample' pre ass post [] = Nothing
findCounterExample' pre ass post (val:vals) =
  if evalBoolExpression pre val
    then if not (evalBoolExpression post (runSimulation ass val))
      then Just val
      else findCounterExample' pre ass post vals
    else findCounterExample' pre ass post vals
