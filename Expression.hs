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
  -- Note that the order of the following lines is important!
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
  | isLower c = TokVariable (c : takeWhile isAlpha cs) : lexer (dropWhile isAlpha cs)
  | isUpper c = TokConstant (c : takeWhile isAlpha cs) : lexer (dropWhile isAlpha cs)
  | isDigit c = TokIntValue (read (c : takeWhile isDigit cs)) : lexer (dropWhile isDigit cs)
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

-- T' -> ("*" | "/" | "%") F {T'}
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
parseF' base (tok : tokens) =
  case tok of
    TokExp ->
      let (exp, rest) = parseF tokens
      in (base :^: exp, rest)
    _ -> (base, (tok : tokens))

-- P -> <Var>
-- P -> <Const>
-- P -> <Integer>
-- P -> "(" E ")"
-- P -> "-" T
parseP :: [Token] -> (ArithExpression, [Token])
parseP [] = error "Token expected"
parseP (tok : tokens) =
  case tok of
    TokVariable str -> (Variable str, tokens)
    TokConstant str -> (Constant str, tokens)
    TokIntValue n -> (IntValue n, tokens)
    TokLpar ->
      let (exp, (next : rest)) = parseE tokens
      in
        if next /= TokRpar
          then error "Missing right parenthesis"
          else (exp, rest)
    TokMinus ->
      let (exp, rest) = parseT tokens
      in (UnaryMinus exp, rest)
    _ -> error ("Syntax Error: " ++ show tok)

--------------------------------------------------------------------------
-- ArithExprList                                                        --
--------------------------------------------------------------------------

data ArithExprList = Add [ArithExprList]
                   | Mul [ArithExprList]
                   | Div [ArithExprList]
                   | Mod [ArithExprList]
                   | Exp ArithExprList ArithExprList
                   | Min ArithExprList
                   | Val Integer
                   | Var String
                   | Const String

ps :: ArithExprList -> String
ps (Val n) = show n
ps (Var v) = v
ps (Const c) = c
ps exp = "(" ++ show exp ++ ")"

instance Show ArithExprList where
  show (Add (e:[])) = ps e
  show (Add (e:es)) = ps e ++ "+" ++ show (Add es)
  show (Mul (e:[])) = ps e
  show (Mul (e:es)) = ps e ++ "*" ++ show (Mul es)
  show (Div (e:[])) = ps e
  show (Div (e:es)) = ps e ++ "/" ++ show (Div es)
  show (Mod (e:[])) = ps e
  show (Mod (e:es)) = ps e ++ "%" ++ show (Mod es)
  show (Exp lhs rhs) = ps lhs ++ "^" ++ ps rhs
  show (Min e) = "-" ++ ps e
  show e = ps e

instance Eq ArithExprList where
  (Add []) == (Add []) = True
  (Add (l:ls)) == (Add (r:rs)) = l == r && Add ls == Add rs
  (Add _) == _ = False
  _ == (Add _) = False

  (Mul []) == (Mul []) = True
  (Mul (l:ls)) == (Mul (r:rs)) = l == r && Mul ls == Mul rs
  (Mul _) == _ = False
  _ == (Mul _) = False

  (Div []) == (Div []) = True
  (Div (l:ls)) == (Div (r:rs)) = l == r && Div ls == Div rs
  (Div _) == _ = False
  _ == (Div _) = False

  (Mod []) == (Mod []) = True
  (Mod (l:ls)) == (Mod (r:rs)) = l == r && Mod ls == Mod rs
  (Mod _) == _ = False
  _ == (Mod _) = False

  (Exp base1 exp1) == (Exp base2 exp2) = base1 == base2 && exp1 == exp2
  (Exp _ _) == _ = False
  _ == (Exp _ _) = False

  (Min l) == (Min r) = l == r
  (Min _) == _ = False
  _ == (Min _) = False

  (Val l) == (Val r) = l == r
  (Val _) == _ = False
  _ == (Val _) = False

  (Var l) == (Var r) = l == r
  (Var _) == _ = False
  _ == (Var _) = False

  (Const l) == (Const r) = l == r

arithExpressionToList :: ArithExpression -> ArithExprList
arithExpressionToList exp = parseArithExprList' (show (eliminateMinus exp))

parseArithExprList :: String -> ArithExprList
parseArithExprList str = arithExpressionToList (parseArithExpression str)

parseArithExprList' :: String -> ArithExprList
parseArithExprList' str =
  let (exp, (tok : tokens)) = parseE2 (lexer str)
  in
    case tok of
      TokEnd -> exp
      _ -> error ("Unused tokens: " ++ show (tok : tokens))

-- E2 -> T2 {E2'}
parseE2 :: [Token] -> (ArithExprList, [Token])
parseE2 tokens =
  let (lhs, rest) = parseT2 tokens
  in parseE2' lhs rest

-- E2' -> "+" T2 {E2'}
-- E2' -> epsilon
parseE2' :: ArithExprList -> [Token] -> (ArithExprList, [Token])
parseE2' lhs (TokPlus : tokens) =
  let (rhs, rest) = parseT2 tokens
  in
    case lhs of
      Add es -> parseE2' (Add (es ++ [rhs])) rest
      _ -> parseE2' (Add [lhs, rhs]) rest
parseE2' lhs tokens = (lhs, tokens)

-- T2 -> F2 {T2'}
parseT2 :: [Token] -> (ArithExprList, [Token])
parseT2 tokens =
  let (lhs, rest) = parseF2 tokens
  in parseT2' lhs rest

-- T2' -> ("*" | "/" | "%") F2 {T2'}
-- T2' -> epsilon
parseT2' :: ArithExprList -> [Token] -> (ArithExprList, [Token])
parseT2' lhs (TokTimes : tokens) =
  let (rhs, rest) = parseF2 tokens
  in
    case lhs of
      Mul es -> parseT2' (Mul (es ++ [rhs])) rest
      _ -> parseT2' (Mul [lhs, rhs]) rest
parseT2' lhs (TokDiv : tokens) =
  let (rhs, rest) = parseF2 tokens
  in
    case lhs of
      Div es -> parseT2' (Div (es ++ [rhs])) rest
      _ -> parseT2' (Div [lhs, rhs]) rest
parseT2' lhs (TokMod : tokens) =
  let (rhs, rest) = parseF2 tokens
  in
    case lhs of
      Mod es -> parseT2' (Mod (es ++ [rhs])) rest
      _ -> parseT2' (Mod [lhs, rhs]) rest
parseT2' lhs tokens = (lhs, tokens)

-- F2 -> P2 {F2'}
parseF2 :: [Token] -> (ArithExprList, [Token])
parseF2 tokens =
  let (lhs, rest) = parseP2 tokens
  in parseF2' lhs rest

-- F2' -> "^" F2
-- F2' -> epsilon
parseF2' :: ArithExprList -> [Token] -> (ArithExprList, [Token])
parseF2' base (TokExp : tokens) =
  let (exp, rest) = parseF2 tokens
  in (Exp base exp, rest)
parseF2' lhs tokens = (lhs, tokens)

-- P2 -> <Var>
-- P2 -> <Const>
-- P2 -> <Val>
-- P2 -> "(" E2 ")"
-- P2 -> "-" T2
parseP2 :: [Token] -> (ArithExprList, [Token])
parseP2 [] = error "Token expected"
parseP2 (tok : tokens) =
  case tok of
    TokVariable str -> (Var str, tokens)
    TokConstant str -> (Const str, tokens)
    TokIntValue n -> (Val n, tokens)
    TokLpar ->
      let (exp, (next : rest)) = parseE2 tokens
      in
        if next /= TokRpar
          then error "Missing right parenthesis"
          else (exp, rest)
    TokMinus ->
      let (exp, rest) = parseT2 tokens
      in (Min exp, rest)
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
      TokLT -> (lhs :<: rhs, rest)
      TokLEQ -> (lhs :<=: rhs, rest)
      TokEQ -> (lhs :=: rhs, rest)
      TokGEQ -> (lhs :>=: rhs, rest)
      TokGT -> (lhs :>: rhs, rest)
      TokNEQ -> (lhs :<>: rhs, rest)
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
  in parseC' (lhs :&: rhs) rest
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
    pushNot 1 (lhs :&: rhs) = pushNot 1 lhs :|: pushNot 1 rhs
    pushNot 1 (lhs :|: rhs) = pushNot 1 lhs :&: pushNot 1 rhs
    pushNot 0 (lhs :|: rhs) = pushNot 0 lhs :|: pushNot 0 rhs
    pushNot 0 (lhs :&: rhs) = pushNot 0 lhs :&: pushNot 0 rhs
    pushNot 0 (Not e) = pushNot 1 e
    pushNot 0 e = e

distributeOrOverAnd :: BoolExpression -> BoolExpression
distributeOrOverAnd ((l :&: r) :|: rhs) = (l :|: rhs) :&: (r :|: rhs)
distributeOrOverAnd (lhs :|: (l :&: r)) = (lhs :|: l) :&: (lhs :|: r)
distributeOrOverAnd (lhs :&: rhs) = distributeOrOverAnd lhs :&: distributeOrOverAnd rhs
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
acceptSemicolon (TokEnd:xs) = TokEnd:xs
acceptSemicolon _ = error "Error: expected semicolon"

parseAssignments :: String -> [Assignment]
parseAssignments str = parseAs (lexer str)
  where parseAs [TokEnd] = []
        parseAs tokens = ass:parseAs (acceptSemicolon toks)
          where (ass, toks) = parseA tokens

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
  in (Assign var exp, rest)
parseA (tok : _) = error ("Syntax error: " ++ show tok)

--------------------------------------------------------------------------
-- Weakest precondition                                                 --
--------------------------------------------------------------------------

wpArithExp :: Assignment -> ArithExpression -> ArithExpression
wpArithExp ass (lhs :+: rhs) = wpArithExp ass lhs :+: wpArithExp ass rhs
wpArithExp ass (lhs :-: rhs) = wpArithExp ass lhs :-: wpArithExp ass rhs
wpArithExp ass (lhs :*: rhs) = wpArithExp ass lhs :*: wpArithExp ass rhs
wpArithExp ass (lhs :/: rhs) = wpArithExp ass lhs :/: wpArithExp ass rhs
wpArithExp ass (lhs :%: rhs) = wpArithExp ass lhs :%: wpArithExp ass rhs
wpArithExp ass (lhs :^: rhs) = wpArithExp ass lhs :^: wpArithExp ass rhs
wpArithExp ass (UnaryMinus exp) = UnaryMinus (wpArithExp ass exp)
wpArithExp (Assign var e) (Variable str) =
  if str == var
  then e
  else Variable str
wpArithExp ass exp = exp

wpRel :: Assignment -> Relation -> Relation
wpRel ass (lhs :<: rhs) = wpArithExp ass lhs :<: wpArithExp ass rhs
wpRel ass (lhs :<=: rhs) = wpArithExp ass lhs :<=: wpArithExp ass rhs
wpRel ass (lhs :=: rhs) = wpArithExp ass lhs :=: wpArithExp ass rhs
wpRel ass (lhs :>=: rhs) = wpArithExp ass lhs :>=: wpArithExp ass rhs
wpRel ass (lhs :>: rhs) = wpArithExp ass lhs :>: wpArithExp ass rhs
wpRel ass (lhs :<>: rhs) = wpArithExp ass lhs :<>: wpArithExp ass rhs

wp0 :: Assignment -> BoolExpression -> BoolExpression
wp0 ass (lhs :&: rhs) = wp0 ass lhs :&: wp0 ass rhs
wp0 ass (lhs :|: rhs) = wp0 ass lhs :|: wp0 ass rhs
wp0 ass (lhs :->: rhs) = wp0 ass lhs :->: wp0 ass rhs
wp0 ass (lhs :<-: rhs) = wp0 ass lhs :<-: wp0 ass rhs
wp0 ass (lhs :==: rhs) = wp0 ass lhs :==: wp0 ass rhs
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
pushMinus (lhs :+: rhs) = pushMinus lhs :+: pushMinus rhs
pushMinus (lhs :-: rhs) = pushMinus lhs :+: pushMinus rhs
pushMinus (lhs :*: rhs) = pushMinus lhs :*: rhs
pushMinus (lhs :/: rhs) = pushMinus lhs :/: rhs
pushMinus (lhs :%: rhs) = pushMinus lhs :%: rhs
pushMinus (lhs :^: rhs) = pushMinus lhs :^: rhs
pushMinus (UnaryMinus exp) = exp
pushMinus (IntValue n) = UnaryMinus (IntValue n)
pushMinus (Variable str) = UnaryMinus (Variable str)
pushMinus (Constant str) = UnaryMinus (Constant str)

eliminateMinus :: ArithExpression -> ArithExpression
eliminateMinus (lhs :-: rhs) =
  let (lhs', rhs') = (eliminateMinus lhs, pushMinus (eliminateMinus rhs))
  in lhs' :+: rhs'
eliminateMinus (lhs :+: rhs) = eliminateMinus lhs :+: eliminateMinus rhs
eliminateMinus (lhs :*: rhs) = eliminateMinus lhs :*: eliminateMinus rhs
eliminateMinus (lhs :/: rhs) = eliminateMinus lhs :/: eliminateMinus rhs
eliminateMinus (lhs :%: rhs) = eliminateMinus lhs :%: eliminateMinus rhs
eliminateMinus (lhs :^: rhs) = eliminateMinus lhs :^: eliminateMinus rhs
eliminateMinus (UnaryMinus exp) = pushMinus (eliminateMinus exp)
eliminateMinus exp = exp

--data ArithExprList = Add [ArithExprList]
--                   | Mul [ArithExprList]
--                   | Div [ArithExprList]
--                   | Mod [ArithExprList]
--                   | Exp ArithExprList ArithExprList
--                   | Min ArithExprList
--                   | Val Integer
--                   | Var String
--                   | Const String

firstLawOfExponents :: ArithExprList -> ArithExprList
firstLawOfExponents (Add es) = Add (firstLawOfExponents' es)
firstLawOfExponents (Mul es) =
  let (e:es') = applyFirstLaw es
  in
    case es' of
      [] -> e
      _ -> Mul (e:es')
firstLawOfExponents (Div es) = Div (firstLawOfExponents' es)
firstLawOfExponents (Mod es) = Mod (firstLawOfExponents' es)
firstLawOfExponents (Exp base exp) = Exp (firstLawOfExponents base) (firstLawOfExponents exp)
firstLawOfExponents (Min exp) = Min (firstLawOfExponents exp)
firstLawOfExponents e = e

firstLawOfExponents' :: [ArithExprList] -> [ArithExprList]
firstLawOfExponents' [] = []
firstLawOfExponents' (e:es) = firstLawOfExponents e : firstLawOfExponents' es

applyFirstLaw :: [ArithExprList] -> [ArithExprList]
applyFirstLaw [] = []
applyFirstLaw (e:es) =
  let rest = applyFirstLaw' e es
  in
    if rest == es ++ [e]
      then firstLawOfExponents e : applyFirstLaw es
      else applyFirstLaw rest

applyFirstLaw' :: ArithExprList -> [ArithExprList] -> [ArithExprList]
applyFirstLaw' (Exp base exp) ((Exp base2 exp2):rest) =
  if base == base2
    then case exp of
      Add es -> applyFirstLaw' (Exp base (Add (es ++ [exp2]))) rest
      _ -> applyFirstLaw' (Exp base (Add [exp, exp2])) rest
    else Exp base2 exp2 : applyFirstLaw' (Exp base exp) rest
applyFirstLaw' (Exp base exp) (e:rest) =
  if base == e
    then case exp of
      Add es -> applyFirstLaw' (Exp base (Add (es ++ [Val 1]))) rest
      _ -> applyFirstLaw' (Exp base (Add [exp, Val 1])) rest
    else e : applyFirstLaw' (Exp base exp) rest
applyFirstLaw' e ((Exp base exp):rest) =
  if e == base
    then case exp of
      Add es -> applyFirstLaw' (Exp base (Add (es ++ [Val 1]))) rest
      _ -> applyFirstLaw' (Exp base (Add [exp, Val 1])) rest
    else Exp base exp : applyFirstLaw' e rest
applyFirstLaw' e (base:rest) =
  if e == base
    then applyFirstLaw' (Exp e (Val 2)) rest
    else base : applyFirstLaw' e rest
applyFirstLaw' e [] = [e]

secondLawOfExponents :: ArithExprList -> ArithExprList
secondLawOfExponents (Add es) = Add (secondLawOfExponents' es)
secondLawOfExponents (Mul es) = Mul (secondLawOfExponents' es)
secondLawOfExponents (Div es) = Div (secondLawOfExponents' es)
secondLawOfExponents (Mod es) = Mod (secondLawOfExponents' es)
secondLawOfExponents (Exp base exp) = applySecondLaw (secondLawOfExponents base) (secondLawOfExponents exp)
secondLawOfExponents (Min exp) = Min (secondLawOfExponents exp)
secondLawOfExponents e = e

secondLawOfExponents' :: [ArithExprList] -> [ArithExprList]
secondLawOfExponents' [] = []
secondLawOfExponents' (e:es) = secondLawOfExponents e : secondLawOfExponents' es

applySecondLaw :: ArithExprList -> ArithExprList -> ArithExprList
applySecondLaw (Exp base exp) exp2 =
  case exp of
    Mul es -> Exp base (Mul (es ++ [exp2]))
    _ -> Exp base (Mul [exp, exp2])
applySecondLaw base exp = Exp base exp

--------------------------------------------------------------------------
-- Evaluation                                                           --
--------------------------------------------------------------------------

type Valuation = (String, Integer)

getValue :: String -> [Valuation] -> Maybe Integer
getValue str [] = Nothing
getValue str ((s, v):vals) = if str == s then Just v else getValue str vals

setValue :: String -> Integer -> [Valuation] -> [Valuation]
setValue str n [] = [(str, n)]
setValue str n ((s, v):vals) = if str == s then (s, n):vals else (s, v):setValue str n vals

popValue :: String -> [Valuation] -> [Valuation]
popValue str [] = []
popValue str ((s, v):vals) = if str == s then vals else (s, v):popValue str vals

sumValuations :: [Valuation] -> [Valuation] -> [Valuation]
sumValuations [] bs = bs
sumValuations (v:as) bs = sumValuations as (sumValuations' v bs)

sumValuations' :: Valuation -> [Valuation] -> [Valuation]
sumValuations' (str, n) [] = [(str, n)]
sumValuations' (str, n) ((s, v):vals) =
  if str == s
    then (s, n+v):vals
    else sumValuations' (str, n) vals

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
evalArithExp (lhs :+: rhs) vals = evalArithExp lhs vals + evalArithExp rhs vals
evalArithExp (lhs :-: rhs) vals = evalArithExp lhs vals - evalArithExp rhs vals
evalArithExp (lhs :*: rhs) vals = evalArithExp lhs vals * evalArithExp rhs vals
evalArithExp (lhs :/: rhs) vals = evalArithExp lhs vals `div` evalArithExp rhs vals
evalArithExp (lhs :%: rhs) vals = evalArithExp lhs vals `mod` evalArithExp rhs vals
evalArithExp (lhs :^: rhs) vals = evalArithExp lhs vals ^ evalArithExp rhs vals

evalRelation :: Relation -> [Valuation] -> Bool
evalRelation (lhs :<: rhs) vals = evalArithExp lhs vals < evalArithExp rhs vals
evalRelation (lhs :=: rhs) vals = evalArithExp lhs vals == evalArithExp rhs vals
evalRelation (lhs :<=: rhs) vals = evalArithExp lhs vals <= evalArithExp rhs vals
evalRelation (lhs :>=: rhs) vals = evalRelation (rhs :<=: lhs) vals
evalRelation (lhs :>: rhs) vals = evalRelation (rhs :<: lhs) vals
evalRelation (lhs :<>: rhs) vals = not (evalRelation (rhs :=: lhs) vals)

evalBoolExpression :: BoolExpression -> [Valuation] -> Bool
evalBoolExpression (lhs :&: rhs) vals = evalBoolExpression lhs vals && evalBoolExpression rhs vals
evalBoolExpression (lhs :|: rhs) vals = evalBoolExpression lhs vals || evalBoolExpression rhs vals
evalBoolExpression (lhs :->: rhs) vals = not (evalBoolExpression lhs vals) || evalBoolExpression rhs vals
evalBoolExpression (lhs :<-: rhs) vals = not (evalBoolExpression rhs vals) || evalBoolExpression lhs vals
evalBoolExpression (lhs :==: rhs) vals = evalBoolExpression lhs vals == evalBoolExpression rhs vals
evalBoolExpression (Not exp) vals = not (evalBoolExpression exp vals)
evalBoolExpression (Compare rel) vals = evalRelation rel vals
evalBoolExpression (BoolConst c) vals = c

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
