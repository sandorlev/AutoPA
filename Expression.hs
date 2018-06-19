import Data.Char
import Data.List
import System.Random

--------------------------------------------------------------------------
-- Knowledge Base                                                       --
--------------------------------------------------------------------------

knowledgeBase = [
       "#x=#y -> #y=#x"
     ]


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
           | TokBoundVariable String
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
  | c == '#' && cs/=[] && isAlpha(head cs) = (TokBoundVariable (takeWhile isAlphaNum cs)) : lexer (dropWhile isAlphaNum cs)
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
  | otherwise = error ("Lexical error: Invalid character '" ++ [c] ++ "'")

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
                     | Constant String
                     | Variable String
                     | BoundVariable String

parens :: ArithExpression -> String
parens (IntValue n) = show n
parens (Variable str) = str
parens (BoundVariable str) = '#':str
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

instance Eq ArithExpression where
  l0 :+: r0 == l1 :+: r1             = l0 == l1 && r0 == r1
  l0 :-: r0 == l1 :-: r1             = l0 == l1 && r0 == r1
  l0 :*: r0 == l1 :*: r1             = l0 == l1 && r0 == r1
  l0 :/: r0 == l1 :/: r1             = l0 == l1 && r0 == r1
  l0 :%: r0 == l1 :%: r1             = l0 == l1 && r0 == r1
  l0 :^: r0 == l1 :^: r1             = l0 == l1 && r0 == r1
  UnaryMinus e0 == UnaryMinus e1     = e0 == e1
  IntValue x == IntValue y           = x == y
  Constant x == Constant y           = x == y
  Variable x == Variable y           = x == y
  BoundVariable x == BoundVariable y = x == y
  e0 == e1                           = False

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
      in (lhs :^: rhs, rest)
    _ -> (lhs, (tok : tokens))

-- P -> <Var>
-- P -> <BoundVar>
-- P -> <Const>
-- P -> <Integer>
-- P -> "(" E ")"
-- P -> "-" T
parseP :: [Token] -> (ArithExpression, [Token])
parseP [] = error "Token expected"
parseP (tok : tokens) =
  case tok of
    (TokVariable str) -> (Variable str, tokens)
    (TokBoundVariable str) -> (BoundVariable str, tokens)
    (TokConstant str) -> (Constant str, tokens)
    (TokIntValue n) -> (IntValue n, tokens)
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

instance Eq Relation where
  l0 :<:  r0 == l1 :<:  r1 = l0 == l1 && r0 == r1
  l0 :<=: r0 == l1 :<=: r1 = l0 == l1 && r0 == r1
  l0 :=:  r0 == l1 :=:  r1 = l0 == l1 && r0 == r1
  l0 :>=: r0 == l1 :>=: r1 = l0 == l1 && r0 == r1
  l0 :>:  r0 == l1 :>:  r1 = l0 == l1 && r0 == r1
  l0 :<>: r0 == l1 :<>: r1 = l0 == l1 && r0 == r1
  e0 == e1                 = False

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

instance Eq BoolExpression where
  l0 :&: r0 == l1 :&: r1       = l0 == l1 && r0 == r1
  l0 :|: r0 == l1 :|: r1       = l0 == l1 && r0 == r1
  l0 :->: r0 == l1 :->: r1     = l0 == l1 && r0 == r1
  l0 :<-: r0 == l1 :<-: r1     = l0 == l1 && r0 == r1
  l0 :==: r0 == l1 :==: r1     = l0 == l1 && r0 == r1
  Not e0 == Not e1             = e0 == e1
  BoolConst x == BoolConst y   = x == y
  Compare rel0 == Compare rel1 = rel0 == rel1
  e0 == e1                     = False

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
-- Conjunctive Normal From
--------------------------------------------------------------------------

eliminateEquiv :: BoolExpression -> BoolExpression
eliminateEquiv (lhs :==: rhs) = (l :->: r) :&: (r :->: l)
  where (l,r)=(eliminateEquiv lhs, eliminateEquiv rhs)
eliminateEquiv (lhs :<-: rhs) = eliminateEquiv lhs :<-: eliminateEquiv rhs
eliminateEquiv (Not e) = Not (eliminateEquiv e)
eliminateEquiv e = e

eliminateImplication :: BoolExpression -> BoolExpression
eliminateImplication (lhs :->: rhs) = (Not l) :|: r
  where (l,r)=(eliminateImplication lhs, eliminateImplication rhs)
eliminateImplication (lhs :<-: rhs) = eliminateImplication (rhs :->: lhs)
eliminateImplication (lhs :&: rhs) = eliminateImplication lhs :&: eliminateImplication rhs
eliminateImplication (lhs :|: rhs) = eliminateImplication lhs :|: eliminateImplication rhs
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

{- this does not work yet
distributeOrOverAnd :: BoolExpression -> BoolExpression
distributeOrOverAnd (Or (And l r) rhs) = And (Or l rhs) (Or r rhs)
distributeOrOverAnd (Or lhs (And l r)) = And (Or lhs l) (Or lhs r)
distributeOrOverAnd (And lhs rhs) = And (distributeOrOverAnd lhs) (distributeOrOverAnd rhs)
distributeOrOverAnd e = e
-}

distributeOrOverAnd :: BoolExpression -> BoolExpression
distributeOrOverAnd exp = if done then e else (distributeOrOverAnd e)
  where (done,e)=distribute exp
        distribute :: BoolExpression -> (Bool, BoolExpression)
        distribute ((l :&: r) :|: rhs) = (False, (l :|: rhs) :&: (r :|: rhs))
        distribute (lhs :|: (l :&: r)) = (False, (lhs :|: l) :&: (lhs :|: r))
        distribute (lhs :|: rhs) = let ((bl,l),(br,r))=(distribute lhs,distribute rhs) in (bl&&br,l :|: r)
        distribute (lhs :&: rhs) = let ((bl,l),(br,r))=(distribute lhs,distribute rhs) in (bl&&br,l :&: r)
        distribute e = (True, e)

cnf :: BoolExpression -> BoolExpression
cnf  = distributeOrOverAnd.pushAndEliminateNot.eliminateImplication.eliminateEquiv

cnfstr :: String -> BoolExpression
cnfstr s = cnf $ parseBoolExpression s

type Clauses = [BoolExpression]

clauses :: BoolExpression -> [Clauses]
clauses e = clauses' $ cnf e

clauses' :: BoolExpression -> [Clauses]
clauses' (lhs :&: rhs) = clauses' lhs ++ clauses' rhs
clauses' e = [clause e]
  where clause :: BoolExpression -> Clauses
        clause (lhs :|: rhs) = clause lhs ++ clause rhs
        clause e = [e]

dropFromClauses :: BoolExpression -> Clauses -> Clauses
dropFromClauses _ [] = []
dropFromClauses exp (c:cs)
  | exp == c = cs
  | otherwise = c : dropFromClauses exp cs

eqClauses :: Clauses -> Clauses -> Bool
eqClauses [] [] = True
eqClauses (a:as) bs =
  let bs' = dropFromClauses a bs
  in
    if bs == bs'
    then False
    else eqClauses as bs'
eqClauses x y = False

subsetOf :: [Clauses] -> [Clauses] -> Bool
subsetOf [] _ = True
subsetOf _ [] = False
subsetOf (a:as) bs = subsetOf' a bs && subsetOf as bs
  where subsetOf' :: Clauses -> [Clauses] -> Bool
        subsetOf' [] _ = True
        subsetOf' _ [] = False
        subsetOf' a (c:cs) = eqClauses a c || subsetOf' a cs

clausesstr :: String -> [Clauses]
clausesstr s = clauses $ parseBoolExpression s

resolution :: [Clauses] -> Bool
resolution [] = False
resolution cs =
  let resolvents = nub $ resolveClauses cs
  in
    if [] `elem` resolvents then True
    else if subsetOf resolvents cs then False
    else resolution (resolvents ++ cs)

resolveClauses :: [Clauses] -> [Clauses]
resolveClauses [] = []
resolveClauses (c:cs) = resolveClauses' c cs ++ resolveClauses cs

resolveClauses' :: Clauses -> [Clauses] -> [Clauses]
resolveClauses' _ [] = []
resolveClauses' a (b:bs) =
  let new  = dropIdenticals [a ++ b]
      new' = dropOpposites new
  in
    if new == new'
    then resolveClauses' a bs
    else new' ++ resolveClauses' a bs

dropIdenticals :: [Clauses] -> [Clauses]
dropIdenticals [] = []
dropIdenticals (c:cs) =
  let c'  = nub c
      cs' = dropIdenticals' c' cs
  in c' : dropIdenticals cs'
  where dropIdenticals' :: Clauses -> [Clauses] -> [Clauses]
        dropIdenticals' a [] = []
        dropIdenticals' a (b:bs) =
          if eqClauses a b
          then dropIdenticals' a bs
          else b : dropIdenticals' a bs

countOpposites :: [Clauses] -> Integer
countOpposites [] = 0
countOpposites (c:cs) = countOpposites' c + countOpposites cs
  where countOpposites' :: Clauses -> Integer
        countOpposites' [] = 0
        countOpposites' (c:cs) =
          let opposite = cnf $ Not c
              cs' = dropFromClauses opposite cs
          in
            if cs == cs'
            then countOpposites' cs
            else 1 + countOpposites' cs

dropOpposites :: [Clauses] -> [Clauses]
dropOpposites [] = []
dropOpposites cs = dropOpposites' cs (countOpposites cs)
  where dropOpposites' :: [Clauses] -> Integer -> [Clauses]
        dropOpposites [] _ = []
        dropOpposites' cs n
          | n < 1 = []
          | otherwise = dropNthOpposites cs (n-1) ++ dropOpposites' cs (n-1)

dropNthOpposites :: [Clauses] -> Integer -> [Clauses]
dropNthOpposites [] _ = []
dropNthOpposites cs n = map (dropNthOpposites' n) cs
  where dropNthOpposites' :: Integer -> Clauses -> Clauses
        dropNthOpposites' _ [] = []
        dropNthOpposites' n (c:cs) =
          let opposite = cnf $ Not c
              cs' = dropFromClauses opposite cs
          in
            if cs == cs' then c : dropNthOpposites' n cs
            else if n < 1 then cs'
            else c : dropNthOpposites' (n-1) cs

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
  else (Variable str)
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
-- Simplify                                                             --
--------------------------------------------------------------------------

pushMinus :: ArithExpression -> ArithExpression
pushMinus (lhs :+: rhs) = (pushMinus lhs) :+: (pushMinus rhs)
pushMinus (lhs :-: rhs) = (pushMinus lhs) :-: (pushMinus rhs)
pushMinus (lhs :*: rhs) = (pushMinus lhs) :*: rhs
pushMinus (lhs :/: rhs) = (pushMinus lhs) :/: rhs
pushMinus (lhs :%: rhs) = (pushMinus lhs) :%: rhs
pushMinus (lhs :^: rhs) = (pushMinus lhs) :^: rhs
pushMinus (UnaryMinus exp) = exp
pushMinus (IntValue n) = UnaryMinus (IntValue n)
pushMinus (Variable str) = UnaryMinus (Variable str)
pushMinus (Constant str) = UnaryMinus (Constant str)

randomList :: Integer -> Integer -> [Integer]
randomList lwb upb = map (\x -> x `mod` (upb+1-lwb) + lwb) rl
  where
    rl = map fst $ scanl (\(r, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()

splitInfiniteList :: Int -> [a] -> [[a]]
splitInfiniteList n xs = take n xs:splitInfiniteList n (drop n xs)

randomSets :: Int -> Int -> Integer -> Integer -> [[Integer]]
randomSets n len lwb upb = take n (splitInfiniteList len (randomList lwb upb))

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
evalAExpression (lhs :+: rhs) vals = (evalAExpression lhs vals) + (evalAExpression rhs vals)
evalAExpression (lhs :-: rhs) vals = (evalAExpression lhs vals) - (evalAExpression rhs vals)
evalAExpression (lhs :*: rhs) vals = (evalAExpression lhs vals) * (evalAExpression rhs vals)
evalAExpression (lhs :/: rhs) vals = (evalAExpression lhs vals) `div` (evalAExpression rhs vals)
evalAExpression (lhs :%: rhs) vals = (evalAExpression lhs vals) `mod` (evalAExpression rhs vals)
evalAExpression (lhs :^: rhs) vals = (evalAExpression lhs vals) ^ (evalAExpression rhs vals)

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
evalBoolExpression (lhs :&: rhs) vals = (evalBoolExpression lhs vals) && (evalBoolExpression rhs vals)
evalBoolExpression (lhs :|: rhs) vals = (evalBoolExpression lhs vals) || (evalBoolExpression rhs vals)
evalBoolExpression (Not exp) vals = not (evalBoolExpression exp vals)
evalBoolExpression (Compare rel) vals = evalRelation rel vals

testHoareTriple :: [Valuation] -> String -> String -> Bool
testHoareTriple vals assStr expStr =
  evalBoolExpression (parseBoolExpression expStr) (runSimulation assStr vals)

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

--------------------------------------------------------------------------
-- Unify                                                                --
--------------------------------------------------------------------------

type Unifier =  [(String, ArithExpression)]

occurCheck :: String -> ArithExpression -> Bool
occurCheck var (BoundVariable e) = (var==e)
occurCheck var (UnaryMinus e) = occurCheck var e
occurCheck var (l :+: r) = occurCheck var l || occurCheck var r
occurCheck var (l :-: r) = occurCheck var l || occurCheck var r
occurCheck var (l :*: r) = occurCheck var l || occurCheck var r
occurCheck var (l :/: r) = occurCheck var l || occurCheck var r
occurCheck var (l :%: r) = occurCheck var l || occurCheck var r
occurCheck var (l :^: r) = occurCheck var l || occurCheck var r
occurCheck var e = False

findUnification :: String -> Unifier -> Bool
findUnification _ [] = False
findUnification var ((x,e):us) = (var==x) || findUnification var us

getUnification :: String -> Unifier -> ArithExpression
getUnification var [] = (BoundVariable var)
getUnification var ((x,e):us) = if var==x then e else getUnification var us

isBoundVar :: ArithExpression -> Bool
isBoundVar (BoundVariable s) = True
isBoundVar e = False

getBoundVarName :: ArithExpression -> String
getBoundVarName (BoundVariable s) = s

unifyVar :: String -> ArithExpression -> Maybe Unifier -> Maybe Unifier
unifyVar var x (Just theta) =
  if findUnification var theta
  then mguAexp (getUnification var theta) x (Just theta)
  else if (isBoundVar x) && (findUnification (getBoundVarName x) theta)
       then mguAexp (BoundVariable var) (getUnification (getBoundVarName x) theta) (Just theta)
       else if occurCheck var x then Nothing else Just ((var,x):theta)
       
mguAexp :: ArithExpression -> ArithExpression -> Maybe Unifier -> Maybe Unifier
mguAexp e0 e1 Nothing = Nothing
mguAexp (BoundVariable x) (BoundVariable y) theta =
  if (x == y) then theta else unifyVar x (BoundVariable y) theta
mguAexp (BoundVariable x) y theta = unifyVar x y theta
mguAexp x (BoundVariable y) theta = unifyVar y x theta
mguAexp (Constant x) (Constant y) theta = if x == y then theta else Nothing
mguAexp (IntValue x) (IntValue y) theta = if x == y then theta else Nothing
mguAexp (Variable x) (Variable y) theta = if x == y then theta else Nothing
mguAexp (UnaryMinus e0) (UnaryMinus e1) theta = mguAexp e0 e1 theta
mguAexp (l0 :+: r0) (l1 :+: r1) theta = mguAexp r0 r1 (mguAexp l0 l1 theta)
mguAexp (l0 :-: r0) (l1 :-: r1) theta = mguAexp r0 r1 (mguAexp l0 l1 theta)
mguAexp (l0 :*: r0) (l1 :*: r1) theta = mguAexp r0 r1 (mguAexp l0 l1 theta)
mguAexp (l0 :/: r0) (l1 :/: r1) theta = mguAexp r0 r1 (mguAexp l0 l1 theta)
mguAexp (l0 :%: r0) (l1 :%: r1) theta = mguAexp r0 r1 (mguAexp l0 l1 theta)
mguAexp (l0 :^: r0) (l1 :^: r1) theta = mguAexp r0 r1 (mguAexp l0 l1 theta)
mguAexp e0 e1 theta = Nothing

mguRel :: Relation -> Relation -> Maybe Unifier -> Maybe Unifier
mguRel e0 e1 Nothing = Nothing
mguRel (l0 :<: l1) (r0 :<: r1) theta   = mguAexp l1 r1 (mguAexp l0 r0 theta)
mguRel (l0 :<=: l1) (r0 :<=: r1) theta = mguAexp l1 r1 (mguAexp l0 r0 theta)
mguRel (l0 :=: l1) (r0 :=: r1) theta   = mguAexp l1 r1 (mguAexp l0 r0 theta)
mguRel (l0 :>=: l1) (r0 :>=: r1) theta = mguAexp l1 r1 (mguAexp l0 r0 theta)
mguRel (l0 :>: l1) (r0 :>: r1) theta   = mguAexp l1 r1 (mguAexp l0 r0 theta)
mguRel (l0 :<>: l1) (r0 :<>: r1) theta = mguAexp l1 r1 (mguAexp l0 r0 theta)
mguRel _ _ _ = Nothing

mguBexp :: BoolExpression -> BoolExpression -> Maybe Unifier -> Maybe Unifier
mguBexp e0 e1 Nothing = Nothing
mguBexp (l0 :&: l1) (r0 :&: r1) theta = mguBexp l1 r1 (mguBexp l0 r0 theta)
mguBexp (l0 :|: l1) (r0 :|: r1) theta = mguBexp l1 r1 (mguBexp l0 r0 theta)
mguBexp (l0 :->: l1) (r0 :->: r1) theta = mguBexp l1 r1 (mguBexp l0 r0 theta)
mguBexp (l0 :<-: l1) (r0 :<-: r1) theta = mguBexp l1 r1 (mguBexp l0 r0 theta)
mguBexp (l0 :==: l1) (r0 :==: r1) theta = mguBexp l1 r1 (mguBexp l0 r0 theta)
mguBexp (Not e0) (Not e1) theta = mguBexp e0 e1 theta
mguBexp (BoolConst e0) (BoolConst e1) theta = if e0==e1 then theta else Nothing
mguBexp (Compare e0) (Compare e1) theta = mguRel e0 e1 theta
mguBexp _ _ _ = Nothing

hasBoundVariable :: ArithExpression -> Bool
hasBoundVariable (BoundVariable e) = True
hasBoundVariable (UnaryMinus e) = hasBoundVariable e
hasBoundVariable (l :+: r) = hasBoundVariable l || hasBoundVariable r
hasBoundVariable (l :-: r) = hasBoundVariable l || hasBoundVariable r
hasBoundVariable (l :*: r) = hasBoundVariable l || hasBoundVariable r
hasBoundVariable (l :/: r) = hasBoundVariable l || hasBoundVariable r
hasBoundVariable (l :%: r) = hasBoundVariable l || hasBoundVariable r
hasBoundVariable (l :^: r) = hasBoundVariable l || hasBoundVariable r
hasBoundVariable e = False

mgu :: BoolExpression -> BoolExpression -> Maybe Unifier -> Maybe Unifier
mgu = mguBexp

sure :: Maybe a -> a
sure (Just x) = x

applyMGU :: Unifier -> ArithExpression -> ArithExpression 
applyMGU theta (BoundVariable v) = let e = getUnification v theta in
   if e /= (BoundVariable v) then applyMGU theta e else e
applyMGU theta (UnaryMinus e) = UnaryMinus (applyMGU theta e)
applyMGU theta (l :+: r) = applyMGU theta l :+: applyMGU theta r
applyMGU theta (l :-: r) = applyMGU theta l :-: applyMGU theta r
applyMGU theta (l :*: r) = applyMGU theta l :*: applyMGU theta r
applyMGU theta (l :/: r) = applyMGU theta l :/: applyMGU theta r
applyMGU theta (l :%: r) = applyMGU theta l :%: applyMGU theta r
applyMGU theta (l :^: r) = applyMGU theta l :^: applyMGU theta r
applyMGU theta e = e

simplifyMGU :: Unifier -> Unifier
simplifyMGU theta = map (\(v,e) -> (v,applyMGU theta e)) theta

testAmgu :: String -> String -> Maybe Unifier
testAmgu s0 s1 = mguAexp (parseArithExpression s0) (parseArithExpression s1) (Just [])

testmgu :: String -> String -> Maybe Unifier
testmgu s0 s1 = mgu (parseBoolExpression s0) (parseBoolExpression s1) (Just [])

chain = sure $ testmgu  "#a+#b+#c+#d<#e"  "#b+#c+#d+#e<0"
--circle= sure $ testmgu  "#a=#b"  "#b=#a"
circle= sure $ testmgu  "#a-#b=0"  "#b-#a=0"

negationOf :: BoolExpression -> BoolExpression -> Bool
negationOf a b = cnf (Not a) == cnf b
