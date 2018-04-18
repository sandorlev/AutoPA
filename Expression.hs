import Data.Char

data Operator = Plus | Minus | Times | Div | Mod
  deriving (Show, Eq)

data Token = TokOp Operator
           | TokParen Char
           | TokVariable String
           | TokConst String
           | TokNum Int
  deriving (Eq, Show)

data Expression = SumNode Operator Expression Expression
                | ProdNode Operator Expression Expression
                | NumNode Int
                | VarNode String
                | ConstNode String
  deriving (Show)

showOperator :: Operator -> String
showOperator (Plus) = "+"
showOperator (Minus) = "-"
showOperator (Times) = "*"
showOperator (Div) = "/"
showOperator (Mod) = "%"

showExpression :: Expression -> String
showExpression (SumNode op lhs rhs) = (showExpression lhs) ++ (showOperator op) ++ (showExpression rhs)
showExpression (ProdNode op lhs rhs) = (showExpression lhs) ++ (showOperator op) ++ (showExpression rhs)
showExpression (NumNode n) = show n
showExpression (VarNode str) = str
showExpression (ConstNode str) = str

main = print $ showExpression (
  -- x + 7 - 2 / Y
  SumNode Minus
    (SumNode Plus (VarNode "x") (NumNode 7))
    (ProdNode Div (NumNode 2) (ConstNode "Y")))
