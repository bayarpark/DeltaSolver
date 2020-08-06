module QBF.Lang where

type VarName = String

data Expr
  = Const Bool
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Var VarName
  | Exist VarName Expr
  | Forall VarName Expr
  deriving (Eq, Show)
