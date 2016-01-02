module Lambda.Syntax where

import Util (Op, Name)

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var Name
    | Call Expr [Expr]
    | Definition Name Expr
    | Function [Expr] Expr
    | Switch Expr [SwitchExpr] SwitchDefault
    | Con Int [Expr]
    | Decon [Name] Expr
    | Extern Name [Expr]
   deriving (Eq, Ord, Show)

type SwitchDefault = Maybe Expr

data SwitchExpr
     = SwitchExpr [Int] Expr
       deriving (Eq, Ord, Show)
