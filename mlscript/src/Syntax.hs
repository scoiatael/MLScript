module Syntax where

type Name = String

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var Name
    | Call Expr [Expr]
    | Definition Name Expr
    | Function [Expr] Expr
    | DataType Name [Constructor]
    | Switch Expr Name [SwitchExpr]
    | Extern Name [Expr]
   deriving (Eq, Ord, Show)

data Constructor
    = Con Name Int
      deriving (Eq, Ord, Show)

data SwitchExpr
     = SwitchE Name [Name] Expr
       deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
      deriving (Eq, Ord, Show)
