module Lambda.Syntax where

type Name = String

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var Name
    | Call Expr [Expr]
    | Definition Name Expr
    | Function [Expr] Expr
    | Switch Expr [SwitchExpr] SwitchDefault
    | Con Int [Expr]
    | Decon Int Expr
    | Extern Name [Expr]
   deriving (Eq, Ord, Show)

data Constructor
    = Constructor Name Int
      deriving (Eq, Ord, Show)

type SwitchDefault = Maybe Expr

data SwitchExpr
     = SwitchExpr [Int] Expr
       deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
      deriving (Eq, Ord, Show)
