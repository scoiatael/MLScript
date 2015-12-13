module Raw.Syntax where

type Name = String

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var Name
    | Call Expr [Expr]
    | Definition Name Expr
    | Function [Expr] Expr
    | Datatype Datatype
    | Switch Expr Name [SwitchExpr]
    | Extern Name [Expr]
   deriving (Eq, Ord, Show)

type Datatype = (Name, [Constructor])

data Constructor
    = Constructor Name Int
      deriving (Eq, Ord, Show)

data SwitchExpr
     = SwitchExpr Name [Name] Expr
       deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
      deriving (Eq, Ord, Show)
