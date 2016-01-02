module Util where

data Op
    = Plus
    | Minus
    | Times
    | Divide
      deriving (Eq, Ord, Show)

type Name = String
