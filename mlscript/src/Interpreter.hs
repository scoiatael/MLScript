module Interpreter where

import Syntax

import qualified Data.Map as Map

type Function = [ValueType] -> ValueType
data ValueType = Value Double | Fun Function | Blank
data State = State (Map.Map Name ValueType)

blankState :: State
blankState = State Map.empty

updateState :: State -> Name -> ValueType -> State
updateState (State s) n a = State $ Map.insert n a s

lookupState :: State -> Name -> Maybe ValueType
lookupState (State s) n = Map.lookup n s

applyOp :: Op -> ValueType -> ValueType -> Double
applyOp o e1 e2 = case (e1, e2) of
                     (Value f1, Value f2) -> case o of
                                                            Plus -> f1 + f2
                                                            Minus -> f1 - f2
                                                            Times -> f1 * f2
                                                            Divide -> f1 / f2
                     _ -> error "Bad arguments to applyOp"

substitute :: State -> (Expr, ValueType) -> State
substitute state (arg, actual_arg) = case arg of
                                       Var v -> updateState state v actual_arg
                                       _ -> error "Non-variable value in function body"

createFun :: State -> [Expr] -> Expr -> Function
createFun state args body actual_args = runEvaluation (foldl substitute state $ zip args actual_args) Blank [body]

runEvaluation :: State -> ValueType -> [Expr] -> ValueType
runEvaluation _ acc [] = acc
runEvaluation state _ (e:es) = case e  of
                                Float x -> runEvaluation state (Value x) es
                                BinOp o e1 e2 -> runEvaluation state (Value $ applyOp o (runEvaluation state Blank [e1]) (runEvaluation state Blank [e2])) es
                                Var n -> case lookupState state n of
                                          Just value -> runEvaluation state value es
                                          _ -> error "Undefined variable"
                                Call fun args -> case runEvaluation state Blank [fun] of
                                                    Fun f -> runEvaluation state (f (map (\a -> runEvaluation state Blank [a]) args)) es
                                                    _ -> error "Application of value to value"
                                Definition n expr -> runEvaluation (updateState state n (runEvaluation state Blank [expr])) Blank es
                                Function args body -> let fun = Fun $ createFun state args body in runEvaluation state fun es
                                _ -> error "NYI"
                                -- DataType Name [Constructor]
                                -- Switch Expr Name [SwitchExpr]
                                -- Extern Name [Expr]

eval :: [Expr] -> IO ()
eval es = case runEvaluation blankState Blank es of
          Value v -> print $ "Value: " ++ show v
          Fun _ -> print "<function>"
          Blank -> print "<blank>"
