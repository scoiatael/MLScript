module Interpreter where

import Syntax

import qualified Data.Map as Map

data ValueType = Value (Either Float String) | Fun ([ValueType] -> ValueType) | Blank
data State = State (Map.Map Name (ValueType, State))

blankState :: State
blankState = State Map.empty

updateState (State s) n a = State $ Map.insert n a s

applyOp o e1 e2 = case (e1, e2) of
                     (Value (Left f1), Value (Left f2)) -> case o of
                                                            Plus -> Value $ Left (f1 + f2)
                                                            Minus -> Value $ Left (f1 - f2)
                                                            Times -> Value $ Left (f1 * f2)
                                                            Divide -> Value $ Left (f1 / f2)
                     _ -> error "Bad arguments to applyOp"

substitute state (arg, actual_arg) = case arg of
                                       Var v -> updateState state v (actual_arg, state)
                                       _ -> error "Non-variable value in function body"
createFun state args body = Fun $ \actual_args -> runEvaluation (foldl substitute state $ zip args actual_args) Blank [body]

runEvaluation _ acc [] = acc
runEvaluation state _ (e:es) = case e  of
                                Float x -> runEvaluation state (Value (Left x)) es
                                BinOp o e1 e2 -> runEvaluation state (Value $ applyOp o (runEvaluation state Blank [e1]) (runEvaluation state Blank [e2])) es
                                Var n -> let (new_expr, old_state) =  Map.lookup n state in runEvaluation state (runEvaluation old_state Blank [new_expr]) es
                                Call fun args -> case runEvaluation state Blank [fun] of
                                                    Fun f -> runEvaluation state (f (map (runEvaluation state))) es
                                                    _ -> error "Application of value to value"
                                Definition n expr -> runEvaluation (updateState state n (expr, state)) (Value $ Right n) es
                                Function args body -> let fun = Fun $ createFun state args body in runEvaluation state fun es
                                _ -> error "NYI"
                                -- DataType Name [Constructor]
                                -- Switch Expr Name [SwitchExpr]
                                -- Extern Name [Expr]

eval :: Expr -> IO ()
eval e = case runEvaluation blankState e of
         Value v -> case v of
                        Left f -> print f
                        Right s -> print s
         Fun _ -> print "<function>"
         Blank -> print "<blank>"
