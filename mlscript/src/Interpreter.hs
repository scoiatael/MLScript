module Interpreter where

import Syntax

import qualified Data.Map as Map

import qualified Control.Monad.Trans.State as State

import Control.Monad.Trans.Except as Except

import Control.Monad.Identity (Identity, runIdentity)

import Control.Monad.Trans (lift)

type Function = [ValueType] -> Interpreter ()
data ValueType = Value Double | Fun Function | Blank
type State = Map.Map Name ValueType

type InterpreterError = String

type Interpreter = State.StateT ValueType (State.StateT State (Except.ExceptT InterpreterError Identity))

runInterpreter :: Interpreter a -> Either InterpreterError a
runInterpreter i = runIdentity $ Except.runExceptT $ flip State.evalStateT Map.empty $ State.evalStateT i Blank

lift2 :: Except.ExceptT InterpreterError Identity a -> Interpreter a
lift2 = lift . lift

temporaryState :: State -> Interpreter a -> Interpreter a
temporaryState s cal = do
                       old_state <- varState
                       lift $ State.put s
                       v <- cal
                       lift $ State.put old_state
                       return v

acc :: Interpreter ValueType
acc = State.get

updateAcc :: ValueType -> Interpreter ()
updateAcc = State.put

varState :: Interpreter State
varState = lift State.get

throw :: InterpreterError -> Interpreter a
throw err = lift2 $ throwE err

lookupVar :: Name -> Interpreter ValueType
lookupVar n = do
                vars <- varState
                case Map.lookup n vars of
                    Just e -> return e
                    _ -> throw "Undefined variable"

updateVar :: Name -> ValueType -> Interpreter ()
updateVar n v = lift $ State.modify (Map.insert n v)

applyOp :: Op -> ValueType -> ValueType -> Interpreter ValueType
applyOp o e1 e2 = case (e1, e2) of
                     (Value f1, Value f2) -> return $ Value $
                      case o of
                        Plus -> f1 + f2
                        Minus -> f1 - f2
                        Times -> f1 * f2
                        Divide -> f1 / f2
                     _ -> throw "Bad arguments to binary operator"

substitute :: (Expr, ValueType) -> Interpreter ()
substitute (arg, actual_arg) = case arg of
                                    Var v -> updateVar v actual_arg
                                    _ -> throw "Non-variable value in function body"

createFun :: [Expr] -> Expr -> Interpreter ValueType
createFun args body = do
  state <- varState
  return $ Fun $ \actual_args -> temporaryState state $
                                    do
                                        mapM_  substitute $ zip args actual_args
                                        runEvaluation body

runEvaluation :: Expr -> Interpreter ()
runEvaluation e = case e of
                    Float x -> updateAcc $ Value x
                    BinOp o e1 e2 -> do
                                        v1 <- runEvaluation e1 >> acc
                                        v2 <- runEvaluation e2 >> acc
                                        applyOp o v1 v2 >>= updateAcc
                    Var n -> lookupVar n >>= updateAcc
                    Call fun args -> runEvaluation fun >> acc >>= \acc_value ->
                      case acc_value of
                        Fun f -> mapM (\a -> runEvaluation a >> acc) args >>= f
                        _ -> throw "Application of value to value"
                    Definition n expr -> runEvaluation expr >> acc >>= updateVar n
                    Function args body -> createFun args body >>= updateAcc
                    _ -> throw "NotYetImplmented"
                    -- DataType Name [Constructor]
                    -- Switch Expr Name [SwitchExpr]
                    -- Extern Name [Expr]

eval :: [Expr] -> IO ()
eval es = print $  case runInterpreter ( mapM_ runEvaluation es >> acc) of
                    Right value -> case value of
                                         Value v -> show v
                                         Fun _ -> "<function>"
                                         Blank -> "<blank>"
                    Left err -> "<error: " ++ show err ++ ">"
