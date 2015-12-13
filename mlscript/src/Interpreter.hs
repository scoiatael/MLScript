module Interpreter where

import Syntax

import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)

type Function = [Value] -> Interpreter ()

data Value = Value Double | Fun Function | Blank | Con Name [Value]

instance Show Value where
  show value = case value of
                    Value v -> show v
                    Con n v -> n ++ " " ++ show v
                    Fun _ -> "<function>"
                    Blank -> "<void>"

type DatatypeState = Map.Map Name [Constructor]

type VarState = Map.Map Name Value

type InterpreterError = String

type Interpreter = State.StateT Value (State.StateT DatatypeState (State.StateT VarState (Except.ExceptT InterpreterError Identity)))

runInterpreter :: Interpreter a -> Either InterpreterError a
runInterpreter i = runIdentity $ Except.runExceptT $ flip State.evalStateT Map.empty $ flip State.evalStateT Map.empty $ State.evalStateT i Blank

lift2 ::  State.StateT VarState (Except.ExceptT InterpreterError Identity) a -> Interpreter a
lift2 = lift . lift

lift3 :: Except.ExceptT InterpreterError Identity a -> Interpreter a
lift3 = lift2 . lift

temporaryState :: VarState -> Interpreter a -> Interpreter a
temporaryState s cal = do
                       old_state <- varState
                       lift2 $ State.put s
                       v <- cal
                       lift2 $ State.put old_state
                       return v

acc :: Interpreter Value
acc = State.get

updateAcc :: Value -> Interpreter ()
updateAcc = State.put

varState :: Interpreter VarState
varState = lift2 State.get

throw :: InterpreterError -> Interpreter a
throw err = lift3 $ Except.throwE err

lookupVar :: Name -> Interpreter Value
lookupVar n = do
                vars <- varState
                case Map.lookup n vars of
                    Just e -> return e
                    _ -> throw "Undefined variable"

datatypeState :: Interpreter DatatypeState
datatypeState = lift State.get

lookupCons :: Name -> Interpreter [Constructor]
lookupCons n = do
                    vars <- datatypeState
                    case Map.lookup n vars of
                        Just e -> return e
                        _ -> throw "Undefined datatype"

updateVar :: Name -> Value -> Interpreter ()
updateVar n v = lift2 $ State.modify (Map.insert n v)

applyOp :: Op -> Value -> Value -> Interpreter Value
applyOp o e1 e2 = case (e1, e2) of
                     (Value f1, Value f2) -> return $ Value $
                      case o of
                        Plus -> f1 + f2
                        Minus -> f1 - f2
                        Times -> f1 * f2
                        Divide -> f1 / f2
                     _ -> throw "Bad arguments to binary operator"

substitute :: (Expr, Value) -> Interpreter ()
substitute (arg, actual_arg) = case arg of
                                    Var v -> updateVar v actual_arg
                                    _ -> throw "Non-variable value in function body"

createFun :: [Expr] -> Expr -> Interpreter Function
createFun args body = do
  state <- varState
  return $ \actual_args -> temporaryState state $
                                    do
                                        mapM_  substitute $ zip args actual_args
                                        runEvaluation body

registerDatatype :: Name -> [Constructor] -> Interpreter ()
registerDatatype n cs = do
                        datatypes <- lift State.get
                        if Map.member n datatypes
                        then throw "Datatype already defined"
                        else lift $ State.modify (Map.insert n cs)

registerConstructor :: Constructor -> Interpreter ()
registerConstructor (Constructor name arity) = updateVar name constructor
                                               where
                                               constructor = if arity == 0
                                                             then Con name []
                                                             else Fun build_constructor_function
                                               build_constructor_function actual_args =
                                                    let actual_arity = length actual_args in
                                                        if actual_arity == arity
                                                        then updateAcc $ Con name actual_args
                                                        else
                                                            if actual_arity < arity
                                                            then throw "Not enough arguments to constructor"
                                                            else throw "Too many arguments to constructor"

handleSwitch :: Expr -> [SwitchExpr] -> Interpreter ()
handleSwitch expr cases =  do
                        v <- runEvaluation expr >> acc
                        case v of
                             Con constructor_name values ->
                                 let cases_map = foldl Map.union Map.empty $ map (\(SwitchExpr con vars body) -> Map.singleton con (vars,body)) cases in
                                     case lookups [constructor_name, "_", "otherwise"] cases_map of
                                         Just (vars, body) -> do
                                             fun <- createFun (map Var vars) body
                                             fun values
                                         _ ->  throw "Constructor pattern not found"
                             _ -> throw "Case with non-constructor value"
                        where
                          pick_first :: [Maybe a] -> Maybe a
                          pick_first = foldl (\o1 o2 -> case o1 of Just _ -> o1; _ -> o2) Nothing
                          lookups :: [Name] -> Map.Map Name a -> Maybe a
                          lookups cons cons_map =  pick_first $ map (`Map.lookup` cons_map) cons


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
                    Function args body -> createFun args body >>= \f ->  updateAcc $ Fun f
                    Datatype (name, cons) -> registerDatatype name cons >> mapM_ registerConstructor cons
                    Switch expr _ cases -> handleSwitch expr cases
                    _ -> throw "NotYetImplmented"
                    -- Extern Name [Expr]

eval :: [Expr] -> IO ()
eval es = print $  case runInterpreter ( mapM_ runEvaluation es >> acc) of
                    Right value -> show value
                    Left err -> "<error: " ++ show err ++ ">"
