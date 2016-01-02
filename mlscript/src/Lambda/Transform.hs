module Lambda.Transform where

import           Raw.Syntax as RS
import           Lambda.Syntax as LS
import Util (Name)

import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Except as Except
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Trans (lift)
import           Control.Monad (replicateM)
import Data.Maybe (fromJust)

type Datatypes = Map.Map Name Int

type Constructors = Map.Map Name (Int, Int)

type TransformationError = String

type Transformation = State.State Int (State.StateT (Datatypes, Constructors) (Except.ExceptT TransformationError Identity))

runTransformation = runIdentity $ Except.runExceptT $ flip State.evalStateT (Map.empty, Map.empty) $ flip
                                                                                                       State.evalStateT
                                                                                                       0

newVar = do
  last_var <- State.get
  State.modify (+ 1)
  return $ "var_" ++ show last_var

constructors = lift State.get >>= lift snd

transformVar v = do
  ds <- constructors
  return $ case Map.lookup v constructors of
              Just (constructor_number, arity) -> do
                args <- replicateM arity newVar
                return $ if arity == 0
                           then Con constructor_number
                           else LS.Function args $ Con constructor_number args
              _ -> return $ LS.Var v

statefulTransform :: [RS.Expr] -> Transformation Maybe LS.Expr
statefulTransform e =
  case e of
    RS.Float d -> Just $ LS.Float d
    RS.BinOp o e1 e2 -> do
      l_es <- mapM statefulTransform [e1, e2]
      let [l_e1, l_e2] = map fromJust l_es
      return $ Just LS.BinOp o l_e1 l_e2
    RS.Var n -> transformVar n
    RS.Call e es -> do
      l_e <- statefulTransform e
      l_es <- mapM statefulTransform es
      return $ LS.Call l_e l_es
    RS.Definition n e -> do
      l_e <- statefulTransform e
      return $ LS.Call n l_e
    RS.Function es e -> do
      l_es <- mapM statefulTransform es
      l_e <- statefulTransform e
      return $ LS.Function l_es l_e
    RS.Datatype d -> registerDatatype d
    RS.Switch e n ses -> do
      l_e <- statefulTransform e
      (l_ses, l_def_expr) <- statefulTransformSwitchExprs n ses
      return $ LS.Switch l_e l_ses l_def_expr
    RS.Extern n es -> do
      l_es <- mapM statefulTransform es
      return $ LS.Extern n l_es

transform :: [RS.Expr] -> Either String [LS.Expr]
transform es =
  case runTransformation (statefulTransform es) of
    Left err -> Left "Error: [" ++ show err ++ "]"
    x        -> x
