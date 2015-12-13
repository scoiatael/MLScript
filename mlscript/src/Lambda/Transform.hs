module Lambda.Transform where

import Raw.Syntax as RS
import Lambda.Syntax as LS


import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)

type DatatypeConstructors = [RS.Constructor]
type Datatypes = Map.Map RS.Name DatatypesConstructors
type Constructors = Map.Map RS.Name DatatypesConstructors
type TransformationError = String
type Transformation = State.StateT (Datatypes, Constructors) (Except.ExceptT TransformationError Identity)

runTransformation = runIdentity $ Except.runExceptT $ flip State.evalStateT Map.empty

transformVarCallToCon n = do
                            ds <- constructors
                            return $ case Map.lookup n constructors of
                                        Just cons -> Con (constructor_number n constructors)

statefulTransform :: [RS.Expr] -> Transformation LS.Expr
statefulTransform e = case e of
                            RS.Float d -> LS.Float d
                            RS.BinOp o e1 e2 -> LS.BinOp o e1 e2
                            RS.Var n -> LS.Var n
                            RS.Call e es -> do
                                l_e <- statefulTransform e
                                l_es <- mapM statefulTransform es
                                case l_e of
                                     LS.Var n ->  transformVarCallToCon n l_es
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
                            RS.Extern n es -> LS.Extern n (statefulTransform es)

transform :: [RS.Expr] -> Either String LS.Expr
transform es = case runTransformation (statefulTransform es) of
                    Left err -> Left "Error: [" ++ show err ++ "]"
                    x -> x
