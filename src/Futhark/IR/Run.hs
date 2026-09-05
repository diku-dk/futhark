-- | Interpreter for the IR.
--
-- Running an IR program requires specifying the name of an entry point along
-- with values for the entry point parameters. Top level statements
-- ('progConsts') are evaluated first, and then the specified entry point is
-- invoked with the provided values.
--
-- The goal of this interpreter is not performance, but operational clarity.
-- Hence you should not expect programs to run fast at all.
module Futhark.IR.Run (runSOACS, runGPU) where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector.Storable qualified as SVec
import Futhark.Data qualified as V
import Futhark.IR
import Futhark.IR.GPU (GPU)
import Futhark.IR.SOACS (SOAC (Screma), SOACS)
import Language.Futhark.Primitive qualified as P

data Val = PrimVal PrimValue | ArrayValue [Val]

type Env = M.Map VName Val

type FunEnv = M.Map Name (FunDef SOACS)

type InterpM a = Either T.Text a

evalBody :: FunEnv -> Env -> Body SOACS -> InterpM [Val]
evalBody funs env (Body _ stms res) = do
  env' <- foldStms env (stmsToList stms)
  mapM (evalSubExp env' . resSubExp) res
  where
    foldStms e [] = pure e
    foldStms e (s : ss) = evalStm funs e s >>= \e' -> foldStms e' ss

-- Evaluate the expression then bind the pattern names to its results
evalStm :: FunEnv -> Env -> Stm SOACS -> InterpM Env
evalStm funs env (Let pat _ e) = do
  vals <- evalExp funs env e
  let names = map patElemName $ patElems pat
  pure $ M.union (M.fromList $ zip names vals) env

-- Produce one Val per pattern element the expression is expected to bind.
evalExp :: FunEnv -> Env -> Exp SOACS -> InterpM [Val]
evalExp _ env (BasicOp op) = evalBasicOp env op
evalExp _ _ Match {} = Left "Match not implemented yet"
evalExp _ _ Loop {} = Left "Loop not implemented yet"
evalExp funs env (Apply fname args _ _) = do
  callee <-
    maybe
      (Left $ "function not found: " <> prettyText fname)
      pure
      (M.lookup fname funs)
  argVals <- mapM (evalSubExp env . fst) args
  let params = map paramName $ funDefParams callee
  if length params /= length argVals
    then Left "function argument count mismatch"
    else
      let bindings = M.fromList $ zip params argVals
          calleeEnv = M.union bindings env
       in evalBody funs calleeEnv (funDefBody callee)
evalExp _ env (Op soac) = evalSOAC env soac -- map/reduction/scan
evalExp _ _ WithAcc {} = Left "WithAcc not implemented yet"

evalSubExp :: Env -> SubExp -> InterpM Val
evalSubExp _ (Constant pv) = pure $ PrimVal pv
evalSubExp env (Var v) =
  maybe (Left $ "unbound variable: " <> prettyText v) pure $ M.lookup v env

expectPrimVal :: Val -> InterpM PrimValue
expectPrimVal (PrimVal pv) = pure pv
expectPrimVal (ArrayValue _) = Left "expected a primitive value"

evalBasicOp :: Env -> BasicOp -> InterpM [Val]
evalBasicOp env (SubExp se) = pure <$> evalSubExp env se
evalBasicOp env (BinOp op x y) = do
  xv <- expectPrimVal =<< evalSubExp env x
  yv <- expectPrimVal =<< evalSubExp env y
  case P.doBinOp op xv yv of
    Just result -> pure [PrimVal result]
    Nothing -> Left "invalid binary operation"
evalBasicOp _ _ = Left "basic operation not implemented yet"

evalSOAC :: Env -> SOAC SOACS -> InterpM [Val]
evalSOAC _ Screma {} = Left "Screma not implemented yet"
evalSOAC _ _ = Left "SOAC not implemented yet"

_evalLambda :: FunEnv -> Env -> Lambda SOACS -> [Val] -> InterpM [Val]
_evalLambda funs env (Lambda ps _ body) args = do
  evalBody funs (M.union (M.fromList $ zip (map paramName ps) args) env) body

-- | Run a program in the SOACS IR.
runSOACS :: Prog SOACS -> Name -> [V.Value] -> Either T.Text [V.Value]
runSOACS prog entry inputs = do
  let funs = M.fromList [(funDefName fun, fun) | fun <- progFuns prog]
  constsEnv <- foldConsts funs mempty (stmsToList (progConsts prog)) -- top-level consts
  fun <- findEntry prog entry
  argVals <- mapM fromValue inputs
  let env = M.union (M.fromList (zip (map paramName (funDefParams fun)) argVals)) constsEnv
  results <- evalBody funs env (funDefBody fun)
  mapM toValue results
  where
    foldConsts _ e [] = pure e
    foldConsts funs e (s : ss) = evalStm funs e s >>= \e' -> foldConsts funs e' ss

findEntry :: Prog SOACS -> Name -> Either T.Text (FunDef SOACS)
findEntry prog name =
  maybe
    (Left $ "entry point not found: " <> prettyText name)
    Right
    $ lookup
      name
      [ (entryName, fun)
      | fun <- progFuns prog,
        Just (entryName, _, _, _) <- [funDefEntryPoint fun]
      ]

fromValue :: V.Value -> InterpM Val
fromValue (V.I32Value shape values)
  | SVec.null shape,
    [v] <- SVec.toList values =
      pure $ PrimVal $ IntValue $ Int32Value v
fromValue _ =
  Left "only scalar i32 values are currently supported"

toValue :: Val -> InterpM V.Value
toValue (PrimVal (IntValue (Int32Value v))) =
  pure $ V.I32Value SVec.empty (SVec.singleton v)
toValue _ =
  Left "only scalar i32 values are currently supported"

-- | Run a program in the GPU IR.
runGPU :: Prog GPU -> Name -> [V.Value] -> Either T.Text [V.Value]
runGPU = undefined
