{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}

-- | Internalising bindings.
module Futhark.Internalise.Bindings
  ( bindingFParams,
    bindingLoopParams,
    bindingLambdaParams,
    stmPattern,
  )
where

import Control.Monad.Reader hiding (mapM)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Futhark.IR.SOACS as I
import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues
import Futhark.Util
import Language.Futhark as E hiding (matchDims)

bindingFParams ::
  [E.TypeParam] ->
  [E.Pattern] ->
  ([I.FParam] -> [[I.FParam]] -> InternaliseM a) ->
  InternaliseM a
bindingFParams tparams params m = do
  flattened_params <- mapM flattenPattern params
  let params_idents = concat flattened_params
  params_ts <-
    internaliseParamTypes $
      map (flip E.setAliases () . E.unInfo . E.identType) params_idents
  let num_param_idents = map length flattened_params
      num_param_ts = map (sum . map length) $ chunks num_param_idents params_ts

  let shape_params = [I.Param v $ I.Prim I.int64 | E.TypeParamDim v _ <- tparams]
      shape_subst = M.fromList [(I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params]
  bindingFlatPattern params_idents (concat params_ts) $ \valueparams -> do
    let (certparams, valueparams') = unzip $ map fixAccParam (concat valueparams)
    I.localScope (I.scopeOfFParams $ catMaybes certparams ++ shape_params ++ valueparams') $
      substitutingVars shape_subst $
        m (catMaybes certparams ++ shape_params) $ chunks num_param_ts valueparams'
  where
    fixAccParam (I.Param pv (I.Acc acc ispace ts u)) =
      ( Just (I.Param acc $ I.Prim I.Unit),
        I.Param pv (I.Acc acc ispace ts u)
      )
    fixAccParam p = (Nothing, p)

bindingLoopParams ::
  [E.TypeParam] ->
  E.Pattern ->
  [I.Type] ->
  ([I.FParam] -> [I.FParam] -> InternaliseM a) ->
  InternaliseM a
bindingLoopParams tparams pat ts m = do
  pat_idents <- flattenPattern pat
  pat_ts <- internaliseLoopParamType (E.patternStructType pat) ts

  let shape_params = [I.Param v $ I.Prim I.int64 | E.TypeParamDim v _ <- tparams]
      shape_subst = M.fromList [(I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params]

  bindingFlatPattern pat_idents pat_ts $ \valueparams ->
    I.localScope (I.scopeOfFParams $ shape_params ++ concat valueparams) $
      substitutingVars shape_subst $ m shape_params $ concat valueparams

bindingLambdaParams ::
  [E.Pattern] ->
  [I.Type] ->
  ([I.LParam] -> InternaliseM a) ->
  InternaliseM a
bindingLambdaParams params ts m = do
  params_idents <- concat <$> mapM flattenPattern params

  bindingFlatPattern params_idents ts $ \params' ->
    I.localScope (I.scopeOfLParams $ concat params') $ m $ concat params'

processFlatPattern ::
  Show t =>
  [E.Ident] ->
  [t] ->
  InternaliseM ([[I.Param t]], VarSubstitutions)
processFlatPattern x y = processFlatPattern' [] x y
  where
    processFlatPattern' pat [] _ = do
      let (vs, substs) = unzip pat
      return (reverse vs, M.fromList substs)
    processFlatPattern' pat (p : rest) ts = do
      (ps, rest_ts) <- handleMapping ts <$> internaliseBindee p
      processFlatPattern' ((ps, (E.identName p, map (I.Var . I.paramName) ps)) : pat) rest rest_ts

    handleMapping ts [] =
      ([], ts)
    handleMapping (t : ts) (r : rs) =
      let (ps, ts') = handleMapping ts rs
       in (I.Param r t : ps, ts')
    handleMapping [] _ =
      error $ "handleMapping: insufficient identifiers in pattern." ++ show (x, y)

    internaliseBindee :: E.Ident -> InternaliseM [VName]
    internaliseBindee bindee = do
      let name = E.identName bindee
      n <- internalisedTypeSize $ E.unInfo $ E.identType bindee
      case n of
        1 -> return [name]
        _ -> replicateM n $ newVName $ baseString name

bindingFlatPattern ::
  Show t =>
  [E.Ident] ->
  [t] ->
  ([[I.Param t]] -> InternaliseM a) ->
  InternaliseM a
bindingFlatPattern idents ts m = do
  (ps, substs) <- processFlatPattern idents ts
  local (\env -> env {envSubsts = substs `M.union` envSubsts env}) $
    m ps

-- | Flatten a pattern.  Returns a list of identifiers.
flattenPattern :: MonadFreshNames m => E.Pattern -> m [E.Ident]
flattenPattern = flattenPattern'
  where
    flattenPattern' (E.PatternParens p _) =
      flattenPattern' p
    flattenPattern' (E.Wildcard t loc) = do
      name <- newVName "nameless"
      flattenPattern' $ E.Id name t loc
    flattenPattern' (E.Id v (Info t) loc) =
      return [E.Ident v (Info t) loc]
    -- XXX: treat empty tuples and records as unit.
    flattenPattern' (E.TuplePattern [] loc) =
      flattenPattern' (E.Wildcard (Info $ E.Scalar $ E.Record mempty) loc)
    flattenPattern' (E.RecordPattern [] loc) =
      flattenPattern' (E.Wildcard (Info $ E.Scalar $ E.Record mempty) loc)
    flattenPattern' (E.TuplePattern pats _) =
      concat <$> mapM flattenPattern' pats
    flattenPattern' (E.RecordPattern fs loc) =
      flattenPattern' $ E.TuplePattern (map snd $ sortFields $ M.fromList fs) loc
    flattenPattern' (E.PatternAscription p _ _) =
      flattenPattern' p
    flattenPattern' (E.PatternLit _ t loc) =
      flattenPattern' $ E.Wildcard t loc
    flattenPattern' (E.PatternConstr _ _ ps _) =
      concat <$> mapM flattenPattern' ps

stmPattern ::
  E.Pattern ->
  [I.Type] ->
  ([VName] -> InternaliseM a) ->
  InternaliseM a
stmPattern pat ts m = do
  pat' <- flattenPattern pat
  bindingFlatPattern pat' ts $ m . map I.paramName . concat
