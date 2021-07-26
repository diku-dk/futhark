{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}

-- | Internalising bindings.
module Futhark.Internalise.Bindings
  ( bindingFParams,
    bindingLoopParams,
    bindingLambdaParams,
    stmPat,
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
  [E.Pat] ->
  ([I.FParam] -> [[I.FParam]] -> InternaliseM a) ->
  InternaliseM a
bindingFParams tparams params m = do
  flattened_params <- mapM flattenPat params
  let params_idents = concat flattened_params
  params_ts <-
    internaliseParamTypes $
      map (flip E.setAliases () . E.unInfo . E.identType) params_idents
  let num_param_idents = map length flattened_params
      num_param_ts = map (sum . map length) $ chunks num_param_idents params_ts

  let shape_params = [I.Param v $ I.Prim I.int64 | E.TypeParamDim v _ <- tparams]
      shape_subst = M.fromList [(I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params]
  bindingFlatPat params_idents (concat params_ts) $ \valueparams -> do
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
  E.Pat ->
  [I.Type] ->
  ([I.FParam] -> [I.FParam] -> InternaliseM a) ->
  InternaliseM a
bindingLoopParams tparams pat ts m = do
  pat_idents <- flattenPat pat
  pat_ts <- internaliseLoopParamType (E.patternStructType pat) ts

  let shape_params = [I.Param v $ I.Prim I.int64 | E.TypeParamDim v _ <- tparams]
      shape_subst = M.fromList [(I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params]

  bindingFlatPat pat_idents pat_ts $ \valueparams ->
    I.localScope (I.scopeOfFParams $ shape_params ++ concat valueparams) $
      substitutingVars shape_subst $ m shape_params $ concat valueparams

bindingLambdaParams ::
  [E.Pat] ->
  [I.Type] ->
  ([I.LParam] -> InternaliseM a) ->
  InternaliseM a
bindingLambdaParams params ts m = do
  params_idents <- concat <$> mapM flattenPat params

  bindingFlatPat params_idents ts $ \params' ->
    I.localScope (I.scopeOfLParams $ concat params') $ m $ concat params'

processFlatPat ::
  Show t =>
  [E.Ident] ->
  [t] ->
  InternaliseM ([[I.Param t]], VarSubsts)
processFlatPat x y = processFlatPat' [] x y
  where
    processFlatPat' pat [] _ = do
      let (vs, substs) = unzip pat
      return (reverse vs, M.fromList substs)
    processFlatPat' pat (p : rest) ts = do
      (ps, rest_ts) <- handleMapping ts <$> internaliseBindee p
      processFlatPat' ((ps, (E.identName p, map (I.Var . I.paramName) ps)) : pat) rest rest_ts

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

bindingFlatPat ::
  Show t =>
  [E.Ident] ->
  [t] ->
  ([[I.Param t]] -> InternaliseM a) ->
  InternaliseM a
bindingFlatPat idents ts m = do
  (ps, substs) <- processFlatPat idents ts
  local (\env -> env {envSubsts = substs `M.union` envSubsts env}) $
    m ps

-- | Flatten a pattern.  Returns a list of identifiers.
flattenPat :: MonadFreshNames m => E.Pat -> m [E.Ident]
flattenPat = flattenPat'
  where
    flattenPat' (E.PatParens p _) =
      flattenPat' p
    flattenPat' (E.Wildcard t loc) = do
      name <- newVName "nameless"
      flattenPat' $ E.Id name t loc
    flattenPat' (E.Id v (Info t) loc) =
      return [E.Ident v (Info t) loc]
    -- XXX: treat empty tuples and records as unit.
    flattenPat' (E.TuplePat [] loc) =
      flattenPat' (E.Wildcard (Info $ E.Scalar $ E.Record mempty) loc)
    flattenPat' (E.RecordPat [] loc) =
      flattenPat' (E.Wildcard (Info $ E.Scalar $ E.Record mempty) loc)
    flattenPat' (E.TuplePat pats _) =
      concat <$> mapM flattenPat' pats
    flattenPat' (E.RecordPat fs loc) =
      flattenPat' $ E.TuplePat (map snd $ sortFields $ M.fromList fs) loc
    flattenPat' (E.PatAscription p _ _) =
      flattenPat' p
    flattenPat' (E.PatLit _ t loc) =
      flattenPat' $ E.Wildcard t loc
    flattenPat' (E.PatConstr _ _ ps _) =
      concat <$> mapM flattenPat' ps

stmPat ::
  E.Pat ->
  [I.Type] ->
  ([VName] -> InternaliseM a) ->
  InternaliseM a
stmPat pat ts m = do
  pat' <- flattenPat pat
  bindingFlatPat pat' ts $ m . map I.paramName . concat
