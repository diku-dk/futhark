{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Bindings
  (
  -- * Internalising bindings
    bindingParams
  , bindingLambdaParams
  , stmPattern
  , MatchPattern
  )
  where

import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Loc

import Language.Futhark as E
import qualified Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames
import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues
import Futhark.Internalise.AccurateSizes
import Futhark.Util

bindingParams :: [E.TypeParam] -> [E.Pattern]
              -> (ConstParams -> [I.FParam] -> [[I.FParam]] -> InternaliseM a)
              -> InternaliseM a
bindingParams tparams params m = do
  flattened_params <- mapM flattenPattern params
  let (params_idents, params_types) = unzip $ concat flattened_params
      bound = boundInTypes tparams
      param_names = M.fromList [ (E.identName x, y) | (x,y) <- params_idents ]
  (params_ts, cm) <- internaliseParamTypes bound param_names params_types
  let num_param_idents = map length flattened_params
      num_param_ts = map (sum . map length) $ chunks num_param_idents params_ts

  (params_ts', unnamed_shape_params) <-
    fmap unzip $ forM params_ts $ \param_ts -> do
      (param_ts', param_unnamed_dims) <- instantiateShapesWithDecls mempty param_ts

      return (param_ts',
              param_unnamed_dims)

  let named_shape_params = [ I.Param v $ I.Prim I.int32 | E.TypeParamDim v _ <- tparams ]
      shape_params = named_shape_params ++ concat unnamed_shape_params
      shape_subst = M.fromList [ (I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params ]
  bindingFlatPattern params_idents (concat params_ts') $ \valueparams ->
    I.localScope (I.scopeOfFParams $ shape_params++concat valueparams) $
    substitutingVars shape_subst $ m cm shape_params $ chunks num_param_ts (concat valueparams)

bindingLambdaParams :: [E.Pattern] -> [I.Type]
                    -> (ConstParams -> [I.LParam] -> InternaliseM a)
                    -> InternaliseM a
bindingLambdaParams params ts m = do
  (params_idents, params_types) <-
    unzip . concat <$> mapM flattenPattern params
  let param_names = M.fromList [ (E.identName x, y) | (x,y) <- params_idents ]
  (params_ts, cm) <- internaliseParamTypes mempty param_names params_types

  let ascript_substs = lambdaShapeSubstitutions (concat params_ts) ts

  bindingFlatPattern params_idents ts $ \params' ->
    local (\env -> env { envSubsts = ascript_substs `M.union` envSubsts env }) $
    I.localScope (I.scopeOfLParams $ concat params') $ m cm $ concat params'

processFlatPattern :: Show t => [(E.Ident,VName)] -> [t]
                   -> InternaliseM ([[I.Param t]], VarSubstitutions)
processFlatPattern x y = processFlatPattern' [] x y
  where
    processFlatPattern' pat []       _  = do
      let (vs, substs) = unzip pat
          substs' = M.fromList substs
          idents = reverse vs
      return (idents, substs')

    processFlatPattern' pat ((p,name):rest) ts = do
      (ps, subst, rest_ts) <- handleMapping ts <$> internaliseBindee (p, name)
      processFlatPattern' ((ps, (E.identName p, map (I.Var . I.paramName) subst)) : pat) rest rest_ts

    handleMapping ts [] =
      ([], [], ts)
    handleMapping ts (r:rs) =
        let (ps, reps, ts')    = handleMapping' ts r
            (pss, repss, ts'') = handleMapping ts' rs
        in (ps++pss, reps:repss, ts'')

    handleMapping' (t:ts) (vname,_) =
      let v' = I.Param vname t
      in ([v'], v', ts)
    handleMapping' [] _ =
      error $ "processFlatPattern: insufficient identifiers in pattern." ++ show (x, y)

    internaliseBindee :: (E.Ident, VName) -> InternaliseM [(VName, I.DeclExtType)]
    internaliseBindee (bindee, name) = do
      -- XXX: we gotta be screwing up somehow by ignoring the extra
      -- return values.  If not, why not?
      (tss, _) <- internaliseParamTypes nothing_bound mempty
                  [flip E.setAliases () $ E.unInfo $ E.identType bindee]
      case concat tss of
        [t] -> return [(name, t)]
        tss' -> forM tss' $ \t -> do
          name' <- newVName $ baseString name
          return (name', t)

    -- Fixed up later.
    nothing_bound = boundInTypes []

bindingFlatPattern :: Show t => [(E.Ident, VName)] -> [t]
                   -> ([[I.Param t]] -> InternaliseM a)
                   -> InternaliseM a
bindingFlatPattern idents ts m = do
  (ps, substs) <- processFlatPattern idents ts
  local (\env -> env { envSubsts = substs `M.union` envSubsts env}) $
    m ps

-- | Flatten a pattern.  Returns a list of identifiers.  The
-- structural type of each identifier is returned separately.
flattenPattern :: MonadFreshNames m => E.Pattern -> m [((E.Ident, VName), E.StructType)]
flattenPattern = flattenPattern'
  where flattenPattern' (E.PatternParens p _) =
          flattenPattern' p
        flattenPattern' (E.Wildcard t loc) = do
          name <- newVName "nameless"
          flattenPattern' $ E.Id name t loc
        flattenPattern' (E.Id v (Info t) loc) = do
          new_name <- newVName $ baseString v
          return [((E.Ident v (Info t) loc, new_name),
                   t `E.setAliases` ())]
        -- XXX: treat empty tuples and records as bool.
        flattenPattern' (E.TuplePattern [] loc) =
          flattenPattern' (E.Wildcard (Info $ E.Scalar $ E.Prim E.Bool) loc)
        flattenPattern' (E.RecordPattern [] loc) =
          flattenPattern' (E.Wildcard (Info $ E.Scalar $ E.Prim E.Bool) loc)
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

type MatchPattern = SrcLoc -> [I.SubExp] -> InternaliseM [I.SubExp]

stmPattern :: E.Pattern -> [I.ExtType]
           -> (ConstParams -> [VName] -> MatchPattern -> InternaliseM a)
           -> InternaliseM a
stmPattern pat ts m = do
  (pat', pat_types) <- unzip <$> flattenPattern pat
  (ts',_) <- instantiateShapes' ts
  (pat_types', cm) <- internaliseParamTypes mempty mempty pat_types
  let pat_types'' = map I.fromDecl $ concat pat_types'
  let addShapeStms l =
        m cm (map I.paramName $ concat l) (matchPattern pat_types'')
  bindingFlatPattern pat' ts' addShapeStms

matchPattern :: [I.ExtType] -> MatchPattern
matchPattern exts loc ses =
  forM (zip exts ses) $ \(et, se) -> do
  se_t <- I.subExpType se
  et' <- unExistentialise mempty et se_t
  ensureExtShape asserting (I.ErrorMsg [I.ErrorString "value cannot match pattern"])
    loc et' "correct_shape" se

unExistentialise :: S.Set VName -> I.ExtType -> I.Type -> InternaliseM I.ExtType
unExistentialise tparam_names et t = do
  new_dims <- zipWithM inspectDim (I.shapeDims $ I.arrayShape et) (I.arrayDims t)
  return $ t `I.setArrayShape` I.Shape new_dims
  where inspectDim (I.Free (I.Var v)) d
          | v `S.member` tparam_names = do
              letBindNames_ [v] $ I.BasicOp $ I.SubExp d
              return $ I.Free $ I.Var v
        inspectDim ed _ = return ed

instantiateShapesWithDecls :: MonadFreshNames m =>
                              M.Map Int I.Ident
                           -> [I.DeclExtType]
                           -> m ([I.DeclType], [I.FParam])
instantiateShapesWithDecls ctx ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate x
          | Just v <- M.lookup x ctx =
            return $ I.Var $ I.identName v

          | otherwise = do
            v <- lift $ nonuniqueParamFromIdent <$> newIdent "size" (I.Prim I.int32)
            tell [v]
            return $ I.Var $ I.paramName v

lambdaShapeSubstitutions :: [I.TypeBase I.ExtShape Uniqueness]
                         -> [I.Type]
                         -> VarSubstitutions
lambdaShapeSubstitutions param_ts ts =
  mconcat $ zipWith matchTypes param_ts ts
  where matchTypes pt t =
          mconcat $ zipWith matchDims (I.shapeDims $ I.arrayShape pt) (I.arrayDims t)
        matchDims (I.Free (I.Var v)) d = M.singleton v [d]
        matchDims _ _ = mempty

nonuniqueParamFromIdent :: I.Ident -> I.FParam
nonuniqueParamFromIdent (I.Ident name t) =
  I.Param name $ I.toDecl t Nonunique
