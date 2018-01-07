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
import Data.Traversable (mapM)

import Language.Futhark as E
import qualified Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues
import Futhark.Internalise.AccurateSizes
import Futhark.Util

boundByPatterns :: [E.Pattern] -> [VName]
boundByPatterns = map E.identName . S.toList . mconcat . map E.patIdentSet

bindingParams :: [E.TypeParam] -> [E.Pattern]
              -> (ConstParams -> [I.FParam] -> [[I.FParam]] -> InternaliseM a)
              -> InternaliseM a
bindingParams tparams params m = do
  flattened_params <- mapM flattenPattern params
  let (params_idents, params_types) = unzip $ concat flattened_params
      bound = boundInTypes tparams
      param_names = M.fromList $ zip (boundByPatterns params) (map snd params_idents)
  (params_ts, shape_ctx, cm) <- internaliseParamTypes bound param_names params_types
  let num_param_idents = map length flattened_params
      num_param_ts = map (sum . map length) $ chunks num_param_idents params_ts
  (shape_ctx', shapesubst) <- makeShapeIdentsFromContext shape_ctx

  (params_ts', unnamed_shape_params) <-
    fmap unzip $ forM params_ts $ \param_ts -> do
      (param_ts', param_unnamed_dims) <- instantiateShapesWithDecls shape_ctx' param_ts

      return (param_ts',
              param_unnamed_dims)

  let named_shape_params = map nonuniqueParamFromIdent (M.elems shape_ctx')
      shape_params = named_shape_params ++ concat unnamed_shape_params
  bindingFlatPattern params_idents (concat params_ts') $ \valueparams ->
    I.localScope (I.scopeOfFParams $ shape_params++concat valueparams) $
    substitutingVars shapesubst $ m cm shape_params $
    chunks num_param_ts (concat valueparams)

bindingLambdaParams :: [E.TypeParam] -> [E.Pattern] -> [I.Type]
                    -> (ConstParams -> [I.LParam] -> InternaliseM a)
                    -> InternaliseM a
bindingLambdaParams tparams params ts m = do
  (params_idents, params_types) <-
    unzip . concat <$> mapM flattenPattern params
  let bound = boundInTypes tparams
      param_names = M.fromList $ zip (boundByPatterns params) (map snd params_idents)
  (params_ts, shape_ctx, cm) <-
    internaliseParamTypes bound param_names params_types

  let ascript_substs = lambdaShapeSubstitutions shape_ctx (concat params_ts) ts

  bindingFlatPattern params_idents ts $ \params' ->
    local (\env -> env { envSubsts = ascript_substs `M.union` envSubsts env }) $
    I.localScope (I.scopeOfLParams $ concat params') $ m cm $ concat params'

processFlatPattern :: [(E.Ident,VName)] -> [t]
                   -> InternaliseM ([[I.Param t]], VarSubstitutions)
processFlatPattern = processFlatPattern' []
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
      error "processFlatPattern: insufficient identifiers in pattern."

    internaliseBindee :: (E.Ident, VName) -> InternaliseM [(VName, I.DeclExtType)]
    internaliseBindee (bindee, name) = do
      -- XXX: we gotta be screwing up somehow by ignoring the extra
      -- return values.  If not, why not?
      (tss, _, _) <- internaliseParamTypes nothing_bound mempty
                     [flip E.setAliases () $ E.vacuousShapeAnnotations $
                      E.unInfo $ E.identType bindee]
      case concat tss of
        [t] -> return [(name, t)]
        tss' -> forM tss' $ \t -> do
          name' <- newVName $ baseString name
          return (name', t)

    -- Fixed up later.
    nothing_bound = boundInTypes []

bindingFlatPattern :: [(E.Ident, VName)] -> [t]
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
          return [((E.Ident v (Info (E.removeShapeAnnotations t)) loc,
                    new_name),
                   t `E.setAliases` ())]
        flattenPattern' (E.TuplePattern pats _) =
          concat <$> mapM flattenPattern' pats
        flattenPattern' (E.RecordPattern fs loc) =
          flattenPattern' $ E.TuplePattern (map snd $ sortFields $ M.fromList fs) loc
        flattenPattern' (E.PatternAscription p _) =
          flattenPattern' p

type MatchPattern = SrcLoc -> [I.SubExp] -> InternaliseM [I.SubExp]

stmPattern :: [E.TypeParam] -> E.Pattern -> [I.ExtType]
           -> (ConstParams -> [VName] -> MatchPattern -> InternaliseM a)
           -> InternaliseM a
stmPattern tparams pat ts m = do
  (pat', pat_types) <- unzip <$> flattenPattern pat
  (ts',_) <- instantiateShapes' ts
  (pat_types', ctx, cm) <- internaliseParamTypes (boundInTypes tparams) mempty pat_types
  let ctx_rev = M.fromList $ map (uncurry $ flip (,)) $ M.toList ctx
      pat_types'' = map I.fromDecl $ concat pat_types'
  let addShapeStms l =
        m cm (map I.paramName $ concat l) (matchPattern ctx_rev pat_types'')
  bindingFlatPattern pat' ts' addShapeStms

matchPattern :: M.Map Int VName -> [I.ExtType] -> MatchPattern
matchPattern ctx exts loc ses =
  forM (zip exts ses) $ \(et, se) -> do
  se_t <- I.subExpType se
  et' <- unExistentialise ctx et se_t
  ensureExtShape asserting "value cannot match pattern"
    loc et' "correct_shape" se

unExistentialise :: M.Map Int VName -> I.ExtType -> I.Type -> InternaliseM I.ExtType
unExistentialise substs et t = do
  new_dims <- zipWithM inspectDim (I.shapeDims $ I.arrayShape et) (I.arrayDims t)
  return $ t `I.setArrayShape` I.Shape new_dims
  where inspectDim (I.Ext i) d | Just v <- M.lookup i substs = do
          letBindNames'_ [v] $ I.BasicOp $ I.SubExp d
          return $ I.Free $ I.Var v
        inspectDim ed _ = return ed

makeShapeIdentsFromContext :: MonadFreshNames m =>
                              M.Map VName Int
                           -> m (M.Map Int I.Ident,
                                 VarSubstitutions)
makeShapeIdentsFromContext ctx = do
  (ctx', substs) <- fmap unzip $ forM (M.toList ctx) $ \(name, i) -> do
    v <- newIdent (baseString name) $ I.Prim I.int32
    return ((i, v), (name, [I.Var $ I.identName v]))
  return (M.fromList ctx', M.fromList substs)

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

lambdaShapeSubstitutions :: M.Map VName Int
                         -> [I.TypeBase I.ExtShape Uniqueness]
                         -> [I.Type]
                         -> VarSubstitutions
lambdaShapeSubstitutions shape_ctx param_ts ts =
  mconcat $ zipWith matchTypes param_ts ts
  where ctx_to_names = M.fromList $ map (uncurry $ flip (,)) $ M.toList shape_ctx

        matchTypes pt t =
          mconcat $ zipWith matchDims (I.shapeDims $ I.arrayShape pt) (I.arrayDims t)
        matchDims (I.Ext i) d
          | Just v <- M.lookup i ctx_to_names = M.singleton v [d]
        matchDims _ _ =
          mempty

nonuniqueParamFromIdent :: I.Ident -> I.FParam
nonuniqueParamFromIdent (I.Ident name t) =
  I.Param name $ I.toDecl t Nonunique
