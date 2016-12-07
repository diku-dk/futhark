{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Bindings
  (
  -- * Internalising bindings
    bindingParams
  , bindingLambdaParams

  , bindingPattern
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Traversable (mapM)

import Language.Futhark as E
import qualified Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues

import Prelude hiding (mapM)

internaliseBindee :: MonadFreshNames m =>
                     E.Ident
                  -> m [(VName, I.DeclExtType)]
internaliseBindee bindee =
  forM (internaliseTypeWithUniqueness $ E.unInfo $ E.identType bindee) $ \t -> do
    name <- newVName base
    return (name, t)
  where base = nameToString $ baseName $ E.identName bindee

bindingParams :: [E.Pattern]
              -> ([I.FParam] -> [I.FParam] -> InternaliseM a)
              -> InternaliseM a
bindingParams params m = do
  (params_idents, params_ascripts, params_types) <-
    unzip3 . concat <$> mapM flattenPattern params
  (params_ts, shape_ctx) <- internaliseParamTypes params_types
  (shape_ctx', shapesubst) <- makeShapeIdentsFromContext shape_ctx

  (params_ts', unnamed_shape_params, ascriptsubsts) <-
    fmap unzip3 $ forM (zip params_ts params_ascripts) $ \(param_ts, param_ascripts) -> do
      (param_ts', param_unnamed_dims) <- instantiateShapesWithDecls shape_ctx' param_ts

      -- Context does not matter for the ascription - we just want the
      -- names so we can map them to the actually bound names from
      -- param_ts'.
      (ascripted_ts, ascript_ctx) <- internaliseParamTypes param_ascripts
      let ascript_ctx_rev = HM.fromList $ map (uncurry $ flip (,)) $ HM.toList ascript_ctx
      return (param_ts', param_unnamed_dims,
              HM.map pure $ mconcat $ zipWith (forwardDims ascript_ctx_rev) param_ts' $
              transpose ascripted_ts)

  let named_shape_params = map nonuniqueParamFromIdent (HM.elems shape_ctx')
      shape_params = named_shape_params ++ concat unnamed_shape_params
  bindingFlatPattern params_idents (concat params_ts') $ \valueparams ->
    bindingIdentTypes (map I.paramIdent $ shape_params++valueparams) $
    local (\env -> env { envSubsts = mconcat ascriptsubsts
                                     `HM.union` shapesubst
                                     `HM.union` envSubsts env}) $
    m shape_params valueparams

    where forwardDims ctx ref =
            mconcat . map (mconcat . zipWith (forwardDim ctx) (I.arrayDims ref) .
                            I.extShapeDims . I.arrayShape)
          forwardDim ctx d (I.Ext i) | Just v <- HM.lookup i ctx,
                                       I.Var v /= d = HM.singleton v d
          forwardDim _ _ _ = HM.empty

bindingLambdaParams :: [E.Pattern] -> [I.Type]
                    -> ([I.LParam] -> InternaliseM a)
                    -> InternaliseM a
bindingLambdaParams params ts m = do
  (params_idents, params_ascripts, params_types) <-
    unzip3 . concat <$> mapM flattenPattern params
  (params_ts, _) <- internaliseParamTypes params_types

  let ts_for_ps = typesForParams params_ts ts

  ascript_substs <- fmap mconcat . forM (zip params_ascripts ts_for_ps) $ \(ascript, p_t) -> do
    (ascript_ts, shape_ctx) <- internaliseParamTypes ascript
    return $ lambdaShapeSubstitutions shape_ctx (concat ascript_ts)
      (concat (replicate (length ascript) p_t))

  bindingFlatPattern params_idents ts $ \params' ->
    local (\env -> env { envSubsts = ascript_substs `HM.union` envSubsts env }) $
    bindingIdentTypes (map I.paramIdent params') $ m params'

  where typesForParams (p:ps) ts' = let (p_ts, ts'') = splitAt (length p) ts'
                                    in p_ts : typesForParams ps ts''
        typesForParams []     _  = []

processFlatPattern :: [E.Ident] -> [t]
                   -> InternaliseM ([I.Param t], VarSubstitutions)
processFlatPattern = processFlatPattern' []
  where
    processFlatPattern' pat []       _  = do
      let (vs, substs) = unzip pat
          substs' = HM.fromList substs
          idents = concat $ reverse vs
      return (idents, substs')

    processFlatPattern' pat (p:rest) ts = do
      (ps, subst, rest_ts) <- handleMapping ts <$> internaliseBindee p
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

bindingFlatPattern :: [E.Ident] -> [t]
                   -> ([I.Param t] -> InternaliseM a)
                   -> InternaliseM a
bindingFlatPattern idents ts m = do
  (ps, substs) <- processFlatPattern idents ts
  local (\env -> env { envSubsts = substs `HM.union` envSubsts env}) $
    m ps

-- | Flatten a pattern.  Returns a list of identifiers.  Each
-- identifier is also associated with a (possibly empty) list of types
-- that indicate type ascriptions.  These are important for retaining
-- shape declarations.  The structural type of each identifier is also
-- returned separately.
flattenPattern :: MonadFreshNames m => E.Pattern -> m [(E.Ident, [E.StructType], E.StructType)]
flattenPattern = flattenPattern' []
  where flattenPattern' ts (E.Wildcard t loc) = do
          name <- newVName "nameless"
          return [(E.Ident name t loc, ts,
                   case ts of [] -> E.vacuousShapeAnnotations $ toStruct $ unInfo t
                              st:_ -> st)]
        flattenPattern' ts (E.Id v) =
          return [(v, ts,
                   case ts of [] -> E.vacuousShapeAnnotations $ toStruct $ unInfo $ identType v
                              st:_ -> st)]
        flattenPattern' ts (E.TuplePattern pats _) =
          concat <$> zipWithM flattenPattern' (tupleComponents ts ++ repeat []) pats
        flattenPattern' ts (E.PatternAscription p td) =
          flattenPattern' (unInfo (expandedType td):ts) p

        tupleComponents = transpose . map tupleComponents'
        tupleComponents' (E.Tuple ts) = ts
        tupleComponents' t            = [t]

bindingPattern :: E.Pattern -> [I.ExtType] -> (I.Pattern -> InternaliseM a)
               -> InternaliseM a
bindingPattern pat ts m = do
  (pat', _, _) <- unzip3 <$> flattenPattern pat
  (ts',shapes) <- instantiateShapes' ts
  let addShapeStms = m . I.basicPattern' shapes . map I.paramIdent
  bindingFlatPattern pat' ts' addShapeStms

makeShapeIdentsFromContext :: MonadFreshNames m =>
                              HM.HashMap VName Int
                           -> m (HM.HashMap Int I.Ident,
                                 VarSubstitutions)
makeShapeIdentsFromContext ctx = do
  (ctx', substs) <- fmap unzip $ forM (HM.toList ctx) $ \(name, i) -> do
    v <- newIdent (baseString name) $ I.Prim I.int32
    return ((i, v), (name, [I.Var $ I.identName v]))
  return (HM.fromList ctx', HM.fromList substs)

instantiateShapesWithDecls :: MonadFreshNames m =>
                              HM.HashMap Int I.Ident
                           -> [I.DeclExtType]
                           -> m ([I.DeclType], [I.FParam])
instantiateShapesWithDecls ctx ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate x
          | Just v <- HM.lookup x ctx =
            return $ I.Var $ I.identName v

          | otherwise = do
            v <- lift $ nonuniqueParamFromIdent <$> newIdent "size" (I.Prim I.int32)
            tell [v]
            return $ I.Var $ I.paramName v

lambdaShapeSubstitutions :: HM.HashMap VName Int
                         -> [I.TypeBase I.ExtShape Uniqueness]
                         -> [I.Type]
                         -> VarSubstitutions
lambdaShapeSubstitutions shape_ctx param_ts ts =
  mconcat $ zipWith matchTypes param_ts ts
  where ctx_to_names = HM.fromList $ map (uncurry $ flip (,)) $ HM.toList shape_ctx

        matchTypes pt t =
          mconcat $ zipWith matchDims (I.extShapeDims $ I.arrayShape pt) (I.arrayDims t)
        matchDims (I.Ext i) d
          | Just v <- HM.lookup i ctx_to_names = HM.singleton v [d]
        matchDims _ _ =
          mempty

nonuniqueParamFromIdent :: I.Ident -> I.FParam
nonuniqueParamFromIdent (I.Ident name t) =
  I.Param name $ I.toDecl t Nonunique
