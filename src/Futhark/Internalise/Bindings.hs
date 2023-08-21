{-# LANGUAGE Strict #-}

-- | Internalising bindings.
module Futhark.Internalise.Bindings
  ( internaliseAttrs,
    internaliseAttr,
    bindingFParams,
    bindingLoopParams,
    bindingLambdaParams,
    stmPat,
  )
where

import Control.Monad
import Control.Monad.Free (Free (..))
import Control.Monad.Reader
import Data.Bifunctor
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.SOACS qualified as I
import Futhark.Internalise.Monad
import Futhark.Internalise.TypesValues
import Futhark.Util
import Language.Futhark as E hiding (matchDims)

internaliseAttr :: E.AttrInfo VName -> InternaliseM I.Attr
internaliseAttr (E.AttrAtom (E.AtomName v) _) =
  pure $ I.AttrName v
internaliseAttr (E.AttrAtom (E.AtomInt x) _) =
  pure $ I.AttrInt x
internaliseAttr (E.AttrComp f attrs _) =
  I.AttrComp f <$> mapM internaliseAttr attrs

internaliseAttrs :: [E.AttrInfo VName] -> InternaliseM I.Attrs
internaliseAttrs = fmap (mconcat . map I.oneAttr) . mapM internaliseAttr

treeLike :: Tree a -> [b] -> Tree b
treeLike (Pure _) [b] = Pure b
treeLike (Pure _) _ = error "treeLike: invalid input"
treeLike (Free ls) bs = Free $ zipWith treeLike ls (chunks (map length ls) bs)

bindingFParams ::
  [E.TypeParam] ->
  [E.Pat E.ParamType] ->
  ([I.FParam I.SOACS] -> [[Tree (I.FParam I.SOACS)]] -> InternaliseM a) ->
  InternaliseM a
bindingFParams tparams params m = do
  flattened_params <- mapM flattenPat params
  let params_idents = concat flattened_params
  params_ts <-
    internaliseParamTypes $
      map (E.unInfo . E.identType . fst) params_idents
  let num_param_idents = map length flattened_params

  let shape_params = [I.Param mempty v $ I.Prim I.int64 | E.TypeParamDim v _ <- tparams]
      shape_subst = M.fromList [(I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params]
  bindingFlatPat params_idents (concatMap (concatMap toList) params_ts) $ \valueparams -> do
    let (certparams, valueparams') =
          first concat $ unzip $ map fixAccParams valueparams
        all_params = certparams ++ shape_params ++ concat valueparams'
    I.localScope (I.scopeOfFParams all_params) $
      substitutingVars shape_subst $ do
        let values_grouped_by_params = chunks num_param_idents valueparams'
            types_grouped_by_params = chunks num_param_idents params_ts

        m (certparams ++ shape_params) $
          zipWith chunkValues types_grouped_by_params values_grouped_by_params
  where
    fixAccParams ps =
      first catMaybes $ unzip $ map fixAccParam ps
    fixAccParam (I.Param attrs pv (I.Acc acc ispace ts u)) =
      ( Just (I.Param attrs acc $ I.Prim I.Unit),
        I.Param attrs pv (I.Acc acc ispace ts u)
      )
    fixAccParam p = (Nothing, p)

    chunkValues ::
      [[Tree (I.TypeBase I.Shape Uniqueness)]] ->
      [[I.FParam I.SOACS]] ->
      [Tree (I.FParam I.SOACS)]
    chunkValues tss vss =
      concat $ zipWith f tss vss
      where
        f ts vs = zipWith treeLike ts (chunks (map length ts) vs)

bindingLoopParams ::
  [E.TypeParam] ->
  E.Pat E.ParamType ->
  [I.Type] ->
  ([I.FParam I.SOACS] -> [I.FParam I.SOACS] -> InternaliseM a) ->
  InternaliseM a
bindingLoopParams tparams pat ts m = do
  pat_idents <- flattenPat pat
  pat_ts <- internaliseLoopParamType (E.patternType pat) ts

  let shape_params = [I.Param mempty v $ I.Prim I.int64 | E.TypeParamDim v _ <- tparams]
      shape_subst = M.fromList [(I.paramName p, [I.Var $ I.paramName p]) | p <- shape_params]

  bindingFlatPat pat_idents pat_ts $ \valueparams ->
    I.localScope (I.scopeOfFParams $ shape_params ++ concat valueparams) $
      substitutingVars shape_subst $
        m shape_params (concat valueparams)

bindingLambdaParams ::
  [E.Pat E.ParamType] ->
  [I.Type] ->
  ([I.LParam I.SOACS] -> InternaliseM a) ->
  InternaliseM a
bindingLambdaParams params ts m = do
  params_idents <- concat <$> mapM flattenPat params

  bindingFlatPat params_idents ts $ \params' ->
    I.localScope (I.scopeOfLParams $ concat params') $
      m (concat params')

type Params t = [I.Param t]

processFlatPat ::
  (Show t) =>
  [(E.Ident ParamType, [E.AttrInfo VName])] ->
  [t] ->
  InternaliseM ([Params t], VarSubsts)
processFlatPat x y = processFlatPat' [] x y
  where
    processFlatPat' pat [] _ = do
      let (vs, substs) = unzip pat
      pure (reverse vs, M.fromList substs)
    processFlatPat' pat ((p, attrs) : rest) ts = do
      attrs' <- internaliseAttrs attrs
      (ps, rest_ts) <- handleMapping attrs' ts <$> internaliseBindee p
      processFlatPat'
        ((ps, (E.identName p, map (I.Var . I.paramName) ps)) : pat)
        rest
        rest_ts

    handleMapping _ ts [] =
      ([], ts)
    handleMapping attrs (t : ts) (r : rs) =
      let (ps, ts') = handleMapping attrs ts rs
       in (I.Param attrs r t : ps, ts')
    handleMapping _ [] _ =
      error $ "handleMapping: insufficient identifiers in pattern.\n" ++ show (x, y)

    internaliseBindee :: E.Ident E.ParamType -> InternaliseM [VName]
    internaliseBindee bindee = do
      let name = E.identName bindee
      case internalisedTypeSize $ E.unInfo $ E.identType bindee of
        1 -> pure [name]
        n -> replicateM n $ newVName $ baseString name

bindingFlatPat ::
  (Show t) =>
  [(E.Ident E.ParamType, [E.AttrInfo VName])] ->
  [t] ->
  ([Params t] -> InternaliseM a) ->
  InternaliseM a
bindingFlatPat idents ts m = do
  (ps, substs) <- processFlatPat idents ts
  local (\env -> env {envSubsts = substs `M.union` envSubsts env}) $
    m ps

-- | Flatten a pattern.  Returns a list of identifiers.
flattenPat :: (MonadFreshNames m) => E.Pat (TypeBase Size u) -> m [(E.Ident (TypeBase Size u), [E.AttrInfo VName])]
flattenPat = flattenPat'
  where
    flattenPat' (E.PatParens p _) =
      flattenPat' p
    flattenPat' (E.PatAttr attr p _) =
      map (second (attr :)) <$> flattenPat' p
    flattenPat' (E.Wildcard t loc) = do
      name <- newVName "nameless"
      flattenPat' $ E.Id name t loc
    flattenPat' (E.Id v (Info t) loc) =
      pure [(E.Ident v (Info t) loc, mempty)]
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
  E.Pat E.ParamType ->
  [I.Type] ->
  ([VName] -> InternaliseM a) ->
  InternaliseM a
stmPat pat ts m = do
  pat' <- flattenPat pat
  bindingFlatPat pat' ts $ m . map I.paramName . concat
