{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module exports facilities for transforming array accesses in
-- a list of 'Stm's (intended to be the bindings in a body).  The
-- idea is that you can state that some variable @x@ is in fact an
-- array indexing @v[i0,i1,...]@.
module Futhark.Optimise.InPlaceLowering.SubstituteIndices
  ( substituteIndices,
    IndexSubstitution,
    IndexSubstitutions,
  )
where

import Control.Monad
import qualified Data.Map.Strict as M
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.Transform.Substitute

-- | Essentially the components of an 'Index' expression.
type IndexSubstitution = (Certs, VName, Type, Slice SubExp)

-- | A mapping from variable names to the indexing operation they
-- should be replaced with.
type IndexSubstitutions = [(VName, IndexSubstitution)]

typeEnvFromSubstitutions :: LParamInfo rep ~ Type => IndexSubstitutions -> Scope rep
typeEnvFromSubstitutions = M.fromList . map (fromSubstitution . snd)
  where
    fromSubstitution (_, name, t, _) =
      (name, LParamName t)

-- | Perform the substitution.
substituteIndices ::
  ( MonadFreshNames m,
    BuilderOps rep,
    Buildable rep,
    Aliased rep,
    LParamInfo rep ~ Type
  ) =>
  IndexSubstitutions ->
  Stms rep ->
  m (IndexSubstitutions, Stms rep)
substituteIndices substs stms =
  runBuilderT (substituteIndicesInStms substs stms) types
  where
    types = typeEnvFromSubstitutions substs

substituteIndicesInStms ::
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions ->
  Stms (Rep m) ->
  m IndexSubstitutions
substituteIndicesInStms = foldM substituteIndicesInStm

substituteIndicesInStm ::
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions ->
  Stm (Rep m) ->
  m IndexSubstitutions
-- FIXME: we likely need to do something similar for all expressions
-- that produce aliases.  Ugh.  See issue #1460.  Or maybe we should
-- look at/copy all consumed arrays up front, instead of ad-hoc.
substituteIndicesInStm substs (Let pat _ (BasicOp (Rotate rots v)))
  | Just (cs, src, src_t, is) <- lookup v substs,
    [v'] <- patNames pat = do
      src' <-
        letExp (baseString v' <> "_subst") $
          BasicOp $ Rotate (replicate (arrayRank src_t - length rots) zero ++ rots) src
      src_t' <- lookupType src'
      pure $ (v', (cs, src', src_t', is)) : substs
  where
    zero = intConst Int64 0
substituteIndicesInStm substs (Let pat _ (BasicOp (Rearrange perm v)))
  | Just (cs, src, src_t, is) <- lookup v substs,
    [v'] <- patNames pat = do
      let extra_dims = arrayRank src_t - length perm
          perm' = [0 .. extra_dims - 1] ++ map (+ extra_dims) perm
      src' <-
        letExp (baseString v' <> "_subst") $ BasicOp $ Rearrange perm' src
      src_t' <- lookupType src'
      pure $ (v', (cs, src', src_t', is)) : substs
substituteIndicesInStm substs (Let pat rep e) = do
  e' <- substituteIndicesInExp substs e
  addStm $ Let pat rep e'
  pure substs

substituteIndicesInExp ::
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions ->
  Exp (Rep m) ->
  m (Exp (Rep m))
substituteIndicesInExp substs (Op op) = do
  let used_in_op = filter ((`nameIn` freeIn op) . fst) substs
  var_substs <- fmap mconcat $
    forM used_in_op $ \(v, (cs, src, src_dec, Slice is)) -> do
      v' <-
        certifying cs $
          letExp (baseString src <> "_op_idx") $
            BasicOp $ Index src $ fullSlice (typeOf src_dec) is
      pure $ M.singleton v v'
  pure $ Op $ substituteNames var_substs op
substituteIndicesInExp substs e = do
  substs' <- copyAnyConsumed e
  let substitute =
        identityMapper
          { mapOnSubExp = substituteIndicesInSubExp substs',
            mapOnVName = substituteIndicesInVar substs',
            mapOnBody = const $ substituteIndicesInBody substs'
          }

  mapExpM substitute e
  where
    copyAnyConsumed =
      let consumingSubst substs' v
            | Just (cs2, src2, src2dec, is2) <- lookup v substs = do
                row <-
                  certifying cs2 $
                    letExp (baseString v ++ "_row") $
                      BasicOp $ Index src2 $ fullSlice (typeOf src2dec) (unSlice is2)
                row_copy <-
                  letExp (baseString v ++ "_row_copy") $ BasicOp $ Copy row
                pure $
                  update
                    v
                    v
                    ( mempty,
                      row_copy,
                      src2dec
                        `setType` ( typeOf src2dec
                                      `setArrayDims` sliceDims is2
                                  ),
                      Slice []
                    )
                    substs'
          consumingSubst substs' _ =
            pure substs'
       in foldM consumingSubst substs . namesToList . consumedInExp

substituteIndicesInSubExp ::
  MonadBuilder m =>
  IndexSubstitutions ->
  SubExp ->
  m SubExp
substituteIndicesInSubExp substs (Var v) =
  Var <$> substituteIndicesInVar substs v
substituteIndicesInSubExp _ se =
  pure se

substituteIndicesInVar ::
  MonadBuilder m =>
  IndexSubstitutions ->
  VName ->
  m VName
substituteIndicesInVar substs v
  | Just (cs2, src2, _, Slice []) <- lookup v substs =
      certifying cs2 $
        letExp (baseString src2) $ BasicOp $ SubExp $ Var src2
  | Just (cs2, src2, src2_dec, Slice is2) <- lookup v substs =
      certifying cs2 $
        letExp (baseString src2 <> "_v_idx") $
          BasicOp $ Index src2 $ fullSlice (typeOf src2_dec) is2
  | otherwise =
      pure v

substituteIndicesInBody ::
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions ->
  Body (Rep m) ->
  m (Body (Rep m))
substituteIndicesInBody substs (Body _ stms res) = do
  (substs', stms') <-
    inScopeOf stms $
      collectStms $ substituteIndicesInStms substs stms
  (res', res_stms) <-
    inScopeOf stms' $
      collectStms $ mapM (onSubExpRes substs') res
  mkBodyM (stms' <> res_stms) res'
  where
    onSubExpRes substs' (SubExpRes cs se) =
      SubExpRes cs <$> substituteIndicesInSubExp substs' se

update ::
  VName ->
  VName ->
  IndexSubstitution ->
  IndexSubstitutions ->
  IndexSubstitutions
update needle name subst ((othername, othersubst) : substs)
  | needle == othername = (name, subst) : substs
  | otherwise = (othername, othersubst) : update needle name subst substs
update needle _ _ [] = error $ "Cannot find substitution for " ++ pretty needle
