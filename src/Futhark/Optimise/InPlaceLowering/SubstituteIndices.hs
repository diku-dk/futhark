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
import Futhark.Util

type IndexSubstitution dec = (Certificates, VName, dec, Slice SubExp)

type IndexSubstitutions dec = [(VName, IndexSubstitution dec)]

typeEnvFromSubstitutions ::
  LetDec lore ~ dec =>
  IndexSubstitutions dec ->
  Scope lore
typeEnvFromSubstitutions = M.fromList . map (fromSubstitution . snd)
  where
    fromSubstitution (_, name, t, _) =
      (name, LetName t)

-- | Perform the substitution.
substituteIndices ::
  ( MonadFreshNames m,
    BinderOps lore,
    Bindable lore,
    Aliased lore,
    LetDec lore ~ dec
  ) =>
  IndexSubstitutions dec ->
  Stms lore ->
  m (IndexSubstitutions dec, Stms lore)
substituteIndices substs bnds =
  runBinderT (substituteIndicesInStms substs bnds) types
  where
    types = typeEnvFromSubstitutions substs

substituteIndicesInStms ::
  (MonadBinder m, Bindable (Lore m), Aliased (Lore m)) =>
  IndexSubstitutions (LetDec (Lore m)) ->
  Stms (Lore m) ->
  m (IndexSubstitutions (LetDec (Lore m)))
substituteIndicesInStms = foldM substituteIndicesInStm

substituteIndicesInStm ::
  (MonadBinder m, Bindable (Lore m), Aliased (Lore m)) =>
  IndexSubstitutions (LetDec (Lore m)) ->
  Stm (Lore m) ->
  m (IndexSubstitutions (LetDec (Lore m)))
substituteIndicesInStm substs (Let pat lore e) = do
  e' <- substituteIndicesInExp substs e
  (substs', pat') <- substituteIndicesInPattern substs pat
  addStm $ Let pat' lore e'
  return substs'

substituteIndicesInPattern ::
  (MonadBinder m, LetDec (Lore m) ~ dec) =>
  IndexSubstitutions (LetDec (Lore m)) ->
  PatternT dec ->
  m (IndexSubstitutions (LetDec (Lore m)), PatternT dec)
substituteIndicesInPattern substs pat = do
  (substs', context) <- mapAccumLM sub substs $ patternContextElements pat
  (substs'', values) <- mapAccumLM sub substs' $ patternValueElements pat
  return (substs'', Pattern context values)
  where
    sub substs' patElem = return (substs', patElem)

substituteIndicesInExp ::
  ( MonadBinder m,
    Bindable (Lore m),
    Aliased (Lore m),
    LetDec (Lore m) ~ dec
  ) =>
  IndexSubstitutions (LetDec (Lore m)) ->
  Exp (Lore m) ->
  m (Exp (Lore m))
substituteIndicesInExp substs (Op op) = do
  let used_in_op = filter ((`nameIn` freeIn op) . fst) substs
  var_substs <- fmap mconcat $
    forM used_in_op $ \(v, (cs, src, src_dec, is)) -> do
      v' <-
        certifying cs $
          letExp "idx" $ BasicOp $ Index src $ fullSlice (typeOf src_dec) is
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
                    BasicOp $ Index src2 $ fullSlice (typeOf src2dec) is2
              row_copy <-
                letExp (baseString v ++ "_row_copy") $
                  BasicOp $ Copy row
              return $
                update
                  v
                  v
                  ( mempty,
                    row_copy,
                    src2dec
                      `setType` ( typeOf src2dec
                                    `setArrayDims` sliceDims is2
                                ),
                    []
                  )
                  substs'
          consumingSubst substs' _ =
            return substs'
       in foldM consumingSubst substs . namesToList . consumedInExp

substituteIndicesInSubExp ::
  MonadBinder m =>
  IndexSubstitutions (LetDec (Lore m)) ->
  SubExp ->
  m SubExp
substituteIndicesInSubExp substs (Var v) =
  Var <$> substituteIndicesInVar substs v
substituteIndicesInSubExp _ se =
  return se

substituteIndicesInVar ::
  MonadBinder m =>
  IndexSubstitutions (LetDec (Lore m)) ->
  VName ->
  m VName
substituteIndicesInVar substs v
  | Just (cs2, src2, _, []) <- lookup v substs =
    certifying cs2 $
      letExp (baseString src2) $ BasicOp $ SubExp $ Var src2
  | Just (cs2, src2, src2_dec, is2) <- lookup v substs =
    certifying cs2 $
      letExp "idx" $ BasicOp $ Index src2 $ fullSlice (typeOf src2_dec) is2
  | otherwise =
    return v

substituteIndicesInBody ::
  (MonadBinder m, Bindable (Lore m), Aliased (Lore m)) =>
  IndexSubstitutions (LetDec (Lore m)) ->
  Body (Lore m) ->
  m (Body (Lore m))
substituteIndicesInBody substs (Body _ stms res) = do
  (substs', stms') <-
    inScopeOf stms $
      collectStms $ substituteIndicesInStms substs stms
  (res', res_stms) <-
    inScopeOf stms' $
      collectStms $ mapM (substituteIndicesInSubExp substs') res
  mkBodyM (stms' <> res_stms) res'

update ::
  VName ->
  VName ->
  IndexSubstitution dec ->
  IndexSubstitutions dec ->
  IndexSubstitutions dec
update needle name subst ((othername, othersubst) : substs)
  | needle == othername = (name, subst) : substs
  | otherwise = (othername, othersubst) : update needle name subst substs
update needle _ _ [] = error $ "Cannot find substitution for " ++ pretty needle
