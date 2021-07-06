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
  LetDec rep ~ dec =>
  IndexSubstitutions dec ->
  Scope rep
typeEnvFromSubstitutions = M.fromList . map (fromSubstitution . snd)
  where
    fromSubstitution (_, name, t, _) =
      (name, LetName t)

-- | Perform the substitution.
substituteIndices ::
  ( MonadFreshNames m,
    BuilderOps rep,
    Buildable rep,
    Aliased rep,
    LetDec rep ~ dec
  ) =>
  IndexSubstitutions dec ->
  Stms rep ->
  m (IndexSubstitutions dec, Stms rep)
substituteIndices substs bnds =
  runBuilderT (substituteIndicesInStms substs bnds) types
  where
    types = typeEnvFromSubstitutions substs

substituteIndicesInStms ::
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions (LetDec (Rep m)) ->
  Stms (Rep m) ->
  m (IndexSubstitutions (LetDec (Rep m)))
substituteIndicesInStms = foldM substituteIndicesInStm

substituteIndicesInStm ::
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions (LetDec (Rep m)) ->
  Stm (Rep m) ->
  m (IndexSubstitutions (LetDec (Rep m)))
substituteIndicesInStm substs (Let pat rep e) = do
  e' <- substituteIndicesInExp substs e
  (substs', pat') <- substituteIndicesInPattern substs pat
  addStm $ Let pat' rep e'
  return substs'

substituteIndicesInPattern ::
  (MonadBuilder m, LetDec (Rep m) ~ dec) =>
  IndexSubstitutions (LetDec (Rep m)) ->
  PatternT dec ->
  m (IndexSubstitutions (LetDec (Rep m)), PatternT dec)
substituteIndicesInPattern substs pat = do
  (substs', context) <- mapAccumLM sub substs $ patternContextElements pat
  (substs'', values) <- mapAccumLM sub substs' $ patternValueElements pat
  return (substs'', Pattern context values)
  where
    sub substs' patElem = return (substs', patElem)

substituteIndicesInExp ::
  ( MonadBuilder m,
    Buildable (Rep m),
    Aliased (Rep m),
    LetDec (Rep m) ~ dec
  ) =>
  IndexSubstitutions (LetDec (Rep m)) ->
  Exp (Rep m) ->
  m (Exp (Rep m))
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
  MonadBuilder m =>
  IndexSubstitutions (LetDec (Rep m)) ->
  SubExp ->
  m SubExp
substituteIndicesInSubExp substs (Var v) =
  Var <$> substituteIndicesInVar substs v
substituteIndicesInSubExp _ se =
  return se

substituteIndicesInVar ::
  MonadBuilder m =>
  IndexSubstitutions (LetDec (Rep m)) ->
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
  (MonadBuilder m, Buildable (Rep m), Aliased (Rep m)) =>
  IndexSubstitutions (LetDec (Rep m)) ->
  Body (Rep m) ->
  m (Body (Rep m))
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
