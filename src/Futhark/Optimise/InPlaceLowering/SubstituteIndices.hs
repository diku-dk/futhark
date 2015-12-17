{-# LANGUAGE FlexibleContexts #-}
-- | This module exports facilities for transforming array accesses in
-- a list of 'Binding's (intended to be the bindings in a body).  The
-- idea is that you can state that some variable @x@ is in fact an
-- array indexing @v[i0,i1,...]@.
module Futhark.Optimise.InPlaceLowering.SubstituteIndices
       (
         substituteIndices
       , IndexSubstitution
       , IndexSubstitutions
       ) where

import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST
import Futhark.Construct
import Futhark.MonadFreshNames
import Futhark.Util

type IndexSubstitution = (Certificates, VName, Type, [SubExp])
type IndexSubstitutions = [(VName, IndexSubstitution)]

typeEnvFromSubstitutions :: IndexSubstitutions -> TypeEnv
typeEnvFromSubstitutions = HM.fromList . map (fromSubstitution . snd)
  where fromSubstitution (_, name, t, _) =
          (name, t)

substituteIndices :: (MonadFreshNames m, Bindable lore) =>
                     IndexSubstitutions -> [Binding lore]
                  -> m (IndexSubstitutions, [Binding lore])
substituteIndices substs bnds =
  runBinderT (substituteIndicesInBindings substs bnds) types
  where types = typeEnvFromSubstitutions substs

substituteIndicesInBindings :: (MonadBinder m, Bindable (Lore m)) =>
                               IndexSubstitutions
                            -> [Binding (Lore m)]
                            -> m IndexSubstitutions
substituteIndicesInBindings = foldM substituteIndicesInBinding

substituteIndicesInBinding :: (MonadBinder m, Bindable (Lore m)) =>
                              IndexSubstitutions
                           -> Binding (Lore m)
                           -> m IndexSubstitutions
substituteIndicesInBinding substs (Let pat lore e) = do
  e' <- substituteIndicesInExp substs e
  (substs', pat') <- substituteIndicesInPattern substs pat
  addBinding $ Let pat' lore e'
  return substs'

substituteIndicesInPattern :: (MonadBinder m, SetType attr) =>
                              IndexSubstitutions
                           -> PatternT attr
                           -> m (IndexSubstitutions, PatternT attr)
substituteIndicesInPattern substs pat = do
  (substs', context) <- mapAccumLM sub substs $ patternContextElements pat
  (substs'', values) <- mapAccumLM sub substs' $ patternValueElements pat
  return (substs'', Pattern context values)
  where sub substs' (PatElem name (BindInPlace cs src is) attr)
          | Just (cs2, src2, src2t, is2) <- lookup src substs = do
              let attr' = attr `setType` src2t
              return (update src name (cs2, name, typeOf attr', is2) substs',
                      PatElem name (BindInPlace (cs++cs2) src2 (is2++is)) attr')
        sub substs' patElem =
          return (substs', patElem)

substituteIndicesInExp :: (MonadBinder m, Bindable (Lore m)) =>
                          IndexSubstitutions
                       -> Exp (Lore m)
                       -> m (Exp (Lore m))
substituteIndicesInExp substs e = do
  substs' <- copyAnyConsumed e
  let substitute = identityMapper { mapOnSubExp = substituteIndicesInSubExp substs'
                                  , mapOnVName  = substituteIndicesInVar substs'
                                  , mapOnBody   = substituteIndicesInBody substs'
                                  }

  mapExpM substitute e
  where -- FIXME: quick and dirty - really, we should use
        -- 'consumedInExp' to also handle branches and function calls.
        -- Another approach would be to just copy out the rows instead
        -- of all this substitute business.
        copyAnyConsumed (LoopOp (DoLoop _ merge _ _)) =
          let consumingSubst substs' (fparam, Var v)
                | unique (paramDeclType fparam),
                  Just (cs2, src2, src2t, is2) <- lookup v substs = do
                    row <- letExp (baseString v ++ "_row") $
                           PrimOp $ Index cs2 src2 is2
                    row_copy <- letExp (baseString v ++ "_row_copy") $
                                PrimOp $ Copy row
                    return $ update v v ([],
                                         row_copy,
                                         stripArray (length is2) src2t,
                                         []) substs'
              consumingSubst substs' _ =
                return substs'
          in foldM consumingSubst substs merge
        copyAnyConsumed _ = return substs

substituteIndicesInSubExp :: MonadBinder m =>
                             IndexSubstitutions
                          -> SubExp
                          -> m SubExp
substituteIndicesInSubExp substs (Var v) = Var <$> substituteIndicesInVar substs v
substituteIndicesInSubExp _      se      = return se

substituteIndicesInVar :: MonadBinder m =>
                          IndexSubstitutions
                       -> VName
                       -> m VName
substituteIndicesInVar substs v
  | Just ([], src2, _, []) <- lookup v substs =
    letExp (baseString src2) $ PrimOp $ SubExp $ Var src2
  | Just (cs2, src2, _, is2) <- lookup v substs =
    letExp "idx" $ PrimOp $ Index cs2 src2 is2
  | otherwise =
    return v

substituteIndicesInBody :: (MonadBinder m, Bindable (Lore m)) =>
                           IndexSubstitutions
                        -> Body (Lore m)
                        -> m (Body (Lore m))
substituteIndicesInBody substs body = do
  (substs', bnds) <-
    collectBindings $ substituteIndicesInBindings substs $ bodyBindings body
  ses <-
    mapM (substituteIndicesInSubExp substs') $ bodyResult body
  mkBodyM bnds ses

update :: VName -> VName -> IndexSubstitution -> IndexSubstitutions
       -> IndexSubstitutions
update needle name subst ((othername, othersubst) : substs)
  | needle == othername = (name, subst)           : substs
  | otherwise           = (othername, othersubst) : update needle name subst substs
update needle _    _ [] = error $ "Cannot find substitution for " ++ pretty needle
