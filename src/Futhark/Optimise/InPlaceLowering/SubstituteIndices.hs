{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Monoid
import Control.Monad
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST
import Futhark.Construct
import Futhark.MonadFreshNames
import Futhark.Util

type IndexSubstitution attr = (Certificates, VName, attr, Slice SubExp)
type IndexSubstitutions attr = [(VName, IndexSubstitution attr)]

typeEnvFromSubstitutions :: LetAttr lore ~ attr =>
                            IndexSubstitutions attr -> Scope lore
typeEnvFromSubstitutions = HM.fromList . map (fromSubstitution . snd)
  where fromSubstitution (_, name, t, _) =
          (name, LetInfo t)

substituteIndices :: (MonadFreshNames m, Bindable lore, Aliased lore, LetAttr lore ~ attr) =>
                     IndexSubstitutions attr -> [Binding lore]
                  -> m (IndexSubstitutions attr, [Binding lore])
substituteIndices substs bnds =
  runBinderT (substituteIndicesInBindings substs bnds) types
  where types = typeEnvFromSubstitutions substs

substituteIndicesInBindings :: (MonadBinder m, Bindable (Lore m), Aliased (Lore m),
                                LocalScope (Lore m) m) =>
                               IndexSubstitutions (LetAttr (Lore m))
                            -> [Binding (Lore m)]
                            -> m (IndexSubstitutions (LetAttr (Lore m)))
substituteIndicesInBindings = foldM substituteIndicesInBinding

substituteIndicesInBinding :: (MonadBinder m, Bindable (Lore m), Aliased (Lore m),
                               LocalScope (Lore m) m) =>
                              IndexSubstitutions (LetAttr (Lore m))
                           -> Binding (Lore m)
                           -> m (IndexSubstitutions (LetAttr (Lore m)))
substituteIndicesInBinding substs (Let pat lore e) = do
  e' <- substituteIndicesInExp substs e
  (substs', pat') <- substituteIndicesInPattern substs pat
  addBinding $ Let pat' lore e'
  return substs'

substituteIndicesInPattern :: (MonadBinder m, SetType attr,
                               Aliased (Lore m),
                               LetAttr (Lore m) ~ attr) =>
                              IndexSubstitutions (LetAttr (Lore m))
                           -> PatternT attr
                           -> m (IndexSubstitutions (LetAttr (Lore m)), PatternT attr)
substituteIndicesInPattern substs pat = do
  (substs', context) <- mapAccumLM sub substs $ patternContextElements pat
  (substs'', values) <- mapAccumLM sub substs' $ patternValueElements pat
  return (substs'', Pattern context values)
  where sub substs' (PatElem name (BindInPlace cs src is) attr)
          | Just (cs2, src2, src2attr, is2) <- lookup src substs = do
              let attr' = attr `setType` typeOf src2attr
              return (update src name (cs2, name, attr', is2) substs',
                      PatElem name (BindInPlace (cs++cs2) src2 $ is2++is) attr')
        sub substs' patElem =
          return (substs', patElem)

substituteIndicesInExp :: (MonadBinder m, Bindable (Lore m), Aliased (Lore m),
                           LocalScope (Lore m) m,
                           LetAttr (Lore m) ~ attr) =>
                          IndexSubstitutions (LetAttr (Lore m))
                       -> Exp (Lore m)
                       -> m (Exp (Lore m))
substituteIndicesInExp substs e = do
  substs' <- copyAnyConsumed e
  let substitute = identityMapper { mapOnSubExp = substituteIndicesInSubExp substs'
                                  , mapOnVName  = substituteIndicesInVar substs'
                                  , mapOnBody   = substituteIndicesInBody substs'
                                  }

  mapExpM substitute e
  where copyAnyConsumed =
          let consumingSubst substs' v
                | Just (cs2, src2, src2attr, is2) <- lookup v substs = do
                    row <- letExp (baseString v ++ "_row") $
                           PrimOp $ Index cs2 src2 is2
                    row_copy <- letExp (baseString v ++ "_row_copy") $
                                PrimOp $ Copy row
                    return $ update v v ([],
                                         row_copy,
                                         src2attr `setType`
                                         stripArray (length is2) (typeOf src2attr),
                                         []) substs'
              consumingSubst substs' _ =
                return substs'
          in foldM consumingSubst substs . HS.toList . consumedInExp

substituteIndicesInSubExp :: MonadBinder m =>
                             IndexSubstitutions (LetAttr (Lore m))
                          -> SubExp
                          -> m SubExp
substituteIndicesInSubExp substs (Var v) = Var <$> substituteIndicesInVar substs v
substituteIndicesInSubExp _      se      = return se

substituteIndicesInVar :: MonadBinder m =>
                          IndexSubstitutions (LetAttr (Lore m))
                       -> VName
                       -> m VName
substituteIndicesInVar substs v
  | Just ([], src2, _, []) <- lookup v substs =
    letExp (baseString src2) $ PrimOp $ SubExp $ Var src2
  | Just (cs2, src2, _, is2) <- lookup v substs =
    letExp "idx" $ PrimOp $ Index cs2 src2 is2
  | otherwise =
    return v

substituteIndicesInBody :: (MonadBinder m, Bindable (Lore m), Aliased (Lore m),
                            LocalScope (Lore m) m) =>
                           IndexSubstitutions (LetAttr (Lore m))
                        -> Body (Lore m)
                        -> m (Body (Lore m))
substituteIndicesInBody substs body = do
  (substs', bnds') <- inScopeOf bnds $
    collectBindings $ substituteIndicesInBindings substs bnds
  (ses, ses_bnds) <- inScopeOf bnds' $
    collectBindings $ mapM (substituteIndicesInSubExp substs') $ bodyResult body
  mkBodyM (bnds'<>ses_bnds) ses
  where bnds = bodyBindings body

update :: VName -> VName -> IndexSubstitution attr -> IndexSubstitutions attr
       -> IndexSubstitutions attr
update needle name subst ((othername, othersubst) : substs)
  | needle == othername = (name, subst)           : substs
  | otherwise           = (othername, othersubst) : update needle name subst substs
update needle _    _ [] = error $ "Cannot find substitution for " ++ pretty needle
