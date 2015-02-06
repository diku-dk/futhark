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

import Futhark.Representation.AST
import Futhark.Tools
import Futhark.MonadFreshNames

type IndexSubstitution = (Certificates, Ident, [SubExp])
type IndexSubstitutions = [(VName, IndexSubstitution)]

substituteIndices :: (MonadFreshNames m, Bindable lore) =>
                     IndexSubstitutions -> [Binding lore]
                  -> m ([IndexSubstitution], [Binding lore])
substituteIndices substs bnds = do
  (substs', bnds') <-
    runBinder'' $ substituteIndicesInBindings substs bnds
  return (substs', bnds')

substituteIndicesInBindings :: MonadBinder m =>
                               IndexSubstitutions
                            -> [Binding (Lore m)]
                            -> m [IndexSubstitution]
substituteIndicesInBindings substs [] = return $ map snd substs
substituteIndicesInBindings substs (bnd:bnds) = do
  substs' <- substituteIndicesInBinding substs bnd
  substituteIndicesInBindings substs' bnds

substituteIndicesInBinding :: MonadBinder m =>
                              IndexSubstitutions
                           -> Binding (Lore m)
                           -> m IndexSubstitutions
substituteIndicesInBinding substs (Let pat _ (PrimOp (Update cs src is val)))
  | Just (cs2, src2, is2) <- lookup srcname substs,
    [name] <- patternNames pat = do
      patv' <- letExp (baseString name) $ PrimOp $
               Update (cs++cs2) src2 (is2++is) val
      return $ update srcname name (cs2, patv', is2) substs
  where srcname = identName src
substituteIndicesInBinding substs (Let pat lore e) = do
  e' <- mapExpM substitute e
  addBinding $ Let pat lore e'
  return substs
  where substitute = identityMapper { mapOnSubExp = substituteIndicesInSubExp substs
                                    , mapOnIdent  = substituteIndicesInIdent substs
                                    }

substituteIndicesInSubExp :: MonadBinder m =>
                             IndexSubstitutions
                          -> SubExp
                          -> m SubExp
substituteIndicesInSubExp substs (Var v) = Var <$> substituteIndicesInIdent substs v
substituteIndicesInSubExp _      se      = return se


substituteIndicesInIdent :: MonadBinder m =>
                            IndexSubstitutions
                         -> Ident
                         -> m Ident
substituteIndicesInIdent substs v
  | Just (cs2, src2, is2) <- lookup (identName v) substs =
    letExp "idx" $ PrimOp $ Index cs2 src2 is2
  | otherwise =
    return v

update :: VName -> VName -> IndexSubstitution -> IndexSubstitutions
       -> IndexSubstitutions
update needle name subst ((othername, othersubst) : substs)
  | needle == othername = (name, subst)           : substs
  | otherwise           = (othername, othersubst) : update needle name subst substs
update needle _    _ [] = error $ "Cannot find substitution for " ++ pretty needle
