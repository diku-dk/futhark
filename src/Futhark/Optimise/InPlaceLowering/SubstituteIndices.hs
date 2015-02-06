module Futhark.Optimise.InPlaceLowering.SubstituteIndices
       (
         substituteIndices
       , IndexSubstitution
       , IndexSubstitutions
       ) where

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
  | Just (cs2, src2, is2) <- lookup (identName src) substs,
    [name] <- patternNames pat = do
      patv' <- letExp (baseString name) $ PrimOp $
               Update (cs++cs2) src2 (is2++is) val
      return $ update name (cs2, patv', is2) substs
substituteIndicesInBinding substs (Let pat lore e) = do
  e' <- mapExpM substitute e
  addBinding $ Let pat lore e'
  return substs
  where substitute = identityMapper

update :: VName -> IndexSubstitution -> IndexSubstitutions
       -> IndexSubstitutions
update name subst ((othername, othersubst) : substs)
  | name == othername = (othername, othersubst) : update name subst substs
  | otherwise         = (name, subst)           : substs
update name _ [] = error $ "Cannot find substitution for " ++ pretty name
