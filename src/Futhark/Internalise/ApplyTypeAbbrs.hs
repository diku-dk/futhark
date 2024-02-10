-- | A minor cleanup pass that runs after defunctorisation and applies
-- any type abbreviations. After this, the program consists entirely
-- value bindings.
module Futhark.Internalise.ApplyTypeAbbrs (transformProg) where

import Control.Monad.Identity
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Language.Futhark
import Language.Futhark.Semantic (TypeBinding (..))
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

type Types = M.Map VName (Subst StructRetType)

getTypes :: Types -> [Dec] -> Types
getTypes types [] = types
getTypes types (TypeDec typebind : ds) = do
  let (TypeBind name l tparams _ (Info (RetType dims t)) _ _) = typebind
      tbinding = TypeAbbr l tparams $ RetType dims $ applySubst (`M.lookup` types) t
      types' = M.insert name (substFromAbbr tbinding) types
  getTypes types' ds
getTypes types (_ : ds) =
  getTypes types ds

-- Perform a given substitution on the types in a pattern.
substPat :: (t -> t) -> Pat t -> Pat t
substPat f pat = case pat of
  TuplePat pats loc -> TuplePat (map (substPat f) pats) loc
  RecordPat fs loc -> RecordPat (map substField fs) loc
    where
      substField (n, p) = (n, substPat f p)
  PatParens p loc -> PatParens (substPat f p) loc
  PatAttr attr p loc -> PatAttr attr (substPat f p) loc
  Id vn (Info tp) loc -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc -> Wildcard (Info $ f tp) loc
  PatAscription p _ _ -> substPat f p
  PatLit e (Info tp) loc -> PatLit e (Info $ f tp) loc
  PatConstr n (Info tp) ps loc -> PatConstr n (Info $ f tp) ps loc

removeTypeVariablesInType :: Types -> StructType -> StructType
removeTypeVariablesInType types =
  applySubst (`M.lookup` types)

substEntry :: Types -> EntryPoint -> EntryPoint
substEntry types (EntryPoint params ret) =
  EntryPoint (map onEntryParam params) (onEntryType ret)
  where
    onEntryParam (EntryParam v t) =
      EntryParam v $ onEntryType t
    onEntryType (EntryType t te) =
      EntryType (removeTypeVariablesInType types t) te

-- Remove all type variables and type abbreviations from a value binding.
removeTypeVariables :: Types -> ValBind -> ValBind
removeTypeVariables types valbind = do
  let (ValBind entry _ _ (Info (RetType dims rettype)) _ pats body _ _ _) = valbind
      mapper =
        ASTMapper
          { mapOnExp = onExp,
            mapOnName = pure,
            mapOnStructType = pure . applySubst (`M.lookup` types),
            mapOnParamType = pure . applySubst (`M.lookup` types),
            mapOnResRetType = pure . applySubst (`M.lookup` types)
          }
      onExp = astMap mapper

  let body' = runIdentity $ onExp body

  valbind
    { valBindRetType = Info (applySubst (`M.lookup` types) $ RetType dims rettype),
      valBindParams = map (substPat $ applySubst (`M.lookup` types)) pats,
      valBindEntryPoint = fmap (substEntry types) <$> entry,
      valBindBody = body'
    }

-- | Apply type abbreviations from a list of top-level declarations. A
-- module-free input program is expected, so only value declarations
-- and type declaration are accepted.
transformProg :: (Monad m) => [Dec] -> m [ValBind]
transformProg decs =
  let types = getTypes mempty decs
      onDec (ValDec valbind) = Just $ removeTypeVariables types valbind
      onDec _ = Nothing
   in pure $ mapMaybe onDec decs
