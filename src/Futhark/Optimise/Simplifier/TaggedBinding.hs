{-# LANGUAGE FlexibleContexts #-}
-- | This module exports an extended variant of the 'Binding' type,
-- which is tagged with extra useful information, particularly a
-- symbol table entry corresponding to the binding.  This is a bit of
-- a hack to work around the fact that we cannot directly embed useful
-- information in the syntax tree yet.
module Futhark.Optimise.Simplifier.TaggedBinding
       ( TaggedBinding (..)
       , tagBinding
       , untagBinding
       , bindingEntries
       , asTail
       , usage
       , requires
       , provides
       )
       where

import Data.Maybe
import Data.Monoid
import Data.Loc
import qualified Data.HashSet as HS

import Futhark.Representation.AST

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Binder (Proper)

data TaggedBinding lore =
  TaggedLet (Pattern lore,[(VName,ST.Entry lore)]) (Lore.Exp lore) (Exp lore, UT.UsageTable)
  -- The [(VName,ST.Entry a)] is just the names
  -- of the idents, as a cache, alongside the
  -- symbol table entry for each of those names.
  -- Similarly, the expression is tagged with
  -- what is free in it.

untagBinding :: TaggedBinding lore -> Binding lore
untagBinding (TaggedLet (pat,_) lore (e,_)) = Let pat lore e

tagBinding :: Proper lore => ST.SymbolTable lore -> Binding lore -> TaggedBinding lore
tagBinding vtable bnd@(Let pat lore e) =
  TaggedLet (pat, zip names entries) lore (e, usageInBinding bnd)
  where entries = ST.bindingEntries (Let pat lore e) vtable
        names = patternNames pat

bindingEntries :: TaggedBinding lore -> [(VName, ST.Entry lore)]
bindingEntries (TaggedLet (_,ds) _ _) = ds

asTail :: TaggedBinding lore -> Body lore
asTail (TaggedLet (pat,_) lore (e,_)) =
  Body [Let pat lore e] $ Result [] [] loc
  where loc = srclocOf e

usage :: TaggedBinding lore -> UT.UsageTable
usage (TaggedLet _ _ (_,usedInE)) = usedInE

requires :: FreeIn (Lore.Exp lore) => TaggedBinding lore -> Names
requires (TaggedLet (pat,_) lore (_,usedInE)) =
  HS.fromList (UT.keys usedInE) `mappend` freeInPat `mappend` freeNamesIn lore
  where freeInPat  = mconcat $ map (freeInType . identType) $ patternIdents pat

provides :: TaggedBinding lore -> [VName]
provides (TaggedLet (_,provs) _ _) = map fst provs

freeInType :: Type -> Names
freeInType = mconcat . map freeNamesInSubExp . arrayDims

usageInBinding :: Proper lore => Binding lore -> UT.UsageTable
usageInBinding (Let pat _ e) =
  usageInPat pat <> usageInPatLore pat <> usageInExp e <> UT.usages (freeNamesInExp e)
  where usageInPat =
          UT.usages . HS.fromList . mapMaybe subExpUsage .
          concatMap (arrayDims . identType) . patternIdents
        usageInPatLore = UT.usages . mconcat . map (freeNamesIn . bindeeLore) . patternBindees
        subExpUsage (Var v)       = Just $ identName v
        subExpUsage (Constant {}) = Nothing

usageInExp :: Exp lore -> UT.UsageTable
usageInExp (Assert (Var v) _) = UT.predicateUsage $ identName v
usageInExp (Update _ src _ _ _) =
  mconcat $ map UT.consumedUsage $
  identName src : HS.toList (aliases $ identType src)
usageInExp (Apply _ args _ _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ aliases $ subExpType arg
          | (arg,d) <- args, d == Consume ]
usageInExp (DoLoop _ merge _ _ _ _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ aliases $ subExpType se
          | (v,se) <- merge, unique $ identType v ]
usageInExp (Map _ f args _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ aliases $ subExpType se
          | (v,se) <- zip (lambdaParams f) args,
            unique $ identType v ]
usageInExp (Reduce _ f args _) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map (aliases . subExpType) $ acc ++ arr,
            unique $ identType v ]
  where (acc, arr) = unzip args
usageInExp (Scan _ f args _) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map (aliases . subExpType) $ acc ++ arr,
            unique $ identType v ]
  where (acc, arr) = unzip args
usageInExp (Redomap _ _ f acc arr _) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map (aliases . subExpType) $ acc ++ arr,
            unique $ identType v ]
usageInExp _ = UT.empty
