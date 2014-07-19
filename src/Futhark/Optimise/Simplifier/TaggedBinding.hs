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

import Futhark.Representation.Basic

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT

data TaggedBinding a = TaggedLet ([Ident],[(VName,ST.Entry a)]) (Exp, UT.UsageTable)
                       -- The [(VName,ST.Entry a)] is just the names
                       -- of the idents, as a cache, alongside the
                       -- symbol table entry for each of those names.
                       -- Similarly, the expression is tagged with
                       -- what is free in it.
                     deriving (Show, Eq)

untagBinding :: TaggedBinding u -> Binding
untagBinding (TaggedLet (pat,_) (e,_)) = Let pat () e

tagBinding :: ST.SymbolTable u -> Binding -> TaggedBinding u
tagBinding vtable (Let pat () e) =
  TaggedLet (pat, zip names entries) (e, usageInBinding $ Let pat () e)
  where entries = ST.bindingEntries (Let pat () e) vtable
        names = map identName pat

bindingEntries :: TaggedBinding u -> [(VName, ST.Entry u)]
bindingEntries (TaggedLet (_,ds) _) = ds

asTail :: TaggedBinding a -> Body
asTail (TaggedLet (pat,_) (e,_)) = Body [Let pat () e] $ Result [] [] loc
  where loc = srclocOf pat

usage :: TaggedBinding a -> UT.UsageTable
usage (TaggedLet _ (_,usedInE)) = usedInE

requires :: TaggedBinding a -> Names
requires (TaggedLet (pat,_) (_,usedInE)) =
  HS.fromList (UT.keys usedInE) `mappend` freeInPat
  where freeInPat  = mconcat $ map (freeInType . identType) pat

provides :: TaggedBinding a -> [VName]
provides (TaggedLet (_,provs) _) = map fst provs

freeInType :: Type -> Names
freeInType = mconcat . map (freeNamesInExp . SubExp) . arrayDims

usageInBinding :: Binding -> UT.UsageTable
usageInBinding (Let pat () e) =
  usageInPat pat <> usageInExp e <> UT.usages (freeNamesInExp e)
  where usageInPat =
          UT.usages . HS.fromList . mapMaybe subExpUsage .
          concatMap (arrayDims . identType)
        subExpUsage (Var v)       = Just $ identName v
        subExpUsage (Constant {}) = Nothing

usageInExp :: Exp -> UT.UsageTable
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
