{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A representation where all bindings are annotated with range
-- information.
module Futhark.IR.Ranges
       ( -- * The Lore definition
         Ranges
       , module Futhark.IR.Prop.Ranges
         -- * Module re-exports
       , module Futhark.IR.Prop
       , module Futhark.IR.Traversals
       , module Futhark.IR.Pretty
       , module Futhark.IR.Syntax
         -- * Adding ranges
       , addRangesToPattern
       , mkRangedBody
       , mkPatternRanges
       , mkBodyRanges
         -- * Removing ranges
       , removeProgRanges
       , removeExpRanges
       , removeBodyRanges
       , removeStmRanges
       , removeLambdaRanges
       , removePatternRanges
       )
where

import Control.Monad.Identity
import Control.Monad.Reader

import Futhark.IR.Syntax
import Futhark.IR.Prop
import Futhark.IR.Prop.Aliases
import Futhark.IR.Prop.Ranges
import Futhark.IR.Traversals
import Futhark.IR.Pretty
import Futhark.Analysis.Rephrase
import qualified Futhark.Util.Pretty as PP

-- | The lore for the basic representation.
data Ranges lore

instance (Decorations lore, CanBeRanged (Op lore)) =>
         Decorations (Ranges lore) where
  type LetDec (Ranges lore) = (Range, LetDec lore)
  type ExpDec (Ranges lore) = ExpDec lore
  type BodyDec (Ranges lore) = ([Range], BodyDec lore)
  type FParamInfo (Ranges lore) = FParamInfo lore
  type LParamInfo (Ranges lore) = LParamInfo lore
  type RetType (Ranges lore) = RetType lore
  type BranchType (Ranges lore) = BranchType lore
  type Op (Ranges lore) = OpWithRanges (Op lore)

withoutRanges :: (HasScope (Ranges lore) m, Monad m) =>
                 ReaderT (Scope lore) m a ->
                 m a
withoutRanges m = do
  scope <- asksScope $ fmap unRange
  runReaderT m scope
    where unRange :: NameInfo (Ranges lore) -> NameInfo lore
          unRange (LetName (_, x)) = LetName x
          unRange (FParamName x) = FParamName x
          unRange (LParamName x) = LParamName x
          unRange (IndexName x) = IndexName x

instance (ASTLore lore, CanBeRanged (Op lore)) =>
         ASTLore (Ranges lore) where
  expTypesFromPattern =
    withoutRanges . expTypesFromPattern . removePatternRanges

instance RangeOf (Range, dec) where
  rangeOf = fst

instance RangesOf ([Range], dec) where
  rangesOf = fst

instance PrettyAnnot (PatElemT dec) =>
  PrettyAnnot (PatElemT (Range, dec)) where

  ppAnnot patelem =
    range_annot <> inner_annot
    where range_annot =
            case fst . patElemDec $ patelem of
              (Nothing, Nothing) -> Nothing
              range ->
                Just $ PP.oneLine $
                PP.text "-- " <> PP.ppr (patElemName patelem) <> PP.text " range: " <>
                PP.ppr range
          inner_annot = ppAnnot $ fmap snd patelem


instance (PrettyLore lore, CanBeRanged (Op lore)) => PrettyLore (Ranges lore) where
  ppExpLore dec = ppExpLore dec . removeExpRanges

removeRanges :: CanBeRanged (Op lore) => Rephraser Identity (Ranges lore) lore
removeRanges = Rephraser { rephraseExpLore = return
                         , rephraseLetBoundLore = return . snd
                         , rephraseBodyLore = return . snd
                         , rephraseFParamLore = return
                         , rephraseLParamLore = return
                         , rephraseRetType = return
                         , rephraseBranchType = return
                         , rephraseOp = return . removeOpRanges
                         }

removeProgRanges :: CanBeRanged (Op lore) =>
                    Prog (Ranges lore) -> Prog lore
removeProgRanges = runIdentity . rephraseProg removeRanges

removeExpRanges :: CanBeRanged (Op lore) =>
                   Exp (Ranges lore) -> Exp lore
removeExpRanges = runIdentity . rephraseExp removeRanges

removeBodyRanges :: CanBeRanged (Op lore) =>
                    Body (Ranges lore) -> Body lore
removeBodyRanges = runIdentity . rephraseBody removeRanges

removeStmRanges :: CanBeRanged (Op lore) =>
                       Stm (Ranges lore) -> Stm lore
removeStmRanges = runIdentity . rephraseStm removeRanges

removeLambdaRanges :: CanBeRanged (Op lore) =>
                      Lambda (Ranges lore) -> Lambda lore
removeLambdaRanges = runIdentity . rephraseLambda removeRanges

removePatternRanges :: PatternT (Range, a)
                    -> PatternT a
removePatternRanges = runIdentity . rephrasePattern (return . snd)

addRangesToPattern :: (ASTLore lore, CanBeRanged (Op lore)) =>
                      Pattern lore -> Exp (Ranges lore)
                   -> Pattern (Ranges lore)
addRangesToPattern pat e =
  uncurry Pattern $ mkPatternRanges pat e

mkRangedBody :: BodyDec lore -> Stms (Ranges lore) -> Result
             -> Body (Ranges lore)
mkRangedBody innerlore bnds res =
  Body (mkBodyRanges bnds res, innerlore) bnds res

mkPatternRanges :: (ASTLore lore, CanBeRanged (Op lore)) =>
                   Pattern lore
                -> Exp (Ranges lore)
                -> ([PatElemT (Range, LetDec lore)],
                    [PatElemT (Range, LetDec lore)])
mkPatternRanges pat e =
  (map (`addRanges` unknownRange) $ patternContextElements pat,
   zipWith addRanges (patternValueElements pat) ranges)
  where addRanges patElem range =
          let innerlore = patElemDec patElem
          in patElem `setPatElemLore` (range, innerlore)
        ranges = expRanges e

mkBodyRanges :: Stms lore -> Result -> [Range]
mkBodyRanges bnds = map $ removeUnknownBounds . rangeOf
  where boundInBnds =
          foldMap (namesFromList . patternNames . stmPattern) bnds
        removeUnknownBounds (lower,upper) =
          (removeUnknownBound lower,
           removeUnknownBound upper)
        removeUnknownBound (Just bound)
          | freeIn bound `namesIntersect` boundInBnds = Nothing
          | otherwise                                 = Just bound
        removeUnknownBound Nothing =
          Nothing

-- It is convenient for a wrapped aliased lore to also be aliased.

instance AliasesOf dec => AliasesOf ([Range], dec) where
  aliasesOf = aliasesOf . snd

instance AliasesOf dec => AliasesOf (Range, dec) where
  aliasesOf = aliasesOf . snd

instance (Aliased lore, CanBeRanged (Op lore),
          AliasedOp (OpWithRanges (Op lore))) => Aliased (Ranges lore) where
  bodyAliases = bodyAliases . removeBodyRanges
  consumedInBody = consumedInBody . removeBodyRanges
