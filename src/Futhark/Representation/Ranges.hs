{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A representation where all bindings are annotated with range
-- information.
module Futhark.Representation.Ranges
       ( -- * The Lore definition
         Ranges
       , module Futhark.Representation.AST.Attributes.Ranges
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
         -- * Adding ranges
       , addRangesToPattern
       , mkRangedLetStm
       , mkRangedBody
       , mkPatternRanges
       , mkBodyRanges
         -- * Removing ranges
       , removeProgRanges
       , removeFunDefRanges
       , removeExpRanges
       , removeBodyRanges
       , removeStmRanges
       , removeLambdaRanges
       , removeExtLambdaRanges
       , removePatternRanges
       )
where

import Control.Monad.Identity
import qualified Data.Set as S
import Data.Hashable
import Data.Maybe
import Data.Monoid

import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Analysis.Rephrase
import qualified Futhark.Util.Pretty as PP

-- | The lore for the basic representation.
data Ranges lore

instance (Annotations lore, CanBeRanged (Op lore)) =>
         Annotations (Ranges lore) where
  type LetAttr (Ranges lore) = (Range, LetAttr lore)
  type ExpAttr (Ranges lore) = ExpAttr lore
  type BodyAttr (Ranges lore) = ([Range], BodyAttr lore)
  type FParamAttr (Ranges lore) = FParamAttr lore
  type LParamAttr (Ranges lore) = LParamAttr lore
  type RetType (Ranges lore) = RetType lore
  type Op (Ranges lore) = OpWithRanges (Op lore)

instance (Attributes lore, CanBeRanged (Op lore)) =>
         Attributes (Ranges lore) where

instance RangeOf (Range, attr) where
  rangeOf = fst

instance RangesOf ([Range], attr) where
  rangesOf = fst

instance PrettyAnnot (PatElemT attr) =>
  PrettyAnnot (PatElemT (Range, attr)) where

  ppAnnot patelem =
    range_annot <> inner_annot
    where range_annot =
            case fst . patElemAttr $ patelem of
              (Nothing, Nothing) -> Nothing
              range ->
                Just $ PP.oneLine $
                PP.text "-- " <> PP.ppr (patElemName patelem) <> PP.text " range: " <>
                PP.ppr range
          inner_annot = ppAnnot $ fmap snd patelem


instance (PrettyLore lore, CanBeRanged (Op lore)) => PrettyLore (Ranges lore) where
  ppExpLore attr = ppExpLore attr . removeExpRanges

removeRanges :: CanBeRanged (Op lore) => Rephraser Identity (Ranges lore) lore
removeRanges = Rephraser { rephraseExpLore = return
                         , rephraseLetBoundLore = return . snd
                         , rephraseBodyLore = return . snd
                         , rephraseFParamLore = return
                         , rephraseLParamLore = return
                         , rephraseRetType = return
                         , rephraseOp = return . removeOpRanges
                         }

removeProgRanges :: CanBeRanged (Op lore) =>
                    Prog (Ranges lore) -> Prog lore
removeProgRanges = runIdentity . rephraseProg removeRanges

removeFunDefRanges :: CanBeRanged (Op lore) =>
                      FunDef (Ranges lore) -> FunDef lore
removeFunDefRanges = runIdentity . rephraseFunDef removeRanges

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

removeExtLambdaRanges :: CanBeRanged (Op lore) =>
                         ExtLambda (Ranges lore) -> ExtLambda lore
removeExtLambdaRanges = runIdentity . rephraseExtLambda removeRanges

removePatternRanges :: PatternT (Range, a)
                    -> PatternT a
removePatternRanges = runIdentity . rephrasePattern (return . snd)

addRangesToPattern :: (Attributes lore, CanBeRanged (Op lore)) =>
                      Pattern lore -> Exp (Ranges lore)
                   -> Pattern (Ranges lore)
addRangesToPattern pat e =
  uncurry Pattern $ mkPatternRanges pat e

mkRangedBody :: BodyAttr lore -> [Stm (Ranges lore)] -> Result
             -> Body (Ranges lore)
mkRangedBody innerlore bnds res =
  Body (mkBodyRanges bnds res, innerlore) bnds res

mkPatternRanges :: (Attributes lore, CanBeRanged (Op lore)) =>
                   Pattern lore
                -> Exp (Ranges lore)
                -> ([PatElemT (Range, LetAttr lore)],
                    [PatElemT (Range, LetAttr lore)])
mkPatternRanges pat e =
  (map (`addRanges` unknownRange) $ patternContextElements pat,
   zipWith addRanges (patternValueElements pat) ranges)
  where addRanges patElem range =
          let innerlore = patElemAttr patElem
              range' = case patElemBindage patElem of BindVar -> range
                                                      _       -> unknownRange
          in patElem `setPatElemLore` (range', innerlore)
        ranges = expRanges e

mkBodyRanges :: [Stm lore] -> Result -> [Range]
mkBodyRanges bnds = map $ removeUnknownBounds . rangeOf
  where boundInBnds =
          mconcat $ map (S.fromList . patternNames . stmPattern) bnds
        removeUnknownBounds (lower,upper) =
          (removeUnknownBound lower,
           removeUnknownBound upper)
        removeUnknownBound (Just bound)
          | freeIn bound `intersects` boundInBnds = Nothing
          | otherwise                             = Just bound
        removeUnknownBound Nothing =
          Nothing

intersects :: (Ord a, Hashable a) => S.Set a -> S.Set a -> Bool
intersects a b = not $ S.null $ a `S.intersection` b

mkRangedLetStm :: (Attributes lore, CanBeRanged (Op lore)) =>
                  Pattern lore
               -> ExpAttr lore
               -> Exp (Ranges lore)
               -> Stm (Ranges lore)
mkRangedLetStm pat explore e =
  Let (addRangesToPattern pat e) explore e
