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
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PrimOp
       , Exp
       , Lambda
       , ExtLambda
       , FunDef
       , RetType
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.ExtLambdaT(ExtLambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.FunDefT(FunDef)
         -- * Adding ranges
       , addRangesToPattern
       , mkRangedLetBinding
       , mkRangedBody
       , mkPatternRanges
       , mkBodyRanges
         -- * Removing ranges
       , removeProgRanges
       , removeFunDefRanges
       , removeExpRanges
       , removeBodyRanges
       , removeBindingRanges
       , removeLambdaRanges
       , removeExtLambdaRanges
       , removePatternRanges
       )
where

import qualified Data.HashSet as HS
import Data.Hashable
import Data.Maybe
import Data.Monoid

import Prelude

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, Exp, Body, Binding,
          Pattern, Lambda, ExtLambda, FunDef, RetType)
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
  type RetType (Ranges lore) = AST.RetType lore
  type Op (Ranges lore) = OpWithRanges (Op lore)

instance (Attributes lore, CanBeRanged (Op lore)) =>
         Attributes (Ranges lore) where

instance RangeOf (Range, attr) where
  rangeOf = fst

instance RangesOf ([Range], attr) where
  rangesOf = fst

type Prog lore = AST.Prog (Ranges lore)
type PrimOp lore = AST.PrimOp (Ranges lore)
type Exp lore = AST.Exp (Ranges lore)
type Body lore = AST.Body (Ranges lore)
type Binding lore = AST.Binding (Ranges lore)
type Pattern lore = AST.Pattern (Ranges lore)
type Lambda lore = AST.Lambda (Ranges lore)
type ExtLambda lore = AST.ExtLambda (Ranges lore)
type FunDef lore = AST.FunDef (Ranges lore)
type RetType lore = AST.RetType (Ranges lore)

instance (PrettyLore lore, CanBeRanged (Op lore)) => PrettyLore (Ranges lore) where
  ppBindingLore binding@(Let pat _ _) =
    case catMaybes [patElemComments,
                    ppBindingLore $ removeBindingRanges binding] of
      [] -> Nothing
      ls -> Just $ PP.folddoc (PP.</>) ls
    where patElemComments =
            case mapMaybe patElemComment $ patternElements pat of
              []    -> Nothing
              attrs -> Just $ PP.folddoc (PP.</>) attrs
          patElemComment patelem =
            case fst . patElemAttr $ patelem of
              (Nothing, Nothing) -> Nothing
              range ->
                Just $ oneline $
                PP.text "-- " <> PP.ppr (patElemName patelem) <> PP.text " range: " <>
                PP.ppr range
          oneline s = PP.text $ PP.displayS (PP.renderCompact s) ""

  ppFunDefLore = ppFunDefLore . removeFunDefRanges
  ppExpLore = ppExpLore . removeExpRanges

removeRanges :: CanBeRanged (Op lore) => Rephraser (Ranges lore) lore
removeRanges = Rephraser { rephraseExpLore = id
                         , rephraseLetBoundLore = snd
                         , rephraseBodyLore = snd
                         , rephraseFParamLore = id
                         , rephraseLParamLore = id
                         , rephraseRetType = id
                         , rephraseOp = removeOpRanges
                         }

removeProgRanges :: CanBeRanged (Op lore) =>
                    AST.Prog (Ranges lore) -> AST.Prog lore
removeProgRanges = rephraseProg removeRanges

removeFunDefRanges :: CanBeRanged (Op lore) =>
                      AST.FunDef (Ranges lore) -> AST.FunDef lore
removeFunDefRanges = rephraseFunDef removeRanges

removeExpRanges :: CanBeRanged (Op lore) =>
                   AST.Exp (Ranges lore) -> AST.Exp lore
removeExpRanges = rephraseExp removeRanges

removeBodyRanges :: CanBeRanged (Op lore) =>
                    AST.Body (Ranges lore) -> AST.Body lore
removeBodyRanges = rephraseBody removeRanges

removeBindingRanges :: CanBeRanged (Op lore) =>
                       AST.Binding (Ranges lore) -> AST.Binding lore
removeBindingRanges = rephraseBinding removeRanges

removeLambdaRanges :: CanBeRanged (Op lore) =>
                      AST.Lambda (Ranges lore) -> AST.Lambda lore
removeLambdaRanges = rephraseLambda removeRanges

removeExtLambdaRanges :: CanBeRanged (Op lore) =>
                         AST.ExtLambda (Ranges lore) -> AST.ExtLambda lore
removeExtLambdaRanges = rephraseExtLambda removeRanges

removePatternRanges :: AST.PatternT (Range, a)
                    -> AST.PatternT a
removePatternRanges = rephrasePattern snd

addRangesToPattern :: (Attributes lore, CanBeRanged (Op lore)) =>
                      AST.Pattern lore -> Exp lore
                   -> Pattern lore
addRangesToPattern pat e =
  uncurry AST.Pattern $ mkPatternRanges pat e

mkRangedBody :: (Attributes lore, CanBeRanged (Op lore)) =>
                BodyAttr lore -> [Binding lore] -> Result
             -> Body lore
mkRangedBody innerlore bnds res =
  AST.Body (mkBodyRanges bnds res, innerlore) bnds res

mkPatternRanges :: (Attributes lore, CanBeRanged (Op lore)) =>
                   AST.Pattern lore
                -> Exp lore
                -> ([PatElem (Range, LetAttr lore)],
                    [PatElem (Range, LetAttr lore)])
mkPatternRanges pat e =
  (map (`addRanges` unknownRange) $ patternContextElements pat,
   zipWith addRanges (patternValueElements pat) ranges)
  where addRanges patElem range =
          let innerlore = patElemAttr patElem
          in patElem `setPatElemLore` (range, innerlore)
        ranges = expRanges e

mkBodyRanges :: Attributes lore =>
                [AST.Binding lore]
             -> Result
             -> [Range]
mkBodyRanges bnds = map $ removeUnknownBounds . rangeOf
  where boundInBnds =
          mconcat $ map (HS.fromList . patternNames . bindingPattern) bnds
        removeUnknownBounds (lower,upper) =
          (removeUnknownBound lower,
           removeUnknownBound upper)
        removeUnknownBound (Just bound)
          | freeIn bound `intersects` boundInBnds = Nothing
          | otherwise                             = Just bound
        removeUnknownBound Nothing =
          Nothing

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

mkRangedLetBinding :: (Attributes lore, CanBeRanged (Op lore)) =>
                      AST.Pattern lore
                   -> ExpAttr lore
                   -> Exp lore
                   -> Binding lore
mkRangedLetBinding pat explore e =
  Let (addRangesToPattern pat e) explore e
