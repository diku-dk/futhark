{-# LANGUAGE TypeFamilies #-}
-- | A representation where all bindings are annotated with range
-- information.
module Futhark.Representation.Ranges
       ( -- * The Lore definition
         Ranges
       , VarRange
       , BodyRanges
       , module Futhark.Representation.AST.Attributes.Ranges
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PrimOp
       , LoopOp
       , Exp
       , Lambda
       , ExtLambda
       , FunDec
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
       , AST.ExpT(LoopOp)
       , AST.FunDecT(FunDec)
         -- * Adding ranges
       , addRangesToPattern
       , mkAliasedLetBinding
       , mkAliasedBody
       , mkPatternRanges
       , mkBodyRanges
         -- * Removing ranges
       , removeProgRanges
       , removeFunDecRanges
       , removeExpRanges
       , removeBodyRanges
       , removeBindingRanges
       , removeLambdaRanges
       , removePatternRanges
       )
where

import qualified Data.HashSet as HS
import Data.Hashable
import Data.Maybe
import Data.Monoid
import qualified Text.PrettyPrint.Mainland as PP

import Prelude

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, Lambda, ExtLambda, FunDec, RetType)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Substitute
import Futhark.Analysis.Rephrase

-- | The lore for the basic representation.
data Ranges lore = Ranges lore

-- | The ranges of the let-bound variable.
type VarRange = Range

-- | Ranges about a body.
type BodyRanges = [Range]

instance Lore.Lore lore => Lore.Lore (Ranges lore) where
  type LetBound (Ranges lore) = (VarRange, Lore.LetBound lore)
  type Exp (Ranges lore) = Lore.Exp lore
  type Body (Ranges lore) = (BodyRanges, Lore.Body lore)
  type FParam (Ranges lore) = Lore.FParam lore
  type RetType (Ranges lore) = Lore.RetType lore

  representative =
    Ranges Lore.representative

  loopResultContext (Ranges lore) =
    Lore.loopResultContext lore

  applyRetType (Ranges lore) =
    Lore.applyRetType lore

instance Lore.Lore lore => Ranged (Ranges lore) where
  bodyRanges = fst . bodyLore

type Prog lore = AST.Prog (Ranges lore)
type PrimOp lore = AST.PrimOp (Ranges lore)
type LoopOp lore = AST.LoopOp (Ranges lore)
type Exp lore = AST.Exp (Ranges lore)
type Body lore = AST.Body (Ranges lore)
type Binding lore = AST.Binding (Ranges lore)
type Pattern lore = AST.Pattern (Ranges lore)
type Lambda lore = AST.Lambda (Ranges lore)
type ExtLambda lore = AST.ExtLambda (Ranges lore)
type FunDec lore = AST.FunDec (Ranges lore)
type RetType lore = AST.RetType (Ranges lore)

instance Renameable lore => Renameable (Ranges lore) where
instance Substitutable lore => Substitutable (Ranges lore) where
instance Proper lore => Proper (Ranges lore) where

instance (PrettyLore lore) => PrettyLore (Ranges lore) where
  ppBindingLore binding@(Let pat _ _) =
    case catMaybes [patElemAttrs,
                    ppBindingLore $ removeBindingRanges binding] of
      [] -> Nothing
      ls -> Just $ PP.folddoc (PP.</>) ls
    where patElemAttrs =
            case mapMaybe patElemAttr $ patternElements pat of
              []    -> Nothing
              attrs -> Just $ PP.folddoc (PP.</>) attrs
          patElemAttr patelem =
            case fst . patElemLore $ patelem of
              (Nothing, Nothing) -> Nothing
              range ->
                Just $ oneline $
                PP.text "// " <> PP.ppr (patElemName patelem) <> PP.text " range: " <>
                PP.ppr range
          oneline s = PP.text $ PP.displayS (PP.renderCompact s) ""

  ppFunDecLore = ppFunDecLore . removeFunDecRanges
  ppExpLore = ppExpLore . removeExpRanges

removeRanges :: Rephraser (Ranges lore) lore
removeRanges = Rephraser { rephraseExpLore = id
                         , rephraseLetBoundLore = snd
                         , rephraseBodyLore = snd
                         , rephraseFParamLore = id
                         , rephraseRetType = id
                         }

removeProgRanges :: AST.Prog (Ranges lore) -> AST.Prog lore
removeProgRanges = rephraseProg removeRanges

removeFunDecRanges :: AST.FunDec (Ranges lore) -> AST.FunDec lore
removeFunDecRanges = rephraseFunDec removeRanges

removeExpRanges :: AST.Exp (Ranges lore) -> AST.Exp lore
removeExpRanges = rephraseExp removeRanges

removeBodyRanges :: AST.Body (Ranges lore) -> AST.Body lore
removeBodyRanges = rephraseBody removeRanges

removeBindingRanges :: AST.Binding (Ranges lore) -> AST.Binding lore
removeBindingRanges = rephraseBinding removeRanges

removeLambdaRanges :: AST.Lambda (Ranges lore) -> AST.Lambda lore
removeLambdaRanges = rephraseLambda removeRanges

removePatternRanges :: AST.Pattern (Ranges lore) -> AST.Pattern lore
removePatternRanges = rephrasePattern removeRanges

addRangesToPattern :: Lore.Lore lore =>
                      AST.Pattern lore -> Exp lore -> Pattern lore
addRangesToPattern pat e =
  AST.Pattern $ mkPatternRanges pat e

mkAliasedBody :: Lore.Lore lore =>
                 Lore.Body lore -> [Binding lore] -> Result
              -> Body lore
mkAliasedBody innerlore bnds res =
  AST.Body (mkBodyRanges bnds res, innerlore) bnds res

mkPatternRanges :: Lore.Lore lore =>
                   AST.Pattern lore -> Exp lore
                -> [PatElemT (Lore.LetBound (Ranges lore))]
mkPatternRanges pat e =
  zipWith addRanges (patternElements pat) ranges
  where addRanges patElem range =
          let innerlore = patElemLore patElem
          in patElem `setPatElemLore` (range, innerlore)
        ranges = expRanges e

mkBodyRanges :: Lore.Lore lore =>
                [AST.Binding lore]
             -> Result
             -> BodyRanges
mkBodyRanges bnds res =
  map (removeUnknownBounds . subExpRange) $ resultSubExps res
  where boundInBnds =
          mconcat $ map (HS.fromList . patternNames . bindingPattern) bnds
        removeUnknownBounds (lower,upper) =
          (removeUnknownBound lower,
           removeUnknownBound upper)
        removeUnknownBound (Just bound)
          | freeNamesIn bound `intersects` boundInBnds = Nothing
          | otherwise                                  = Just bound
        removeUnknownBound Nothing =
          Nothing

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

mkAliasedLetBinding :: Lore.Lore lore =>
                       AST.Pattern lore -> Lore.Exp lore -> Exp lore
                    -> Binding lore
mkAliasedLetBinding pat explore e =
  Let (addRangesToPattern pat e) explore e

instance Bindable lore => Bindable (Ranges lore) where
  mkLet pat e =
    let Let pat' explore _ = mkLet pat $ removeExpRanges e
    in mkAliasedLetBinding pat' explore e

  mkLetNames names e = do
    Let pat explore _ <- mkLetNames names $ removeExpRanges e
    return $ mkAliasedLetBinding pat explore e

  mkBody bnds res =
    let AST.Body bodylore _ _ = mkBody (map removeBindingRanges bnds) res
    in mkAliasedBody bodylore bnds res
