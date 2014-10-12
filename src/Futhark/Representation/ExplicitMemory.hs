{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Futhark.Representation.ExplicitMemory
       ( -- * The Lore definition
         ExplicitMemory
       , MemSummary (..)
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PrimOp
       , LoopOp
       , Exp
       , Lambda
       , FunDec
       , FParam
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.ExpT(LoopOp)
       , AST.FunDecT(FunDec)
       )
where

import Control.Applicative
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Monoid
import qualified Text.PrettyPrint.Mainland as PP

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, Lambda, FunDec, FParam)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Substitute
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun

-- | A lore containing explicit memory information.
data ExplicitMemory

type Prog = AST.Prog ExplicitMemory
type PrimOp = AST.PrimOp ExplicitMemory
type LoopOp = AST.LoopOp ExplicitMemory
type Exp = AST.Exp ExplicitMemory
type Body = AST.Body ExplicitMemory
type Binding = AST.Binding ExplicitMemory
type Pattern = AST.Pattern ExplicitMemory
type Lambda = AST.Lambda ExplicitMemory
type FunDec = AST.FunDec ExplicitMemory
type FParam = AST.FParam ExplicitMemory

data MemSummary = MemSummary Ident IxFun.IxFun
                | Scalar
                deriving (Show)

instance Eq MemSummary where
  _ == _ = True

instance Ord MemSummary where
  _ `compare` _ = EQ

instance FreeIn MemSummary where
  freeIn (MemSummary ident _) = HS.singleton ident
  freeIn Scalar               = HS.empty

instance Substitute MemSummary where
  substituteNames subst (MemSummary name f) =
    MemSummary (substituteNames subst name) f
  substituteNames _ Scalar =
    Scalar

instance Rename MemSummary where
  rename (MemSummary ident f) =
    MemSummary <$> rename ident <*> pure f
  rename Scalar =
    return Scalar

instance Lore.Lore ExplicitMemory where
  type Binding ExplicitMemory = MemSummary
  type FParam  ExplicitMemory = MemSummary

instance TypeCheck.Checkable ExplicitMemory where
  checkExpLore = return
  checkBindingLore = return
  checkBodyLore = return
  checkFParamLore = const $ return ()

instance Renameable ExplicitMemory where
instance Substitutable ExplicitMemory where
instance Proper ExplicitMemory where

instance PrettyLore ExplicitMemory where
  ppBindingLore binding =
    case mapMaybe bindeeAnnotation $ patternBindees $ bindingPattern binding of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppFunDecLore fundec =
    case mapMaybe bindeeAnnotation $ funDecParams fundec of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots

bindeeAnnotation :: Bindee MemSummary -> Maybe PP.Doc
bindeeAnnotation bindee =
  case bindeeLore bindee of
    MemSummary ident fun ->
      Just $
      PP.text "// " <>
      PP.ppr (bindeeName bindee) <>
      PP.text "@" <>
      PP.ppr (identName ident) <>
      PP.text "->" <>
      PP.text (show fun)
    Scalar ->
      Nothing
