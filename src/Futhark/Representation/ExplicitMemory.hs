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
       , ResType
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
       , AST.ResTypeT(ResType)
       )
where

import Control.Applicative
import qualified Data.HashSet as HS
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Loc
import Data.Monoid
import qualified Text.PrettyPrint.Mainland as PP

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, Lambda, FunDec, FParam, ResType)
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
type ResType = AST.ResType ExplicitMemory

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

data MemReturn = ReturnsInBlock Ident
               | ReturnsNewBlock
               | ReturnsScalar
               deriving (Eq, Ord, Show)

instance Lore.ResType (ResTypeT MemReturn) where
  resTypeValues = map fst . resTypeElems
  simpleType = mapM simple . resTypeElems
    where simple (_, ReturnsNewBlock) = Nothing
          simple (t, _)               = hasStaticShape t
  staticResType = extResType . staticShapes
  generaliseResTypes rt1 rt2 =
    let (ts1,attrs1) = unzip $ resTypeElems rt1
        (ts2,attrs2) = unzip $ resTypeElems rt2
    in AST.ResType $ zip (generaliseExtTypes ts1 ts2) $
       zipWith generaliseMemReturn attrs1 attrs2
    where generaliseMemReturn ReturnsNewBlock _ = ReturnsNewBlock
          generaliseMemReturn _ ReturnsNewBlock = ReturnsNewBlock
          generaliseMemReturn r               _ = r
  extResType = AST.ResType . map addAttr
    where addAttr (Basic t) = (Basic t, ReturnsScalar)
          addAttr (Mem sz)  = (Mem sz,  ReturnsScalar)
          addAttr t         = (t,       ReturnsNewBlock)

instance Monoid (ResTypeT MemReturn) where
  mempty = AST.ResType []
  AST.ResType xs `mappend` AST.ResType ys =
    AST.ResType $ xs <> ys

instance Rename MemReturn where
  rename = return

instance Substitute MemReturn where
  substituteNames = const id

instance PP.Pretty (ResTypeT MemReturn) where
  ppr = PP.braces . PP.commasep . map pp . resTypeElems
    where pp (t, ReturnsScalar)    = PP.ppr t
          pp (t, ReturnsInBlock v) = PP.ppr t <> PP.parens (PP.text $ textual $identName v)
          pp (t, ReturnsNewBlock)  = PP.ppr t <> PP.parens (PP.text "allocs")

instance Lore.Lore ExplicitMemory where
  type LetBound ExplicitMemory = MemSummary
  type FParam   ExplicitMemory = MemSummary
  type ResTypeAttr ExplicitMemory = MemReturn

instance TypeCheck.Checkable ExplicitMemory where
  checkExpLore = return
  checkBindingLore = checkMemSummary
  checkBodyLore = return
  checkFParamLore = checkMemSummary
  checkResType = mapM_ TypeCheck.checkExtType . resTypeValues
  matchPattern loc pat ts =
    return ()

checkMemSummary :: MemSummary
                -> TypeCheck.TypeM ExplicitMemory ()
checkMemSummary Scalar = return ()
checkMemSummary (MemSummary v ixfun) = do
  _ <- checkMemIdent v
  traverse_ TypeCheck.checkIdent $ freeIn ixfun
  where checkMemIdent ident = do
          _ <- TypeCheck.checkIdent ident
          case identType ident of
            Mem size ->
              TypeCheck.require [Basic Int] <$> TypeCheck.checkSubExp size
            t        ->
              TypeCheck.bad $ TypeCheck.TypeError (srclocOf ident) $
              "Variable " ++ textual (identName v) ++
              " used as memory block, but is of type " ++
              ppType t ++ "."

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
