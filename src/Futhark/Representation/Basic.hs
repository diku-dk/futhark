{-# LANGUAGE TypeFamilies #-}
-- | A simple representation with known shapes, but no other
-- particular information.
module Futhark.Representation.Basic
       ( -- * The Lore definition
         Basic
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
       , FParam
       , RetType
       , PatElem
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.ExtLambdaT(ExtLambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.PatElemT(PatElem)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.ExpT(LoopOp)
       , AST.FunDecT(FunDec)
       , AST.FParamT(FParam)
         -- Utility
       , basicPattern
       , basicPattern'
         -- Removing lore
       , removeProgLore
       , removeFunDecLore
       , removeBodyLore
       )
where

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, Lambda, ExtLambda, FunDec, FParam, RetType,
          PatElem)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Tools
import Futhark.Substitute
import qualified Futhark.TypeCheck as TypeCheck
import Futhark.Analysis.Rephrase
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Optimise.Simplifier.Simplifiable

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

-- | The lore for the basic representation.
data Basic = Basic

instance Lore.Lore Basic where
  representative = Futhark.Representation.Basic.Basic

  loopResultContext _ res merge =
    loopShapeContext res $ map fparamIdent merge

  applyRetType _ ret =
    applyExtType ret . map fparamIdent

type Prog = AST.Prog Basic
type PrimOp = AST.PrimOp Basic
type LoopOp = AST.LoopOp Basic
type Exp = AST.Exp Basic
type Body = AST.Body Basic
type Binding = AST.Binding Basic
type Pattern = AST.Pattern Basic
type Lambda = AST.Lambda Basic
type ExtLambda = AST.ExtLambda Basic
type FunDec = AST.FunDecT Basic
type FParam = AST.FParam Basic
type RetType = AST.RetType Basic
type PatElem = AST.PatElem Basic

instance TypeCheck.Checkable Basic where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore = return
  checkLetBoundLore = return
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  matchPattern pat e =
    TypeCheck.matchExtPattern (patternElements pat) (expExtType e)
  basicFParam _ name t =
    AST.FParam (Ident name (AST.Basic t)) ()
  matchReturnType name (ExtRetType ts) =
    TypeCheck.matchExtReturnType name ts

instance Renameable Basic where
instance Substitutable Basic where
instance Proper Basic where

instance Ranged Basic where
  bodyRanges body =
    replicate (length $ resultSubExps $ bodyResult body) (Nothing, Nothing)
  patternRanges pat =
    replicate (patternSize pat) (Nothing, Nothing)

instance Simplifiable Basic where

instance Bindable Basic where
  mkBody = AST.Body ()
  mkLet pat =
    AST.Let (basicPattern pat) ()
  mkLetNames names e = do
    (ts, shapes) <- instantiateShapes' $ expExtType e
    let shapeElems = [ AST.PatElem shapeident BindVar ()
                  | shapeident <- shapes
                  ]
        valElems = zipWith mkValElem names ts
        mkValElem (name, BindVar) t =
          AST.PatElem (Ident name t) BindVar ()
        mkValElem (name, bindage@(BindInPlace _ src _)) _ =
          AST.PatElem (Ident name (identType src)) bindage ()
    return $ AST.Let (AST.Pattern $ shapeElems++valElems) () e

instance PrettyLore Basic where

basicPattern :: [(Ident,Bindage)] -> Pattern
basicPattern idents =
  AST.Pattern [ AST.PatElem ident bindage () | (ident,bindage) <- idents ]

basicPattern' :: [Ident] -> Pattern
basicPattern' = basicPattern . map addBindVar
    where addBindVar name = (name, BindVar)

removeLore :: Lore.Lore lore => Rephraser lore Basic
removeLore =
  Rephraser { rephraseExpLore = const ()
            , rephraseLetBoundLore = const ()
            , rephraseBodyLore = const ()
            , rephraseFParamLore = const ()
            , rephraseRetType = removeRetTypeLore
            }

removeProgLore :: Lore.Lore lore => AST.Prog lore -> Prog
removeProgLore = rephraseProg removeLore

removeFunDecLore :: Lore.Lore lore => AST.FunDec lore -> FunDec
removeFunDecLore = rephraseFunDec removeLore

removeBodyLore :: Lore.Lore lore => AST.Body lore -> Body
removeBodyLore = rephraseBody removeLore

removeRetTypeLore :: IsRetType rt => rt -> RetType
removeRetTypeLore = ExtRetType . retTypeValues
