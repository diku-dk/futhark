{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- | Facilities for determining which names are used in some syntactic
-- construct.  The most important interface is the 'FreeIn' class and
-- its instances, but for reasons related to the Haskell type system,
-- some constructs have specialised functions.
module Futhark.Representation.AST.Attributes.Names
       (
         -- * Class
           FreeIn (..)
         , Names
         -- * Specialised Functions
         , freeInBody
         , freeInExp
         , freeInStm
         , freeInLambda
         , freeInExtLambda
         -- * Bound Names
         , boundInBody
         , boundByStm
         , boundByStms
         , boundByLambda
         , boundByExtLambda
       )
       where

import Control.Monad.Writer
import qualified Data.HashSet as HS
import Data.Foldable (foldMap)

import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Attributes.Patterns

freeWalker :: (FreeIn (ExpAttr lore),
               FreeIn (BodyAttr lore),
               FreeIn (FParamAttr lore),
               FreeIn (LParamAttr lore),
               FreeIn (LetAttr lore),
               FreeIn (Op lore)) =>
              Walker lore (Writer Names)
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnStm = bindingFree
             , walkOnVName = tell . HS.singleton
             , walkOnCertificates = tell . HS.fromList
             , walkOnOp = tell . freeIn
             }
  where subExpFree = tell . freeIn

        binding bound = censor (`HS.difference` bound)

        bodyFree (Body lore [] ses) = do
          tell $ freeIn lore
          mapM_ subExpFree ses
        bodyFree (Body lore (Let pat annot e:bnds) res) = do
          tell $ freeIn annot
          expFree e
          binding (HS.fromList $ patternNames pat) $ do
            tell $ freeIn pat
            bodyFree $ Body lore bnds res

        bindingFree (Let pat annot e) = do
          tell $ freeIn annot
          expFree e
          binding (HS.fromList $ patternNames pat) $
            tell $ freeIn pat

        expFree (DoLoop ctxmerge valmerge (ForLoop i boundexp) loopbody) = do
          let (ctxparams, ctxinits) = unzip ctxmerge
              (valparams, valinits) = unzip valmerge
          mapM_ subExpFree $ ctxinits ++ valinits
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList (map paramName $ ctxparams ++ valparams)) $ do
            mapM_ (tell . freeIn) $ ctxparams ++ valparams
            bodyFree loopbody

        expFree (DoLoop ctxmerge valmerge (WhileLoop cond) loopbody) = do
          let (ctxparams, ctxinits) = unzip ctxmerge
              (valparams, valinits) = unzip valmerge
          mapM_ subExpFree $ ctxinits ++ valinits
          tell $ freeIn cond
          binding (HS.fromList (map paramName $ ctxparams ++ valparams)) $ do
            mapM_ (tell . freeIn) $ ctxparams ++ valparams
            bodyFree loopbody

        expFree e = walkExpM freeWalker e

-- | Return the set of variable names that are free in the given body.
freeInBody :: (FreeIn (ExpAttr lore),
               FreeIn (BodyAttr lore),
               FreeIn (FParamAttr lore),
               FreeIn (LParamAttr lore),
               FreeIn (LetAttr lore),
               FreeIn (Op lore)) =>
              Body lore -> Names
freeInBody = execWriter . walkOnBody freeWalker

-- | Return the set of variable names that are free in the given
-- expression.
freeInExp :: (FreeIn (ExpAttr lore),
              FreeIn (BodyAttr lore),
              FreeIn (FParamAttr lore),
              FreeIn (LParamAttr lore),
              FreeIn (LetAttr lore),
              FreeIn (Op lore)) =>
             Exp lore -> Names
freeInExp = execWriter . walkExpM freeWalker

-- | Return the set of variable names that are free in the given
-- binding.
freeInStm :: (FreeIn (ExpAttr lore),
                  FreeIn (BodyAttr lore),
                  FreeIn (FParamAttr lore),
                  FreeIn (LParamAttr lore),
                  FreeIn (LetAttr lore),
                  FreeIn (Op lore)) =>
                 Stm lore -> Names
freeInStm = execWriter . walkOnStm freeWalker

-- | Return the set of variable names that are free in the given
-- lambda, including shape annotations in the parameters.
freeInLambda :: (FreeIn (ExpAttr lore),
                 FreeIn (BodyAttr lore),
                 FreeIn (FParamAttr lore),
                 FreeIn (LParamAttr lore),
                 FreeIn (LetAttr lore),
                 FreeIn (Op lore)) =>
                Lambda lore -> Names
freeInLambda (Lambda params body rettype) =
    freeInLambdaIsh params body rettype

-- | Return the set of identifiers that are free in the given
-- existential lambda, including shape annotations in the parameters.
freeInExtLambda :: (FreeIn (ExpAttr lore),
                    FreeIn (BodyAttr lore),
                    FreeIn (FParamAttr lore),
                    FreeIn (LParamAttr lore),
                    FreeIn (LetAttr lore),
                    FreeIn (Op lore)) =>
                   ExtLambda lore -> Names
freeInExtLambda (ExtLambda params body rettype) =
  freeInLambdaIsh params body rettype

freeInLambdaIsh :: (FreeIn attr, FreeIn a, FreeIn (ExpAttr lore),
                    FreeIn (BodyAttr lore), FreeIn (FParamAttr lore),
                    FreeIn (LParamAttr lore), FreeIn (LetAttr lore),
                    FreeIn (Op lore)) =>
                   [ParamT attr] -> Body lore -> [a] -> Names
freeInLambdaIsh params body rettype =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeIn params
        inBody = HS.filter (`notElem` paramnames) $ freeInBody body
        paramnames = map paramName params

-- | A class indicating that we can obtain free variable information
-- from values of this type.
class FreeIn a where
  freeIn :: a -> Names

instance FreeIn () where
  freeIn () = mempty

instance (FreeIn a, FreeIn b) => FreeIn (a,b) where
  freeIn (a,b) = freeIn a <> freeIn b

instance (FreeIn a, FreeIn b, FreeIn c) => FreeIn (a,b,c) where
  freeIn (a,b,c) = freeIn a <> freeIn b <> freeIn c

instance FreeIn a => FreeIn [a] where
  freeIn = mconcat . map freeIn

instance FreeIn Names where
  freeIn = id

instance FreeIn Bool where
  freeIn _ = mempty

instance FreeIn a => FreeIn (Maybe a) where
  freeIn = maybe mempty freeIn

instance FreeIn VName where
  freeIn = HS.singleton

instance FreeIn Ident where
  freeIn = freeIn . identType

instance FreeIn SubExp where
  freeIn (Var v) = freeIn v
  freeIn Constant{} = mempty

instance FreeIn Shape where
  freeIn = mconcat . map freeIn . shapeDims

instance FreeIn ExtShape where
  freeIn = mconcat . map freeInExtDimSize . extShapeDims
    where freeInExtDimSize (Free se) = freeIn se
          freeInExtDimSize (Ext _)   = mempty

instance (ArrayShape shape, FreeIn shape) => FreeIn (TypeBase shape u) where
  freeIn (Array _ shape _) = freeIn shape
  freeIn (Mem size _)      = freeIn size
  freeIn (Prim _)          = mempty

instance FreeIn attr => FreeIn (ParamT attr) where
  freeIn (Param _ attr) =
    freeIn attr

instance FreeIn attr => FreeIn (PatElemT attr) where
  freeIn (PatElem _ bindage attr) =
    freeIn bindage <> freeIn attr

instance FreeIn Bindage where
  freeIn BindVar = mempty
  freeIn (BindInPlace cs src is) =
    freeIn cs <> freeIn src <> freeIn is

instance FreeIn ExtRetType where
  freeIn = mconcat . map freeIn . retTypeValues

instance FreeIn LoopForm where
  freeIn (ForLoop _ bound) = freeIn bound
  freeIn (WhileLoop cond) = freeIn cond

instance FreeIn d => FreeIn (DimChange d) where
  freeIn = Data.Foldable.foldMap freeIn

instance FreeIn d => FreeIn (DimIndex d) where
  freeIn = Data.Foldable.foldMap freeIn

instance FreeIn attr => FreeIn (PatternT attr) where
  freeIn (Pattern context values) =
    mconcat $ map freeIn $ context ++ values

-- | The names bound by the bindings immediately in a 'Body'.
boundInBody :: Body lore -> Names
boundInBody = boundByStms . bodyStms

-- | The names bound by a binding.
boundByStm :: Stm lore -> Names
boundByStm = HS.fromList . patternNames . bindingPattern

-- | The names bound by the bindings.
boundByStms :: [Stm lore] -> Names
boundByStms = mconcat . map boundByStm

-- | The names of the lambda parameters plus the index parameter.
boundByLambda :: Lambda lore -> [VName]
boundByLambda lam = map paramName (lambdaParams lam)

-- | The names of the lambda parameters plus the index parameter.
boundByExtLambda :: ExtLambda lore -> [VName]
boundByExtLambda lam = map paramName (extLambdaParams lam)
