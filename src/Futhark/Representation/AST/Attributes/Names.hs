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
         , freeInBinding
         , freeInLambda
         , freeInExtLambda
         -- * Bound Names
         , boundInBody
         , boundByBindings
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
import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.RetType

freeWalker :: (FreeIn (Annotations.Exp lore),
               FreeIn (Annotations.Body lore),
               FreeIn (Annotations.FParam lore),
               FreeIn (Annotations.LParam lore),
               FreeIn (Annotations.LetBound lore),
               FreeIn (Op lore)) =>
              Walker lore (Writer Names)
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnBinding = bindingFree
             , walkOnLambda = lambdaFree
             , walkOnExtLambda = extLambdaFree
             , walkOnVName = tell . HS.singleton
             , walkOnCertificates = tell . HS.fromList
             , walkOnOp = tell . freeIn
             }
  where subExpFree = tell . freeIn

        lambdaFree = tell . freeInLambda

        extLambdaFree = tell . freeInExtLambda

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

        expFree (LoopOp (DoLoop _ merge (ForLoop i boundexp) loopbody)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList (map paramName mergepat)) $ do
            mapM_ (tell . freeIn) mergepat
            bodyFree loopbody

        expFree (LoopOp (DoLoop _ merge (WhileLoop cond) loopbody)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          tell $ freeIn cond
          binding (HS.fromList (map paramName mergepat)) $ do
            mapM_ (tell . freeIn) mergepat
            bodyFree loopbody

        expFree e = walkExpM freeWalker e

-- | Return the set of variable names that are free in the given body.
freeInBody :: (FreeIn (Annotations.Exp lore),
               FreeIn (Annotations.Body lore),
               FreeIn (Annotations.FParam lore),
               FreeIn (Annotations.LParam lore),
               FreeIn (Annotations.LetBound lore),
               FreeIn (Op lore)) =>
              Body lore -> Names
freeInBody = execWriter . walkOnBody freeWalker

-- | Return the set of variable names that are free in the given
-- expression.
freeInExp :: (FreeIn (Annotations.Exp lore),
              FreeIn (Annotations.Body lore),
              FreeIn (Annotations.FParam lore),
              FreeIn (Annotations.LParam lore),
              FreeIn (Annotations.LetBound lore),
              FreeIn (Op lore)) =>
             Exp lore -> Names
freeInExp = execWriter . walkExpM freeWalker

-- | Return the set of variable names that are free in the given
-- binding.
freeInBinding :: (FreeIn (Annotations.Exp lore),
                  FreeIn (Annotations.Body lore),
                  FreeIn (Annotations.FParam lore),
                  FreeIn (Annotations.LParam lore),
                  FreeIn (Annotations.LetBound lore),
                  FreeIn (Op lore)) =>
                 Binding lore -> Names
freeInBinding = execWriter . walkOnBinding freeWalker

-- | Return the set of variable names that are free in the given
-- lambda, including shape annotations in the parameters.
freeInLambda :: (FreeIn (Annotations.Exp lore),
                 FreeIn (Annotations.Body lore),
                 FreeIn (Annotations.FParam lore),
                 FreeIn (Annotations.LParam lore),
                 FreeIn (Annotations.LetBound lore),
                 FreeIn (Op lore)) =>
                Lambda lore -> Names
freeInLambda (Lambda index params body rettype) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeIn params
        inBody = HS.filter (`notElem` paramnames) $ freeInBody body
        paramnames = index : map paramName params

-- | Return the set of identifiers that are free in the given
-- existential lambda, including shape annotations in the parameters.
freeInExtLambda :: (FreeIn (Annotations.Exp lore),
                    FreeIn (Annotations.Body lore),
                    FreeIn (Annotations.FParam lore),
                    FreeIn (Annotations.LParam lore),
                    FreeIn (Annotations.LetBound lore),
                    FreeIn (Op lore)) =>
                   ExtLambda lore -> Names
freeInExtLambda (ExtLambda index params body rettype) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeIn params
        inBody = HS.filter (`notElem` paramnames) $ freeInBody body
        paramnames = index : map paramName params

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
  freeIn (Basic _)         = mempty

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

instance FreeIn attr => FreeIn (PatternT attr) where
  freeIn (Pattern context values) =
    mconcat $ map freeIn $ context ++ values

-- | The names bound by the bindings immediately in a 'Body'.
boundInBody :: Body lore -> Names
boundInBody = boundByBindings . bodyBindings

-- | The names bound by the bindings.
boundByBindings :: [Binding lore] -> Names
boundByBindings = mconcat . map bound
  where bound (Let pat _ _) = HS.fromList $ patternNames pat

-- | The names of the lambda parameters plus the index parameter.
boundByLambda :: Lambda lore -> [VName]
boundByLambda lam = lambdaIndex lam : map paramName (lambdaParams lam)

-- | The names of the lambda parameters plus the index parameter.
boundByExtLambda :: ExtLambda lore -> [VName]
boundByExtLambda lam = extLambdaIndex lam : map paramName (extLambdaParams lam)
