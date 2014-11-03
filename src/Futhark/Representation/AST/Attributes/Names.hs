{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Names
       (
           FreeIn (..)
         , freeNamesIn
         , freeInBody
         , freeNamesInBody
         , freeInExp
         , freeNamesInExp
         , freeInSubExp
         , freeNamesInSubExp
         , freeInLambda
         , freeNamesInLambda
         , progNames
       )
       where

import Control.Monad.Writer
import qualified Data.HashSet as HS

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns

freeWalker :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
              Walker lore (Writer (HS.HashSet Ident))
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnBinding = bindingFree
             , walkOnLambda = lambdaFree
             , walkOnIdent = identFree
             , walkOnCertificates = mapM_ identFree
             }
  where identFree = tell . freeInIdent

        subExpFree = tell . freeInSubExp

        lambdaFree = tell . freeInLambda

        typeFree = tell . freeInType

        binding bound = censor (`HS.difference` bound)

        bodyFree (Body lore [] (Result cs ses _)) = do
          tell $ freeIn lore
          mapM_ identFree cs
          mapM_ subExpFree ses
        bodyFree (Body lore (Let pat annot e:bnds) res) = do
          expFree e
          tell $ freeIn annot
          binding (HS.fromList $ patternIdents pat) $ do
            mapM_ typeFree $ patternTypes pat
            bodyFree $ Body lore bnds res

        bindingFree (Let pat annot e) = do
          tell $ freeIn annot
          binding (HS.fromList $ patternIdents pat) $ do
            mapM_ typeFree $ patternTypes pat
            expFree e

        expFree (LoopOp (DoLoop _ merge i boundexp loopbody _)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList mergepat) $ do
            mapM_ (typeFree . identType) mergepat
            bodyFree loopbody

        expFree e = walkExpM freeWalker e

-- | Return the set of identifiers that are free in the given
-- identifier (including its type).
freeInIdent :: Ident -> HS.HashSet Ident
freeInIdent ident =
  ident `HS.insert`
  mconcat (map freeInSubExp $ arrayDims $ identType ident)

-- | Return the set of identifiers that are free in the given
-- subexpression.
freeInSubExp :: SubExp -> HS.HashSet Ident
freeInSubExp (Var ident)   = freeInIdent ident
freeInSubExp (Constant {}) = HS.empty

-- | As 'freeInSubExp', but returns the raw names rather than
-- 'Ident's.
freeNamesInSubExp :: SubExp -> Names
freeNamesInSubExp = HS.map identName . freeInSubExp

-- | Return the set of identifiers that are free in the given
-- type
freeInType :: TypeBase Shape -> HS.HashSet Ident
freeInType = mconcat . map freeInSubExp . arrayDims

-- | Return the set of identifiers that are free in the given
-- body.
freeInBody :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
              Body lore -> HS.HashSet Ident
freeInBody = execWriter . walkOnBody freeWalker

-- | As 'freeInBody', but returns the raw names rather than 'Ident's.
freeNamesInBody :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
                   Body lore -> Names
freeNamesInBody = HS.map identName . freeInBody

-- | Return the set of identifiers that are free in the given
-- expression.
freeInExp :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
             Exp lore -> HS.HashSet Ident
freeInExp = execWriter . walkExpM freeWalker

-- | As 'freeInExp', but returns the raw names rather than 'Ident's.
freeNamesInExp :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
                  Exp lore -> Names
freeNamesInExp = HS.map identName . freeInExp

-- | Return the set of identifiers that are free in the given lambda,
-- including shape annotations in the parameters.
freeInLambda :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
                Lambda lore -> HS.HashSet Ident
freeInLambda (Lambda params body rettype _) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeInType rettype
        inParams = mconcat $ map freeInParam params
        freeInParam = freeInType . identType
        inBody = HS.filter ((`notElem` paramnames) . identName) $ freeInBody body
        paramnames = map identName params

-- | A class indicating that we can obtain free variable information
-- from values of this type.
class FreeIn a where
  freeIn :: a -> HS.HashSet Ident

instance FreeIn () where
  freeIn () = mempty

instance (FreeIn a, FreeIn b) => FreeIn (a,b) where
  freeIn (a,b) = freeIn a <> freeIn b

instance FreeIn a => FreeIn [a] where
  freeIn = mconcat . map freeIn

instance FreeIn Bool where
  freeIn _ = mempty

instance FreeIn a => FreeIn (Maybe a) where
  freeIn = maybe mempty freeIn

instance FreeIn Ident where
  freeIn = freeInIdent

instance FreeIn SubExp where
  freeIn (Var v) = freeIn v
  freeIn (Constant {}) = mempty

instance FreeIn Shape where
  freeIn = mconcat . map freeIn . shapeDims

instance FreeIn ExtShape where
  freeIn = mconcat . map freeInExtDimSize . extShapeDims
    where freeInExtDimSize (Free se) = freeIn se
          freeInExtDimSize (Ext _)   = mempty

instance (ArrayShape shape, FreeIn shape) => FreeIn (TypeBase shape) where
  freeIn = freeIn . arrayShape

instance FreeIn attr => FreeIn (ResTypeT attr) where
  freeIn = freeIn . resTypeElems

freeNamesIn :: FreeIn a => a -> Names
freeNamesIn = HS.map identName . freeIn

-- | As 'freeInLambda', but returns the raw names rather than
-- 'IdentBase's.
freeNamesInLambda :: (FreeIn (Lore.Exp lore), FreeIn (Lore.Body lore)) =>
                     Lambda lore -> Names
freeNamesInLambda = HS.map identName . freeInLambda

-- | Return the set of all variable names bound in a program.
progNames :: Prog lore -> Names
progNames = execWriter . mapM funNames . progFunctions
  where names = identityWalker {
                  walkOnBinding = bindingNames
                , walkOnBody = bodyNames
                , walkOnLambda = lambdaNames
                }

        one = tell . HS.singleton
        funNames fundec = do
          mapM_ (one . bindeeName) $ funDecParams fundec
          bodyNames $ funDecBody fundec

        bodyNames = mapM_ bindingNames . bodyBindings

        bindingNames (Let pat _ e) =
          mapM_ one (patternNames pat) >> expNames e

        expNames (LoopOp (DoLoop _ pat i _ loopbody _)) = do
          mapM_ (one . identName . fst) pat
          one $ identName i
          bodyNames loopbody
        expNames e = walkExpM names e

        lambdaNames (Lambda params body _ _) =
          mapM_ (one . identName) params >> bodyNames body
