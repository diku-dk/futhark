{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Names
       (
           FreeIn (..)
         , freeNamesIn
         , freeInBody
         , freeNamesInBody
         , freeInExp
         , freeNamesInExp
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
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.RetType

freeWalker :: (FreeIn (Lore.Exp lore),
               FreeIn (Lore.Body lore),
               FreeIn (Lore.FParam lore),
               FreeIn (Lore.LetBound lore)) =>
              Walker lore (Writer (HS.HashSet Ident))
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnBinding = bindingFree
             , walkOnLambda = lambdaFree
             , walkOnIdent = identFree
             , walkOnCertificates = mapM_ identFree
             }
  where identFree = tell . freeIn

        subExpFree = tell . freeIn

        lambdaFree = tell . freeInLambda

        typeFree = tell . freeIn

        binding bound = censor (`HS.difference` bound)

        bodyFree (Body lore [] (Result ses)) = do
          tell $ freeIn lore
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
            mapM_ (tell . freeIn) $ patternElements pat
            expFree e

        expFree (LoopOp (DoLoop _ merge (ForLoop i boundexp) loopbody)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList (map fparamIdent mergepat)) $ do
            mapM_ (tell . freeIn) mergepat
            bodyFree loopbody

        expFree (LoopOp (DoLoop _ merge (WhileLoop cond) loopbody)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          tell $ freeIn cond
          binding (HS.fromList (map fparamIdent mergepat)) $ do
            mapM_ (tell . freeIn) mergepat
            bodyFree loopbody

        expFree e = walkExpM freeWalker e

-- | Return the set of identifiers that are free in the given
-- body.
freeInBody :: (FreeIn (Lore.Exp lore),
               FreeIn (Lore.Body lore),
               FreeIn (Lore.FParam lore),
               FreeIn (Lore.LetBound lore)) =>
              Body lore -> HS.HashSet Ident
freeInBody = execWriter . walkOnBody freeWalker

-- | As 'freeInBody', but returns the raw names rather than 'Ident's.
freeNamesInBody :: (FreeIn (Lore.Exp lore),
                    FreeIn (Lore.Body lore),
                    FreeIn (Lore.FParam lore),
                    FreeIn (Lore.LetBound lore)) =>
                   Body lore -> Names
freeNamesInBody = HS.map identName . freeInBody

-- | Return the set of identifiers that are free in the given
-- expression.
freeInExp :: (FreeIn (Lore.Exp lore),
              FreeIn (Lore.Body lore),
              FreeIn (Lore.FParam lore),
              FreeIn (Lore.LetBound lore)) =>
             Exp lore -> HS.HashSet Ident
freeInExp = execWriter . walkExpM freeWalker

-- | As 'freeInExp', but returns the raw names rather than 'Ident's.
freeNamesInExp :: (FreeIn (Lore.Exp lore),
                   FreeIn (Lore.Body lore),
                   FreeIn (Lore.FParam lore),
                   FreeIn (Lore.LetBound lore)) =>
                  Exp lore -> Names
freeNamesInExp = HS.map identName . freeInExp

-- | Return the set of identifiers that are free in the given lambda,
-- including shape annotations in the parameters.
freeInLambda :: (FreeIn (Lore.Exp lore),
                 FreeIn (Lore.Body lore),
                 FreeIn (Lore.FParam lore),
                 FreeIn (Lore.LetBound lore)) =>
                Lambda lore -> HS.HashSet Ident
freeInLambda (Lambda params body rettype) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeInParam params
        freeInParam = freeIn . identType
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

instance (FreeIn a, FreeIn b, FreeIn c) => FreeIn (a,b,c) where
  freeIn (a,b,c) = freeIn a <> freeIn b <> freeIn c

instance FreeIn a => FreeIn [a] where
  freeIn = mconcat . map freeIn

instance FreeIn Bool where
  freeIn _ = mempty

instance FreeIn a => FreeIn (Maybe a) where
  freeIn = maybe mempty freeIn

instance FreeIn Ident where
  freeIn ident = ident `HS.insert` freeIn (identType ident)

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
  freeIn (Array _ shape _) = freeIn shape
  freeIn (Mem size)        = freeIn size
  freeIn (Basic _)         = mempty

instance FreeIn attr => FreeIn (FParamT attr) where
  freeIn (FParam ident attr) =
    freeIn ident <> freeIn attr

instance FreeIn attr => FreeIn (PatElemT attr) where
  freeIn (PatElem ident bindage attr) =
    freeIn ident <> freeIn bindage <> freeIn attr

instance FreeIn Bindage where
  freeIn BindVar = mempty
  freeIn (BindInPlace cs src is) =
    freeIn cs <> freeIn src <> freeIn is

instance FreeIn ExtRetType where
  freeIn = mconcat . map freeIn . retTypeValues

instance FreeIn LoopForm where
  freeIn (ForLoop _ bound) = freeIn bound
  freeIn (WhileLoop cond) = freeIn cond

freeNamesIn :: FreeIn a => a -> Names
freeNamesIn = HS.map identName . freeIn

-- | As 'freeInLambda', but returns the raw names rather than
-- 'IdentBase's.
freeNamesInLambda :: (FreeIn (Lore.Exp lore),
                      FreeIn (Lore.Body lore),
                      FreeIn (Lore.FParam lore),
                      FreeIn (Lore.LetBound lore)) =>
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
          mapM_ (one . fparamName) $ funDecParams fundec
          bodyNames $ funDecBody fundec

        bodyNames = mapM_ bindingNames . bodyBindings

        bindingNames (Let pat _ e) =
          mapM_ one (patternNames pat) >> expNames e

        expNames (LoopOp (DoLoop _ pat form loopbody)) = do
          mapM_ (one . fparamName . fst) pat
          case form of ForLoop i _ ->
                         one $ identName i
                       WhileLoop _ ->
                         return ()
          bodyNames loopbody

        expNames e = walkExpM names e

        lambdaNames (Lambda params body _) =
          mapM_ (one . identName) params >> bodyNames body
