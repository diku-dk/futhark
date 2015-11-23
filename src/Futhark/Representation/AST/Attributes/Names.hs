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
         , freeInPattern
         , freeInLambda
         , freeInExtLambda
         -- * Bound Names
         , progNames
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
               FreeIn (Annotations.LetBound lore)) =>
              Walker lore (Writer Names)
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnBinding = bindingFree
             , walkOnLambda = lambdaFree
             , walkOnExtLambda = extLambdaFree
             , walkOnVName = tell . HS.singleton
             , walkOnCertificates = tell . HS.fromList
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
            tell $ freeInPattern pat
            bodyFree $ Body lore bnds res

        bindingFree (Let pat annot e) = do
          tell $ freeIn annot
          expFree e
          binding (HS.fromList $ patternNames pat) $
            tell $ freeInPattern pat

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

        expFree (LoopOp (Kernel cs w index ispace inps returns body)) = do
          tell $ freeIn w
          tell $ freeIn cs
          tell $ freeIn index
          tell $ freeIn $ map snd ispace
          tell $ freeIn $ map fst returns
          binding (HS.fromList $ index : map fst ispace) $ do
            tell $ freeIn $ map kernelInputParam inps
            tell $ freeIn $ map kernelInputArray inps
            tell $ freeIn $ map kernelInputIndices inps
            binding (HS.fromList $ map kernelInputName inps) $
              bodyFree body

        expFree e = walkExpM freeWalker e

-- | Return the set of variable names that are free in the given body.
freeInBody :: (FreeIn (Annotations.Exp lore),
               FreeIn (Annotations.Body lore),
               FreeIn (Annotations.FParam lore),
               FreeIn (Annotations.LParam lore),
               FreeIn (Annotations.LetBound lore)) =>
              Body lore -> Names
freeInBody = execWriter . walkOnBody freeWalker

-- | Return the set of variable names that are free in the given
-- expression.
freeInExp :: (FreeIn (Annotations.Exp lore),
              FreeIn (Annotations.Body lore),
              FreeIn (Annotations.FParam lore),
              FreeIn (Annotations.LParam lore),
              FreeIn (Annotations.LetBound lore)) =>
             Exp lore -> Names
freeInExp = execWriter . walkExpM freeWalker

-- | Return the set of variable names that are free in the given
-- binding.
freeInBinding :: (FreeIn (Annotations.Exp lore),
                  FreeIn (Annotations.Body lore),
                  FreeIn (Annotations.FParam lore),
                  FreeIn (Annotations.LParam lore),
                  FreeIn (Annotations.LetBound lore)) =>
                 Binding lore -> Names
freeInBinding = execWriter . walkOnBinding freeWalker

-- | Return the set of variable names that are free in the given
-- lambda, including shape annotations in the parameters.
freeInLambda :: (FreeIn (Annotations.Exp lore),
                 FreeIn (Annotations.Body lore),
                 FreeIn (Annotations.FParam lore),
                 FreeIn (Annotations.LParam lore),
                 FreeIn (Annotations.LetBound lore)) =>
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
                    FreeIn (Annotations.LetBound lore)) =>
                   ExtLambda lore -> Names
freeInExtLambda (ExtLambda index params body rettype) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeIn params
        inBody = HS.filter (`notElem` paramnames) $ freeInBody body
        paramnames = index : map paramName params

freeInPattern :: FreeIn (Annotations.LetBound lore) => Pattern lore -> Names
freeInPattern (Pattern context values) =
  mconcat $ map freeIn $ context ++ values

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

instance (ArrayShape shape, FreeIn shape) => FreeIn (TypeBase shape) where
  freeIn (Array _ shape _) = freeIn shape
  freeIn (Mem size _)      = freeIn size
  freeIn (Basic _)         = mempty

instance FreeIn attr => FreeIn (ParamT attr) where
  freeIn (Param ident attr) =
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

instance FreeIn KernelSize where
  freeIn (KernelSize num_workgroups workgroup_size elems_per_thread
          num_elems thread_offset num_threads) =
    mconcat $ map freeIn [num_workgroups,
                          workgroup_size,
                          elems_per_thread,
                          num_elems,
                          thread_offset,
                          num_threads]

instance FreeIn d => FreeIn (DimChange d) where
  freeIn = Data.Foldable.foldMap freeIn

-- | Return the set of all variable names bound in a program.
progNames :: Prog lore -> Names
progNames = execWriter . mapM funNames . progFunctions
  where names = identityWalker {
                  walkOnBinding = bindingNames
                , walkOnBody = bodyNames
                , walkOnLambda = lambdaNames
                , walkOnExtLambda = extLambdaNames
                }

        one = tell . HS.singleton
        funNames fundec = do
          mapM_ (one . paramName) $ funDecParams fundec
          bodyNames $ funDecBody fundec

        bodyNames = mapM_ bindingNames . bodyBindings

        bindingNames (Let pat _ e) =
          mapM_ one (patternNames pat) >> expNames e

        expNames (LoopOp (DoLoop _ pat form loopbody)) = do
          mapM_ (one . paramName . fst) pat
          case form of ForLoop i _ ->
                         one i
                       WhileLoop _ ->
                         return ()
          bodyNames loopbody

        expNames (LoopOp (Kernel _ _ index ispace inps _ body)) = do
          one index
          mapM_ (one . kernelInputName) inps
          mapM_ (one . fst) ispace
          bodyNames body

        expNames e = walkExpM names e

        lambdaNames (Lambda index params body _) = do
          one index
          mapM_ (one . paramName) params
          bodyNames body

        extLambdaNames (ExtLambda index params body _) = do
          one index
          mapM_ (one . paramName) params
          bodyNames body

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
