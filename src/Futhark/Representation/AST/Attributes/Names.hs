{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
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
         , freeInStmsAndRes
         , freeInBody
         , freeInExp
         , freeInStm
         , freeInLambda
         -- * Bound Names
         , boundInBody
         , boundByStm
         , boundByStms
         , boundByLambda

         , FreeAttr(..)
       )
       where

import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Scope

freeWalker :: (FreeAttr (ExpAttr lore),
               FreeAttr (BodyAttr lore),
               FreeIn (FParamAttr lore),
               FreeIn (LParamAttr lore),
               FreeIn (LetAttr lore),
               FreeIn (Op lore)) =>
              Walker lore (Writer Names)
freeWalker = identityWalker {
               walkOnSubExp = tell . freeIn
             , walkOnBody = tell . freeInBody
             , walkOnVName = tell . S.singleton
             , walkOnCertificates = tell . freeIn
             , walkOnOp = tell . freeIn
             }

-- | Return the set of variable names that are free in the given
-- statements and result.  Filters away the names that are bound by
-- the statements.
freeInStmsAndRes :: (FreeIn (Op lore),
                     FreeIn (LetAttr lore),
                     FreeIn (LParamAttr lore),
                     FreeIn (FParamAttr lore),
                     FreeAttr (BodyAttr lore),
                     FreeAttr (ExpAttr lore)) =>
                    Stms lore -> Result -> Names
freeInStmsAndRes stms res =
  (freeIn res `mappend` fold (fmap freeInStm stms))
  `S.difference` boundByStms stms

-- | Return the set of variable names that are free in the given body.
freeInBody :: (FreeAttr (ExpAttr lore),
               FreeAttr (BodyAttr lore),
               FreeIn (FParamAttr lore),
               FreeIn (LParamAttr lore),
               FreeIn (LetAttr lore),
               FreeIn (Op lore)) =>
              Body lore -> Names
freeInBody (Body attr stms res) =
  precomputed attr $ freeIn attr <> freeInStmsAndRes stms res

-- | Return the set of variable names that are free in the given
-- expression.
freeInExp :: (FreeAttr (ExpAttr lore),
              FreeAttr (BodyAttr lore),
              FreeIn (FParamAttr lore),
              FreeIn (LParamAttr lore),
              FreeIn (LetAttr lore),
              FreeIn (Op lore)) =>
             Exp lore -> Names
freeInExp (DoLoop ctxmerge valmerge form loopbody) =
  let (ctxparams, ctxinits) = unzip ctxmerge
      (valparams, valinits) = unzip valmerge
      bound_here = S.fromList $ M.keys $
                   scopeOf form <>
                   scopeOfFParams (ctxparams ++ valparams)
  in (freeIn (ctxinits ++ valinits) <> freeIn form <>
      freeIn (ctxparams ++ valparams) <> freeInBody loopbody)
     `S.difference` bound_here
freeInExp e = execWriter $ walkExpM freeWalker e

-- | Return the set of variable names that are free in the given
-- binding.
freeInStm :: (FreeAttr (ExpAttr lore),
              FreeAttr (BodyAttr lore),
              FreeIn (FParamAttr lore),
              FreeIn (LParamAttr lore),
              FreeIn (LetAttr lore),
              FreeIn (Op lore)) =>
             Stm lore -> Names
freeInStm (Let pat (StmAux cs attr) e) =
  freeIn cs <> precomputed attr (freeIn attr <> freeInExp e <> freeIn pat)

-- | Return the set of variable names that are free in the given
-- lambda, including shape annotations in the parameters.
freeInLambda :: (FreeAttr (ExpAttr lore),
                 FreeAttr (BodyAttr lore),
                 FreeIn (FParamAttr lore),
                 FreeIn (LParamAttr lore),
                 FreeIn (LetAttr lore),
                 FreeIn (Op lore)) =>
                Lambda lore -> Names
freeInLambda (Lambda params body rettype) =
  S.filter (`notElem` paramnames) $ inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeIn params
        inBody = freeInBody body
        paramnames = map paramName params

-- | A class indicating that we can obtain free variable information
-- from values of this type.
class FreeIn a where
  freeIn :: a -> Names

instance FreeIn () where
  freeIn () = mempty

instance FreeIn Int where
  freeIn = const mempty

instance (FreeIn a, FreeIn b) => FreeIn (a,b) where
  freeIn (a,b) = freeIn a <> freeIn b

instance (FreeIn a, FreeIn b, FreeIn c) => FreeIn (a,b,c) where
  freeIn (a,b,c) = freeIn a <> freeIn b <> freeIn c

instance FreeIn a => FreeIn [a] where
  freeIn = fold . fmap freeIn

instance FreeIn (Stm lore) => FreeIn (Stms lore) where
  freeIn = fold . fmap freeIn

instance FreeIn Names where
  freeIn = id

instance FreeIn Bool where
  freeIn _ = mempty

instance FreeIn a => FreeIn (Maybe a) where
  freeIn = maybe mempty freeIn

instance FreeIn VName where
  freeIn = S.singleton

instance FreeIn Ident where
  freeIn = freeIn . identType

instance FreeIn SubExp where
  freeIn (Var v) = freeIn v
  freeIn Constant{} = mempty

instance FreeIn d => FreeIn (ShapeBase d) where
  freeIn = mconcat . map freeIn . shapeDims

instance FreeIn d => FreeIn (Ext d) where
  freeIn (Free x) = freeIn x
  freeIn (Ext _)  = mempty

instance FreeIn shape => FreeIn (TypeBase shape u) where
  freeIn (Array _ shape _) = freeIn shape
  freeIn (Mem size _)      = freeIn size
  freeIn (Prim _)          = mempty

instance FreeIn attr => FreeIn (ParamT attr) where
  freeIn (Param _ attr) = freeIn attr

instance FreeIn attr => FreeIn (PatElemT attr) where
  freeIn (PatElem _ attr) = freeIn attr

instance FreeIn (LParamAttr lore) => FreeIn (LoopForm lore) where
  freeIn (ForLoop _ _ bound loop_vars) = freeIn bound <> freeIn loop_vars
  freeIn (WhileLoop cond) = freeIn cond

instance FreeIn d => FreeIn (DimChange d) where
  freeIn = Data.Foldable.foldMap freeIn

instance FreeIn d => FreeIn (DimIndex d) where
  freeIn = Data.Foldable.foldMap freeIn

instance FreeIn attr => FreeIn (PatternT attr) where
  freeIn (Pattern context values) =
    mconcat (map freeIn $ context ++ values) `S.difference` bound_here
    where bound_here = S.fromList $ map patElemName $ context ++ values

instance FreeIn Certificates where
  freeIn (Certificates cs) = freeIn cs

instance FreeIn attr => FreeIn (StmAux attr) where
  freeIn (StmAux cs attr) = freeIn cs <> freeIn attr

instance FreeIn a => FreeIn (IfAttr a) where
  freeIn (IfAttr r _) = freeIn r

-- | Either return precomputed free names stored in the attribute, or
-- the freshly computed names.  Relies on lazy evaluation to avoid the
-- work.
class FreeIn attr => FreeAttr attr where
  precomputed :: attr -> Names -> Names
  precomputed _ = id

instance FreeAttr () where

instance (FreeAttr a, FreeIn b) => FreeAttr (a,b) where
  precomputed (a,_) = precomputed a

instance FreeAttr a => FreeAttr [a] where
  precomputed [] = id
  precomputed (a:_) = precomputed a

instance FreeAttr a => FreeAttr (Maybe a) where
  precomputed Nothing = id
  precomputed (Just a) = precomputed a

-- | The names bound by the bindings immediately in a 'Body'.
boundInBody :: Body lore -> Names
boundInBody = boundByStms . bodyStms

-- | The names bound by a binding.
boundByStm :: Stm lore -> Names
boundByStm = S.fromList . patternNames . stmPattern

-- | The names bound by the bindings.
boundByStms :: Stms lore -> Names
boundByStms = fold . fmap boundByStm

-- | The names of the lambda parameters plus the index parameter.
boundByLambda :: Lambda lore -> [VName]
boundByLambda lam = map paramName (lambdaParams lam)
