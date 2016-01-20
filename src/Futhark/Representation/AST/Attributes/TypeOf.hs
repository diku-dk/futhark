{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module provides facilities for obtaining the types of
-- various Futhark constructs.  Typically, you will need to execute
-- these in a context where type information is available as a
-- 'Scope'; usually by using a monad that is an instance of
-- 'HasScope'.  The information is returned as a list of 'ExtType'
-- values - one for each of the values the Futhark construct returns.
-- Some constructs (such as subexpressions) can produce only a single
-- value, and their typing functions hence do not return a list.
--
-- Some representations may have more specialised facilities enabling
-- even more information - for example,
-- "Futhark.Representation.ExplicitMemory" exposes functionality for
-- also obtaining information about the storage location of results.
module Futhark.Representation.AST.Attributes.TypeOf
       (
         expExtType
       , expExtTypeSize
       , subExpType
       , bodyExtType
       , primOpType
       , loopOpExtType
       , mapType
       , valueShapeContext
       , subExpShapeContext
       , loopShapeContext
       , loopExtType

       -- * Return type
       , module Futhark.Representation.AST.RetType
       -- * Type environment
       , module Futhark.Representation.AST.Attributes.Scope

         -- * Extensibility
       , TypedOp(..)
       )
       where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS
import Data.Traversable hiding (mapM)

import Prelude hiding (mapM)

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Attributes.Scope

-- | The type of a subexpression.
subExpType :: HasScope t m => SubExp -> m Type
subExpType (Constant val) = pure $ Prim $ primValueType val
subExpType (Var name)     = lookupType name

-- | @mapType f arrts@ wraps each element in the return type of @f@ in
-- an array with size equal to the outermost dimension of the first
-- element of @arrts@.
mapType :: SubExp -> Lambda lore -> [Type]
mapType outersize f = [ arrayOf t (Shape [outersize]) NoUniqueness
                      | t <- lambdaReturnType f ]

-- | The type of a primitive operation.
primOpType :: HasScope t m =>
              PrimOp lore -> m [Type]
primOpType (SubExp se) =
  pure <$> subExpType se
primOpType (ArrayLit es rt) =
  pure [arrayOf rt (Shape [n]) NoUniqueness]
  where n = Constant (value (length es))
primOpType (BinOp bop _ _) =
  pure [Prim $ binOpType bop]
primOpType (UnOp _ x) =
  pure <$> subExpType x
primOpType CmpOp{} =
  pure [Prim Bool]
primOpType (ConvOp conv _) =
  pure [Prim $ snd $ convTypes conv]
primOpType (Index _ ident idx) =
  result <$> lookupType ident
  where result t = [stripArray (length idx) t]
primOpType (Iota ne) =
  pure [arrayOf (Prim $ IntType Int32) (Shape [ne]) NoUniqueness]
primOpType (Replicate ne e) =
  result <$> subExpType e
  where result t = [arrayOf t (Shape [ne]) NoUniqueness]
primOpType (Scratch t shape) =
  pure [arrayOf (Prim t) (Shape shape) NoUniqueness]
primOpType (Reshape _ [] e) =
  result <$> lookupType e
  where result t = [Prim $ elemType t]
primOpType (Reshape _ shape e) =
  result <$> lookupType e
  where result t = [t `setArrayShape` newShape shape]
primOpType (Rearrange _ perm e) =
  result <$> lookupType e
  where result t = [rearrangeType perm t]
primOpType (Stripe _ _ e) =
  pure <$> lookupType e
primOpType (Unstripe _ _ e) =
  pure <$> lookupType e
primOpType (Split _ sizeexps e) =
  result <$> lookupType e
  where result t = map (t `setOuterSize`) sizeexps
primOpType (Concat _ x _ ressize) =
  result <$> lookupType x
  where result xt =
          [xt `setOuterSize` ressize]
primOpType (Copy v) =
  pure <$> lookupType v
primOpType (Assert _ _) =
  pure [Prim Cert]
primOpType (Partition _ n _ arrays) =
  result <$> traverse lookupType arrays
  where result ts = replicate n (Prim $ IntType Int32) ++ ts

-- | The type of a loop operation.
loopOpExtType :: Typed (FParamAttr lore) =>
                 LoopOp lore -> [ExtType]
loopOpExtType (DoLoop res merge _ _) =
  loopExtType res $ map (paramIdent . fst) merge

-- | The type of an expression.
expExtType :: (HasScope lore m,
               IsRetType (RetType lore),
               Typed (FParamAttr lore),
               TypedOp (Op lore)) =>
              Exp lore -> m [ExtType]
expExtType (Apply _ _ rt) = pure $ map fromDecl $ retTypeValues rt
expExtType (If _ _ _ rt)  = pure rt
expExtType (LoopOp op)    = pure $ loopOpExtType op
expExtType (PrimOp op)    = staticShapes <$> primOpType op
expExtType (Op op)        = opType op

-- | The number of values returned by an expression.
expExtTypeSize :: (Annotations lore, TypedOp (Op lore)) =>
                  Exp lore -> Int
expExtTypeSize = length . feelBad . expExtType

-- FIXME, this is a horrible quick hack.
newtype FeelBad lore a = FeelBad { feelBad :: a }

instance Functor (FeelBad lore) where
  fmap f = FeelBad . f . feelBad

instance Applicative (FeelBad lore) where
  pure = FeelBad
  f <*> x = FeelBad $ feelBad f $ feelBad x

instance Annotations lore => HasScope lore (FeelBad lore) where
  lookupType = const $ pure $ Prim $ IntType Int32
  askScope = pure mempty

-- | The type of a body.
bodyExtType :: (Annotations lore, HasScope lore m, Monad m) =>
               Body lore -> m [ExtType]
bodyExtType (Body _ bnds res) =
  existentialiseExtTypes bound <$> staticShapes <$>
  extendedScope (traverse subExpType res) bndscope
  where bndscope = scopeOf bnds
        boundInLet (Let pat _ _) = patternNames pat
        bound = HS.fromList $ concatMap boundInLet bnds

-- | Given an the return type of a function and the values returned by
-- that function, return the size context.
valueShapeContext :: [TypeBase ExtShape u] -> [Value] -> [Value]
valueShapeContext rettype values =
  map (PrimVal . value) $ extractShapeContext rettype $ map valueShape values

-- | Given the return type of a function and the subexpressions
-- returned by that function, return the size context.
subExpShapeContext :: HasScope t m =>
                      [TypeBase ExtShape u] -> [SubExp] -> m [SubExp]
subExpShapeContext rettype ses =
  extractShapeContext rettype <$> traverse (liftA arrayDims . subExpType) ses

-- | A loop pures not only the values indicated in the result
-- pattern, but also any shapes of arrays that are merge variables.
-- Thus, @loopResult res merge@ pures those variables in @merge@
-- that constitute the shape context.
loopShapeContext :: [VName] -> [Ident] -> [VName]
loopShapeContext res merge = resShapes
  where isMergeVar (Constant _) = Nothing
        isMergeVar (Var v)
          | v `elem` mergenames = Just v
          | otherwise           = Nothing
        resShapes =
          nub $ concatMap (mapMaybe isMergeVar . arrayDims . identType) res'
        mergenames = map identName merge
        res' = mapMaybe (\name -> find ((==name) . identName) merge) res

-- | Given the result list and the merge parameters of a Futhark
-- @loop@, produce the return type.
loopExtType :: [VName] -> [Ident] -> [ExtType]
loopExtType res merge =
  existentialiseExtTypes inaccessible $ staticShapes $ map identType res'
  where inaccessible = HS.fromList $ map identName merge
        res' = mapMaybe (\name -> find ((==name) . identName) merge) res

class TypedOp op where
  opType :: HasScope t m => op -> m [ExtType]

instance TypedOp () where
  opType () = pure []
