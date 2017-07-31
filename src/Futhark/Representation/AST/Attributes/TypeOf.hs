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
       , mapType
       , valueShapeContext
       , subExpShapeContext
       , loopResultContext
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
import qualified Data.Set as S
import Data.Traversable hiding (mapM)

import Prelude hiding (mapM)

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Names
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
              BasicOp lore -> m [Type]
primOpType (SubExp se) =
  pure <$> subExpType se
primOpType (Opaque se) =
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
primOpType (Index _ ident slice) =
  result <$> lookupType ident
  where result t = [Prim (elemType t) `arrayOfShape` shape]
        shape = Shape $ mapMaybe dimSize slice
        dimSize (DimSlice _ d _) = Just d
        dimSize DimFix{}         = Nothing
primOpType (Iota n _ _ et) =
  pure [arrayOf (Prim (IntType et)) (Shape [n]) NoUniqueness]
primOpType (Replicate (Shape []) e) =
  pure <$> subExpType e
primOpType (Repeat shape innershape v) =
  pure . modifyArrayShape repeatDims <$> lookupType v
  where repeatDims (Shape ds) =
          Shape $ concat (zipWith (++) (map shapeDims shape) (map pure ds)) ++
          shapeDims innershape
primOpType (Replicate shape e) =
  pure . flip arrayOfShape shape <$> subExpType e
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
primOpType (Rotate _ _ e) =
  pure <$> lookupType e
primOpType (Split _ i sizeexps e) =
  result <$> lookupType e
  where result t = map (setDimSize i t) sizeexps
primOpType (Concat _ i x _ ressize) =
  result <$> lookupType x
  where result xt = [setDimSize i xt ressize]
primOpType (Copy v) =
  pure <$> lookupType v
primOpType (Manifest _ v) =
  pure <$> lookupType v
primOpType (Assert _ _) =
  pure [Prim Cert]
primOpType (Partition _ n _ arrays) =
  result <$> traverse lookupType arrays
  where result ts = replicate n (Prim $ IntType Int32) ++ ts


-- | The type of an expression.
expExtType :: (HasScope lore m, TypedOp (Op lore)) =>
              Exp lore -> m [ExtType]
expExtType (Apply _ _ rt) = pure $ map fromDecl $ retTypeValues rt
expExtType (If _ _ _ rt)  = pure $ ifExtType rt
expExtType (DoLoop ctxmerge valmerge _ _) =
  pure $ loopExtType (map (paramIdent . fst) ctxmerge) (map (paramIdent . fst) valmerge)
expExtType (BasicOp op)    = staticShapes <$> primOpType op
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
bodyExtType :: (HasScope lore m, Monad m) =>
               Body lore -> m [ExtType]
bodyExtType (Body _ bnds res) =
  existentialiseExtTypes bound . staticShapes <$>
  extendedScope (traverse subExpType res) bndscope
  where bndscope = scopeOf bnds
        boundInLet (Let pat _ _) = patternNames pat
        bound = S.fromList $ concatMap boundInLet bnds

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
  extractShapeContext rettype <$> traverse (fmap arrayDims . subExpType) ses

-- | A loop returns not only its value merge parameters, but may also
-- have an existential context.  Thus, @loopResult ctxmergeparams
-- valmergeparams@ returns those paramters in @ctxmergeparams@ that
-- constitute the returned context.
loopResultContext :: FreeIn attr => [Param attr] -> [Param attr] -> [Param attr]
loopResultContext ctx val = filter usedInValue ctx
  where usedInValue = (`S.member` used) . paramName
        used = freeIn val <> freeIn ctx

-- | Given the context and value merge parameters of a Futhark @loop@,
-- produce the return type.
loopExtType :: [Ident] -> [Ident] -> [ExtType]
loopExtType ctx val =
  existentialiseExtTypes inaccessible $ staticShapes $ map identType val
  where inaccessible = S.fromList $ map identName ctx

-- | Any operation must define an instance of this class, which
-- describes the type of the operation (at the value level).
class TypedOp op where
  opType :: HasScope t m => op -> m [ExtType]

instance TypedOp () where
  opType () = pure []
