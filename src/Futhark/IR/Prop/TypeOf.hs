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
-- "Futhark.IR.Mem" exposes functionality for
-- also obtaining information about the storage location of results.
module Futhark.IR.Prop.TypeOf
  ( expExtType,
    expExtTypeSize,
    subExpType,
    subExpResType,
    basicOpType,
    mapType,

    -- * Return type
    module Futhark.IR.RetType,

    -- * Type environment
    module Futhark.IR.Prop.Scope,

    -- * Extensibility
    TypedOp (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Futhark.IR.Prop.Constants
import Futhark.IR.Prop.Reshape
import Futhark.IR.Prop.Scope
import Futhark.IR.Prop.Types
import Futhark.IR.RetType
import Futhark.IR.Syntax

-- | The type of a subexpression.
subExpType :: HasScope t m => SubExp -> m Type
subExpType (Constant val) = pure $ Prim $ primValueType val
subExpType (Var name) = lookupType name

-- | Type type of a 'SubExpRes' - not that this might refer to names
-- bound in the body containing the result.
subExpResType :: HasScope t m => SubExpRes -> m Type
subExpResType = subExpType . resSubExp

-- | @mapType f arrts@ wraps each element in the return type of @f@ in
-- an array with size equal to the outermost dimension of the first
-- element of @arrts@.
mapType :: SubExp -> Lambda rep -> [Type]
mapType outersize f =
  [ arrayOf t (Shape [outersize]) NoUniqueness
    | t <- lambdaReturnType f
  ]

-- | The type of a primitive operation.
basicOpType :: HasScope rep m => BasicOp -> m [Type]
basicOpType (SubExp se) =
  pure <$> subExpType se
basicOpType (Opaque _ se) =
  pure <$> subExpType se
basicOpType (ArrayLit es rt) =
  pure [arrayOf rt (Shape [n]) NoUniqueness]
  where
    n = intConst Int64 $ toInteger $ length es
basicOpType (BinOp bop _ _) =
  pure [Prim $ binOpType bop]
basicOpType (UnOp _ x) =
  pure <$> subExpType x
basicOpType CmpOp {} =
  pure [Prim Bool]
basicOpType (ConvOp conv _) =
  pure [Prim $ snd $ convOpType conv]
basicOpType (Index ident slice) =
  result <$> lookupType ident
  where
    result t = [Prim (elemType t) `arrayOfShape` shape]
    shape = Shape $ sliceDims slice
basicOpType (Update _ src _ _) =
  pure <$> lookupType src
basicOpType (FlatIndex ident slice) =
  result <$> lookupType ident
  where
    result t = [Prim (elemType t) `arrayOfShape` shape]
    shape = Shape $ flatSliceDims slice
basicOpType (FlatUpdate src _ _) =
  pure <$> lookupType src
basicOpType (Iota n _ _ et) =
  pure [arrayOf (Prim (IntType et)) (Shape [n]) NoUniqueness]
basicOpType (Replicate (Shape []) e) =
  pure <$> subExpType e
basicOpType (Replicate shape e) =
  pure . flip arrayOfShape shape <$> subExpType e
basicOpType (Scratch t shape) =
  pure [arrayOf (Prim t) (Shape shape) NoUniqueness]
basicOpType (Reshape [] e) =
  result <$> lookupType e
  where
    result t = [Prim $ elemType t]
basicOpType (Reshape shape e) =
  result <$> lookupType e
  where
    result t = [t `setArrayShape` newShape shape]
basicOpType (Rearrange perm e) =
  result <$> lookupType e
  where
    result t = [rearrangeType perm t]
basicOpType (Rotate _ e) =
  pure <$> lookupType e
basicOpType (Concat i (x :| _) ressize) =
  result <$> lookupType x
  where
    result xt = [setDimSize i xt ressize]
basicOpType (Copy v) =
  pure <$> lookupType v
basicOpType (Manifest _ v) =
  pure <$> lookupType v
basicOpType Assert {} =
  pure [Prim Unit]
basicOpType (UpdateAcc v _ _) =
  pure <$> lookupType v

-- | The type of an expression.
expExtType ::
  (HasScope rep m, TypedOp (Op rep)) =>
  Exp rep ->
  m [ExtType]
expExtType (Apply _ _ rt _) = pure $ map (fromDecl . declExtTypeOf) rt
expExtType (If _ _ _ rt) = pure $ map extTypeOf $ ifReturns rt
expExtType (DoLoop merge _ _) =
  pure $ loopExtType $ map fst merge
expExtType (BasicOp op) = staticShapes <$> basicOpType op
expExtType (WithAcc inputs lam) =
  fmap staticShapes $
    (<>)
      <$> (concat <$> traverse inputType inputs)
      <*> pure (drop num_accs (lambdaReturnType lam))
  where
    inputType (_, arrs, _) = traverse lookupType arrs
    num_accs = length inputs
expExtType (Op op) = opType op

-- | The number of values returned by an expression.
expExtTypeSize ::
  (RepTypes rep, TypedOp (Op rep)) =>
  Exp rep ->
  Int
expExtTypeSize = length . feelBad . expExtType

-- FIXME, this is a horrible quick hack.
newtype FeelBad rep a = FeelBad {feelBad :: a}

instance Functor (FeelBad rep) where
  fmap f = FeelBad . f . feelBad

instance Applicative (FeelBad rep) where
  pure = FeelBad
  f <*> x = FeelBad $ feelBad f $ feelBad x

instance RepTypes rep => HasScope rep (FeelBad rep) where
  lookupType = const $ pure $ Prim $ IntType Int64
  askScope = pure mempty

-- | Given the parameters of a loop, produce the return type.
loopExtType :: Typed dec => [Param dec] -> [ExtType]
loopExtType params =
  existentialiseExtTypes inaccessible $ staticShapes $ map typeOf params
  where
    inaccessible = map paramName params

-- | Any operation must define an instance of this class, which
-- describes the type of the operation (at the value level).
class TypedOp op where
  opType :: HasScope t m => op -> m [ExtType]

instance TypedOp () where
  opType () = pure []
