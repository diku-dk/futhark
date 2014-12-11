{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.Representation.AST.Attributes.TypeOf
       (
         subExpType
       , bodyType
       , primOpType
       , loopOpType
       , typeOf
       , mapType
       , scanType
       , filterType
       , valueShapeContext
       , subExpShapeContext
       , loopShapeContext
       )
       where

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Representation.AST.ResType hiding (ResType)
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values

subExpType :: SubExp -> Type
subExpType (Constant val _) = Basic $ basicValueType val
subExpType (Var ident)      = identType ident

mapType :: Lambda lore -> [Type] -> [Type]
mapType f arrts = [ arrayOf t (Shape [outersize]) (uniqueness t)
                 | t <- lambdaReturnType f ]
  where outersize = arraysSize 0 arrts

scanType :: [Type] -> [Type]
scanType = map (`setUniqueness` Unique)

filterType :: Lambda lore -> [Type] -> [ExtType]
filterType _ =
  map extOuterDim
  where extOuterDim t =
          t `setArrayShape` ExtShape (extOuterDim' $ arrayShape t)
        extOuterDim' (Shape dims) =
          Ext 0 : map Free (drop 1 dims)

primOpType :: PrimOp lore -> [Type]
primOpType (SubExp se) =
  [subExpType se]
primOpType (ArrayLit es rt loc) =
  [arrayOf rt (Shape [n]) $
   mconcat $ map (uniqueness . subExpType) es]
  where n = Constant (value (length es)) loc
primOpType (BinOp _ _ _ t _) =
  [t]
primOpType (Not _ _) =
  [Basic Bool]
primOpType (Negate e _) =
  [subExpType e]
primOpType (Index _ ident idx _) =
  [stripArray (length idx) (identType ident)]
primOpType (Update _ src _ _ _) =
  [identType src]
primOpType (Iota ne _) =
  [arrayOf (Basic Int) (Shape [ne]) Nonunique]
primOpType (Replicate ne e _) =
  [arrayOf (subExpType e) (Shape [ne]) u]
  where u = uniqueness $ subExpType e
primOpType (Reshape _ [] e _) =
  [Basic $ elemType $ subExpType e]
primOpType (Reshape _ shape e _) =
  [subExpType e `setArrayShape` Shape shape]
primOpType (Rearrange _ perm e _) =
  [subExpType e `setArrayShape` Shape (permuteShape perm shape)]
  where Shape shape = arrayShape $ subExpType e
primOpType (Rotate _ _ e _) =
  [subExpType e]
primOpType (Split _ ne e secsize _) =
  [subExpType e `setOuterSize` ne,
   subExpType e `setOuterSize` secsize]
primOpType (Concat _ x y ressize _) =
  [subExpType x `setUniqueness` u `setOuterSize` ressize]
  where u = uniqueness (subExpType x) <> uniqueness (subExpType y)
primOpType (Copy e _) =
  [subExpType e `setUniqueness` Unique]
primOpType (Assert _ _) =
  [Basic Cert]
primOpType (Conjoin _ _) =
  [Basic Cert]
primOpType (Alloc e _) =
  [Mem e]

loopOpType :: Lore lore => LoopOp lore -> ResType lore
loopOpType (DoLoop res merge _ _ _ _) =
  existentialiseType bound $ staticResType $ map identType res
  where bound = HS.fromList $ map (bindeeName . fst) merge
loopOpType (Map _ f arrs _) =
  staticResType $ mapType f $ map subExpType arrs
loopOpType (Reduce _ fun _ _) =
  staticResType $ lambdaReturnType fun
loopOpType (Scan _ _ inputs _) =
  staticResType $ scanType $ map (subExpType . snd) inputs
loopOpType (Filter _ f arrs _) =
  extResType $ filterType f $ map subExpType arrs
loopOpType (Redomap _ outerfun _ _ _ _) =
  staticResType $ lambdaReturnType outerfun

-- | The type of a Futhark term.
typeOf :: Lore lore => Exp lore -> ResType lore
typeOf (PrimOp op) = staticResType $ primOpType op
typeOf (LoopOp op) = loopOpType op
typeOf (If _ _ _ t _) = t
typeOf (Apply _ _ t _) = t

bodyType :: Lore lore => Body lore -> ResType lore
bodyType (Body _ bnds res) =
  existentialiseType bound $
  staticResType $ map subExpType $ resultSubExps res
  where boundInLet (Let pat _ _) = patternNames pat
        bound = HS.fromList $ concatMap boundInLet bnds

valueShapeContext :: [ExtType] -> [Value] -> [Value]
valueShapeContext rettype values =
  map (BasicVal . value) $ extractShapeContext rettype $ map valueShape values

subExpShapeContext :: [ExtType] -> [SubExp] -> [SubExp]
subExpShapeContext rettype ses =
  extractShapeContext rettype $ map (arrayDims . subExpType) ses

-- | A loop returns not only the values indicated in the result
-- pattern, but also any shapes of arrays that are merge variables.
-- Thus, @loopResult res merge@ returns those variables in @merge@
-- that constitute the shape context.
loopShapeContext :: [Ident] -> [Ident] -> [Ident]
loopShapeContext res merge = resShapes
  where isMergeVar (Constant _ _) = Nothing
        isMergeVar (Var v)
          | v `elem` merge = Just v
          | otherwise      = Nothing
        resShapes =
          nub $ concatMap (mapMaybe isMergeVar . arrayDims . identType) res
