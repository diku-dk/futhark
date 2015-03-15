{-# LANGUAGE FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.TypeOf
       (
         expExtType
       , subExpType
       , bodyExtType
       , primOpType
       , loopOpExtType
       , mapType
       , filterType
       , valueShapeContext
       , subExpShapeContext
       , loopShapeContext
       , loopExtType
       , module Futhark.Representation.AST.RetType
       )
       where

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.RetType

--import Debug.Trace

subExpType :: SubExp -> Type
subExpType (Constant val) = Basic $ basicValueType val
subExpType (Var ident)    = identType ident

mapType :: Lambda lore -> [Type] -> [Type]
mapType f arrts = [ arrayOf t (Shape [outersize]) (uniqueness t)
                 | t <- lambdaReturnType f ]
  where outersize = arraysSize 0 arrts

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
primOpType (ArrayLit es rt) =
  [arrayOf rt (Shape [n]) $
   mconcat $ map (uniqueness . subExpType) es]
  where n = Constant (value (length es))
primOpType (BinOp _ _ _ t) =
  [Basic t]
primOpType (Not _) =
  [Basic Bool]
primOpType (Negate e) =
  [subExpType e]
primOpType (Index _ ident idx) =
  [stripArray (length idx) (identType ident)]
primOpType (Iota ne) =
  [arrayOf (Basic Int) (Shape [ne]) Nonunique]
primOpType (Replicate ne e) =
  [arrayOf (subExpType e) (Shape [ne]) u]
  where u = uniqueness $ subExpType e
primOpType (Scratch t shape) =
  [arrayOf (Basic t) (Shape shape) Unique]
primOpType (Reshape _ [] e) =
  [Basic $ elemType $ identType e]
primOpType (Reshape _ shape e) =
  [identType e `setArrayShape` Shape shape]
primOpType (Rearrange _ perm e) =
  [identType e `setArrayShape` Shape (permuteShape perm shape)]
  where Shape shape = arrayShape $ identType e
primOpType (Split _ sizeexps e) =
  map (identType e `setOuterSize`) sizeexps
primOpType (Concat _ x ys ressize) =
  [identType x `setUniqueness` u `setOuterSize` ressize]
  where u = uniqueness (identType x) <> mconcat (map (uniqueness . identType) ys)
primOpType (Copy e) =
  [subExpType e `setUniqueness` Unique]
primOpType (Assert _ _) =
  [Basic Cert]
primOpType (Alloc e) =
  [Mem e]

loopOpExtType :: LoopOp lore -> [ExtType]
loopOpExtType (DoLoop res merge _ _) =
  loopExtType res $ map (fparamIdent . fst) merge
loopOpExtType (Map _ f arrs) =
  staticShapes $ mapType f $ map identType arrs
loopOpExtType (ConcatMap _ f _) =
  [ Array (elemType t) (ExtShape $ Ext 0 : map Free (arrayDims t)) Unique
  | t <- lambdaReturnType f ]
loopOpExtType (Reduce _ fun _) =
  staticShapes $ lambdaReturnType fun
loopOpExtType (Scan _ _ inputs) =
  staticShapes $ map (identType . snd) inputs
loopOpExtType (Filter _ f arrs) =
  filterType f $ map identType arrs
loopOpExtType (Redomap _ outerfun innerfun _ ids) =
  let acc_tp    = lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
  in  case res_el_tp of
        [] -> staticShapes $ acc_tp
        _  -> let outersize  = arraysSize 0 (map identType ids) 
                  res_arr_tp :: [Type]
                  res_arr_tp = map (\eltp -> arrayOf eltp 
                                                     (Shape [outersize]) 
                                                     (uniqueness eltp  )
                                   ) res_el_tp 
              in  staticShapes $ (acc_tp ++ res_arr_tp)

expExtType :: IsRetType (RetType lore) => Exp lore -> [ExtType]
expExtType (Apply _ _ rt) = retTypeValues rt
expExtType (If _ _ _ rt)  = rt
expExtType (LoopOp op)    = loopOpExtType op
expExtType (PrimOp op)    = staticShapes $ primOpType op

bodyExtType :: Body lore -> [ExtType]
bodyExtType (Body _ bnds res) =
  existentialiseExtTypes bound $
  staticShapes $ map subExpType $ resultSubExps res
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
  where isMergeVar (Constant _) = Nothing
        isMergeVar (Var v)
          | v `elem` merge = Just v
          | otherwise      = Nothing
        resShapes =
          nub $ concatMap (mapMaybe isMergeVar . arrayDims . identType) res

loopExtType :: [Ident] -> [Ident] -> [ExtType]
loopExtType res merge =
  existentialiseExtTypes inaccessible $ staticShapes $ map identType res
  where inaccessible = HS.fromList $ map identName merge
