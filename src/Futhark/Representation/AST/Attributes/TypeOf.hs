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
       , loopResultType
       , valueShapeContext
       , subExpShapeContext
       , loopResult
       )
       where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values

subExpType :: SubExp -> Type
subExpType (Constant val _) = valueType val
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

loopResultType :: [Type] -> [Ident] -> [ExtType]
loopResultType restypes merge = evalState (mapM inspect restypes) 0
  where bound = map identName merge
        inspect t = do
          shape <- mapM inspectShape $ arrayDims t
          return $ t `setArrayShape` ExtShape shape
        inspectShape (Var v)
          | identName v `elem` bound = do
            i <- get
            put $ i + 1
            return $ Ext i
        inspectShape se = return $ Free se

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
  doLoopResType res $ map fst merge
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

-- | The type of a Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: Lore lore => Exp lore -> ResType lore
typeOf (PrimOp op) = staticResType $ primOpType op
typeOf (LoopOp op) = loopOpType op
typeOf (If _ _ _ t _) = t
typeOf (Apply _ _ t _) = t

bodyType :: Body lore -> [ExtType]
bodyType (Body _ bnds res) =
  evalState (mapM makeBoundShapesFree $
             staticShapes $ map subExpType $ resultSubExps res)
  (0, HM.empty, HM.empty)
  where boundInLet (Let pat _ _) = patternNames pat
        bound = HS.fromList $ concatMap boundInLet bnds
        makeBoundShapesFree t = do
          shape <- mapM checkDim $ extShapeDims $ arrayShape t
          return $ t `setArrayShape` ExtShape shape
        checkDim (Free (Var v))
          | identName v `HS.member` bound =
            replaceVar $ identName v
        checkDim (Free se) = return $ Free se
        checkDim (Ext x)   = replaceExt x
        replaceExt x = do
          (n, extmap, varmap) <- get
          case HM.lookup x extmap of
            Nothing -> do put (n+1, HM.insert x (Ext n) extmap, varmap)
                          return $ Ext $ n+1
            Just replacement -> return replacement
        replaceVar name = do
          (n, extmap, varmap) <- get
          case HM.lookup name varmap of
            Nothing -> do put (n+1, extmap, HM.insert name (Ext n) varmap)
                          return $ Ext $ n+1
            Just replacement -> return replacement

valueShapeContext :: [ExtType] -> [Value] -> [Value]
valueShapeContext rettype values =
  map value $ extractShapeContext rettype $ map valueShape values

subExpShapeContext :: [ExtType] -> [SubExp] -> [SubExp]
subExpShapeContext rettype ses =
  extractShapeContext rettype $ map (arrayDims . subExpType) ses

-- | A loop returns not only the values indicated in the result
-- pattern, but also any non-static shapes of arrays.  Thus,
-- @loopResult res merge@ returns @res@ prefixed with with those
-- variables in @merge@ that constitute the shape context.
loopResult :: [Ident] -> [Ident] -> [Ident]
loopResult res merge = resShapes ++ res
  where notInRes (Constant _ _) = Nothing
        notInRes (Var v)
          | v `notElem` res,
            v `elem` merge = Just v
          | otherwise       = Nothing
        resShapes =
          nub $ concatMap (mapMaybe notInRes . arrayDims . identType) res
