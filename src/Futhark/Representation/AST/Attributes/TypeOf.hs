{-# LANGUAGE FlexibleContexts #-}
-- | This module provides facilities for obtaining the types of
-- various Futhark constructs.  Typically, you will need to execute
-- these in a context where type information is available as a
-- 'TypeEnv'; usually by using a monad that is an instance of
-- 'HasTypeEnv'.  The information is returned as a list of 'ExtType'
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
       , applyExtType

       -- * Return type
       , module Futhark.Representation.AST.RetType
       -- * Type environment
       , module Futhark.Representation.AST.Attributes.TypeEnv
       , typeEnvFromBindings
       , typeEnvFromParams
       , typeEnvFromPattern
       , typeEnvFromIdents
       , withParamTypes
       )
       where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Traversable hiding (mapM)

import Prelude hiding (mapM)

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Attributes.TypeEnv

-- | The type of a subexpression.
subExpType :: HasTypeEnv m => SubExp -> m Type
subExpType (Constant val) = pure $ Basic $ basicValueType val
subExpType (Var name)     = lookupType name

-- | @mapType f arrts@ wraps each element in the return type of @f@ in
-- an array with size equal to the outermost dimension of the first
-- element of @arrts@.
mapType :: SubExp -> Lambda lore -> [Type]
mapType outersize f = [ arrayOf t (Shape [outersize]) Unique
                      | t <- lambdaReturnType f ]

-- | The type of a primitive operation.
primOpType :: HasTypeEnv m =>
              PrimOp lore -> m [Type]
primOpType (SubExp se) =
  pure <$> subExpType se
primOpType (ArrayLit es rt) =
  arrays <$> traverse subExpType es
  where n = Constant (value (length es))
        arrays ts = [arrayOf rt (Shape [n]) $ mconcat $ map uniqueness ts]
primOpType (BinOp _ _ _ t) =
  pure [Basic t]
primOpType (Not _) =
  pure [Basic Bool]
primOpType (Complement _) =
  pure [Basic Int]
primOpType (Negate e) =
  pure <$> subExpType e
primOpType (Abs _) =
  pure [Basic Int]
primOpType (Signum _) =
  pure [Basic Int]
primOpType (Index _ ident idx) =
  result <$> lookupType ident
  where result t = [stripArray (length idx) t]
primOpType (Iota ne) =
  pure [arrayOf (Basic Int) (Shape [ne]) Nonunique]
primOpType (Replicate ne e) =
  result <$> subExpType e
  where result t = [arrayOf t (Shape [ne]) Unique]
primOpType (Scratch t shape) =
  pure [arrayOf (Basic t) (Shape shape) Unique]
primOpType (Reshape _ [] e) =
  result <$> lookupType e
  where result t = [Basic $ elemType t]
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
          [xt `setUniqueness` Unique `setOuterSize` ressize]
primOpType (Copy v) =
  result <$> lookupType v
  where result t = [t `setUniqueness` Unique]
primOpType (Assert _ _) =
  pure [Basic Cert]
primOpType (Alloc e space) =
  pure [Mem e space]
primOpType (Partition _ n _ arrays) =
  result <$> traverse lookupType arrays
  where result ts = replicate n (Basic Int) ++ ts

-- | The type of a loop operation.
loopOpExtType :: LoopOp lore -> [ExtType]
loopOpExtType (DoLoop res merge _ _) =
  loopExtType res $ map (paramIdent . fst) merge
loopOpExtType (Map _ size f _) =
  staticShapes $ mapType size f
loopOpExtType (ConcatMap _ _ f _) =
  [ Array (elemType t) (ExtShape $ Ext 0 : map Free (arrayDims t)) Unique
  | t <- lambdaReturnType f ]
loopOpExtType (Reduce _ _ fun _) =
  staticShapes $ lambdaReturnType fun
loopOpExtType (Scan _ width lam _) =
  staticShapes $
  map ((`setUniqueness` Unique) . (`arrayOfRow` width)) $
  lambdaReturnType lam
loopOpExtType (Redomap _ outersize outerfun innerfun _ _) =
  staticShapes $
  let acc_tp    = lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
  in  case res_el_tp of
        [] -> acc_tp
        _  -> acc_tp ++ [ arrayOf eltp (Shape [outersize]) Unique |
                          eltp <- res_el_tp ]
loopOpExtType (Stream _ outersize form lam _ _) =
  map (substNamesInExtType substs . (`setUniqueness` Unique)) rtp
  where nms = map paramName $ take (1 + length accs) params
        substs = HM.fromList $ zip nms (outersize:accs)
        ExtLambda _ params _ rtp = lam
        accs = case form of
                MapLike _ -> []
                RedLike _ _ acc -> acc
                Sequential  acc -> acc
loopOpExtType (MapKernel _ _ _ is _ returns _) =
  staticShapes
  [ rearrangeType perm (arrayOfShape t outer_shape) `setUniqueness` Unique
  | (t, perm) <- returns ]
  where outer_shape = Shape $ map snd is
loopOpExtType (ReduceKernel _ _ size parlam _ _ _) =
  staticShapes $
  map (`arrayOfRow` kernelWorkgroups size) $ lambdaReturnType parlam
loopOpExtType (ScanKernel _ w size _ lam _) =
  staticShapes $
  map (`arrayOfRow` w) (lambdaReturnType lam) ++
  map ((`arrayOfRow` kernelWorkgroups size) .
       (`arrayOfRow` kernelWorkgroupSize size))
  (lambdaReturnType lam)

-- | The type of a segmented operation.
segOpExtType :: HasTypeEnv m => SegOp lore -> m [ExtType]
segOpExtType (SegReduce _ size fun _ _) =
  pure $ staticShapes $ mapType size fun
segOpExtType (SegScan _ _ _ _ inputs _) =
  staticShapes <$> traverse (lookupType . snd) inputs
segOpExtType (SegReplicate _ _ dataarr _) =
  result <$> lookupType dataarr
  where result t = [Array (elemType t) (ExtShape [Ext 0]) Nonunique]

-- | The type of an expression.
expExtType :: (HasTypeEnv m, IsRetType (RetType lore)) =>
              Exp lore -> m [ExtType]
expExtType (Apply _ _ rt) = pure $ retTypeValues rt
expExtType (If _ _ _ rt)  = pure rt
expExtType (LoopOp op)    = pure $ loopOpExtType op
expExtType (PrimOp op)    = staticShapes <$> primOpType op
expExtType (SegOp op)     = segOpExtType op

-- | The number of values returned by an expression.
expExtTypeSize :: IsRetType (RetType lore) => Exp lore -> Int
expExtTypeSize = length . feelBad . expExtType

-- FIXME, this is a horrible quick hack.
newtype FeelBad a = FeelBad { feelBad :: a }

instance Functor FeelBad where
  fmap f = FeelBad . f . feelBad

instance Applicative FeelBad where
  pure = FeelBad
  f <*> x = FeelBad $ feelBad f $ feelBad x

instance HasTypeEnv FeelBad where
  lookupType = const $ pure $ Basic Int
  askTypeEnv = pure mempty

-- | The type of a body.
bodyExtType :: (HasTypeEnv m, Monad m) =>
               Body lore -> m [ExtType]
bodyExtType (Body _ bnds res) =
  existentialiseExtTypes bound <$> staticShapes <$>
  extendedTypeEnv (traverse subExpType res) bndtypes
  where bndtypes = typeEnvFromBindings bnds
        boundInLet (Let pat _ _) = patternNames pat
        bound = HS.fromList $ concatMap boundInLet bnds

-- | Given an the return type of a function and the values returned by
-- that function, return the size context.
valueShapeContext :: [ExtType] -> [Value] -> [Value]
valueShapeContext rettype values =
  map (BasicVal . value) $ extractShapeContext rettype $ map valueShape values

-- | Given the return type of a function and the subexpressions
-- returned by that function, return the size context.
subExpShapeContext :: HasTypeEnv m =>
                      [ExtType] -> [SubExp] -> m [SubExp]
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

-- | Produce the return type resulting from providing the given
-- subexpressions (with associated type) as arguments to a function of
-- the given return type and parameters.  Returns 'Nothing' if the
-- application is invalid.
applyExtType :: ExtRetType -> [Ident]
             -> [(SubExp,Type)]
             -> Maybe ExtRetType
applyExtType (ExtRetType extret) params args =
  if length args == length params &&
     and (zipWith subtypeOf
          (map rankShaped argtypes)
          (map rankShaped paramtypes))
  then Just $ ExtRetType $ map correctDims extret
  else Nothing
  where argtypes = map snd args
        paramtypes = map identType params

        parammap :: HM.HashMap VName SubExp
        parammap = HM.fromList $
                   zip (map identName params) (map fst args)

        correctDims t =
          t `setArrayShape`
          ExtShape (map correctDim $ extShapeDims $ arrayShape t)

        correctDim (Ext i) =
          Ext i
        correctDim (Free (Constant v)) =
          Free $ Constant v
        correctDim (Free (Var v))
          | Just se <- HM.lookup v parammap =
            Free se
          | otherwise =
            Free $ Var v

-- | Create a type environment consisting of the names bound in the
-- list of bindings.
typeEnvFromBindings :: [Binding lore] -> TypeEnv
typeEnvFromBindings = mconcat . map (typeEnvFromPattern . bindingPattern)

-- | Create a type environment from function parameters.
typeEnvFromParams :: [Param attr] -> TypeEnv
typeEnvFromParams = typeEnvFromIdents . map paramIdent

-- | Create a type environment from 'Ident's.
typeEnvFromIdents :: [Ident] -> TypeEnv
typeEnvFromIdents = HM.fromList . map assoc
  where assoc param = (identName param, identType param)

-- | Create a type environment a pattern.
typeEnvFromPattern :: Pattern lore -> TypeEnv
typeEnvFromPattern = typeEnvFromIdents . patternIdents

-- | Execute an action with a locally extended type environment.
withParamTypes :: LocalTypeEnv m =>
                  [Param attr] -> m a -> m a
withParamTypes = localTypeEnv . typeEnvFromParams

substNamesInExtType :: HM.HashMap VName SubExp -> ExtType -> ExtType
substNamesInExtType _ tp@(Basic _) = tp
substNamesInExtType subs (Mem se space) =
  Mem (substNamesInSubExp subs se) space
substNamesInExtType subs (Array btp shp u) =
  let shp' = ExtShape $ map (substNamesInExtDimSize subs) (extShapeDims shp)
  in  Array btp shp' u
substNamesInSubExp :: HM.HashMap VName SubExp -> SubExp -> SubExp
substNamesInSubExp _ e@(Constant _) = e
substNamesInSubExp subs (Var idd) =
  HM.lookupDefault (Var idd) idd subs
substNamesInExtDimSize :: HM.HashMap VName SubExp -> ExtDimSize -> ExtDimSize
substNamesInExtDimSize _ (Ext o) = Ext o
substNamesInExtDimSize subs (Free o) = Free $ substNamesInSubExp subs o
