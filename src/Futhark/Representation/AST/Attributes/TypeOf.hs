{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
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
       , substNamesInExtType
       , module Futhark.Representation.AST.RetType
       , typeEnvFromBindings
       , module Futhark.Representation.AST.Attributes.TypeEnv
       )
       where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Attributes.TypeEnv

subExpType :: HasTypeEnv m => SubExp -> m Type
subExpType (Constant val) = pure $ Basic $ basicValueType val
subExpType (Var name)     = lookupType name

mapType :: Lambda lore -> [Type] -> [Type]
mapType f arrts = [ arrayOf t (Shape [outersize]) (uniqueness t)
                 | t <- lambdaReturnType f ]
  where outersize = arraysSize 0 arrts

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
primOpType (Negate e) =
  pure <$> subExpType e
primOpType (Index _ ident idx) =
  result <$> lookupType ident
  where result t = [stripArray (length idx) t]
primOpType (Iota ne) =
  pure [arrayOf (Basic Int) (Shape [ne]) Nonunique]
primOpType (Replicate ne e) =
  result <$> subExpType e
  where result t = [arrayOf t (Shape [ne]) $ uniqueness t]
primOpType (Scratch t shape) =
  pure [arrayOf (Basic t) (Shape shape) Unique]
primOpType (Reshape _ [] e) =
  result <$> lookupType e
  where result t = [Basic $ elemType t]
primOpType (Reshape _ shape e) =
  result <$> lookupType e
  where result t = [t `setArrayShape` Shape shape]
primOpType (Rearrange _ perm e) =
  result <$> lookupType e
  where result t = [t `setArrayShape` Shape (permuteShape perm $ arrayDims t)]
primOpType (Split _ sizeexps e) =
  result <$> lookupType e
  where result t = map (t `setOuterSize`) sizeexps
primOpType (Concat _ x ys ressize) =
  result <$> lookupType x <*> traverse lookupType ys
  where result xt yts =
          let u = uniqueness xt <> mconcat (map uniqueness yts)
          in [xt `setUniqueness` u `setOuterSize` ressize]
primOpType (Copy e) =
  result <$> subExpType e
  where result t = [t `setUniqueness` Unique]
primOpType (Assert _ _) =
  pure [Basic Cert]
primOpType (Alloc e) =
  pure [Mem e]
primOpType (Partition _ n _ array) =
  result <$> lookupType array
  where result t = replicate n (Basic Int) ++ [t]

loopOpExtType :: HasTypeEnv m =>
                 LoopOp lore -> m [ExtType]
loopOpExtType (DoLoop res merge _ _) =
  pure $ loopExtType res $ map (fparamIdent . fst) merge
loopOpExtType (Map _ f arrs) =
  staticShapes <$> mapType f <$> traverse lookupType arrs
loopOpExtType (ConcatMap _ f _) =
  pure [ Array (elemType t) (ExtShape $ Ext 0 : map Free (arrayDims t)) Unique
         | t <- lambdaReturnType f ]
loopOpExtType (Reduce _ fun _) =
  pure $ staticShapes $ lambdaReturnType fun
loopOpExtType (Scan _ _ inputs) =
  staticShapes <$> traverse (lookupType . snd) inputs
loopOpExtType (Redomap _ outerfun innerfun _ ids) =
  let acc_tp    = lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
      result outersize =
        acc_tp ++ [ arrayOf eltp (Shape [outersize]) (uniqueness eltp) |
                    eltp <- res_el_tp ]
  in  case res_el_tp of
        [] -> pure $ staticShapes acc_tp
        _  -> staticShapes <$> result <$> arraysSize 0 <$> traverse lookupType ids
loopOpExtType (Stream _ accs arrs lam) =
  result <$> lookupType (head arrs)
  where ExtLambda params _ rtp = lam
        result (Array _ shp _) =
          let nms = map identName $ take (2 + length accs) params
              (outersize, i0) = (head $ shapeDims shp, Constant $ IntVal 0)
              substs = HM.fromList $ zip nms (outersize:i0:accs)
          in map (substNamesInExtType substs) rtp
        result _ =
          rtp

segOpExtType :: HasTypeEnv m => SegOp lore -> m [ExtType]
segOpExtType (SegReduce _ fun _ descp) =
  staticShapes <$> mapType fun <$> pure <$> lookupType descp

expExtType :: (HasTypeEnv m, IsRetType (RetType lore)) =>
              Exp lore -> m [ExtType]
expExtType (Apply _ _ rt) = pure $ retTypeValues rt
expExtType (If _ _ _ rt)  = pure rt
expExtType (LoopOp op)    = loopOpExtType op
expExtType (PrimOp op)    = staticShapes <$> primOpType op
expExtType (SegOp op)    = segOpExtType op

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

bodyExtType' :: [Binding lore] -> [Type] -> [ExtType]
bodyExtType' bnds sets =
  existentialiseExtTypes bound $ staticShapes sets
  where boundInLet (Let pat _ _) = patternNames pat
        bound = HS.fromList $ concatMap boundInLet bnds

bodyExtType :: (HasTypeEnv m, Monad m) =>
               Body lore -> m [ExtType]
bodyExtType (Body _ bnds res) =
  bodyExtType' bnds <$>
  extendedTypeEnv (mapM subExpType (resultSubExps res)) bndtypes
  where bndtypes = typeEnvFromBindings bnds

valueShapeContext :: [ExtType] -> [Value] -> [Value]
valueShapeContext rettype values =
  map (BasicVal . value) $ extractShapeContext rettype $ map valueShape values

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

loopExtType :: [VName] -> [Ident] -> [ExtType]
loopExtType res merge =
  existentialiseExtTypes inaccessible $ staticShapes $ map identType res'
  where inaccessible = HS.fromList $ map identName merge
        res' = mapMaybe (\name -> find ((==name) . identName) merge) res

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
typeEnvFromBindings = HM.fromList . concatMap assoc
  where assoc bnd =
          [ (identName ident, identType ident)
          | ident <- patternIdents $ bindingPattern bnd
          ]

substNamesInExtType :: HM.HashMap VName SubExp -> ExtType -> ExtType
substNamesInExtType _ tp@(Basic _) = tp
substNamesInExtType subs (Mem se) =
  Mem $ substNamesInSubExp subs se
substNamesInExtType subs (Array btp shp u) =
  let shp' = ExtShape $ map (substNamesInExtDimSize subs) (extShapeDims shp)
  in  Array btp shp' u
substNamesInSubExp :: HM.HashMap VName SubExp -> SubExp -> SubExp
substNamesInSubExp _ e@(Constant _) = e
substNamesInSubExp subs (Var idd) =
  fromMaybe (Var idd) (HM.lookup idd subs)
substNamesInExtDimSize :: HM.HashMap VName SubExp -> ExtDimSize -> ExtDimSize
substNamesInExtDimSize _ (Ext o) = Ext o
substNamesInExtDimSize subs (Free o) = Free $ substNamesInSubExp subs o
