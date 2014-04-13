{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.AccurateSizes
  ( subExpShape
  , identShapes
  , splitBody
  , shapeFunctionName
  , splitFunction
  , splitLambda
  , splitType
  , splitIdents
  , typeSizes
  , subExpsWithShapes
  , allEqual
  , UnsizedLambda(..)
  , annotateArrayShape
  , annotateIdents
  , addTypeShapes
  , addIdentShapes
  )
  where

import Control.Applicative
import Control.Monad

import Data.Loc
import Data.Monoid

import Futhark.Internalise.Monad
import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Tools

subExpShape :: SubExp -> [SubExp]
subExpShape = shapeDims . arrayShape . subExpType

subExpsWithShapes :: [SubExp] -> [SubExp]
subExpsWithShapes = concatMap addShapes
  where addShapes se = se : subExpShape se

shapeFunctionName :: Name -> Name
shapeFunctionName fname = fname <> nameFromString "_shape"

splitFunction :: FunDec -> InternaliseM (FunDec, FunDec)
splitFunction (fname,rettype,params,body,loc) = do
  (params', copies) <-
    liftM unzip $ forM params $ \param ->
      if unique $ identType param then do
        param' <- nonuniqueParam <$> newIdent' (++"_nonunique") param
        return (param',
                [([fromParam param],
                  Copy (Var $ fromParam param') $ srclocOf param')])
      else
        return (param, [])
  shapeBody' <- insertBindingsM $ do
                  mapM_ (uncurry letBind) $ concat copies
                  return shapeBody
  return ((shapeFname, map toDecl shapeRettype, params', shapeBody', loc),
          (fname,      valueRettype,            params,  valueBody,  loc))
  where (shapeBody,valueBody) = splitBody body
        (shapeRettype, valueRettype) = splitType rettype
        shapeFname = shapeFunctionName fname

        nonuniqueParam param =
          param { identType = identType param `setUniqueness` Nonunique }

splitLambda :: ([Param], Body, [DeclType])
            -> (([Param], Body, [DeclType]),
                ([Param], Body, [DeclType]))
splitLambda (params, body, rettype) =
  ((params, shapeBody, map toDecl sizeRettype),
   (params, valueBody, valueRettype))
    where (shapeBody,valueBody) = splitBody body
          (sizeRettype, valueRettype) = splitType rettype

splitType :: ArrayShape shape =>
             [TypeBase als shape] -> ([Type], [TypeBase als shape])
splitType ts = let (shape_ts, value_ts) = splitTyped id ts
               in (map asType shape_ts, value_ts)
  where asType (Basic bt) = Basic bt
        asType _          = error "Non-basic shape type"

splitBody :: Body -> (Body, Body)
splitBody body = (shapeBody, valueBody)
    where shapeBody = flip mapResult body $ \(Result cs es _) ->
                      resultBody cs (fst $ splitTyped subExpType es) loc
          valueBody = flip mapResult body $ \(Result cs es _) ->
                      resultBody cs (snd $ splitTyped subExpType es) loc
          loc = srclocOf body

splitIdents :: [Ident] -> ([Ident], [Ident])
splitIdents = splitTyped identType

splitTyped :: ArrayShape shape => (a -> TypeBase as shape) -> [a] -> ([a], [a])
splitTyped _ []     = ([],[])
splitTyped f (x:xs) =
  let (sizes, values) = splitTyped f (drop n xs)
  in (take n xs ++ sizes, x : values)
  where n = arrayRank $ f x

identShapes :: (MonadFreshNames m, ArrayShape shape) =>
               IdentBase Names shape -> m (Ident, [Ident])
identShapes v = do
  shape <- replicateM rank $ newIdent (base ++ "_size") (Basic Int) $ srclocOf v
  let vshape = Shape $ map Var shape
  return (v { identType = identType v `setArrayShape` vshape },
          shape)
  where base = textual $ baseName $ identName v
        rank = arrayRank $ identType v

typeSizes :: ArrayShape shape =>
             [TypeBase als shape] -> [TypeBase als shape]
typeSizes = concatMap addShapeTypes
  where addShapeTypes t = t : replicate (arrayRank t) (Basic Int)

allEqual :: Ident -> InternaliseM (Ident, Ident)
allEqual comp_shape = do
  x <- newIdent "x" (Basic Int) loc
  y <- newIdent "y" (Basic Int) loc
  compFun <- makeLambda [toParam x, toParam y] $ eBody $
    pure $ BinOp Equal (Var x) (Var y) (Basic Bool) loc
  bacc <- newIdent "bacc" (Basic Bool) loc
  nacc <- newIdent "nacc" (Basic Int) loc
  belm <- newIdent "belm" (Basic Bool) loc
  nelm <- newIdent "nelm" (Basic Int) loc
  checkFun <- makeLambda (map toParam [bacc,nacc,belm,nelm]) $ eBody $
    eSubExps [ pure $ BinOp LogAnd (Var bacc) (Var belm) (Basic Bool) loc
             , pure $ subExp $ Var nelm ] loc
  comp_shape_rot1 <- letExp "comp_shape_rot1" $ Rotate [] 1 (Var comp_shape) loc
  comp <- letExp "map_size_checks" $
          Map [] compFun [Var comp_shape, Var comp_shape_rot1] loc
  checked <- newIdent "all_equal_checked" (Basic Bool) loc
  shape   <- newIdent "all_equal_shape" (Basic Int) loc
  letBind [checked, shape] $
          Reduce [] checkFun [(constant True loc,Var comp),
                              (intconst 0 loc,Var comp_shape)] loc
  cert <- letExp "all_equal_cert" $ Assert (Var checked) loc
  return (cert, shape)
  where loc  = srclocOf comp_shape

data UnsizedLambda = UnsizedLambda {
    unsizedLambdaParams     :: [Param]
  , unsizedLambdaBody       :: Body
  , unsizedLambdaReturnType :: [DeclType]
  , unsizedLambdaSrcLoc     :: SrcLoc
  }
  deriving (Eq, Ord, Show)

annotateArrayShape :: ArrayShape shape =>
                      TypeBase als shape -> ([Int], SrcLoc) -> TypeBase als Shape
annotateArrayShape t (newshape, loc) =
  t `setArrayShape` Shape (take (arrayRank t) (map (`intconst` loc) $ newshape ++ repeat 0))

addTypeShapes :: [TypeBase als Rank]
              -> [SubExp]
              -> [TypeBase als Shape]
addTypeShapes [] _ = []
addTypeShapes (t:ts) shapes =
  let (shape,shapes') = splitAt (arrayRank t) shapes
      t' = t `setArrayShape` Shape shape
  in t' : addTypeShapes ts shapes'

addIdentShapes :: ArrayShape oldshape =>
                  [IdentBase als oldshape]
               -> [SubExp]
               -> [IdentBase als Shape]
addIdentShapes [] _ = []
addIdentShapes (v:vs) shapes =
  let (shape,shapes') = splitAt (arrayRank $ identType v) shapes
      t' = identType v `setArrayShape` Shape shape
  in v { identType = t' } : addIdentShapes vs shapes'

addShapeAnnotations :: [IdentBase Names Rank] -> [Type] -> [Ident]
addShapeAnnotations = zipWith addShapeAnnotation
  where addShapeAnnotation v t =
          v { identType = identType v `setArrayShape` arrayShape t }

annotateIdents :: [IdentBase Names Rank]
               -> [Type] -> ([Ident], [Type])
annotateIdents vs ts =
  let (ts', rest_ts) = splitAt (length vs) ts
      vs' = addShapeAnnotations vs ts'
  in (vs', rest_ts)
