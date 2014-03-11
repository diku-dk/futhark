{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.Internalise.AccurateSizes
  ( subExpShape
  , paramShapes
  , splitBody
  , shapeFunctionName
  , splitFunction
  , splitLambda
  , splitType
  , typeSizes
  , subExpsWithShapes
  , allEqual
  , UnsizedLambda(..)
  , annotateArrayShape
  , annotateIdents
  , addTypeShapes
  )
  where

import Control.Monad

import Data.Loc
import Data.Monoid

import L0C.Internalise.Monad
import L0C.InternalRep
import L0C.MonadFreshNames

subExpShape :: SubExp -> [SubExp]
subExpShape = shapeDims . arrayShape . subExpType

subExpsWithShapes :: [SubExp] -> [SubExp]
subExpsWithShapes = concatMap addShapes
  where addShapes se = se : subExpShape se

shapeFunctionName :: Name -> Name
shapeFunctionName fname = fname <> nameFromString "_shape"

splitFunction :: FunDec -> (FunDec, FunDec)
splitFunction (fname,rettype,params,body,loc) =
  ((shapeFname, map toDecl shapeRettype, params, shapeBody, loc),
   (fname,      valueRettype,            params, valueBody, loc))
  where (shapeBody,valueBody) = splitBody body
        (shapeRettype, valueRettype) = splitType rettype
        shapeFname = shapeFunctionName fname

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
    where shapeBody = flip mapTail body $ \cs es ->
                      Result cs (fst $ splitTyped subExpType es) loc
          valueBody = flip mapTail body $ \cs es ->
                      Result cs (snd $ splitTyped subExpType es) loc
          loc = srclocOf body

splitTyped :: ArrayShape shape => (a -> TypeBase as shape) -> [a] -> ([a], [a])
splitTyped _ []     = ([],[])
splitTyped f (x:xs) =
  let (sizes, values) = splitTyped f (drop n xs)
  in (take n xs ++ sizes, x : values)
  where n = arrayRank $ f x

paramShapes :: MonadFreshNames m => IdentBase Names Rank -> m (Ident, [Ident])
paramShapes v = do
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

allEqual :: Ident -> InternaliseM (Ident, Ident, ([Ident], Exp))
allEqual comp_shape = do
  let loc = srclocOf comp_shape
  cert  <- newIdent "map_cert" (Basic Cert) loc
  shape <- newIdent "map_shape" (Basic Int) loc
  return (cert, shape,
          ([cert, shape],
           Apply (nameFromString "all_equal")
           [(Var comp_shape, Observe)]
           [Basic Cert, Basic Int] loc))

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
  t `setArrayShape` Shape (take (arrayRank t) (map intlit $ newshape ++ repeat 0))
  where intlit x = Constant (BasicVal $ IntVal x) loc

addTypeShapes :: [TypeBase als Rank]
              -> [SubExp]
              -> [TypeBase als Shape]
addTypeShapes [] _ = []
addTypeShapes (t:ts) shapes =
  let (shape,shapes') = splitAt (arrayRank t) shapes
      t' = t `setArrayShape` Shape shape
  in t' : addTypeShapes ts shapes'

addShapeAnnotations :: [IdentBase Names Rank] -> [Type] -> [Ident]
addShapeAnnotations = zipWith addShapeAnnotation
  where addShapeAnnotation v t = v { identType = t }

annotateIdents :: [IdentBase Names Rank]
               -> [Type] -> ([Ident], [Type])
annotateIdents vs ts =
  let (ts', rest_ts) = splitAt (length vs) ts
      vs' = addShapeAnnotations vs ts'
  in (vs', rest_ts)
