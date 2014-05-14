module Futhark.Internalise.Splitting
  ( splitBody
  , shapeFunctionName
  , splitFunction
  , splitLambda
  , splitType
  , splitIdents
  , splitFuncall
  )
  where

import Control.Monad

import Data.Loc
import Data.Monoid

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.InternalRep
import Futhark.Tools

shapeFunctionName :: Name -> Name
shapeFunctionName fname = fname <> nameFromString "_shape"

-- | Returns f, f_shape.
splitFunction :: FunDec -> InternaliseM (FunDec, FunDec)
splitFunction (fname,rettype,params,body,loc) = do
  let (shapeBody,valueBody) = splitBody body
  (params', copies) <- nonuniqueParams params
  shapeBody' <- insertBindingsM $ do
                  mapM_ addBinding copies
                  return shapeBody
  let f_shape = (shapeFname, map toDecl shapeRettype, params', shapeBody', loc)
      f       = (fname, valueRettype, params, valueBody, loc)
  return (f, f_shape)
  where (shapeRettype, valueRettype) = splitType rettype
        shapeFname = shapeFunctionName fname

splitFuncall :: MonadBinder m =>
                Name -> [(SubExp, Diet)] -> [TypeBase Names Rank] -> SrcLoc
             -> m Exp
splitFuncall fname args rettype loc = do
  result_shape <- resultShape
  let valueRettype' = addTypeShapes valueRettype result_shape
  return $ Apply fname args valueRettype' loc
  where (shapeRettype, valueRettype) = splitType $ typeSizes rettype
        shapeFname = shapeFunctionName fname

        resultShape
          | []      <- shapeRettype = return []
          | otherwise               =
            liftM (map Var) $
            letTupExp "fun_shapes" $
            Apply shapeFname [ (arg, Observe) | (arg, _) <- args]
            shapeRettype loc

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
splitTyped f l = let (sizes,values) = splitTyped' $ reverse l
                 in (reverse sizes, reverse values)
  where splitTyped' []    = ([],[])
        splitTyped' (x:xs) =
          let (sizes, values) = splitTyped' $ drop n xs
          in (take n xs ++ sizes, x : values)
          where n = arrayRank $ f x
