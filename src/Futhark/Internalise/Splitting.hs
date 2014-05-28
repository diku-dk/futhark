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
  let (shape_body,value_body) = splitBody body
  (params', copies) <- nonuniqueParams params
  shape_body' <- insertBindingsM $ do
                  mapM_ addBinding copies
                  return shape_body
  let f_shape = (shapeFname, staticShapes $ map (`setAliases` ()) shapeRettype,
                 params', shape_body', loc)
      f       = (fname, valueRettype, params, value_body, loc)
  return (f, f_shape)
  where (shapeRettype, valueRettype) = splitType rettype
        shapeFname = shapeFunctionName fname

splitFuncall :: MonadBinder m =>
                Name -> [(SubExp, Diet)] -> [TypeBase Names Rank] -> SrcLoc
             -> m Exp
splitFuncall fname args rettype loc = do
  result_shape <- resultShape
  let valueRettype' = addTypeShapes rettype result_shape
  return $ Apply fname args (staticShapes valueRettype') loc
  where shapeRettype = typeShapes rettype
        shapeFname = shapeFunctionName fname

        resultShape
          | []      <- shapeRettype = return []
          | otherwise               =
            liftM (map Var) $
            letTupExp "fun_shapes" $
            Apply shapeFname [ (arg, Observe) | (arg, _) <- args]
            (staticShapes shapeRettype) loc

splitLambda :: ([Param], Body, [DeclType])
            -> (([Param], Body, [DeclType]),
                ([Param], Body, [DeclType]))
splitLambda (params, body, rettype) =
  ((params, shape_body, map toDecl sizeRettype),
   (params, value_body, valueRettype))
    where (shape_body,value_body) = splitBody body
          (sizeRettype, valueRettype) = splitType rettype

splitType :: ArrayShape shape =>
             [TypeBase als shape] -> ([Type], [TypeBase als shape])
splitType ts = let (shape_ts, value_ts) = splitTyped id ts
               in (map asType shape_ts, value_ts)
  where asType (Basic bt) = Basic bt
        asType _          = error "Non-basic shape type"

splitBody :: Body -> (Body, Body)
splitBody body = (shape_body, value_body)
    where shape_body = flip mapResult body $ \(Result cs es _) ->
            resultBody cs (fst $ splitTyped subExpType es) loc
          value_body = flip mapResult body $ \(Result cs es _) ->
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
