module Futhark.Internalise.Splitting
  ( splitBody
  , shapeFunctionName
  , predicateFunctionName
  , valueFunctionName
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
import qualified Futhark.Internalise.GenPredicate as GenPredicate
import Futhark.InternalRep
import Futhark.Tools

shapeFunctionName :: Name -> Name
shapeFunctionName fname = fname <> nameFromString "_shape"

predicateFunctionName :: Name -> Name
predicateFunctionName fname = fname <> nameFromString "_pred"

valueFunctionName :: Name -> Name
valueFunctionName fname = fname <> nameFromString "_val"

sliceNames :: Name -> (Name, Name, Name)
sliceNames fname = (predicateFunctionName fname,
                    shapeFunctionName fname,
                    valueFunctionName fname)

-- | Returns f, f_pred, f_shape, f_value.
splitFunction :: FunDec -> InternaliseM (FunDec, FunDec, FunDec, FunDec)
splitFunction fundec@(fname,rettype,origparams,_,loc) = do
  (f_pred, (_,_,params,body,_)) <-
    GenPredicate.splitFunction predFname fname fundec
  let (shapeBody,valueBody) = splitBody body
  (params', copies) <- nonuniqueParams params
  shapeBody' <- insertBindingsM $ do
                  mapM_ addBinding copies
                  return shapeBody
  fBody <- makeFullBody
  let f       = (fname, valueRettype, origparams, fBody, loc)
      f_shape = (shapeFname, map toDecl shapeRettype, params', shapeBody', loc)
      f_value = (valueFname, valueRettype, params, valueBody, loc)
  return (f, f_pred, f_shape, f_value)
  where (shapeRettype, valueRettype) = splitType rettype
        (predFname,shapeFname,valueFname) = sliceNames fname

        makeFullBody = runBinder $ do
          let args = [ (Var $ fromParam arg, Observe) | arg <- origparams ]
          eBody $ splitFuncall fname args (map (`setAliases` mempty) valueRettype) loc

splitFuncall :: MonadBinder m =>
                Name -> [(SubExp, Diet)] -> [TypeBase Names Rank] -> SrcLoc
             -> m Exp
splitFuncall fname args rettype loc = do
  ok <- letSubExp "pred" $ Apply predFname args [Basic Bool] loc
  cert <- letSubExp "cert" $ Assert ok loc
  result_shape <- resultShape cert
  let valueRettype' = addTypeShapes valueRettype result_shape
  return $ Apply valueFname ((cert, Observe):args) valueRettype' loc
  where (shapeRettype, valueRettype) = splitType $ typeSizes rettype
        (predFname,shapeFname,valueFname) = sliceNames fname

        resultShape cert
          | []      <- shapeRettype = return []
          | otherwise               =
            liftM (map Var) $
            letTupExp "fun_shapes" $
            Apply shapeFname ((cert,Observe) : [ (arg, Observe) | (arg, _) <- args])
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
