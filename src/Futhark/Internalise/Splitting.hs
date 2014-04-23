module Futhark.Internalise.Splitting
  ( splitBody
  , shapeFunctionName
  , predFunctionName
  , splitFunction
  , splitLambda
  , splitType
  , splitIdents
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

shapeFunctionName :: Name -> Name
shapeFunctionName fname = fname <> nameFromString "_shape"

predFunctionName :: Name -> Name
predFunctionName fname = fname <> nameFromString "_shape"

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
