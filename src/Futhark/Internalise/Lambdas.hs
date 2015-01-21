module Futhark.Internalise.Lambdas
  ( internaliseMapLambda
  , internaliseFoldLambda
  , internaliseFilterLambda
  )
  where

import Control.Applicative
import Control.Monad

import Data.List
import Data.Loc

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I
import Futhark.MonadFreshNames
import Futhark.Tools

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings

import Prelude hiding (mapM)

ensureLambda :: E.Lambda -> InternaliseM ([E.Parameter], E.Exp, E.DeclType, SrcLoc)
ensureLambda (E.AnonymFun params body rettype loc) =
  return (params, body, rettype, loc)
ensureLambda (E.CurryFun fname curargs rettype loc) = do
  (params, body, rettype') <- curryToLambda fname curargs rettype loc
  return (params, body, rettype', loc)

curryToLambda :: Name -> [E.Exp] -> E.Type -> SrcLoc
              -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
curryToLambda fname curargs rettype loc = do
  (_,paramtypes) <- externalFun <$> lookupFunction fname
  let missing = drop (length curargs) paramtypes
      diets = map E.diet paramtypes
  params <- forM missing $ \t -> do
              s <- newNameFromString "curried"
              return E.Ident {
                         E.identType   = t
                       , E.identSrcLoc = loc
                       , E.identName   = s
                       }
  let addDiet d x = (x, d)
      call = E.Apply fname
             (zipWith addDiet diets $
              curargs ++ map (E.Var . E.fromParam) params)
             rettype loc
  return (params, call, E.toDecl rettype)

lambdaBinding :: [E.Parameter] -> [I.Type]
              -> InternaliseM I.Body -> InternaliseM (I.Body, [I.Param])
lambdaBinding params ts m =
  bindingFlatPattern (map E.fromParam params) ts $ \params' -> do
    body <- m
    return (body, params')

internaliseLambda :: (E.Exp -> InternaliseM Body)
                  -> E.Lambda
                  -> [I.Type]
                  -> InternaliseM ([I.Param], I.Body, [I.ExtType])
internaliseLambda internaliseBody lam rowtypes = do
  (params, body, rettype, _) <- ensureLambda lam
  (body', params') <- lambdaBinding params rowtypes $
                      internaliseBody body
  return (params', body', internaliseType rettype)

internaliseMapLambda :: (E.Exp -> InternaliseM Body)
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseBody lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda internaliseBody lam rowtypes
  (rettype', inner_shapes) <- instantiateShapes' loc rettype
  let outer_shape = arraysSize 0 argtypes
      shape_body = shapeBody (map I.identName inner_shapes) rettype' body
  shapefun <- makeShapeFun params shape_body (length inner_shapes) loc
  bindMapShapes inner_shapes shapefun args outer_shape
  body' <- assertResultShape rettype' body
  return $ I.Lambda params body' rettype' loc
  where loc = srclocOf lam

makeShapeFun :: [I.Param] -> I.Body -> Int -> SrcLoc
             -> InternaliseM I.Lambda
makeShapeFun params body n loc = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copybnds) <- nonuniqueParams params
  return $ I.Lambda params' (insertBindings copybnds body) rettype loc
  where rettype = replicate n $ I.Basic Int

assertResultShape :: [I.Type] -> I.Body -> InternaliseM I.Body
assertResultShape rettype body = runBinder $ do
  es <- bodyBind body
  let assertProperShape t se =
        let name = "result_proper_shape"
        in ensureShape t name se
  reses <- zipWithM assertProperShape rettype es
  return $ resultBody reses loc
  where loc = srclocOf body

bindMapShapes :: [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise =
    void $
    letBind (basicPattern inner_shapes) =<<
    eIf isempty emptybranch nonemptybranch loc
  where loc = srclocOf sizefun
        zero = intconst 0 loc
        isempty = eBinOp I.Equal
                  (pure $ I.PrimOp $ I.SubExp outer_shape)
                  (pure $ I.PrimOp $ SubExp zero)
                  (I.Basic I.Bool) loc
        emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun) loc
        nonemptybranch = insertBindingsM $
          resultBody <$> (eLambda sizefun =<< mapM index0 args) <*> pure loc
        index0 arg = do
          arg' <- letExp "arg" $ I.PrimOp $ I.SubExp arg
          letSubExp "elem" $ I.PrimOp $ I.Index [] arg' [intconst 0 loc] loc

internaliseFoldLambda :: (E.Exp -> InternaliseM Body)
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseBody lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda internaliseBody lam $
                             acctypes ++ rowtypes
  let rettype' = [ t `setArrayShape` arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.  We accomplish this with an assertion and
  -- reshape().
  body' <- assertResultShape rettype' body

  return $ I.Lambda params body' rettype' loc
  where loc = srclocOf lam

internaliseFilterLambda :: (E.Exp -> InternaliseM Body)
                        -> E.Lambda
                        -> [I.SubExp]
                        -> InternaliseM I.Lambda
internaliseFilterLambda internaliseBody lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, _) <- internaliseLambda internaliseBody lam rowtypes
  return $ I.Lambda params body [I.Basic Bool] loc
  where loc = srclocOf lam
