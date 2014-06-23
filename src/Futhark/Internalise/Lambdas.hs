module Futhark.Internalise.Lambdas
  ( curryToLambda
  , ensureLambda
  , internaliseMapLambda
  , internaliseFoldLambda
  , internaliseFilterLambda
  )
  where

import Control.Applicative
import Control.Monad

import Data.List
import Data.Loc

import Futhark.ExternalRep as E
import Futhark.InternalRep as I
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
  (_,paramtypes) <- lookupFunction fname
  let missing = drop (length curargs) paramtypes
  params <- forM missing $ \t -> do
              s <- newNameFromString "curried"
              return E.Ident {
                         E.identType   = t
                       , E.identSrcLoc = loc
                       , E.identName   = s
                       }
  let observe x = (x, E.Observe) -- Actual diet doesn't matter here, the
                                 -- type checker will eventually fix it.
      call = E.Apply fname
             (map observe $ curargs ++ map (E.Var . E.fromParam) params)
             rettype loc
  return (params, call, E.toDecl rettype)

internaliseLambdaBody :: (E.Exp -> InternaliseM I.Body)
                      -> E.Exp -> InternaliseM I.Body
internaliseLambdaBody internaliseBody body = do
  body' <- internaliseBody body
  flip mapResultM body' $ \(Result cs es _) -> do
    -- Some of the subexpressions are actually
    -- certificates... filter them out!  This is slightly hacky, as
    -- we assume that the original input program does not contain
    -- certificates (or at least, that they are not part of the
    -- lambda return type).
    let (certs,vals) = partition ((==I.Basic I.Cert) . subExpType) es
    insertBindingsM $ do
      certs' <- letExps "lambda_cert" $ map I.SubExp certs
      return $ I.resultBody (cs++certs') vals loc
  where loc = srclocOf body

lambdaBinding :: I.Ident -> [E.Parameter] -> [I.Type]
              -> InternaliseM I.Body -> InternaliseM (I.Body, [I.Param])
lambdaBinding ce params ts m =
  bindingFlatPatternWithCert (I.Var ce) (map E.fromParam params) ts $ \params' -> do
    body <- m
    return (body, map I.toParam params')

outerShape :: SrcLoc -> [I.Type] -> SubExp
outerShape _ (t:_) = arraySize 0 t
outerShape loc _   = I.intconst 0 loc

internaliseLambda :: (E.Exp -> InternaliseM Body)
                  -> I.Ident
                  -> E.Lambda
                  -> [I.Type]
                  -> InternaliseM ([I.Param], I.Body, [I.DeclType])
internaliseLambda internaliseBody ce lam rowtypes = do
  (params, body, rettype, _) <- ensureLambda lam
  (body', params') <- lambdaBinding ce params rowtypes $
                      internaliseLambdaBody internaliseBody body
  return (params', body',
          map noInfoToUnit $ internaliseType' rettype)

internaliseMapLambda :: (E.Exp -> InternaliseM Body)
                     -> I.Ident
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseBody ce lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda internaliseBody ce lam rowtypes
  let rettype_shape = typeShapes rettype
      outer_shape = outerShape loc argtypes
  shapefun <- makeShapeFun params (shapeBody body)
              (replicate (length rettype_shape) $ I.Basic Int) loc
  inner_shapes <- bindMapShapes [ce] shapefun args outer_shape
  let rettype' = addTypeShapes rettype $
                 map I.Var inner_shapes
  body' <- assertResultShape inner_shapes body
  return $ I.Lambda params body' rettype' loc
  where loc = srclocOf lam

makeShapeFun :: MonadFreshNames m =>
                [I.Param] -> I.Body -> [I.ConstType] -> SrcLoc -> m I.Lambda
makeShapeFun params body rettype loc = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copybnds) <- nonuniqueParams params
  return $ I.Lambda params' (insertBindings copybnds body) rettype loc

assertResultShape :: [I.Ident] -> I.Body -> InternaliseM I.Body
assertResultShape desiredShapes (I.Body bnds res) = do
  certs <- replicateM (length desiredShapes) $
           newIdent "shape_cert" (I.Basic I.Cert) loc
  checks <- replicateM (length desiredShapes) $
            newIdent "shape_check" (I.Basic I.Bool) loc
  let check desired computed = I.BinOp I.Equal (I.Var desired) computed
                               (I.Basic I.Bool) loc
      cmps = zipWith Let (map pure checks) $
             zipWith check desiredShapes computedShapes
      asserts = zipWith Let (map pure certs) $
                map ((`I.Assert` loc) . I.Var) checks
  return $ I.Body (bnds++cmps++asserts)
    res { resultCertificates =
             resultCertificates res ++ certs
        }
  where computedShapes = concatMap subExpShape $ resultSubExps res
        loc = srclocOf res

bindMapShapes :: I.Certificates -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM [I.Ident]
bindMapShapes cs sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return []
  | otherwise =
    letTupExp "shape" =<< eIf isempty emptybranch nonemptybranch rt loc
  where loc = srclocOf sizefun
        zero = intconst 0 loc
        rt = map (const $ I.Basic I.Int) $ I.lambdaReturnType sizefun
        -- Cosmin this is UNSAFE!!!
        --        but makes HiperfitEgCos.fut work!
        --        i.e., instead of if size == 0 then 0 else inner_size
        --        just wrote inner_size (without considering the outer size)
        --        otherwise problems with invariant sizes being hoisted out.
        --        Troels & Cosmin will sleep more on it!
        fals = constant False loc
        isempty = pure $ SubExp fals
--        isempty = eBinOp I.Equal (pure $ I.SubExp outer_shape) (pure $ SubExp zero)
--                  (I.Basic I.Bool) loc
        emptybranch =
          pure $ resultBody
          [] (map (const zero) $ I.lambdaReturnType sizefun) loc
        nonemptybranch = insertBindingsM $
          resultBody [] <$> (eLambda sizefun =<< mapM index0 args) <*> pure loc
        index0 arg = do
          arg' <- letExp "arg" $ I.SubExp arg
          letSubExp "elem" $ I.Index cs arg' [intconst 0 loc] loc

internaliseFoldLambda :: (E.Exp -> InternaliseM Body)
                      -> I.Ident
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseBody ce lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda internaliseBody ce lam $ acctypes ++ rowtypes
  let rettype' = [ t `setArrayShape` arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same
  -- shape as the initial accumulator.  Generate an assertion and insert
  -- it at the end of the body.
  body' <-
    flip mapResultM body $ \(I.Result cs es resloc) -> do
      let subExpChecks :: I.Type -> I.Type -> InternaliseM [I.Ident]
          subExpChecks rest acct =
            forM (zip (I.arrayDims rest) (I.arrayDims acct)) $ \(res_n,acc_n) -> do
              size_cmp <- letSubExp "fold_size_cmp" $
                          I.BinOp I.Equal res_n acc_n (I.Basic I.Bool) resloc
              letExp "fold_size_chk" $ I.Assert size_cmp resloc
      insertBindingsM $ do
        cs2 <-
          liftM concat $ zipWithM subExpChecks (map subExpType es) acctypes
        return $ I.resultBody (cs++cs2) es resloc

  return $ I.Lambda params body' rettype' loc
  where loc = srclocOf lam

internaliseFilterLambda :: (E.Exp -> InternaliseM Body)
                     -> I.Ident
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseFilterLambda internaliseBody ce lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, _) <- internaliseLambda internaliseBody ce lam rowtypes
  return $ I.Lambda params body [I.Basic Bool] loc
  where loc = srclocOf lam
