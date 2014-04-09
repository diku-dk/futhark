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
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

import Futhark.ExternalRep as E
import Futhark.InternalRep as I
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Substitute

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
      certs' <- letExps "lambda_cert" $ map I.subExp certs
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
          typeSizes $ map noInfoToUnit $ internaliseType' rettype)

internaliseMapLambda :: (E.Exp -> InternaliseM Body)
                     -> I.Ident
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM (I.Certificates, I.Lambda)
internaliseMapLambda internaliseBody ce lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda internaliseBody ce lam rowtypes
  let (shape_body, value_body) = splitBody body
      (rettype_shape, rettype_value) = splitType rettype
      shapefun = Lambda params shape_body
                 (replicate (length rettype_shape) $ I.Basic Int) loc
      outer_shape = outerShape loc argtypes
  (cs,inner_shapes) <- bindMapShapes [ce] shapefun args outer_shape
  let rettype' = addTypeShapes rettype_value $
                 map I.Var inner_shapes
  value_body' <- sanitiseValueSlice inner_shapes value_body
  return (cs, I.Lambda params value_body' rettype' loc)
  where loc = srclocOf lam

sanitiseValueSlice :: [I.Ident] -> I.Body -> InternaliseM I.Body
sanitiseValueSlice resShapes (I.Body bnds res) = do
  -- First, we rename any bindings of the currently computed shapes.
  bnds' <- mapM renameShapeBindings bnds
  -- Then, we replace any use of the old names.
  return $ removeSOACCerts $ substituteNames substs $ I.Body bnds' res
  where substs = HM.fromList $ mapMaybe (uncurry shapeSubst) $
                 zip resShapes $
                 concatMap subExpShape (resultSubExps res)

        renameShapeBindings (I.Let pat e) =
          Let <$> mapM renameShapeBinding pat <*> pure e
        renameShapeBindings (I.DoLoop merge i bound body) = do
          mergepat' <- mapM renameShapeBinding mergepat
          return $ I.DoLoop (zip mergepat' mergeexps) i bound body
          where (mergepat, mergeexps) = unzip merge

        renameShapeBinding var
          | isShapeBinding var = newIdent' (const "unused_shape") var
          | otherwise          = return var

        isShapeBinding = (`HM.member` substs) . I.identName

        shapeSubst resShape (I.Var v) = Just (I.identName v,
                                              I.identName resShape)
        shapeSubst _        _         = Nothing

removeSOACCerts :: Body -> Body
removeSOACCerts (Body bnds res) = Body (map removeCert bnds) res
  where
    removeCert (I.Let pat e) =
      I.Let pat $ case e of
                    I.Map _ fun arrs loc -> I.Map [] fun arrs loc
                    I.Reduce _ fun input loc -> I.Reduce [] fun input loc
                    I.Scan _ fun input loc -> I.Scan [] fun input loc
                    I.Filter _ fun arrs size loc -> I.Filter [] fun arrs size loc
                    I.Redomap _ outerfun innerfun acc arrs loc ->
                      I.Redomap [] outerfun innerfun acc arrs loc
                    _ -> e
    removeCert (I.DoLoop merge i bound body) =
      I.DoLoop merge i bound $ removeSOACCerts body

bindMapShapes :: I.Certificates -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM (I.Certificates, [I.Ident])
bindMapShapes cs sizefun args outer_shape = do
  comp_shapes <- replicateM (length (I.lambdaReturnType sizefun)) $
                 newIdent "map_computed_shape"
                 (I.arrayOf (I.Basic I.Int) (I.Shape [outer_shape]) I.Unique) loc
  let sizecomp = if null comp_shapes
                 then []
                 else [(comp_shapes, I.Map cs sizefun args loc)]
  mapM_ (uncurry letBind) sizecomp
  (certs, inner_shapes) <- unzip <$> mapM allEqual comp_shapes
  return (certs, inner_shapes)
  where loc = srclocOf sizefun

internaliseFoldLambda :: (E.Exp -> InternaliseM Body)
                      -> I.Ident
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM (I.Certificates, I.Lambda)
internaliseFoldLambda internaliseBody ce lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda internaliseBody ce lam $ acctypes ++ rowtypes
  let (_, value_body) = splitBody body
      (_, rettype_value) = splitType rettype
      rettype' = [ t `setArrayShape` arrayShape shape
                   | (t,shape) <- zip rettype_value acctypes ]
  -- The result of the body must have the exact same
  -- shape as the initial accumulator.  Generate an assertion and insert
  -- it at the end of the body.
  value_body' <-
    flip mapResultM value_body $ \(I.Result cs es resloc) -> do
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

  return ([], I.Lambda params value_body' rettype' loc)
  where loc = srclocOf lam

internaliseFilterLambda :: (E.Exp -> InternaliseM Body)
                     -> I.Ident
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM (I.Ident, I.Lambda)
internaliseFilterLambda internaliseBody ce lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, _) <- internaliseLambda internaliseBody ce lam rowtypes
  let (_, value_body) = splitBody body
      arg_outer_shape = outerShape loc argtypes
      filtfun         = I.Lambda params value_body [I.Basic Bool] loc
  result_size <- bindFilterResultOuterShape ce filtfun args arg_outer_shape
  return (result_size, filtfun)
  where loc = srclocOf lam

bindFilterResultOuterShape :: I.Ident -> I.Lambda -> [I.SubExp] -> I.DimSize
                           -> InternaliseM I.Ident
bindFilterResultOuterShape ce lam args input_size = do
  outershape <- newIdent "filter_size"
                (I.Basic Int) loc
  markarray  <- newIdent "filter_mark"
                (I.arrayOf (I.Basic Int) (I.Shape [input_size]) Nonunique) loc
  markfunBody <-
    flip mapResultM (lambdaBody lam) $ \res -> do
      let [ok] = resultSubExps res -- XXX
          funcs = resultCertificates res
          result e = resultBody funcs [e] loc
      ok_int <- newIdent "ok" (I.Basic Int) loc
      return $ Body [I.Let [ok_int] $
                      I.If ok (result one) (result zero) [I.Basic Int] loc] $
               I.Result funcs [I.Var ok_int] loc
  countfun <- binOpLambda I.Plus (I.Basic Int) loc
  let markfun = Lambda { I.lambdaParams = lambdaParams lam
                       , I.lambdaBody = markfunBody
                       , I.lambdaReturnType = [I.Basic Int]
                       , I.lambdaSrcLoc = loc
                       }
      markmap = I.Map [ce] markfun args loc
      countcomp = I.Reduce [ce] countfun [(zero, I.Var markarray)] loc
  letBind [markarray] markmap
  letBind [outershape] countcomp
  return outershape
  where loc = srclocOf lam
        zero = I.intconst 0 loc
        one  = I.intconst 1 loc
