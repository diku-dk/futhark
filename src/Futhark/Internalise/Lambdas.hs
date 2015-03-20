module Futhark.Internalise.Lambdas
  ( internaliseMapLambda
  , internaliseConcatMapLambda
  , internaliseFoldLambda
  , internaliseRedomapInnerLambda
  , internalisePartitionLambdas
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

ensureLambda :: E.Lambda -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
ensureLambda (E.AnonymFun params body rettype _) =
  return (params, body, rettype)
ensureLambda (E.CurryFun fname curargs _ _) =
  curryToLambda fname curargs
ensureLambda (E.UnOpFun unop t _) =
  unOpFunToLambda unop t
ensureLambda (E.BinOpFun unop t _) =
  binOpFunToLambda unop t
ensureLambda (E.CurryBinOpLeft binop e t _) =
  binOpCurriedToLambda binop t e $ uncurry $ flip (,)
ensureLambda (E.CurryBinOpRight binop e t _) =
  binOpCurriedToLambda binop t e id

curryToLambda :: Name -> [E.Exp]
              -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
curryToLambda fname curargs = do
  (rettype,paramtypes) <- externalFun <$> lookupFunction fname
  let missing = drop (length curargs) paramtypes
      diets = map E.diet paramtypes
  params <- forM missing $ \t -> do
              s <- newNameFromString "curried"
              return E.Ident {
                         E.identType   = t
                       , E.identSrcLoc = noLoc
                       , E.identName   = s
                       }
  let addDiet d x = (x, d)
      call = E.Apply fname
             (zipWith addDiet diets $
              curargs ++ map (E.Var . E.fromParam) params)
             (fromDecl $ removeShapeAnnotations rettype) noLoc
  return (params, call, E.toDecl rettype)

unOpFunToLambda :: E.UnOp -> E.Type
                -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
unOpFunToLambda op t = do
  paramname <- newNameFromString "unop_param"
  let param = E.Ident { E.identType = t
                      , E.identSrcLoc = noLoc
                      , E.identName = paramname
                      }
  return ([toParam param],
          E.UnOp op (E.Var param) noLoc,
          E.vacuousShapeAnnotations $ E.toDecl t)

binOpFunToLambda :: E.BinOp -> E.Type
                 -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
binOpFunToLambda op t = do
  x_name <- newNameFromString "binop_param_x"
  y_name <- newNameFromString "binop_param_y"
  let param_x = E.Ident { E.identType = t
                        , E.identSrcLoc = noLoc
                        , E.identName = x_name
                        }
      param_y = E.Ident { E.identType = t
                        , E.identSrcLoc = noLoc
                        , E.identName = y_name
                        }
  return ([toParam param_x, toParam param_y],
          E.BinOp op (E.Var param_x) (E.Var param_y) t noLoc,
          E.vacuousShapeAnnotations $ E.toDecl t)

binOpCurriedToLambda :: E.BinOp -> E.Type
                     -> E.Exp
                     -> ((E.Exp,E.Exp) -> (E.Exp,E.Exp))
                     -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
binOpCurriedToLambda op t e swap = do
  paramname <- newNameFromString "binop_param_noncurried"
  let param = E.Ident { E.identType = t
                      , E.identSrcLoc = noLoc
                      , E.identName = paramname
                        }
      (x', y') = swap (E.Var param, e)
  return ([toParam param],
          E.BinOp op x' y' t noLoc,
          E.vacuousShapeAnnotations $ E.toDecl t)

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
  (params, body, rettype) <- ensureLambda lam
  (body', params') <- lambdaBinding params rowtypes $
                      internaliseBody body
  (rettype', _) <- internaliseDeclType rettype
  return (params', body', rettype')

internaliseMapLambda :: (E.Exp -> InternaliseM Body)
                     -> E.Lambda
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda internaliseBody lam args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  (params, body, rettype) <- internaliseLambda internaliseBody lam rowtypes
  (rettype', inner_shapes) <- instantiateShapes' rettype
  let outer_shape = arraysSize 0 argtypes
      shape_body = shapeBody (map I.identName inner_shapes) rettype' body
  shapefun <- makeShapeFun params shape_body (length inner_shapes)
  bindMapShapes inner_shapes shapefun args outer_shape
  body' <- ensureResultShape (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'

internaliseConcatMapLambda :: (E.Exp -> InternaliseM Body)
                           -> E.Lambda
                           -> [I.SubExp]
                           -> InternaliseM I.Lambda
internaliseConcatMapLambda internaliseBody lam _ = do
  (params, body, rettype) <- ensureLambda lam
  rettype' <- fst <$> internaliseDeclType rettype
  bindingParams params $ \shapeparams params' -> do
    body' <- internaliseBody body
    case rettype' of
      [I.Array bt (ExtShape [_]) _] ->
        return $ I.Lambda (shapeparams++params') body' [I.Basic bt]
      _ ->
        fail "concatMap lambda does not return a single-dimensional array"

makeShapeFun :: [I.Param] -> I.Body -> Int
             -> InternaliseM I.Lambda
makeShapeFun params body n = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copybnds) <- nonuniqueParams params
  return $ I.Lambda params' (insertBindings copybnds body) rettype
  where rettype = replicate n $ I.Basic Int

bindMapShapes :: [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise =
    letBind_ (basicPattern' inner_shapes) =<<
    eIf isempty emptybranch nonemptybranch
  where zero = intconst 0
        isempty = eBinOp I.Equal
                  (pure $ I.PrimOp $ I.SubExp outer_shape)
                  (pure $ I.PrimOp $ SubExp zero)
                  I.Bool
        emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun)
        nonemptybranch = insertBindingsM $
          resultBody <$> (eLambda sizefun =<< mapM index0 args)
        index0 arg = do
          arg' <- letExp "arg" $ I.PrimOp $ I.SubExp arg
          letSubExp "elem" $ I.PrimOp $ I.Index [] arg' [intconst 0]

internaliseFoldLambda :: (E.Exp -> InternaliseM Body)
                      -> E.Lambda
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseBody lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda internaliseBody lam $
                             acctypes ++ rowtypes
  let rettype' = [ t `I.setArrayShape` arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.  We accomplish this with an assertion and
  -- reshape().
  body' <- ensureResultShape (srclocOf lam) rettype' body

  return $ I.Lambda params body' rettype'


internaliseRedomapInnerLambda ::
                            (E.Exp -> InternaliseM Body)
                         -> E.Lambda
                         -> [I.SubExp]
                         -> [I.SubExp]
                         -> InternaliseM I.Lambda
internaliseRedomapInnerLambda internaliseBody lam nes arr_args = do
  let arrtypes = map I.subExpType arr_args
      rowtypes = map I.rowType arrtypes
      acctypes = map I.subExpType nes

  (params, body, rettype) <- internaliseLambda internaliseBody lam $
                             acctypes ++ rowtypes
  -- split rettype into (i) accummulator types && (ii) result-array-elem types
  let acc_len = length acctypes
      (acc_tps, res_el_tps) = (take acc_len rettype, drop acc_len rettype)
  -- For the map part:  for shape computation we build
  -- a map lambda from the inner lambda by dropping from
  -- the result the accumular and binding the accumulating
  -- param to their corresponding neutral-element subexp.
  -- Troels: would this be correct?
  (rettypearr', inner_shapes) <- instantiateShapes' res_el_tps
  let outer_shape = arraysSize 0 arrtypes
      acc_params  = take acc_len params
      map_bodyres = I.Result $ drop acc_len $ I.resultSubExps $ I.bodyResult body
      acc_bindings= map (\(ac_var,ac_val) ->
                            mkLet' [ac_var] (PrimOp $ SubExp ac_val)
                        ) (zip acc_params nes)

      map_bindings= acc_bindings ++ bodyBindings body
      map_lore    = bodyLore body
      map_body = I.Body map_lore map_bindings map_bodyres
      shape_body = shapeBody (map I.identName inner_shapes) rettypearr' map_body
  shapefun <- makeShapeFun (drop acc_len params) shape_body (length inner_shapes)
  bindMapShapes inner_shapes shapefun arr_args outer_shape

  -- for the reduce part
  let acctype' = [ t `I.setArrayShape` arrayShape shape
                   | (t,shape) <- zip acc_tps acctypes ]
  -- The reduce-part result of the body must have the exact same
  -- shape as the initial accumulator.  We accomplish this with
  -- an assertion and reshape().

  -- finally, place assertions and return result
  body' <- ensureResultShape (srclocOf lam) (acctype'++rettypearr') body
  return $ I.Lambda params body' (acctype'++rettypearr')

-- Given @n@ lambdas, this will return a lambda that returns an
-- integer in the range @[0,n]@.
internalisePartitionLambdas :: (E.Exp -> InternaliseM Body)
                            -> [E.Lambda]
                            -> [I.SubExp]
                            -> InternaliseM I.Lambda
internalisePartitionLambdas internaliseBody lams args = do
  let argtypes = map I.subExpType args
      rowtypes = map I.rowType argtypes
  lams' <- forM lams $ \lam -> do
    (params, body, _) <- internaliseLambda internaliseBody lam rowtypes
    return (params, body)
  params <- newIdents "partition_param" rowtypes
  body <- mkCombinedLambdaBody params 0 lams'
  return $ I.Lambda params body [I.Basic Int]
  where mkCombinedLambdaBody :: [I.Param]
                             -> Int
                             -> [([I.Param], I.Body)]
                             -> InternaliseM I.Body
        mkCombinedLambdaBody _      i [] =
          return $ resultBody [intconst i]
        mkCombinedLambdaBody params i ((lam_params,lam_body):lams') =
          case lam_body of
            Body () bodybnds (Result [boolres]) -> do
              intres <- newIdent "partition_equivalence_class" $ I.Basic Int
              next_lam_body <-
                mkCombinedLambdaBody lam_params (i+1) lams'
              let parambnds =
                    [ mkLet' [top] $ I.PrimOp $ I.SubExp $ I.Var fromp
                    | (top,fromp) <- zip lam_params params ]
                  branchbnd = mkLet' [intres] $ I.If boolres
                              (resultBody [intconst i])
                              next_lam_body
                              [I.Basic Int]
              return $ mkBody
                (parambnds++bodybnds++[branchbnd])
                (Result [I.Var intres])
            _ ->
              fail "Partition lambda returns too many values."
