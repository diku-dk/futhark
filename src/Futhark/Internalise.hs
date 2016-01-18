{-# LANGUAGE TupleSections #-}
-- |
--
-- This module implements a transformation from external to internal
-- Futhark.
--
module Futhark.Internalise
  ( internaliseProg
  , internaliseValue
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM, sequence)
import Control.Monad.Reader hiding (mapM, sequence)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Traversable (mapM, sequence)
import Data.Loc

import Prelude hiding (mapM, sequence)

import Futhark.Representation.External as E
import Futhark.Representation.SOACS as I
import Futhark.Transform.Rename as I
import Futhark.Transform.Substitute
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings
import Futhark.Internalise.Lambdas

-- | Convert a program in external Futhark to a program in internal
-- Futhark.  If the boolean parameter is false, do not add bounds
-- checks to array indexing.
internaliseProg :: MonadFreshNames m =>
                   Bool -> E.Prog -> m (Either String I.Prog)
internaliseProg doBoundsCheck prog = do
  res <- do
    ftable_attempt <- buildFtable prog
    case ftable_attempt of
      Left err -> return $ Left err
      Right ftable -> do
        funs <- runInternaliseM doBoundsCheck ftable $
                mapM internaliseFun $ E.progFunctions prog
        return $ fmap I.Prog funs
  sequence $ fmap I.renameProg res

buildFtable :: MonadFreshNames m => E.Prog
            -> m (Either String FunTable)
buildFtable = runInternaliseM True HM.empty .
              liftM HM.fromList . mapM inspect . E.progFunctions
  where inspect (fname, rettype, params, _, _) =
          bindingParams params $ \shapes values -> do
            (rettype', _) <- internaliseReturnType rettype
            let shapenames = map I.paramName shapes
            return (fname,
                    FunBinding { internalFun = (shapenames,
                                                map I.paramDeclType values,
                                                applyRetType
                                                (ExtRetType rettype')
                                                (shapes++values)
                                               )
                               , externalFun = (rettype, map E.identType params)
                               })

internaliseFun :: E.FunDec -> InternaliseM I.FunDec
internaliseFun (fname,rettype,params,body,loc) =
  bindingParams params $ \shapeparams params' -> do
    (rettype', _) <- internaliseReturnType rettype
    firstbody <- internaliseBody body
    body' <- ensureResultExtShape asserting loc
             (map I.fromDecl rettype') firstbody
    return $ FunDec
      fname (ExtRetType rettype')
      (shapeparams ++ params')
      body'

internaliseIdent :: E.Ident -> InternaliseM I.VName
internaliseIdent (E.Ident name tp _) =
  case internaliseType tp of
    [I.Prim _] -> return name
    _           -> fail $ "Futhark.Internalise.internaliseIdent: asked to internalise non-prim-typed ident '"
                   ++ pretty name ++ "'."

internaliseBody :: E.Exp -> InternaliseM Body
internaliseBody e = insertBindingsM $ do
  ses <- internaliseExp "res" e
  return $ resultBody ses

internaliseBodyBindings :: E.Exp -> ([SubExp] -> InternaliseM (Body, a))
                        -> InternaliseM (Body, a)
internaliseBodyBindings e m = do
  ((Body _ bnds res,x), otherbnds) <-
    collectBindings $ m =<< internaliseExp "res" e
  (,x) <$> mkBodyM (otherbnds <> bnds) res

extraBodyBindings :: [Binding]
                  -> InternaliseM (Body, a)
                  -> InternaliseM (Body, a)
extraBodyBindings bnds m = do
  (body, x) <- m
  return (insertBindings bnds body, x)

internaliseExp :: String -> E.Exp -> InternaliseM [I.SubExp]

internaliseExp _ (E.Var var) = do
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  case subst of
    Nothing     -> (:[]) . I.Var <$> internaliseIdent var
    Just substs -> return $ map I.Var substs

internaliseExp desc (E.Index var idxs loc) = do
  idxs' <- mapM (internaliseExp1 "i") idxs
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  case subst of
    Nothing ->
      fail $ "Futhark.Internalise.internaliseExp Index: unknown variable " ++ textual (E.identName var) ++ "."
    Just vs -> do
      csidx' <- boundsChecks loc vs idxs'
      let index v = I.PrimOp $ I.Index csidx' v idxs'
      letSubExps desc (map index vs)

internaliseExp desc (E.TupLit es _) =
  concat <$> mapM (internaliseExp desc) es

internaliseExp desc (E.ArrayLit [] et _) =
  letSubExps desc $ map arrayLit $ internaliseType et
  where arrayLit et' =
          I.PrimOp $ I.ArrayLit [] $ et' `annotateArrayShape` []

internaliseExp desc (E.ArrayLit es rowtype _) = do
  aes <- mapM (internaliseExpToVars "arr_elem") es
  let es'@((e':_):_) = aes --- XXX, ugh.
  Shape rowshape <- arrayShape <$> lookupType e'
  case internaliseType rowtype of
    [et] -> letTupExp' desc $ I.PrimOp $
            I.ArrayLit (map I.Var $ concat es')
            (et `I.setArrayShape` Shape rowshape)
    ets   -> do
      let arraylit ks et =
            I.PrimOp $ I.ArrayLit (map I.Var ks)
            (et `I.setArrayShape` Shape rowshape)
      letSubExps desc (zipWith arraylit (transpose es') ets)

internaliseExp desc (E.Apply fname args _ _)
  | "trace" <- nameToString fname = do
  args' <- mapM (internaliseExp "arg" . fst) args
  let args'' = concatMap tag args'
  rettype <- ExtRetType <$>
             map (`I.toDecl` Nonunique) <$>
             staticShapes <$>
             mapM (subExpType . fst) args''
  letTupExp' desc $ I.Apply fname args'' rettype
  where tag ses = [ (se, I.Observe) | se <- ses ]

internaliseExp desc (E.Apply fname args _ _)
  | Just (rettype, _) <- HM.lookup fname builtInFunctions = do
  args' <- mapM (internaliseExp "arg" . fst) args
  let args'' = concatMap tag args'
  letTupExp' desc $ I.Apply fname args'' (ExtRetType [I.Prim rettype])
  where tag ses = [ (se, I.Observe) | se <- ses ]

internaliseExp desc (E.Apply fname args _ _) = do
  args' <- liftM concat $ mapM (internaliseExp "arg" . fst) args
  (shapes, paramts, rettype_fun) <- internalFun <$> lookupFunction fname
  argts <- mapM subExpType args'
  let diets = map I.diet paramts
      shapeargs = argShapes shapes paramts argts
      args'' = zip shapeargs (repeat I.Observe) ++
               zip args' diets
  case rettype_fun $ map (,I.Prim int32) shapeargs ++ zip args' argts of
    Nothing -> fail $ "Cannot apply " ++ pretty fname ++ " to arguments " ++
               pretty (shapeargs ++ args')
    Just rettype ->
      letTupExp' desc $ I.Apply fname args'' rettype

internaliseExp desc (E.LetPat pat e body _) = do
  ses <- internaliseExp desc e
  t <- I.staticShapes <$> mapM I.subExpType ses
  bindingTupIdent pat t $ \pat' -> do
    forM_ (zip (patternIdents pat') ses) $ \(p,se) ->
      letBind (basicPattern' [] [p]) $ I.PrimOp $ I.SubExp se
    internaliseExp desc body

internaliseExp desc (E.DoLoop mergepat mergeexp form loopbody letbody _) = do
  mergeinit <- internaliseExp "loop_init" mergeexp
  mergeinit_ts <- mapM subExpType mergeinit

  (wrap, form_contents) <- case form of
    E.For dir lbound i ubound -> do
      lbound' <- internaliseExp1 "lower_bound" lbound
      ubound' <- internaliseExp1 "upper_bound" ubound
      num_iterations <- letSubExp "num_iterations" $
                        PrimOp $ I.BinOp (I.Sub Int32) ubound' lbound'
      i' <- internaliseIdent i
      j <- newVName $ baseString i'
      let i_ident = I.Ident i' $ I.Prim I.int32
      i_bnds <- case dir of
        E.FromUpTo ->
          return [mkLet' [] [i_ident] $
                  I.PrimOp $ I.BinOp (I.Add Int32) lbound' (I.Var j)]
        E.FromDownTo -> do
          upper_bound_less_one <-
            letSubExp "upper_bound_less_one" $
            PrimOp $ I.BinOp (I.Sub Int32) ubound' (intconst Int32 1)
          return [mkLet' [] [i_ident] $
                  I.PrimOp $ I.BinOp (I.Sub Int32) upper_bound_less_one (I.Var j)]
      return ( bindingIdentTypes [I.Ident j $ I.Prim I.int32, i_ident] .
               extraBodyBindings i_bnds
             , Left (j, num_iterations))
    E.While cond ->
      return (id, Right cond)

  mergeparams <- map E.toParam <$> flattenPattern mergepat
  (loopbody', (form', shapepat, mergepat', res, mergeinit', pre_bnds)) <-
    wrap $ bindingParams mergeparams $ \shapepat mergepat' ->
    internaliseBodyBindings loopbody $ \ses -> do
      sets <- mapM subExpType ses
      let shapeinit = argShapes
                      (map I.paramName shapepat)
                      (map I.paramType mergepat')
                      mergeinit_ts
          shapeargs = argShapes
                      (map I.paramName shapepat)
                      (map I.paramType mergepat')
                      sets
      case form_contents of
        Left (i', bound) ->
             return (resultBody $ shapeargs ++ ses,
                  (I.ForLoop i' bound,
                   shapepat,
                   mergepat',
                   map I.paramName mergepat',
                   mergeinit,
                   []))
        Right cond -> do
          -- We need to insert 'cond' twice - once for the initial
          -- condition (do we enter the loop at all?), and once with
          -- the result values of the loop (do we continue into the
          -- next iteration?).  This is safe, as the type rules for
          -- the external language guarantees that 'cond' does not
          -- consume anything.
          loop_while <- newParam "loop_while" $ I.Prim Bool
          (loop_cond, loop_cond_bnds) <-
            collectBindings $ internaliseExp1 "loop_cond" cond
          let initsubst = [ (I.paramName mergeparam, initval)
                            | (mergeparam, initval) <-
                               zip (shapepat++mergepat') (shapeinit++mergeinit)
                            ]
              endsubst = [ (I.paramName mergeparam, endval)
                         | (mergeparam, endval) <-
                              zip (shapepat++mergepat') (shapeargs++ses)
                         ]
          (loop_initial_cond, init_loop_cond_bnds) <-
            collectBindings $
            shadowIdentsInExp initsubst loop_cond_bnds loop_cond
          (loop_end_cond, loop_end_cond_bnds) <-
            collectBindings $
            shadowIdentsInExp endsubst loop_cond_bnds loop_cond
          return (mkBody loop_end_cond_bnds $
                  shapeargs++[loop_end_cond]++ses,
                  (I.WhileLoop $ I.paramName loop_while,
                   shapepat,
                   loop_while : mergepat',
                   map I.paramName mergepat',
                   loop_initial_cond : mergeinit,
                   init_loop_cond_bnds))

  mapM_ addBinding pre_bnds

  mergeinit_ts' <- mapM subExpType mergeinit'

  let mergeexp' = argShapes
                  (map I.paramName shapepat)
                  (map I.paramType mergepat')
                  mergeinit_ts' ++
                  mergeinit'
      merge = zip (shapepat ++ mergepat') mergeexp'
      loop = I.LoopOp $ I.DoLoop res merge form' loopbody'
  loopt <- I.expExtType loop
  bindingTupIdent mergepat loopt $ \mergepat'' -> do
    letBind_ mergepat'' loop
    internaliseExp desc letbody

internaliseExp desc (E.LetWith name src idxs ve body loc) = do
  idxs' <- mapM (internaliseExp1 "idx") idxs
  srcs <- internaliseExpToVars "src" $ E.Var src
  ves <- internaliseExp "lw_val" ve
  idxcs' <- boundsChecks loc srcs idxs'
  let comb sname ve' = do
        rowtype <- I.stripArray (length idxs) <$> lookupType sname
        ve'' <- ensureShape asserting loc rowtype "lw_val_correct_shape" ve'
        letInPlace "letwith_dst" idxcs' sname idxs' $
          PrimOp $ SubExp ve''
  dsts <- zipWithM comb srcs ves
  dstt <- I.staticShapes <$> mapM lookupType dsts
  bindingTupIdent (E.Id name) dstt $ \pat' -> do
    forM_ (zip (patternIdents pat') dsts) $ \(p,dst) ->
      letBind (basicPattern' [] [p]) $ I.PrimOp $ I.SubExp $ I.Var dst
    internaliseExp desc body

internaliseExp desc (E.Replicate ne ve _) = do
  ne' <- internaliseExp1 "n" ne
  ves <- internaliseExp "replicate_v" ve
  letSubExps desc [I.PrimOp $ I.Replicate ne' ve' | ve' <- ves ]

internaliseExp desc (E.Size i e _) = do
  ks <- internaliseExp desc e
  case ks of
    (k:_) -> do kt <- I.subExpType k
                return [I.arraySize i kt]
    _     -> return [I.intconst Int32 0] -- Will this ever happen?

internaliseExp desc (E.Unzip e _ _) =
  internaliseExp desc e

internaliseExp _ (E.Zip [] _) =
  return []

internaliseExp _ (E.Zip (e:es) loc) = do
  e' <- internaliseExpToVars "zip_arg" $ fst e
  es_unchecked' <- mapM (internaliseExpToVars "zip_arg" . fst) es
  -- Now we will reshape all of es_unchecked' to have the same outer
  -- size as e'.  We will not change any of the outer dimensions.
  -- This will cause a runtime error if the outer sizes do not match,
  -- thus preserving the semantics of zip().
  e_outer <- arraysSize 0 <$> mapM lookupType e'
  let reshapeToOuter e_unchecked' = do
        unchecked_t <- lookupType e_unchecked'
        case I.arrayDims unchecked_t of
          []      -> return e_unchecked' -- Probably type error
          outer:inner -> do
            cmp <- letSubExp "zip_cmp" $ I.PrimOp $
                   I.CmpOp I.CmpEq e_outer outer
            c   <- assertingOne $
                   letExp "zip_assert" $ I.PrimOp $
                   I.Assert cmp loc
            letExp (postfix e_unchecked' "_zip_res") $
              shapeCoerce c (e_outer:inner) e_unchecked'
  es' <- mapM (mapM reshapeToOuter) es_unchecked'
  return $ concatMap (map I.Var) $ e' : es'

  where
    postfix i s = baseString i ++ s

internaliseExp _ (E.Transpose k n e _) =
  internaliseOperation "transpose" e $ \v -> do
    rank <- I.arrayRank <$> lookupType v
    let perm = I.transposeIndex k n [0..rank-1]
    return $ I.Rearrange [] perm v

internaliseExp _ (E.Rearrange perm e _) =
  internaliseOperation "rearrange" e $ \v ->
    return $ I.Rearrange [] perm v

internaliseExp _ (E.Stripe stride e _) = do
  e' <- internaliseExp1 "stride" stride
  internaliseOperation "stripe" e $ \v ->
    return $ I.Stripe [] e' v

internaliseExp _ (E.Unstripe stride e _) = do
  e' <- internaliseExp1 "stride" stride
  internaliseOperation "unstripe" e $ \v ->
    return $ I.Unstripe [] e' v

internaliseExp _ (E.Reshape shape e loc) = do
  shape' <- mapM (internaliseExp1 "shape") shape
  internaliseOperation "reshape" e $ \v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    dims <- I.arrayDims <$> lookupType v
    shapeOk <- assertingOne $
               letExp "shape_ok" =<<
               eAssert (eCmpOp I.CmpEq (prod dims) (prod shape'))
               loc
    return $ I.Reshape shapeOk (DimNew <$> shape') v
  where prod = foldBinOp (I.Mul Int32) (intconst Int32 1)

internaliseExp _ (E.Split splitexps arrexp loc) = do
  splits' <- mapM (internaliseExp1 "n") splitexps
  -- Note that @arrs@ is an array, because of array-of-tuples transformation
  arrs <- internaliseExpToVars "split_arr" arrexp
  arrayOuterdim <- arraysSize 0 <$> mapM lookupType arrs

  -- Assertions
  indexAsserts <- asserting $ do
    let indexConds = zipWith (\beg end -> PrimOp $ I.CmpOp (I.CmpSle Int32) beg end)
                     (I.intconst Int32 0:splits') (splits'++[arrayOuterdim])
    indexChecks <- mapM (letSubExp "split_index_cnd") indexConds
    forM indexChecks$ \cnd ->
      letExp "split_index_assert" $ PrimOp $ I.Assert cnd loc

  -- Calculate diff between each split index
  let sizeExps = zipWith (\beg end -> PrimOp $ I.BinOp (I.Sub I.Int32) end beg)
                 (I.intconst Int32 0:splits') (splits'++[arrayOuterdim])
  sizeVars <- mapM (letSubExp "split_size") sizeExps
  splitExps <- forM arrs $ \arr -> letTupExp' "split_res" $
                                   PrimOp $ I.Split indexAsserts sizeVars arr

  return $ concat $ transpose splitExps

internaliseExp desc (E.Concat x ys loc) = do
  xs  <- internaliseExpToVars "concat_x" x
  yss <- mapM (internaliseExpToVars "concat_y") ys
  outer_size <- arraysSize 0 <$> mapM lookupType xs
  ressize <- foldM sumdims outer_size =<<
             mapM (liftM (arraysSize 0) . mapM lookupType) yss

  let conc xarr yarrs = do
        -- The inner sizes must match.
        xt  <- lookupType xarr
        yts <- mapM lookupType yarrs
        let matches n m =
              letExp "match" =<<
              eAssert (pure $ I.PrimOp $ I.CmpOp I.CmpEq n m) loc
            x_inner_dims  = drop 1 $ I.arrayDims xt
            ys_inner_dims = map (drop 1 . I.arrayDims) yts
        matchcs <- asserting $
                   concat <$> mapM (zipWithM matches x_inner_dims) ys_inner_dims
        yarrs'  <- forM yarrs $ \yarr -> do
                        yt <- lookupType yarr
                        letExp "concat_y_reshaped" $
                          shapeCoerce matchcs (arraySize 0 yt : x_inner_dims) yarr
        return $ I.PrimOp $ I.Concat [] xarr yarrs' ressize
  letSubExps desc =<< zipWithM conc xs (transpose yss)

    where
        sumdims xsize ysize = letSubExp "conc_tmp" $ I.PrimOp $
                                        I.BinOp (I.Add I.Int32) xsize ysize

internaliseExp desc (E.Map lam arr _) = do
  arrs <- internaliseExpToVars "map_arr" arr
  lam' <- internaliseMapLambda internaliseLambda asserting lam $ map I.Var arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ I.Map [] w lam' arrs

internaliseExp desc (E.Reduce comm lam ne arr loc) = do
  arrs <- internaliseExpToVars "reduce_arr" arr
  nes <- internaliseExp "reduce_ne" ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape asserting loc rowtype "reduce_ne_right_shape" ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda asserting lam nests arrts
  let input = zip nes' arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ I.Reduce [] w comm lam' input

internaliseExp desc (E.Scan lam ne arr loc) = do
  arrs <- internaliseExpToVars "scan_arr" arr
  nes <- internaliseExp "scan_ne" ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape asserting loc rowtype "scan_ne_right_shape" ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda asserting lam nests arrts
  let input = zip nes' arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ I.Scan [] w lam' input

internaliseExp desc (E.Filter lam arr _) = do
  arrs <- internaliseExpToVars "filter_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda [lam] $ map I.Var arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  flags <- letExp "filter_partition_flags" $ I.Op $ I.Map [] w lam' arrs
  filter_size <- newIdent "filter_size" $ I.Prim int32
  filter_perms <- mapM (newIdent "filter_perm" <=< lookupType) arrs
  addBinding $ mkLet' [] (filter_size : filter_perms) $
    I.PrimOp $ I.Partition [] 1 flags arrs
  forM filter_perms $ \filter_perm ->
    letSubExp desc $
      I.PrimOp $ I.Split [] [I.Var $ I.identName filter_size] $
      I.identName filter_perm

internaliseExp desc (E.Partition lams arr _) = do
  arrs <- internaliseExpToVars "partition_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda lams $ map I.Var arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  flags <- letExp "partition_partition_flags" $ I.Op $ I.Map [] w lam' arrs
  liftM (map I.Var . concat . transpose) $ forM arrs $ \arr' -> do
    partition_sizes <- replicateM n $ newIdent "partition_size" $ I.Prim int32
    partition_perm <- newIdent "partition_perm" =<< lookupType arr'
    addBinding $ mkLet' [] (partition_sizes++[partition_perm]) $
      I.PrimOp $ I.Partition [] n flags [arr']
    letTupExp desc $
      I.PrimOp $ I.Split [] (map (I.Var . I.identName) partition_sizes) $
      I.identName partition_perm
  where n = length lams + 1

internaliseExp desc (E.Redomap comm lam1 lam2 ne arrs _) = do
  arrs' <- internaliseExpToVars "redomap_arr" arrs
  nes <- internaliseExp "redomap_ne" ne
  acc_tps <- mapM I.subExpType nes
  outersize <- arraysSize 0 <$> mapM lookupType arrs'
  let acc_arr_tps = [ I.arrayOf t (Shape [outersize]) NoUniqueness
                    | t <- acc_tps ]
  nests <- mapM I.subExpType nes
  lam1' <- internaliseFoldLambda internaliseLambda asserting lam1 nests acc_arr_tps
  lam2' <- internaliseRedomapInnerLambda internaliseLambda asserting lam2
           nes (map I.Var arrs')
  w <- arraysSize 0 <$> mapM lookupType arrs'
  letTupExp' desc $ I.Op $
    I.Redomap [] w comm lam1' lam2' nes arrs'

internaliseExp desc (E.Stream form (AnonymFun (chunk:remparams) body lamrtp pos) arr ii _) = do
  arrs' <- internaliseExpToVars "stream_arr" arr
  accs' <- case form of
             E.MapLike _         -> return []
             E.RedLike _ _ _ acc -> internaliseExp "stream_acc" acc
             E.Sequential  acc   -> internaliseExp "stream_acc" acc
  lam'  <- bindingParams [E.toParam chunk] $ \_ [chunk'] -> do
             rowts <- mapM (liftM (I.stripArray 1) . lookupType) arrs'
             let lam_arrs' = [ I.arrayOf t
                              (Shape [I.Var $ I.paramName chunk'])
                              NoUniqueness
                              | t <- rowts
                             ]
                 lamf = AnonymFun remparams body lamrtp pos
             lam'' <- internaliseStreamLambda internaliseLambda asserting lamf accs' lam_arrs'
             return $ lam'' { extLambdaParams = fmap I.fromDecl chunk' : extLambdaParams lam'' }
  form' <- case form of
             E.MapLike o -> return $ I.MapLike o
             E.RedLike o comm lam0 _ -> do
                 acctps <- mapM I.subExpType accs'
                 outsz  <- arraysSize 0 <$> mapM lookupType arrs'
                 let acc_arr_tps = [ I.arrayOf t (Shape [outsz]) NoUniqueness | t <- acctps ]
                 lam0'  <- internaliseFoldLambda internaliseLambda asserting lam0 acctps acc_arr_tps
                 return $ I.RedLike o comm lam0' accs'
             E.Sequential _ -> return $ I.Sequential accs'
  w <- arraysSize 0 <$> mapM lookupType arrs'
  letTupExp' desc $
    I.Op $ I.Stream [] w form' lam' arrs' ii
internaliseExp _ E.Stream{} =
  fail "In internalise: stream's lambda is NOT an anonymous function with at least one param (chunk)!"

internaliseExp desc (E.ConcatMap lam arr arrs _) = do
  arr' <- internaliseExpToVars "concatMap_arr" arr
  arrs' <- mapM (internaliseExpToVars "concatMap_arr") arrs
  lam' <- internaliseConcatMapLambda internaliseLambda lam
  w <- arraysSize 0 <$> mapM lookupType arr'
  letTupExp' desc $ I.Op $ I.ConcatMap [] w lam' $ arr':arrs'

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp desc (E.Iota e _) = do
  e' <- internaliseExp1 "n" e
  letTupExp' desc $ I.PrimOp $ I.Iota e'

internaliseExp _ (E.Literal v _) =
  case internaliseValue v of
    Nothing -> throwError $ "Invalid value: " ++ pretty v
    Just v' -> mapM (letSubExp "literal" <=< eValue) v'

internaliseExp desc (E.If ce te fe t _) = do
  ce' <- internaliseExp1 "cond" ce
  te' <- internaliseBody te
  fe' <- internaliseBody fe
  let t' = internaliseType t
  letTupExp' desc $ I.If ce' te' fe' t'

internaliseExp desc (E.BinOp bop xe ye _ _) = do
  xe' <- internaliseExp1 "x" xe
  ye' <- internaliseExp1 "y" ye
  case (internaliseType $ E.typeOf xe, internaliseType $ E.typeOf ye) of
    ([I.Prim t1], [I.Prim t2]) ->
      internaliseBinOp desc bop xe' ye' t1 t2
    _            ->
      fail "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."

internaliseExp desc (E.UnOp E.Not e _) = do
  e' <- internaliseExp1 "not_arg" e
  letTupExp' desc $ I.PrimOp $ I.UnOp I.Not e'

internaliseExp desc (E.UnOp E.Complement e _) = do
  e' <- internaliseExp1 "complement_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.PrimOp $ I.UnOp (I.Complement t) e'
             _ ->
               fail "Futhark.Internalise.internaliseExp: non-integer type in Complement"

internaliseExp desc (E.UnOp E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.PrimOp $ I.BinOp (I.Sub t) (I.intconst t 0) e'
             I.Prim (I.FloatType t) ->
               letTupExp' desc $ I.PrimOp $ I.BinOp (I.FSub t) (I.floatconst t 0) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in Negate"

internaliseExp desc (E.UnOp E.Abs e _) = do
  e' <- internaliseExp1 "abs_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.PrimOp $ I.UnOp (I.Abs t) e'
             I.Prim (I.FloatType t) ->
               letTupExp' desc $ I.PrimOp $ I.UnOp (I.FAbs t) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-integer type in Abs"

internaliseExp desc (E.UnOp E.Signum e _) = do
  e' <- internaliseExp1 "signum_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.PrimOp $ I.UnOp (I.Signum t) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-integer type in Signum"

internaliseExp desc (E.UnOp (E.ToFloat float_to) e _) = do
  e' <- internaliseExp1 "tofloat_arg" e
  case E.typeOf e of
    E.Prim (E.IntType int_from) ->
      letTupExp' desc $ I.PrimOp $ I.ConvOp (I.SIToFP int_from float_to) e'
    E.Prim (E.FloatType float_from) ->
      let op = if float_to < float_from then I.FPTrunc else I.FPExt
      in letTupExp' desc $ I.PrimOp $ I.ConvOp (op float_from float_to) e'
    _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToFloat"

internaliseExp desc (E.Copy e _) = do
  ses <- internaliseExpToVars "copy_arg" e
  letSubExps desc [I.PrimOp $ I.Copy se | se <- ses]

internaliseExp1 :: String -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of [se] -> return se
             _ -> fail "Internalise.internaliseExp1: was passed not just a single subexpression"

internaliseExpToVars :: String -> E.Exp -> InternaliseM [I.VName]
internaliseExpToVars desc e =
  mapM asIdent =<< internaliseExp desc e
  where asIdent (I.Var v) = return v
        asIdent se        = letExp desc $ I.PrimOp $ I.SubExp se

internaliseOperation :: String
                     -> E.Exp
                     -> (I.VName -> InternaliseM I.PrimOp)
                     -> InternaliseM [I.SubExp]
internaliseOperation s e op = do
  vs <- internaliseExpToVars s e
  letSubExps s =<< mapM (liftM I.PrimOp . op) vs

internaliseBinOp :: String
                 -> E.BinOp
                 -> I.SubExp -> I.SubExp
                 -> E.PrimType
                 -> E.PrimType
                 -> InternaliseM [I.SubExp]
internaliseBinOp desc E.Plus x y (E.IntType t) _ =
  simpleBinOp desc (I.Add t) x y
internaliseBinOp desc E.Plus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FAdd t) x y
internaliseBinOp desc E.Minus x y (E.IntType t) _ =
  simpleBinOp desc (I.Sub t) x y
internaliseBinOp desc E.Minus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FSub t) x y
internaliseBinOp desc E.Times x y (E.IntType t) _ =
  simpleBinOp desc (I.Mul t) x y
internaliseBinOp desc E.Times x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMul t) x y
internaliseBinOp desc E.Divide x y (E.IntType t) _ =
  simpleBinOp desc (I.SDiv t) x y
internaliseBinOp desc E.Divide x y (E.FloatType t) _ =
  simpleBinOp desc (I.FDiv t) x y
internaliseBinOp desc E.Pow x y (E.FloatType t) _ =
  simpleBinOp desc (I.FPow t) x y
internaliseBinOp desc E.Pow x y (E.IntType t) _ =
  simpleBinOp desc (I.SPow t) x y
internaliseBinOp desc E.Mod x y (E.IntType t) _ =
  simpleBinOp desc (I.SMod t) x y
internaliseBinOp desc E.Quot x y (E.IntType t) _ =
  simpleBinOp desc (I.SQuot t) x y
internaliseBinOp desc E.Rem x y (E.IntType t) _ =
  simpleBinOp desc (I.SRem t) x y
internaliseBinOp desc E.ShiftR x y (E.IntType t) _ =
  simpleBinOp desc (I.AShr t) x y
internaliseBinOp desc E.ShiftL x y (E.IntType t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp desc E.Band x y (E.IntType t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp desc E.Xor x y (E.IntType t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp desc E.Bor x y (E.IntType t) _ =
  simpleBinOp desc (I.Or t) x y
internaliseBinOp desc E.LogAnd x y _ _ =
  simpleBinOp desc I.LogAnd x y
internaliseBinOp desc E.LogOr x y _ _ =
  simpleBinOp desc I.LogOr x y
internaliseBinOp desc E.Equal x y _ _ =
  simpleCmpOp desc I.CmpEq x y
internaliseBinOp desc E.Less x y (E.IntType t) _ =
  simpleCmpOp desc (I.CmpSlt t) x y
internaliseBinOp desc E.Leq x y (E.IntType t) _ =
  simpleCmpOp desc (I.CmpSle t) x y
internaliseBinOp desc E.Greater x y (E.IntType t) _ =
  simpleCmpOp desc (I.CmpSlt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.IntType t) _ =
  simpleCmpOp desc (I.CmpSle t) y x -- Note the swapped x and y
internaliseBinOp desc E.Less x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLt t) x y
internaliseBinOp desc E.Leq x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLe t) x y
internaliseBinOp desc E.Greater x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLe t) y x -- Note the swapped x and y
internaliseBinOp _ op _ _ t1 t2 =
  fail $ "Invalid binary operator " ++ pretty op ++
  " with operand types " ++ pretty t1 ++ ", " ++ pretty t2

simpleBinOp :: String
            -> I.BinOp
            -> I.SubExp -> I.SubExp
            -> InternaliseM [I.SubExp]
simpleBinOp desc bop x y =
  letTupExp' desc $ I.PrimOp $ I.BinOp bop x y

simpleCmpOp :: String
            -> I.CmpOp
            -> I.SubExp -> I.SubExp
            -> InternaliseM [I.SubExp]
simpleCmpOp desc op x y =
  letTupExp' desc $ I.PrimOp $ I.CmpOp op x y


internaliseLambda :: InternaliseLambda

internaliseLambda (E.AnonymFun params body rettype _) (Just rowtypes) = do
  (body', params') <- bindingLambdaParams params rowtypes $
                      internaliseBody body
  (rettype', _) <- internaliseReturnType rettype
  return (params', body', map I.fromDecl rettype')

internaliseLambda (E.AnonymFun params body rettype _) Nothing = do
  (body', params', rettype') <- bindingParams params $ \shapeparams valparams -> do
    body' <- internaliseBody body
    (rettype', _) <- internaliseReturnType rettype
    return (body', shapeparams ++ valparams, rettype')
  return (map (fmap I.fromDecl) params', body', map I.fromDecl rettype')

internaliseLambda (E.CurryFun fname curargs _ _) (Just rowtypes) = do
  fun_entry <- lookupFunction fname
  let (shapes, paramts, int_rettype_fun) = internalFun fun_entry
  curargs' <- concat <$> mapM (internaliseExp "curried") curargs
  curarg_types <- mapM subExpType curargs'
  params <- mapM (newParam "not_curried") rowtypes
  let valargs = curargs' ++ map (I.Var . I.paramName) params
      valargs_types = curarg_types ++ rowtypes
      diets = map I.diet paramts
      shapeargs = argShapes shapes paramts valargs_types
      allargs = zip shapeargs (repeat I.Observe) ++
                zip valargs diets
  case int_rettype_fun $
       map (,I.Prim int32) shapeargs ++ zip valargs valargs_types of
    Nothing ->
      fail $ "Cannot apply " ++ pretty fname ++ " to arguments " ++
      pretty (shapeargs ++ valargs)
    Just (ExtRetType ts) -> do
      funbody <- insertBindingsM $ do
        res <- letTupExp "curried_fun_result" $
               I.Apply fname allargs $ ExtRetType ts
        resultBodyM $ map I.Var res
      return (params, funbody, map I.fromDecl ts)

internaliseLambda (E.CurryFun fname curargs _ _) Nothing = do
  fun_entry <- lookupFunction fname
  let (shapes, paramts, int_rettype_fun) = internalFun fun_entry
      (_, ext_param_ts)                  = externalFun fun_entry
  curargs' <- concat <$> mapM (internaliseExp "curried") curargs
  curarg_types <- mapM subExpType curargs'
  ext_params <- forM ext_param_ts $ \param_t -> do
    name <- newVName "not_curried"
    return E.Ident { E.identName = name
                   , E.identType = param_t
                   , E.identSrcLoc = noLoc
                   }
  bindingParams ext_params $ \shape_params value_params -> do
    let params = map (fmap I.fromDecl) $ shape_params ++ value_params
        valargs = curargs' ++ map (I.Var . I.paramName) value_params
        valargs_types = curarg_types ++ map I.paramType value_params
        diets = map I.diet paramts
        shapeargs = argShapes shapes paramts valargs_types
        allargs = zip shapeargs (repeat I.Observe) ++
                  zip valargs diets
    case int_rettype_fun $
         map (,I.Prim int32) shapeargs ++ zip valargs valargs_types of
      Nothing ->
        fail $ "Cannot apply " ++ pretty fname ++ " to arguments " ++
        pretty (shapeargs ++ valargs)
      Just (ExtRetType ts) -> do
        funbody <- insertBindingsM $ do
          res <- letTupExp "curried_fun_result" $
                 I.Apply fname allargs $ ExtRetType ts
          resultBodyM $ map I.Var res
        return (params, funbody, map I.fromDecl ts)

internaliseLambda (E.UnOpFun unop paramtype rettype loc) rowts = do
  (params, body, rettype') <- unOpFunToLambda unop paramtype rettype
  internaliseLambda (E.AnonymFun params body rettype' loc) rowts

internaliseLambda (E.BinOpFun unop xtype ytype rettype loc) rowts = do
  (params, body, rettype') <- binOpFunToLambda unop xtype ytype rettype
  internaliseLambda (AnonymFun params body rettype' loc) rowts

internaliseLambda (E.CurryBinOpLeft binop e paramtype rettype loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e $ uncurry $ flip (,)
  internaliseLambda (AnonymFun params body rettype' loc) rowts

internaliseLambda (E.CurryBinOpRight binop e paramtype rettype loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e id
  internaliseLambda (AnonymFun params body rettype' loc) rowts

unOpFunToLambda :: E.UnOp -> E.Type -> E.Type
                -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
unOpFunToLambda op paramtype rettype = do
  paramname <- newNameFromString "unop_param"
  let param = E.Ident { E.identType = paramtype
                      , E.identSrcLoc = noLoc
                      , E.identName = paramname
                      }
  return ([toParam param],
          E.UnOp op (E.Var param) noLoc,
          E.vacuousShapeAnnotations $ E.toDecl rettype)

binOpFunToLambda :: E.BinOp -> E.Type -> E.Type -> E.Type
                 -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
binOpFunToLambda op xtype ytype rettype = do
  x_name <- newNameFromString "binop_param_x"
  y_name <- newNameFromString "binop_param_y"
  let param_x = E.Ident { E.identType = xtype
                        , E.identSrcLoc = noLoc
                        , E.identName = x_name
                        }
      param_y = E.Ident { E.identType = ytype
                        , E.identSrcLoc = noLoc
                        , E.identName = y_name
                        }
  return ([toParam param_x, toParam param_y],
          E.BinOp op (E.Var param_x) (E.Var param_y) rettype noLoc,
          E.vacuousShapeAnnotations $ E.toDecl rettype)

binOpCurriedToLambda :: E.BinOp -> E.Type -> E.Type
                     -> E.Exp
                     -> ((E.Exp,E.Exp) -> (E.Exp,E.Exp))
                     -> InternaliseM ([E.Parameter], E.Exp, E.DeclType)
binOpCurriedToLambda op paramtype rettype e swap = do
  paramname <- newNameFromString "binop_param_noncurried"
  let param = E.Ident { E.identType = paramtype
                      , E.identSrcLoc = noLoc
                      , E.identName = paramname
                        }
      (x', y') = swap (E.Var param, e)
  return ([toParam param],
          E.BinOp op x' y' rettype noLoc,
          E.vacuousShapeAnnotations $ E.toDecl rettype)

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
asserting :: InternaliseM I.Certificates
          -> InternaliseM I.Certificates
asserting m = do
  doBoundsChecks <- asks envDoBoundsChecks
  if doBoundsChecks
  then m
  else return []

-- | Execute the given action if 'envDoBoundsChecks' is true, otherwise
-- just return an empty list.
assertingOne :: InternaliseM VName
             -> InternaliseM I.Certificates
assertingOne m = asserting $ liftM pure m

boundsChecks :: SrcLoc -> [VName] -> [I.SubExp] -> InternaliseM I.Certificates
boundsChecks _ []    _  = return []
boundsChecks loc (v:_) es =
  asserting $ zipWithM (boundsCheck loc v) [0..] es

boundsCheck :: SrcLoc -> VName -> Int -> I.SubExp -> InternaliseM I.VName
boundsCheck loc v i e = do
  size <- arraySize i <$> lookupType v
  let check = eBinOp I.LogAnd (pure lowerBound) (pure upperBound)
      lowerBound = I.PrimOp $
                   I.CmpOp (I.CmpSle I.Int32) (I.intconst Int32 0) e
      upperBound = I.PrimOp $
                   I.CmpOp (I.CmpSlt I.Int32) e size
  letExp "bounds_check" =<< eAssert check loc

shadowIdentsInExp :: [(VName, I.SubExp)] -> [Binding] -> I.SubExp
                  -> InternaliseM I.SubExp
shadowIdentsInExp substs bnds res = do
  body <- renameBody <=< insertBindingsM $ do
    -- XXX: we have to substitute names to fix type annotations in the
    -- bindings.  This goes away once we get rid of these type
    -- annotations.
    let handleSubst nameSubsts (name, I.Var v)
          | v == name =
            return nameSubsts
          | otherwise =
            return $ HM.insert name v nameSubsts
        handleSubst nameSubsts (name, se) = do
          letBindNames'_ [name] $ PrimOp $ SubExp se
          return nameSubsts
    nameSubsts <- foldM handleSubst HM.empty substs
    mapM_ addBinding $ substituteNames nameSubsts bnds
    return $ resultBody [substituteNames nameSubsts res]
  res' <- bodyBind body
  case res' of
    [se] -> return se
    _    -> fail "Internalise.shadowIdentsInExp: something went very wrong"
