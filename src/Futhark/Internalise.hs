{-# LANGUAGE TupleSections #-}
-- |
--
-- This module implements a transformation from source to core
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

import Language.Futhark as E
import Futhark.Representation.SOACS as I hiding (bindingPattern)
import Futhark.Transform.Rename as I
import Futhark.Transform.Substitute
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings
import Futhark.Internalise.Lambdas
import Futhark.Util (dropAt)

-- | Convert a program in source Futhark to a program in the Futhark
-- core language.
internaliseProg :: MonadFreshNames m =>
                   E.Prog -> m (Either String I.Prog)
internaliseProg prog = do
  res <- do
    ftable_attempt <- buildFtable prog
    case ftable_attempt of
      Left err -> return $ Left err
      Right ftable -> do
        funs <- runInternaliseM ftable $
                mapM internaliseFun $ funsFromProg prog
        return $ fmap I.Prog funs
  sequence $ fmap I.renameProg res

-- | All functions, including those nested in structures.  Treats
-- constants as 0-ary functions.
funsFromProg :: ProgBase f vn -> [FunDefBase f vn]
funsFromProg prog = concatMap getFuns $ progDecs prog
  where getFuns (FunOrTypeDec (FunDec a)) = [a]
        getFuns (FunOrTypeDec (ConstDec (E.ConstDef name t e loc))) =
          [E.FunDef False name t [] e loc]
        getFuns (FunOrTypeDec TypeDec{}) = []
        getFuns (ModDec d) = concatMap getFuns $ modDecls d
        getFuns SigDec{} = []

buildFtable :: MonadFreshNames m => E.Prog
            -> m (Either String FunTable)
buildFtable = fmap (HM.union builtinFtable<$>) .
              runInternaliseM mempty .
              fmap HM.fromList . mapM inspect . funsFromProg

  where inspect (E.FunDef entry fname (TypeDecl _ (Info rettype)) params _ _) =
          bindingParams params $ \shapes values -> do
            (rettype', _, cm) <- internaliseReturnType rettype
            let shapenames = map I.paramName shapes
                fname' | entry     = nameFromString $ pretty $ baseName fname
                       | otherwise = nameFromString $ pretty fname ++ "f"
                consts = map ((`Param` I.Prim int32) . snd) cm
            return (fname,
                    FunBinding { internalFun = (fname',
                                                cm,
                                                shapenames,
                                                map declTypeOf values,
                                                applyRetType
                                                (ExtRetType rettype')
                                                (consts++shapes++values)
                                               )
                               , externalFun = (rettype,
                                                map E.patternStructType params)
                               })

        builtinFtable = HM.fromList $ map addBuiltin $ HM.toList E.builtInFunctions
        addBuiltin (name, (t, paramts)) =
          (name,
           FunBinding
           (baseName name,
            [], [], map (I.Prim . internalisePrimType) paramts,
            const $ Just $ ExtRetType [I.Prim $ internalisePrimType t])
           (E.Prim t, map E.Prim paramts))

internaliseFun :: E.FunDef -> InternaliseM I.FunDef
internaliseFun (E.FunDef entry fname (TypeDecl _ (Info rettype)) params body loc) =
  bindingParams params $ \shapeparams params' -> do
    (rettype', _, cm) <- internaliseReturnType rettype
    firstbody <- internaliseBody body
    body' <- ensureResultExtShape asserting loc
             (map I.fromDecl rettype') firstbody
    let mkConstParam name = Param name $ I.Prim int32
        constparams = map (mkConstParam . snd) cm
    return $ I.FunDef entry fname'
      (ExtRetType rettype') (constparams ++ shapeparams ++ params') body'
      where fname' | entry     = nameFromString $ pretty $ baseName fname
                   | otherwise = nameFromString $ pretty fname ++ "f"

internaliseIdent :: E.Ident -> InternaliseM I.VName
internaliseIdent (E.Ident name (Info tp) _) =
  case internaliseType tp of
    [I.Prim _] -> return name
    _          -> fail $ "Futhark.Internalise.internaliseIdent: asked to internalise non-prim-typed ident '"
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

internaliseExp _ (E.Var (QualName (_, name)) t loc) = do
  -- If this identifier is the name of a constant, we have to turn it
  -- into a call to the corresponding function.
  is_const <- lookupConstant name
  case is_const of
    Just ses ->
      return ses
    _ -> do
      subst <- asks $ HM.lookup name . envSubsts
      case subst of
        Nothing     -> (:[]) . I.Var <$> internaliseIdent (E.Ident name t loc)
        Just substs -> return substs

internaliseExp desc (E.Index e idxs loc) = do
  vs <- internaliseExpToVars "indexed" e
  dims <- case vs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', idx_cs) <- unzip <$> zipWithM (internaliseDimIndex loc) dims idxs
  let index v = do
        v_t <- lookupType v
        return $ I.BasicOp $ I.Index (concat idx_cs) v $ fullSlice v_t idxs'
  letSubExps desc =<< mapM index vs

internaliseExp desc (E.TupleIndex e i (Info rt) _) =
  take n . drop i' <$> internaliseExp desc e
  where n = length $ internaliseType rt
        i' = sum $ map (length . internaliseType) $ take i $
             case E.typeOf e of
               Tuple ts -> ts
               t        -> [t]

internaliseExp desc (E.TupLit es _) =
  concat <$> mapM (internaliseExp desc) es

internaliseExp desc (E.ArrayLit [] (Info et) _) =
  letSubExps desc $ map arrayLit $ internaliseType et
  where arrayLit et' =
          I.BasicOp $ I.ArrayLit [] $ et' `annotateArrayShape` []

internaliseExp desc (E.ArrayLit es (Info rowtype) loc) = do
  es' <- mapM (internaliseExp "arr_elem") es
  case es' of
    [] -> do
      let rowtypes = map zeroDim $ internaliseType rowtype
          zeroDim t = t `I.setArrayShape`
                      I.Shape (replicate (I.arrayRank t) (constant (0::Int32)))
          arraylit rt = I.BasicOp $ I.ArrayLit [] rt
      letSubExps desc $ map arraylit rowtypes
    e' : _ -> do
      rowtypes <- mapM subExpType e'
      let arraylit ks rt = do
            ks' <- mapM (ensureShape asserting loc rt "elem_reshaped") ks
            return $ I.BasicOp $ I.ArrayLit ks' rt
      letSubExps desc =<< zipWithM arraylit (transpose es') rowtypes

internaliseExp desc (E.Empty (TypeDecl _(Info et)) loc) =
  internaliseExp desc $ E.ArrayLit [] (Info et') loc
  where et' = E.removeShapeAnnotations $ E.fromStruct et

internaliseExp desc (E.Apply (QualName ([], fname)) args _ _)
  | Just (rettype, _) <- HM.lookup fname' I.builtInFunctions = do
  args' <- mapM (internaliseExp "arg" . fst) args
  let args'' = concatMap tag args'
  letTupExp' desc $ I.Apply fname' args'' (ExtRetType [I.Prim rettype])
  where tag ses = [ (se, I.Observe) | se <- ses ]
        fname' = nameFromString $ pretty $ baseName fname

internaliseExp desc (E.Apply (QualName (_, fname)) args _ loc) = do
  args' <- concat <$> mapM (internaliseExp "arg" . fst) args
  (fname', constparams, shapes, value_paramts, rettype_fun) <- internalFun <$> lookupFunction fname
  (constargs, const_ds, const_ts) <- unzip3 <$> constFunctionArgs constparams
  argts <- mapM subExpType args'
  let shapeargs = argShapes shapes value_paramts argts
      diets = const_ds ++ replicate (length shapeargs) I.Observe ++ map I.diet value_paramts
      paramts = const_ts ++ map (const $ I.Prim int32) shapeargs ++ value_paramts
  args'' <- ensureArgShapes asserting loc shapes paramts $ constargs ++ shapeargs ++ args'
  argts' <- mapM subExpType args''
  case rettype_fun $ zip args'' argts' of
    Nothing -> fail $ "Cannot apply " ++ pretty fname ++ " to arguments\n " ++
               pretty args'' ++ "\nof types\n " ++
               pretty argts'
    Just rettype -> letTupExp' desc $ I.Apply fname' (zip args'' diets) rettype

internaliseExp desc (E.LetPat pat e body _) = do
  ses <- internaliseExp desc e
  t <- I.staticShapes <$> mapM I.subExpType ses
  bindingPattern pat t $ \pat' -> do
    forM_ (zip (patternIdents pat') ses) $ \(p,se) ->
      letBind (basicPattern' [] [p]) $ I.BasicOp $ I.SubExp se
    internaliseExp desc body

internaliseExp desc (E.DoLoop mergepat mergeexp form loopbody letbody _) = do
  mergeinit <- internaliseExp "loop_init" mergeexp
  mergeinit_ts <- mapM subExpType mergeinit

  (wrap, form_contents) <- case form of
    E.For dir lbound i ubound -> do
      lbound' <- internaliseExp1 "lower_bound" lbound
      ubound' <- internaliseExp1 "upper_bound" ubound
      num_iterations <- letSubExp "num_iterations" $
                        BasicOp $ I.BinOp (I.Sub I.Int32) ubound' lbound'
      i' <- internaliseIdent i
      j <- newVName $ baseString i'
      let i_ident = I.Ident i' $ I.Prim I.int32
      i_bnds <- case dir of
        E.FromUpTo ->
          return [mkLet' [] [i_ident] $
                  I.BasicOp $ I.BinOp (I.Add I.Int32) lbound' (I.Var j)]
        E.FromDownTo -> do
          upper_bound_less_one <-
            letSubExp "upper_bound_less_one" $
            BasicOp $ I.BinOp (I.Sub I.Int32) ubound' (constant (1 :: I.Int32))
          return [mkLet' [] [i_ident] $
                  I.BasicOp $ I.BinOp (I.Sub I.Int32) upper_bound_less_one (I.Var j)]
      return ( bindingIdentTypes [I.Ident j $ I.Prim I.int32, i_ident] .
               extraBodyBindings i_bnds
             , Left (j, num_iterations))
    E.While cond ->
      return (id, Right cond)

  (loopbody', (form', shapepat, mergepat', frob, mergeinit', pre_bnds)) <-
    wrap $ bindingParams [mergepat] $ \shapepat mergepat' ->
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
                      id,
                      mergeinit,
                      []))
        Right cond -> do
          -- We need to insert 'cond' twice - once for the initial
          -- condition (do we enter the loop at all?), and once with
          -- the result values of the loop (do we continue into the
          -- next iteration?).  This is safe, as the type rules for
          -- the external language guarantees that 'cond' does not
          -- consume anything.
          loop_while <- newParam "loop_while" $ I.Prim I.Bool
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
                   addAnother,
                   loop_initial_cond : mergeinit,
                   init_loop_cond_bnds))

  mapM_ addBinding pre_bnds

  mergeinit_ts' <- mapM subExpType mergeinit'

  let ctxinit = argShapes
                (map I.paramName shapepat)
                (map I.paramType mergepat')
                mergeinit_ts'
      ctxmerge = zip shapepat ctxinit
      valmerge = zip mergepat' mergeinit'
      loop = I.DoLoop ctxmerge valmerge form' loopbody'
  loopt <- I.expExtType loop
  bindingPattern (frob mergepat) loopt $ \mergepat'' -> do
    letBind_ mergepat'' loop
    internaliseExp desc letbody

  where addAnother t =
          TuplePattern [E.Wildcard (Info $ E.Prim $ E.Signed E.Int32) (srclocOf t), t] noLoc

internaliseExp desc (E.LetWith name src idxs ve body loc) = do
  srcs <- internaliseExpToVars "src" $
          E.Var (QualName ([], E.identName src)) (E.identType src) (srclocOf src)
  ves <- internaliseExp "lw_val" ve
  dims <- case srcs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', idx_cs) <- unzip <$> zipWithM (internaliseDimIndex loc) dims idxs
  let comb sname ve' = do
        sname_t <- lookupType sname
        let slice = fullSlice sname_t idxs'
            rowtype = sname_t `setArrayDims` sliceDims slice
        ve'' <- ensureShape asserting loc rowtype "lw_val_correct_shape" ve'
        letInPlace "letwith_dst" (concat idx_cs) sname (fullSlice sname_t idxs') $
          BasicOp $ SubExp ve''
  dsts <- zipWithM comb srcs ves
  dstt <- I.staticShapes <$> mapM lookupType dsts
  bindingPattern (E.Id name) dstt $ \pat' -> do
    forM_ (zip (patternIdents pat') dsts) $ \(p,dst) ->
      letBind (basicPattern' [] [p]) $ I.BasicOp $ I.SubExp $ I.Var dst
    internaliseExp desc body

internaliseExp desc (E.Replicate ne ve _) = do
  ne' <- internaliseExp1 "n" ne
  ves <- internaliseExp "replicate_v" ve
  letSubExps desc $ I.BasicOp . I.Replicate (I.Shape [ne']) <$> ves

internaliseExp desc (E.Shape e _) = do
  ks <- internaliseExp (desc<>"_shape") e
  case ks of
    (k:_) -> do kt <- I.subExpType k
                letSubExps desc [I.BasicOp $ I.ArrayLit (I.arrayDims kt) $ I.Prim int32]
    _     -> return [I.constant (0 :: I.Int32)] -- Will this ever happen?

internaliseExp desc (E.Unzip e _ _) =
  internaliseExp desc e

internaliseExp desc (E.Unsafe e _) =
  local (\env -> env { envDoBoundsChecks = False }) $
  internaliseExp desc e

internaliseExp _ (E.Zip _ e es loc) = do
  e' <- internaliseExpToVars "zip_arg" $ TupLit (e:es) loc
  case e' of
    e_key:es_unchecked -> do
      -- We will reshape all of es_unchecked' to have the same outer
      -- size as ts.  We will not change any of the inner dimensions.
      -- This will cause a runtime error if the outer sizes do not match,
      -- thus preserving the semantics of zip().
      w <- arraySize 0 <$> lookupType e_key
      let reshapeToOuter e_unchecked' = do
            unchecked_t <- lookupType e_unchecked'
            case I.arrayDims unchecked_t of
              []      -> return e_unchecked' -- Probably type error
              outer:inner -> do
                cmp <- letSubExp "zip_cmp" $ I.BasicOp $
                       I.CmpOp (I.CmpEq I.int32) w outer
                c   <- assertingOne $
                       letExp "zip_assert" $ I.BasicOp $
                       I.Assert cmp loc
                letExp (postfix e_unchecked' "_zip_res") $
                  shapeCoerce c (w:inner) e_unchecked'
      es' <- mapM reshapeToOuter es_unchecked
      return $ map I.Var $ e_key : es'
    [] -> return []

  where postfix i s = baseString i ++ s

internaliseExp _ (E.Transpose e _) =
  internaliseOperation "transpose" e $ \v ->
    return $ I.Rearrange [] [1,0] v

internaliseExp _ (E.Rearrange perm e _) =
  internaliseOperation "rearrange" e $ \v ->
    return $ I.Rearrange [] perm v

internaliseExp _ (E.Rotate d offset e _) = do
  offset' <- internaliseExp1 "rotation_offset" offset
  internaliseOperation "rotate" e $ \v -> do
    rank <- I.arrayRank <$> lookupType v
    let zero = constant (0::Int32)
        offsets = replicate d zero ++ [offset'] ++ replicate (rank-d-1) zero
    return $ I.Rotate [] offsets v

internaliseExp _ (E.Reshape shape e loc) = do
  shape' <- internaliseExp "shape" shape
  internaliseOperation "reshape" e $ \v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    dims <- I.arrayDims <$> lookupType v
    shapeOk <- assertingOne $
               letExp "shape_ok" =<<
               eAssert (eCmpOp (I.CmpEq I.int32) (prod dims) (prod shape'))
               loc
    return $ I.Reshape shapeOk (DimNew <$> shape') v
  where prod = foldBinOp (I.Mul I.Int32) (constant (1 :: I.Int32))

internaliseExp _ (E.Split i splitexp arrexp loc) = do
  splits' <- internaliseExp "n" splitexp
  -- Note that @arrs@ is an array, because of array-of-tuples transformation
  arrs <- internaliseExpToVars "split_arr" arrexp
  split_dim <- arraysSize i <$> mapM lookupType arrs

  -- Assertions
  indexAsserts <- asserting $ do
    let indexConds = zipWith (\beg end -> BasicOp $ I.CmpOp (I.CmpSle I.Int32) beg end)
                     (I.constant (0 :: I.Int32):splits') (splits'++[split_dim])
    indexChecks <- mapM (letSubExp "split_index_cnd") indexConds
    forM indexChecks$ \cnd ->
      letExp "split_index_assert" $ BasicOp $ I.Assert cnd loc

  -- Calculate diff between each split index
  let sizeExps = zipWith (\beg end -> BasicOp $ I.BinOp (I.Sub I.Int32) end beg)
                 (I.constant (0 :: I.Int32):splits') (splits'++[split_dim])
  sizeVars <- mapM (letSubExp "split_size") sizeExps
  splitExps <- forM arrs $ \arr -> letTupExp' "split_res" $
                                   BasicOp $ I.Split indexAsserts i sizeVars arr

  return $ concat $ transpose splitExps

internaliseExp desc (E.Concat i x ys loc) = do
  xs  <- internaliseExpToVars "concat_x" x
  yss <- mapM (internaliseExpToVars "concat_y") ys
  outer_size <- arraysSize i <$> mapM lookupType xs
  ressize <- foldM sumdims outer_size =<<
             mapM (fmap (arraysSize i) . mapM lookupType) yss

  let conc xarr yarrs = do
        -- All dimensions except for dimension 'i' must match.
        xt  <- lookupType xarr
        yts <- mapM lookupType yarrs
        let matches n m =
              letExp "match" =<<
              eAssert (pure $ I.BasicOp $ I.CmpOp (I.CmpEq I.int32) n m) loc
            x_inner_dims  = dropAt i 1 $ I.arrayDims xt
            ys_inner_dims = map (dropAt i 1 . I.arrayDims) yts
            updims = zipWith3 updims' [0..] (I.arrayDims xt)
            updims' j xd yd | i == j    = yd
                            | otherwise = xd
        matchcs <- asserting $
                   concat <$> mapM (zipWithM matches x_inner_dims) ys_inner_dims
        yarrs'  <- forM yarrs $ \yarr -> do
          yt <- lookupType yarr
          letExp "concat_y_reshaped" $
            shapeCoerce matchcs (updims $ I.arrayDims yt) yarr
        return $ I.BasicOp $ I.Concat [] i xarr yarrs' ressize
  letSubExps desc =<< zipWithM conc xs (transpose yss)

    where
        sumdims xsize ysize = letSubExp "conc_tmp" $ I.BasicOp $
                                        I.BinOp (I.Add I.Int32) xsize ysize

internaliseExp _ (E.Map _ [] _) = return []

internaliseExp desc (E.Map lam (arr:arrs) loc) = do
  -- Pretend the arrs were zipped to get the necessary reshapes in.
  -- This would be a type error in the source language, but it's the
  -- same in the core language.
  arrs' <- internaliseExpToVars "map_arr" (Zip 0 arr arrs loc)
  lam' <- internaliseMapLambda internaliseLambda asserting lam $ map I.Var arrs'
  w <- arraysSize 0 <$> mapM lookupType arrs'
  letTupExp' desc $ I.Op $ I.Map [] w lam' arrs'

internaliseExp desc (E.Reduce comm lam ne arr loc) =
  internaliseScanOrReduce desc "reduce"
    (\cs w -> I.Reduce cs w comm) (lam, ne, arr, loc)

internaliseExp desc (E.Scan lam ne arr loc) =
  internaliseScanOrReduce desc "scan" I.Scan (lam, ne, arr, loc)

internaliseExp desc (E.Filter lam arr _) = do
  arrs <- internaliseExpToVars "filter_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda [lam] $ map I.Var arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  flags <- letExp "filter_partition_flags" $ I.Op $ I.Map [] w lam' arrs
  filter_size <- newIdent "filter_size" $ I.Prim int32
  filter_perms <- mapM (newIdent "filter_perm" <=< lookupType) arrs
  addBinding $ mkLet' [] (filter_size : filter_perms) $
    I.BasicOp $ I.Partition [] 1 flags arrs
  forM filter_perms $ \filter_perm ->
    letSubExp desc $
      I.BasicOp $ I.Split [] 0 [I.Var $ I.identName filter_size] $
      I.identName filter_perm

internaliseExp desc (E.Partition lams arr _) = do
  arrs <- internaliseExpToVars "partition_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda lams $ map I.Var arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  flags <- letExp "partition_partition_flags" $ I.Op $ I.Map [] w lam' arrs
  fmap (map I.Var . concat . transpose) $ forM arrs $ \arr' -> do
    partition_sizes <- replicateM n $ newIdent "partition_size" $ I.Prim int32
    partition_perm <- newIdent "partition_perm" =<< lookupType arr'
    addBinding $ mkLet' [] (partition_sizes++[partition_perm]) $
      I.BasicOp $ I.Partition [] n flags [arr']
    letTupExp desc $
      I.BasicOp $ I.Split [] 0 (map (I.Var . I.identName) partition_sizes) $
      I.identName partition_perm
  where n = length lams + 1

internaliseExp desc (E.Stream form (AnonymFun (chunk:remparams) body maybe_ret lamrtp pos) arr _) = do
  arrs' <- internaliseExpToVars "stream_arr" arr
  accs' <- case form of
             E.MapLike _         -> return []
             E.RedLike _ _ _ acc -> internaliseExp "stream_acc" acc
             E.Sequential  acc   -> internaliseExp "stream_acc" acc
  lam'  <- bindingParams [chunk] $ \_ [chunk'] -> do
             rowts <- mapM (fmap (I.stripArray 1) . lookupType) arrs'
             let lam_arrs' = [ I.arrayOf t
                              (I.Shape [I.Var $ I.paramName chunk'])
                              NoUniqueness
                              | t <- rowts
                             ]
                 lamf = AnonymFun remparams body maybe_ret lamrtp pos
             lam'' <- internaliseStreamLambda internaliseLambda asserting lamf accs' lam_arrs'
             return $ lam'' { extLambdaParams = fmap I.fromDecl chunk' : extLambdaParams lam'' }
  form' <- case form of
             E.MapLike o -> return $ I.MapLike o
             E.RedLike o comm lam0 _ -> do
                 acctps <- mapM I.subExpType accs'
                 outsz  <- arraysSize 0 <$> mapM lookupType arrs'
                 let acc_arr_tps = [ I.arrayOf t (I.Shape [outsz]) NoUniqueness | t <- acctps ]
                 lam0'  <- internaliseFoldLambda internaliseLambda asserting lam0 acctps acc_arr_tps
                 return $ I.RedLike o comm lam0' accs'
             E.Sequential _ -> return $ I.Sequential accs'
  w <- arraysSize 0 <$> mapM lookupType arrs'
  letTupExp' desc $
    I.Op $ I.Stream [] w form' lam' arrs'
internaliseExp _ E.Stream{} =
  fail "In internalise: stream's lambda is NOT an anonymous function with at least one param (chunk)!"

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp desc (E.Iota e _) = do
  e' <- internaliseExp1 "n" e
  letTupExp' desc $ I.BasicOp $
    I.Iota e' (constant (0::Int32)) (constant (1::Int32))

internaliseExp _ (E.Literal v _) =
  case internaliseValue v of
    Nothing -> throwError $ "Invalid value: " ++ pretty v
    Just v' -> mapM (letSubExp "literal" <=< eValue) v'

internaliseExp desc (E.If ce te fe (Info t) _) = do
  ce' <- internaliseExp1 "cond" ce
  te' <- internaliseBody te
  fe' <- internaliseBody fe
  let t' = internaliseType t
  letTupExp' desc $ I.If ce' te' fe' t'

internaliseExp desc (E.BinOp E.LogAnd xe ye t loc) =
  internaliseExp desc $
  E.If xe ye (E.Literal (E.PrimValue (E.BoolValue False)) loc) t loc

internaliseExp desc (E.BinOp E.LogOr xe ye t loc) =
  internaliseExp desc $
  E.If xe (E.Literal (E.PrimValue (E.BoolValue True)) loc) ye t loc

internaliseExp desc (E.BinOp bop xe ye _ _) = do
  xe' <- internaliseExp1 "x" xe
  ye' <- internaliseExp1 "y" ye
  case (E.typeOf xe, E.typeOf ye) of
    (E.Prim t1, E.Prim t2) ->
      internaliseBinOp desc bop xe' ye' t1 t2
    _            ->
      fail "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."

internaliseExp desc (E.UnOp E.Not e _) = do
  e' <- internaliseExp1 "not_arg" e
  letTupExp' desc $ I.BasicOp $ I.UnOp I.Not e'

internaliseExp desc (E.UnOp E.Complement e _) = do
  e' <- internaliseExp1 "complement_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.BasicOp $ I.UnOp (I.Complement t) e'
             _ ->
               fail "Futhark.Internalise.internaliseExp: non-integer type in Complement"

internaliseExp desc (E.UnOp E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.Sub t) (I.intConst t 0) e'
             I.Prim (I.FloatType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.FSub t) (I.floatConst t 0) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in Negate"

internaliseExp desc (E.UnOp E.Abs e _) = do
  e' <- internaliseExp1 "abs_arg" e
  case E.typeOf e of
    E.Prim (E.Signed t) ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.Abs t) e'
    E.Prim (E.Unsigned _) ->
      return [e']
    E.Prim (E.FloatType t) ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.FAbs t) e'
    _ -> fail "Futhark.Internalise.internaliseExp: non-integer type in Abs"

internaliseExp desc (E.UnOp E.Signum e _) = do
  e' <- internaliseExp1 "signum_arg" e
  case E.typeOf e of
    E.Prim (E.Signed t) ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.SSignum t) e'
    E.Prim (E.Unsigned t) ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.USignum t) e'
    _ -> fail "Futhark.Internalise.internaliseExp: non-integer type in Signum"

internaliseExp desc (E.UnOp (E.ToFloat float_to) e _) = do
  e' <- internaliseExp1 "tofloat_arg" e
  case E.typeOf e of
    E.Prim (E.Signed int_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SIToFP int_from float_to) e'
    E.Prim (E.Unsigned int_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.UIToFP int_from float_to) e'
    E.Prim (E.FloatType float_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (FPConv float_from float_to) e'
    _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToFloat"

internaliseExp desc (E.UnOp (E.ToSigned int_to) e _) = do
  e' <- internaliseExp1 "trunc_arg" e
  case E.typeOf e of
    E.Prim (E.Signed int_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SExt int_from int_to) e'
    E.Prim (E.Unsigned int_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SExt int_from int_to) e'
    E.Prim (E.FloatType float_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToSI float_from int_to) e'
    _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToSigned"

internaliseExp desc (E.UnOp (E.ToUnsigned int_to) e _) = do
  e' <- internaliseExp1 "trunc_arg" e
  case E.typeOf e of
    E.Prim (E.Signed int_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
    E.Prim (E.Unsigned int_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
    E.Prim (E.FloatType float_from) ->
      letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToUI float_from int_to) e'
    _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToUnsigned"

internaliseExp desc (E.Copy e _) = do
  ses <- internaliseExpToVars "copy_arg" e
  letSubExps desc [I.BasicOp $ I.Copy se | se <- ses]

internaliseExp desc (E.Write si v a loc) = do
  si' <- letExp "write_si" . BasicOp . SubExp =<< internaliseExp1 "write_arg_i" si
  svs <- internaliseExpToVars "write_arg_v" v
  sas <- internaliseExpToVars "write_arg_a" a

  si_shape <- arrayShape <$> lookupType si'
  let si_w = shapeSize 0 si_shape

  (tvs, svs') <- fmap unzip $ forM svs $ \sv -> do
    tv <- rowType <$> lookupType sv -- the element type
    sv_shape <- arrayShape <$> lookupType sv
    let sv_w = shapeSize 0 sv_shape

    -- Generate an assertion and reshapes to ensure that sv and si' are the same
    -- size.
    cmp <- letSubExp "write_cmp" $ I.BasicOp $
      I.CmpOp (I.CmpEq I.int32) si_w sv_w
    c   <- assertingOne $
      letExp "write_cert" $ I.BasicOp $
      I.Assert cmp loc
    sv' <- letExp (baseString sv ++ "_write_sv") $
      I.BasicOp $ I.Reshape c (reshapeOuter [DimCoercion si_w] 1 sv_shape) sv

    return (tv, sv')

  let indexType = I.Prim (IntType Int32)
      bodyTypes = replicate (length tvs) indexType ++ tvs

  indexName <- newVName "write_index"
  valueNames <- replicateM (length tvs) $ newVName "write_value"

  let bodyNames = indexName : valueNames
  let bodyParams = zipWith I.Param bodyNames bodyTypes

  -- This body is pretty boring right now, as every input is exactly the output.
  -- But it can get funky later on if fused with something else.
  (body, _) <- runBinderEmptyEnv $ insertBindingsM $ do
    let outs = replicate (length valueNames) indexName ++ valueNames
    results <- forM outs $ \name ->
      letSubExp "write_res" $ I.BasicOp $ I.SubExp $ I.Var name
    return $ resultBody results

  let lam = Lambda { I.lambdaParams = bodyParams
                   , I.lambdaReturnType = bodyTypes
                   , I.lambdaBody = body
                   }
      sivs = si' : svs'
  aws <- mapM (fmap (arraySize 0) . lookupType) sas
  letTupExp' desc $ I.Op $ I.Write [] si_w lam sivs $ zip aws sas

internaliseDimIndex :: SrcLoc -> SubExp -> E.DimIndex
                    -> InternaliseM (I.DimIndex SubExp, Certificates)
internaliseDimIndex loc w (E.DimFix i) = do
  i' <- internaliseExp1 "i" i
  cs <- assertingOne $ boundsCheck loc w i'
  return (I.DimFix i', cs)
internaliseDimIndex loc w (E.DimSlice i j) = do
  i' <- internaliseExp1 "i" i
  j' <- internaliseExp1 "j" j
  cs_i <- assertingOne $ boundsCheck loc w i'
  cs_j <- assertingOne $ do
    wp1 <- letSubExp "wp1" $ BasicOp $ I.BinOp (Add Int32) j' (constant (1::Int32))
    boundsCheck loc wp1 j'
  cs_pos <- assertingOne $
    letExp "pos" =<< eAssert (pure $ BasicOp $ I.CmpOp (CmpSle Int32) i' j') loc
  n <- letSubExp "n" $ BasicOp $ I.BinOp (Sub Int32) j' i'
  return (I.DimSlice i' n, cs_i <> cs_j <> cs_pos)

internaliseScanOrReduce :: String -> String
                        -> (Certificates -> SubExp -> I.Lambda -> [(SubExp, VName)] -> SOAC SOACS)
                        -> (E.Lambda, E.Exp, E.Exp, SrcLoc)
                        -> InternaliseM [SubExp]
internaliseScanOrReduce desc what f (lam, ne, arr, loc) = do
  arrs <- internaliseExpToVars (what++"_arr") arr
  nes <- internaliseExp (what++"_ne") ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape asserting loc rowtype (what++"_ne_right_shape") ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda asserting lam nests arrts
  let input = zip nes' arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ f [] w lam' input

internaliseExp1 :: String -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of [se] -> return se
             _ -> fail "Internalise.internaliseExp1: was passed not just a single subexpression"

internaliseExpToVars :: String -> E.Exp -> InternaliseM [I.VName]
internaliseExpToVars desc e =
  mapM asIdent =<< internaliseExp desc e
  where asIdent (I.Var v) = return v
        asIdent se        = letExp desc $ I.BasicOp $ I.SubExp se

internaliseOperation :: String
                     -> E.Exp
                     -> (I.VName -> InternaliseM I.BasicOp)
                     -> InternaliseM [I.SubExp]
internaliseOperation s e op = do
  vs <- internaliseExpToVars s e
  letSubExps s =<< mapM (fmap I.BasicOp . op) vs

internaliseBinOp :: String
                 -> E.BinOp
                 -> I.SubExp -> I.SubExp
                 -> E.PrimType
                 -> E.PrimType
                 -> InternaliseM [I.SubExp]
internaliseBinOp desc E.Plus x y (E.Signed t) _ =
  simpleBinOp desc (I.Add t) x y
internaliseBinOp desc E.Plus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Add t) x y
internaliseBinOp desc E.Plus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FAdd t) x y
internaliseBinOp desc E.Minus x y (E.Signed t) _ =
  simpleBinOp desc (I.Sub t) x y
internaliseBinOp desc E.Minus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Sub t) x y
internaliseBinOp desc E.Minus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FSub t) x y
internaliseBinOp desc E.Times x y (E.Signed t) _ =
  simpleBinOp desc (I.Mul t) x y
internaliseBinOp desc E.Times x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Mul t) x y
internaliseBinOp desc E.Times x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMul t) x y
internaliseBinOp desc E.Divide x y (E.Signed t) _ =
  simpleBinOp desc (I.SDiv t) x y
internaliseBinOp desc E.Divide x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UDiv t) x y
internaliseBinOp desc E.Divide x y (E.FloatType t) _ =
  simpleBinOp desc (I.FDiv t) x y
internaliseBinOp desc E.Pow x y (E.FloatType t) _ =
  simpleBinOp desc (I.FPow t) x y
internaliseBinOp desc E.Pow x y (E.Signed t) _ =
  simpleBinOp desc (I.Pow t) x y
internaliseBinOp desc E.Pow x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Pow t) x y
internaliseBinOp desc E.Mod x y (E.Signed t) _ =
  simpleBinOp desc (I.SMod t) x y
internaliseBinOp desc E.Mod x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UMod t) x y
internaliseBinOp desc E.Quot x y (E.Signed t) _ =
  simpleBinOp desc (I.SQuot t) x y
internaliseBinOp desc E.Quot x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UDiv t) x y
internaliseBinOp desc E.Rem x y (E.Signed t) _ =
  simpleBinOp desc (I.SRem t) x y
internaliseBinOp desc E.Rem x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UMod t) x y
internaliseBinOp desc E.ShiftR x y (E.Signed t) _ =
  simpleBinOp desc (I.AShr t) x y
internaliseBinOp desc E.ShiftR x y (E.Unsigned t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp desc E.ZShiftR x y (E.Signed t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp desc E.ZShiftR x y (E.Unsigned t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp desc E.ShiftL x y (E.Signed t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp desc E.ShiftL x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp desc E.Band x y (E.Signed t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp desc E.Band x y (E.Unsigned t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp desc E.Xor x y (E.Signed t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp desc E.Xor x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp desc E.Bor x y (E.Signed t) _ =
  simpleBinOp desc (I.Or t) x y
internaliseBinOp desc E.Bor x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Or t) x y

internaliseBinOp desc E.Equal x y t _ =
  simpleCmpOp desc (I.CmpEq $ internalisePrimType t) x y
internaliseBinOp desc E.NotEqual x y t _ = do
  eq <- letSubExp (desc++"true") $ I.BasicOp $ I.CmpOp (I.CmpEq $ internalisePrimType t) x y
  fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp I.Not eq
internaliseBinOp desc E.Less x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSlt t) x y
internaliseBinOp desc E.Less x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUlt t) x y
internaliseBinOp desc E.Leq x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSle t) x y
internaliseBinOp desc E.Leq x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUle t) x y
internaliseBinOp desc E.Greater x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSlt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Greater x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUlt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSle t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUle t) y x -- Note the swapped x and y
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
  letTupExp' desc $ I.BasicOp $ I.BinOp bop x y

simpleCmpOp :: String
            -> I.CmpOp
            -> I.SubExp -> I.SubExp
            -> InternaliseM [I.SubExp]
simpleCmpOp desc op x y =
  letTupExp' desc $ I.BasicOp $ I.CmpOp op x y


internaliseLambda :: InternaliseLambda

internaliseLambda (E.AnonymFun params body _ (Info rettype) _) rowtypes =
  bindingLambdaParams params rowtypes $ \params' -> do
    body' <- internaliseBody body
    (rettype', _, cm) <- internaliseReturnType rettype
    mapM_ (uncurry internaliseDimConstant) cm
    return (params', body', map I.fromDecl rettype')

internaliseLambda (E.CurryFun (QualName (_, fname)) curargs _ loc) rowtypes = do
  fun_entry <- lookupFunction fname
  let (fname', constparams, shapes, value_paramts, int_rettype_fun) = internalFun fun_entry
  curargs' <- concat <$> mapM (internaliseExp "curried") curargs
  curarg_types <- mapM subExpType curargs'
  params <- mapM (newParam "not_curried") rowtypes
  (constargs, const_ds, const_ts) <- unzip3 <$> constFunctionArgs constparams
  let valargs = curargs' ++ map (I.Var . I.paramName) params
      valargs_types = curarg_types ++ rowtypes
      shapeargs = argShapes shapes value_paramts valargs_types
      diets = const_ds ++ replicate (length shapeargs) I.Observe ++ map I.diet value_paramts
      paramts = const_ts ++ map (const $ I.Prim int32) shapeargs ++ value_paramts
  ((res, ts), fun_bnds) <- localScope (scopeOfLParams params) $ collectBindings $ do
    allargs <- ensureArgShapes asserting loc shapes paramts $ constargs ++ shapeargs ++ valargs
    argts' <- mapM subExpType allargs
    case int_rettype_fun $ zip allargs argts' of
      Nothing ->
        fail $ "Cannot apply " ++ pretty fname ++ " to arguments\n " ++
        pretty (constargs ++ shapeargs ++ valargs) ++ "\nof types\n " ++
        pretty (map (const $ I.Prim int32) shapeargs ++ valargs_types)
      Just (ExtRetType ts) -> do
        res <- letTupExp "curried_fun_result" $
               I.Apply fname' (zip allargs diets) $ ExtRetType ts
        return (map I.Var res, map I.fromDecl ts)
  return (params, mkBody fun_bnds res, ts)

internaliseLambda (E.UnOpFun unop (Info paramtype) (Info rettype) loc) rowts = do
  (params, body, rettype') <- unOpFunToLambda unop paramtype rettype
  internaliseLambda (E.AnonymFun params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.BinOpFun unop (Info xtype) (Info ytype) (Info rettype) loc) rowts = do
  (params, body, rettype') <- binOpFunToLambda unop xtype ytype rettype
  internaliseLambda (AnonymFun params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.CurryBinOpLeft binop e (Info paramtype) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e $ uncurry $ flip (,)
  internaliseLambda (AnonymFun params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.CurryBinOpRight binop e (Info paramtype) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e id
  internaliseLambda (AnonymFun params body Nothing (Info rettype') loc) rowts

unOpFunToLambda :: E.UnOp -> E.Type -> E.Type
                -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
unOpFunToLambda op paramtype rettype = do
  paramname <- newNameFromString "unop_param"
  let ident = E.Ident paramname (Info paramtype) noLoc
  return ([E.Id ident],
          E.UnOp op (E.Var (QualName ([],paramname)) (Info paramtype) noLoc) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

binOpFunToLambda :: E.BinOp -> E.Type -> E.Type -> E.Type
                 -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
binOpFunToLambda op xtype ytype rettype = do
  x_name <- newNameFromString "binop_param_x"
  y_name <- newNameFromString "binop_param_y"
  let ident_x = E.Ident x_name (Info xtype) noLoc
      ident_y = E.Ident y_name (Info ytype) noLoc
  return ([E.Id ident_x, E.Id ident_y],
          E.BinOp op
           (E.Var (QualName ([],x_name)) (Info xtype) noLoc)
           (E.Var (QualName ([],y_name)) (Info ytype) noLoc)
           (Info rettype) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

binOpCurriedToLambda :: E.BinOp -> E.Type -> E.Type
                     -> E.Exp
                     -> ((E.Exp,E.Exp) -> (E.Exp,E.Exp))
                     -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
binOpCurriedToLambda op paramtype rettype e swap = do
  paramname <- newNameFromString "binop_param_noncurried"
  let ident = E.Ident paramname (Info paramtype) noLoc
      (x', y') = swap (E.Var (QualName ([],paramname)) (Info paramtype) noLoc, e)
  return ([E.Id ident],
          E.BinOp op x' y' (Info rettype) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

internaliseDimConstant :: Name -> VName -> InternaliseM ()
internaliseDimConstant fname name =
  letBind_ (basicPattern' [] [I.Ident name $ I.Prim I.int32]) $
  I.Apply fname [] $ primRetType I.int32

-- | Is the name a value constant?  If so, create the necessary
-- function call and return the corresponding subexpressions.
lookupConstant :: VName -> InternaliseM (Maybe [SubExp])
lookupConstant name = do
  is_const <- asks $ fmap internalFun . HM.lookup name . envFtable
  case is_const of
    Just (fname, constparams, _, _, mk_rettype) -> do
      (constargs, const_ds, const_ts) <- unzip3 <$> constFunctionArgs constparams
      case mk_rettype $ zip constargs $ map I.fromDecl const_ts of
        Nothing -> fail $ "lookupConstant: " ++ pretty name ++ " failed"
        Just rettype ->
          fmap (Just . map I.Var) $
          letTupExp (baseString name) $ I.Apply fname (zip constargs const_ds) rettype
    Nothing -> return Nothing

constFunctionArgs :: [(Name,VName)] -> InternaliseM [(SubExp, I.Diet, I.DeclType)]
constFunctionArgs = mapM arg
  where arg (fname, name) = do
          se <- letSubExp (baseString name ++ "_arg") $
                I.Apply fname [] $ primRetType I.int32
          return (se, I.Observe, I.Prim I.int32)

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
assertingOne m = asserting $ fmap pure m

boundsCheck :: SrcLoc -> I.SubExp -> I.SubExp -> InternaliseM I.VName
boundsCheck loc w e = do
  let check = eBinOp I.LogAnd (pure lowerBound) (pure upperBound)
      lowerBound = I.BasicOp $
                   I.CmpOp (I.CmpSle I.Int32) (I.constant (0 :: I.Int32)) e
      upperBound = I.BasicOp $
                   I.CmpOp (I.CmpSlt I.Int32) e w
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
          letBindNames'_ [name] $ BasicOp $ SubExp se
          return nameSubsts
    nameSubsts <- foldM handleSubst HM.empty substs
    mapM_ addBinding $ substituteNames nameSubsts bnds
    return $ resultBody [substituteNames nameSubsts res]
  res' <- bodyBind body
  case res' of
    [se] -> return se
    _    -> fail "Internalise.shadowIdentsInExp: something went very wrong"
