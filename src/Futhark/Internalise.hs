-- |
--
-- This module implements a transformation from external to internal
-- Futhark.
--
module Futhark.Internalise
  ( internaliseProg
  , internaliseType
  , internaliseValue
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.List
import Data.Traversable (mapM)
import Data.Loc
import Futhark.Representation.External as E
import Futhark.Representation.Basic as I
import Futhark.Renamer as I
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Substitute
import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings
import Futhark.Internalise.Lambdas

import Prelude hiding (mapM)

-- | Convert a program in external Futhark to a program in internal
-- Futhark.  If the boolean parameter is false, do not add bounds
-- checks to array indexing.
internaliseProg :: Bool -> E.Prog -> I.Prog
internaliseProg doBoundsCheck prog =
  I.renameProg $ flip evalState src $ do
    ftable <- buildFtable prog
    liftM I.Prog $ runInternaliseM doBoundsCheck ftable $
      mapM internaliseFun $ E.progFunctions prog
  where src = E.newNameSourceForProg prog

buildFtable :: MonadFreshNames m => E.Prog -> m FunTable
buildFtable = liftM HM.fromList . mapM inspect . E.progFunctions
  where inspect (fname, rettype, params, _, _) = do
          let rettype' = internaliseType rettype
          (shapes, params') <- unzip <$> mapM internaliseFunParam params
          return (fname,
                  FunBinding { internalFun = (rettype',
                                              map I.identName $ concat shapes,
                                              map I.identType $ concat params')
                             , externalFun = (rettype, map E.identType params)
                             })

internaliseFun :: E.FunDec -> InternaliseM I.FunDec
internaliseFun (fname,rettype,params,body,_) =
  bindingParams params $ \shapeparams params' -> do
    body' <- internaliseBody body
    let mkFParam = flip FParam ()
    return $ FunDec
      fname rettype'
      (map mkFParam $ shapeparams ++ params')
      body'
  where rettype' = ExtRetType $ internaliseType rettype

internaliseIdent :: E.Ident -> InternaliseM I.Ident
internaliseIdent (E.Ident name tp _) =
  case internaliseType tp of
    [I.Basic tp'] -> return $ I.Ident name (I.Basic tp')
    _             -> fail "Futhark.Internalise.internaliseIdent: asked to internalise non-basic-typed ident."

internaliseBody :: E.Exp -> InternaliseM Body
internaliseBody e = insertBindingsM $ do
  ses <- internaliseExp "res" e
  return $ resultBody ses

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
  aes <- mapM (internaliseExpToIdents "arr_elem") es
  let es'@((e':_):_) = aes --- XXX, ugh.
      Shape rowshape = arrayShape $ I.identType e'
  case internaliseType rowtype of
    [et] -> letTupExp' desc $ I.PrimOp $
            I.ArrayLit (map I.Var $ concat es')
            (et `setArrayShape` Shape rowshape)
    ets   -> do
      let arraylit ks et =
            I.PrimOp $ I.ArrayLit (map I.Var ks)
            (et `setArrayShape` Shape rowshape)
      letSubExps desc (zipWith arraylit (transpose es') ets)

internaliseExp desc (E.Apply fname args _ _)
  | "trace" <- nameToString fname = do
  args' <- mapM (internaliseExp "arg" . fst) args
  let args'' = concatMap tag args'
  letTupExp' desc $
    I.Apply fname args''
    (ExtRetType $ staticShapes $ map (subExpType . fst) args'')
  where tag ses = [ (se, I.Observe) | se <- ses ]

internaliseExp desc (E.Apply fname args _ _)
  | Just (rettype, _) <- HM.lookup fname builtInFunctions = do
  args' <- mapM (internaliseExp "arg" . fst) args
  let args'' = concatMap tag args'
  letTupExp' desc $ I.Apply fname args'' (ExtRetType [I.Basic rettype])
  where tag ses = [ (se, I.Observe) | se <- ses ]

internaliseExp desc (E.Apply fname args _ _) = do
  args' <- liftM concat $ mapM (internaliseExp "arg" . fst) args
  (rettype, shapes, paramts) <- internalFun <$> lookupFunction fname
  let diets = map I.diet paramts
      args'' = zip (argShapes shapes paramts args') (repeat I.Observe) ++
               zip args' diets
  letTupExp' desc $ I.Apply fname args'' (ExtRetType rettype)

internaliseExp desc (E.LetPat pat e body _) = do
  ses <- internaliseExp desc e
  bindingTupIdent pat
    (I.staticShapes $ map I.subExpType ses) $ \pat' -> do
    forM_ (zip (patternIdents pat') ses) $ \(p,se) ->
      letBind (basicPattern' [p]) $ I.PrimOp $ I.SubExp se
    internaliseExp desc body

internaliseExp desc (E.DoLoop mergepat mergeexp form loopbody letbody _) = do
  mergeinit <- internaliseExp "loop_init" mergeexp
  mergeparams <- map E.toParam <$> flattenPattern mergepat
  (form', loopbody', shapepat, mergepat', res, mergeinit') <-
    withNonuniqueReplacements $ bindingParams mergeparams $ \shapepat mergepat' -> do
      loopbody' <- internaliseBody loopbody
      let Result ses = bodyResult loopbody'
          shapeargs = argShapes
                      (map I.identName shapepat)
                      (map I.identType mergepat')
                      ses
          shapeinit = argShapes
                      (map I.identName shapepat)
                      (map I.identType mergepat')
                      mergeinit
      case form of
        E.ForLoop i bound -> do
          bound' <- internaliseExp1 "bound" bound
          i' <- internaliseIdent i
          let loopbody'' = loopbody' { bodyResult = Result (shapeargs++ses) }
          return (I.ForLoop i' bound',
                  loopbody'',
                  shapepat,
                  mergepat',
                  mergepat',
                  mergeinit)
        E.WhileLoop cond -> do
          -- We need to insert 'cond' twice - once for the initial
          -- condition (do we enter the loop at all?), and once with
          -- the result values of the loop (do we continue into the
          -- next iteration?).  This is safe, as the type rules for
          -- the external language guarantees that 'cond' does not
          -- consume anything.
          loop_while <- newIdent "loop_while" $ I.Basic Bool
          let initsubst = [ (I.identName mergeparam, initval)
                            | (mergeparam, initval) <-
                               zip (shapepat++mergepat') (shapeinit++mergeinit)
                            ]
              endsubst = [ (I.identName mergeparam, endval)
                         | (mergeparam, endval) <-
                              zip (shapepat++mergepat') (shapeargs++ses)
                         ]
          (loop_cond, loop_cond_bnds) <-
            collectBindings $ internaliseExp1 "loop_cond" cond
          loop_initial_cond <-
            shadowIdentsInExp initsubst loop_cond_bnds loop_cond
          (loop_end_cond, loop_end_cond_bnds) <-
            collectBindings $
            shadowIdentsInExp endsubst loop_cond_bnds loop_cond
          let loopbody'' =
                loopbody' { bodyResult =
                               Result $ shapeargs++[loop_end_cond]++ses
                          , bodyBindings =
                            bodyBindings loopbody' ++ loop_end_cond_bnds
                          }
          return (I.WhileLoop loop_while,
                  loopbody'',
                  shapepat,
                  loop_while : mergepat',
                  mergepat',
                  loop_initial_cond : mergeinit)

  let mergeexp' = argShapes
                  (map I.identName shapepat)
                  (map I.identType mergepat')
                  mergeinit' ++
                  mergeinit'
      merge = [ (FParam ident (), e) |
                (ident, e) <- zip (shapepat ++ mergepat') mergeexp' ]
      loop = I.LoopOp $ I.DoLoop res merge form' loopbody'
  bindingTupIdent mergepat (I.expExtType loop) $ \mergepat'' -> do
    letBind_ mergepat'' loop
    internaliseExp desc letbody

internaliseExp desc (E.LetWith name src idxs ve body loc) = do
  idxs' <- mapM (internaliseExp1 "idx") idxs
  srcs <- internaliseExpToIdents "src" $ E.Var src
  ves <- internaliseExp "lw_val" ve
  idxcs' <- boundsChecks loc srcs idxs'
  let comb sname ve' = do
        let rowtype = I.stripArray (length idxs) $ I.identType sname
        ve'' <- ensureShape loc rowtype "lw_val_correct_shape" ve'
        letInPlace "letwith_dst" idxcs' sname idxs' $
          PrimOp $ SubExp ve''
  dsts <- zipWithM comb srcs ves
  bindingTupIdent (E.Id name)
    (I.staticShapes $ map I.identType dsts) $ \pat' -> do
    forM_ (zip (patternIdents pat') dsts) $ \(p,dst) ->
      letBind (basicPattern' [p]) $ I.PrimOp $ I.SubExp $ I.Var dst
    internaliseExp desc body

internaliseExp desc (E.Replicate ne ve _) = do
  ne' <- internaliseExp1 "n" ne
  ves <- internaliseExp "replicate_v" ve
  letSubExps desc [I.PrimOp $ I.Replicate ne' ve' | ve' <- ves ]

internaliseExp desc (E.Size i e _) = do
  ks <- internaliseExp desc e
  case ks of
    (k:_) -> return [I.arraySize i $ I.subExpType k]
    _     -> return [I.intconst 0] -- Will this ever happen?

internaliseExp desc (E.Unzip e _ _) =
  internaliseExp desc e

internaliseExp _ (E.Zip [] _) =
  return []

internaliseExp _ (E.Zip (e:es) loc) = do
  e' <- internaliseExpToIdents "zip_arg" $ fst e
  es_unchecked' <- mapM (internaliseExpToIdents "zip_arg" . fst) es
  -- Now we will reshape all of es_unchecked' to have the same outer
  -- size as e'.  We will not change any of the outer dimensions.
  -- This will cause a runtime error if the outer sizes do not match,
  -- thus preserving the semantics of zip().
  let e_outer = arraysSize 0 $ map I.identType e'
      reshapeToOuter e_unchecked' =
        case I.arrayDims $ I.identType e_unchecked' of
          []      -> return e_unchecked' -- Probably type error
          outer:inner -> do
            cmp <- letSubExp "zip_cmp" $ I.PrimOp $
                   I.BinOp I.Equal e_outer outer I.Bool
            c   <- letExp "zip_assert" $ I.PrimOp $
                   I.Assert cmp loc
            letExp "zip_result" $ I.PrimOp $
              I.Reshape [c] (e_outer:inner) e_unchecked'
  es' <- mapM (mapM reshapeToOuter) es_unchecked'
  return $ concatMap (map I.Var) $ e' : es'

internaliseExp _ (E.Transpose k n e _) =
  internaliseOperation "transpose" e $ \v ->
    let rank = I.arrayRank $ I.identType v
        perm = I.transposeIndex k n [0..rank-1]
    in  return $ I.Rearrange [] perm v

internaliseExp _ (E.Rearrange perm e _) =
  internaliseOperation "rearrange" e $ \v ->
    return $ I.Rearrange [] perm v

internaliseExp _ (E.Reshape shape e loc) = do
  shape' <- mapM (internaliseExp1 "shape") shape
  internaliseOperation "reshape" e $ \v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    shapeOk <- letExp "shape_ok" =<<
               eAssert (eBinOp I.Equal (prod $ I.arrayDims $ I.identType v)
                                       (prod shape')
                                       I.Bool)
               loc
    return $ I.Reshape [shapeOk] shape' v
  where prod l = foldBinOp I.Times (intconst 1) l I.Int

internaliseExp _ (E.Split splitexps arrexp loc) = do
  splits' <- mapM (internaliseExp1 "n") splitexps
  -- Note that @arrs@ is an array, because of array-of-tuples transformation
  arrs <- internaliseExpToIdents "split_arr" arrexp
  let arrayOuterdim = arraysSize 0 (map I.identType arrs)

  -- Assertions
  let indexConds = zipWith (\beg end -> PrimOp $ I.BinOp I.Leq beg end I.Bool)
                     (I.intconst 0:splits') (splits'++[arrayOuterdim])
  indexChecks <- mapM (letSubExp "split_index_cnd") indexConds
  indexAsserts <- mapM (\cnd -> letExp "split_index_assert" $ PrimOp $ I.Assert cnd loc)
                  indexChecks

  -- Calculate diff between each split index
  let sizeExps = zipWith (\beg end -> PrimOp $ I.BinOp I.Minus end beg I.Int)
                 (I.intconst 0:splits') (splits'++[arrayOuterdim])
  sizeVars <- mapM (letSubExp "split_size") sizeExps
  splitExps <- forM arrs $ \arr -> letTupExp' "split_res" $
                                   PrimOp $ I.Split indexAsserts sizeVars arr

  return $ concat $ transpose splitExps

internaliseExp desc (E.Concat x ys loc) = do
  xs  <- internaliseExpToIdents "concat_x" x
  yss <- mapM (internaliseExpToIdents "concat_y") ys
  ressize <- foldM sumdims
                   (arraysSize 0 $ map I.identType xs) $
                   map (arraysSize 0 . map I.identType) yss

  let conc xarr yarrs = do
        -- The inner sizes must match.
        let matches n m =
              letExp "match" =<<
              eAssert (pure $ I.PrimOp $ I.BinOp I.Equal n m I.Bool) loc
            xt  = I.identType xarr
            yts = map I.identType yarrs
            x_inner_dims  = drop 1 $ I.arrayDims xt
            ys_inner_dims = map (drop 1 . I.arrayDims) yts
        matchcs <- concat <$> mapM (zipWithM matches x_inner_dims) ys_inner_dims
        yarrs'  <- forM yarrs $ \yarr ->
                        let yt = I.identType yarr
                        in  letExp "concat_y_reshaped" $ I.PrimOp $
                                   I.Reshape matchcs (arraySize 0 yt : x_inner_dims) yarr
        return $ I.PrimOp $ I.Concat [] xarr yarrs' ressize
  letSubExps desc =<< zipWithM conc xs (transpose yss)

    where
        sumdims xsize ysize = letSubExp "conc_tmp" $ I.PrimOp $
                                        I.BinOp I.Plus xsize ysize I.Int

internaliseExp desc (E.Map lam arr _) = do
  arrs <- internaliseExpToIdents "map_arr" arr
  lam' <- withNonuniqueReplacements $
          internaliseMapLambda internaliseBody lam $ map I.Var arrs
  letTupExp' desc $ I.LoopOp $ I.Map [] lam' arrs

internaliseExp desc (E.Reduce lam ne arr loc) = do
  arrs <- internaliseExpToIdents "reduce_arr" arr
  nes <- internaliseExp "reduce_ne" ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') ->
    ensureShape loc (I.stripArray 1 $ I.identType arr')
      "scan_ne_right_shape" ne'
  lam' <- withNonuniqueReplacements $
          internaliseFoldLambda internaliseBody lam
          (map I.subExpType nes') (map I.identType arrs)
  let input = zip nes' arrs
  letTupExp' desc $ I.LoopOp $ I.Reduce [] lam' input

internaliseExp desc (E.Scan lam ne arr loc) = do
  arrs <- internaliseExpToIdents "scan_arr" arr
  nes <- internaliseExp "scan_ne" ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') ->
    ensureShape loc (I.stripArray 1 $ I.identType arr')
      "scan_ne_right_shape" ne'
  lam' <- withNonuniqueReplacements $
          internaliseFoldLambda internaliseBody lam
          (map I.subExpType nes') (map I.identType arrs)
  let input = zip nes' arrs
  letTupExp' desc $ I.LoopOp $ I.Scan [] lam' input

internaliseExp desc (E.Filter lam arr _) = do
  arrs <- internaliseExpToIdents "filter_arr" arr
  lam' <- withNonuniqueReplacements $
          internaliseFilterLambda internaliseBody lam $ map I.Var arrs
  letTupExp' desc $ I.LoopOp $ I.Filter [] lam' arrs

internaliseExp desc (E.Redomap lam1 lam2 ne arrs _) = do
  arrs' <- internaliseExpToIdents "redomap_arr" arrs
  nes <- internaliseExp "redomap_ne" ne
  let acc_tps     = map I.subExpType nes
  let outersize   = arraysSize 0 $ map I.identType arrs'
  let acc_arr_tps = [ I.arrayOf t (Shape [outersize]) (I.uniqueness t)
                        | t <- acc_tps ]
  lam1' <- withNonuniqueReplacements $
           internaliseFoldLambda internaliseBody lam1
           (map I.subExpType nes) 0 --acc_arr_tps
  lam2' <- withNonuniqueReplacements $
           internaliseRedomapInnerLambda internaliseBody lam2
           nes (map I.Var arrs')
  letTupExp' desc $ I.LoopOp $
    I.Redomap [] lam1' lam2' nes arrs'

internaliseExp desc (E.ConcatMap lam arr arrs _) = do
  arr' <- internaliseExpToIdents "concatMap_arr" arr
  arrs' <- mapM (internaliseExpToIdents "concatMap_arr") arrs
  lam' <- withNonuniqueReplacements $
          internaliseConcatMapLambda internaliseBody lam $ map I.Var arr'
  letTupExp' desc $ I.LoopOp $ I.ConcatMap [] lam' $ arr':arrs'

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp desc (E.Iota e _) = do
  e' <- internaliseExp1 "n" e
  letTupExp' desc $ I.PrimOp $ I.Iota e'

internaliseExp _ (E.Literal v _) =
  mapM (letSubExp "literal" <=< eValue) $ internaliseValue v

internaliseExp desc (E.If ce te fe t _) = do
  ce' <- internaliseExp1 "cond" ce
  te' <- internaliseBody te
  fe' <- internaliseBody fe
  let t' = internaliseType t
  letTupExp' desc $ I.If ce' te' fe' t'

internaliseExp desc (E.BinOp bop xe ye t _) = do
  xe' <- internaliseExp1 "x" xe
  ye' <- internaliseExp1 "y" ye
  case internaliseType t of
    [I.Basic t'] -> letTupExp' desc $
                    I.PrimOp $ I.BinOp bop xe' ye' t'
    _            -> fail "Futhark.Internalise.internaliseExp: non-basic type in BinOp."

internaliseExp desc (E.Not e _) = do
  e' <- internaliseExp1 "not_arg" e
  letTupExp' desc $ I.PrimOp $ I.Not e'

internaliseExp desc (E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  letTupExp' desc $ I.PrimOp $ I.Negate e'

internaliseExp desc (E.Copy e _) = do
  ses <- internaliseExp "copy_arg" e
  letSubExps desc [I.PrimOp $ I.Copy se | se <- ses]

internaliseExp1 :: String -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of [se] -> return se
             _ -> fail "Internalise.internaliseExp1: was passed not just a single subexpression"

internaliseExpToIdents :: String -> E.Exp -> InternaliseM [I.Ident]
internaliseExpToIdents desc e =
  mapM asIdent =<< internaliseExp desc e
  where asIdent (I.Var v) = return v
        asIdent se        = letExp desc $ I.PrimOp $ I.SubExp se

internaliseOperation :: String
                     -> E.Exp
                     -> (I.Ident -> InternaliseM I.PrimOp)
                     -> InternaliseM [I.SubExp]
internaliseOperation s e op = do
  vs <- internaliseExpToIdents s e
  letSubExps s =<< mapM (liftM I.PrimOp . op) vs

boundsChecks :: SrcLoc -> [I.Ident] -> [I.SubExp] -> InternaliseM I.Certificates
boundsChecks _ []    _  = return []
boundsChecks loc (v:_) es = do
  doBoundsChecks <- asks envDoBoundsChecks
  if doBoundsChecks
  then zipWithM (boundsCheck loc v) [0..] es
  else return []

boundsCheck :: SrcLoc -> I.Ident -> Int -> I.SubExp -> InternaliseM I.Ident
boundsCheck loc v i e = do
  let size  = arraySize i $ I.identType v
      check = eBinOp LogAnd (pure lowerBound) (pure upperBound) I.Bool
      lowerBound = I.PrimOp $
                   I.BinOp Leq (I.intconst 0) e I.Bool
      upperBound = I.PrimOp $
                   I.BinOp Less e size I.Bool
  letExp "bounds_check" =<< eAssert check loc

shadowIdentsInExp :: [(VName, I.SubExp)] -> [Binding] -> I.SubExp
                  -> InternaliseM I.SubExp
shadowIdentsInExp substs bnds res = do
  body <- renameBody <=< insertBindingsM $ do
    -- XXX: we have to substitute names to fix type annotations in the
    -- bindings.  This goes away once we get rid of these type
    -- annotations.
    let handleSubst nameSubsts (name, I.Var v)
          | I.identName v == name =
            return nameSubsts
          | otherwise =
            return $ HM.insert name (I.identName v) nameSubsts
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
