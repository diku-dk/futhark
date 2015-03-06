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

internaliseCerts :: E.Certificates -> I.Certificates
internaliseCerts = map internaliseCert
  where internaliseCert (E.Ident name _ _) =
          I.Ident name (I.Basic I.Cert)

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

internaliseExp desc (E.Index cs var csidx idxs loc) = do
  idxs' <- mapM (internaliseExp1 "i") idxs
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  let cs' = internaliseCerts cs
      mkCerts vs = case csidx of
                     Just csidx' -> return $ internaliseCerts csidx'
                     Nothing     -> boundsChecks loc vs idxs'
  case subst of
    Nothing ->
      fail $ "Futhark.Internalise.internaliseExp Index: unknown variable " ++ textual (E.identName var) ++ "."
    Just vs -> do
      csidx' <- mkCerts vs
      let index v = I.PrimOp $ I.Index (cs' ++ csidx') v idxs'
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

internaliseExp desc (E.DoLoop mergepat mergeexp i bound loopbody letbody _) = do
  bound' <- internaliseExp1 "bound" bound
  mergeinit <- internaliseExp "loop_init" mergeexp
  i' <- internaliseIdent i
  mergeparams <- map E.toParam <$> flattenPattern mergepat
  (loopbody', shapepat, mergepat') <-
    withNonuniqueReplacements $ bindingParams mergeparams $ \shapepat mergepat' -> do
      loopbody' <- internaliseBody loopbody
      let Result ses = bodyResult loopbody'
          shapeargs = argShapes
                      (map I.identName shapepat)
                      (map I.identType mergepat')
                      ses
          loopbody'' = loopbody' { bodyResult = Result (shapeargs++ses) }
      return (loopbody'',
              shapepat,
              mergepat')
  let mergeexp' = argShapes
                  (map I.identName shapepat)
                  (map I.identType mergepat')
                  mergeinit ++
                  mergeinit
      merge = [ (FParam ident (), e) |
                (ident, e) <- zip (shapepat ++ mergepat') mergeexp' ]
      loop = I.LoopOp $ I.DoLoop mergepat' merge i' bound' loopbody'
  bindingTupIdent mergepat (I.expExtType loop) $ \mergepat'' -> do
    letBind_ mergepat'' loop
    internaliseExp desc letbody

internaliseExp desc (E.LetWith cs name src idxcs idxs ve body loc) = do
  idxs' <- mapM (internaliseExp1 "idx") idxs
  srcs <- internaliseExpToIdents "src" $ E.Var src
  ves <- internaliseExp "lw_val" ve
  let cs' = internaliseCerts cs
  idxcs' <- case idxcs of
              Just idxcs' -> return $ internaliseCerts idxcs'
              Nothing     -> boundsChecks loc srcs idxs'
  let comb sname ve' = do
        let rowtype = I.stripArray (length idxs) $ I.identType sname
        ve'' <- ensureShape loc rowtype "lw_val_correct_shape" ve'
        letInPlace "letwith_dst" (cs'++idxcs') sname idxs' $
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

internaliseExp desc (E.Size _ i e _) = do
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

internaliseExp _ (E.Transpose cs k n e _) =
  internaliseOperation "transpose" cs e $ \cs' v ->
    let rank = I.arrayRank $ I.identType v
        perm = I.transposeIndex k n [0..rank-1]
    in  return $ I.Rearrange cs' perm v

internaliseExp _ (E.Rearrange cs perm e _) =
  internaliseOperation "rearrange" cs e $ \cs' v ->
    return $ I.Rearrange cs' perm v

internaliseExp _ (E.Rotate cs n e _) =
  internaliseOperation "rotate" cs e $ \cs' v ->
    return $ I.Rotate cs' n v

internaliseExp _ (E.Reshape cs shape e loc) = do
  shape' <- mapM (internaliseExp1 "shape") shape
  internaliseOperation "reshape" cs e $ \cs' v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    shapeOk <- letExp "shape_ok" =<<
               eAssert (eBinOp I.Equal (prod $ I.arrayDims $ I.identType v)
                                       (prod shape')
                                       I.Bool)
               loc
    return $ I.Reshape (shapeOk:cs') shape' v
  where prod l = foldBinOp I.Times (intconst 1) l I.Int

internaliseExp _ (E.Split cs nexp arrexp _) = do
  let cs' = internaliseCerts cs
  nexp' <- internaliseExp1 "n" nexp
  arrs <- internaliseExpToIdents "split_arr" arrexp
  ressize <- letSubExp "split_size" $
             PrimOp $ I.BinOp I.Minus (arraysSize 0 $ map I.identType arrs)
             nexp' I.Int
  partnames <- forM (map I.identType arrs) $ \et -> do
    a <- fst <$> newVar "split_a" (et `setOuterSize` nexp')
    b <- fst <$> newVar "split_b" (et `setOuterSize` ressize)
    return (a, b)
  let combsplit arr (a,b) =
        letBind_ (basicPattern' [a,b]) $
        PrimOp $ I.Split cs' nexp' arr ressize
  zipWithM_ combsplit arrs partnames
  return $
    map (I.Var . fst) partnames ++
    map (I.Var . snd) partnames

internaliseExp desc (E.Concat cs x ys loc) = do
  xs  <- internaliseExpToIdents "concat_x" x
  yss <- mapM (internaliseExpToIdents "concat_y") ys
  let cs' = internaliseCerts cs
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
        return $ I.PrimOp $ I.Concat cs' xarr yarrs' ressize
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
  lam1' <- withNonuniqueReplacements $
           internaliseFoldLambda internaliseBody lam1
           (map I.subExpType nes) (map I.subExpType nes)
  lam2' <- withNonuniqueReplacements $
           internaliseFoldLambda internaliseBody lam2
           (map I.subExpType nes) (map I.identType arrs')
  letTupExp' desc $ I.LoopOp $
    I.Redomap [] lam1' lam2' nes arrs'

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

internaliseExp desc (E.Assert e loc) = do
  e' <- internaliseExp1 "assert_arg" e
  letTupExp' desc $ I.PrimOp $ I.Assert e' loc

internaliseExp desc (E.Copy e _) = do
  ses <- internaliseExp "copy_arg" e
  letSubExps desc [I.PrimOp $ I.Copy se | se <- ses]

internaliseExp desc (E.Conjoin es _) = do
  es' <- concat <$> mapM (internaliseExp "conjoin_arg") es
  letTupExp' desc $ I.PrimOp $ I.Conjoin es'

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
                     -> E.Certificates
                     -> E.Exp
                     -> (I.Certificates -> I.Ident -> InternaliseM I.PrimOp)
                     -> InternaliseM [I.SubExp]
internaliseOperation s cs e op = do
  vs <- internaliseExpToIdents s e
  let cs' = internaliseCerts cs
  letSubExps s =<< mapM (liftM I.PrimOp . op cs') vs

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
