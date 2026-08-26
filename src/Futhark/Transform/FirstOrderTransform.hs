{-# LANGUAGE TypeFamilies #-}

-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive,
-- and - it's certainly worth considering when we can express such
-- transformations in-place.
module Futhark.Transform.FirstOrderTransform
  ( transformFunDef,
    transformConsts,
    FirstOrderRep,
    Transformer,
    transformStmRecursively,
    transformLambda,
    transformSOAC,
    transformScrema,
    transformFlatMap,
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (find, uncons, zip4)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.Alias qualified as Alias
import Futhark.IR qualified as AST
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Util (chunks, splitAt3)

-- | The constraints that must hold for a rep in order to be the
-- target of first-order transformation.
type FirstOrderRep rep =
  ( Buildable rep,
    BuilderOps rep,
    LetDec SOACS ~ LetDec rep,
    LParamInfo SOACS ~ LParamInfo rep,
    Alias.AliasableRep rep
  )

-- | First-order-transform a single function, with the given scope
-- provided by top-level constants.
transformFunDef ::
  (MonadFreshNames m, FirstOrderRep torep) =>
  Scope torep ->
  FunDef SOACS ->
  m (AST.FunDef torep)
transformFunDef consts_scope (FunDef entry attrs fname rettype params body) = do
  (body', _) <- modifyNameSource $ runState $ runBuilderT m consts_scope
  pure $ FunDef entry attrs fname rettype params body'
  where
    m = localScope (scopeOfFParams params) $ transformBody body

-- | First-order-transform these top-level constants.
transformConsts ::
  (MonadFreshNames m, FirstOrderRep torep) =>
  Stms SOACS ->
  m (AST.Stms torep)
transformConsts stms =
  fmap snd $ modifyNameSource $ runState $ runBuilderT m mempty
  where
    m = mapM_ transformStmRecursively stms

-- | The constraints that a monad must uphold in order to be used for
-- first-order transformation.
type Transformer m =
  ( MonadBuilder m,
    LocalScope (Rep m) m,
    Buildable (Rep m),
    BuilderOps (Rep m),
    LParamInfo SOACS ~ LParamInfo (Rep m),
    Alias.AliasableRep (Rep m)
  )

transformBody ::
  (Transformer m, LetDec (Rep m) ~ LetDec SOACS) =>
  Body SOACS ->
  m (AST.Body (Rep m))
transformBody (Body () stms res) = buildBody_ $ do
  mapM_ transformStmRecursively stms
  pure res

-- | First transform any nested t'Body' or t'Lambda' elements, then
-- apply 'transformSOAC' if the expression is a SOAC.
transformStmRecursively ::
  (Transformer m, LetDec (Rep m) ~ LetDec SOACS) => Stm SOACS -> m ()
transformStmRecursively (Let pat aux (Op soac)) =
  auxing aux $ transformSOAC pat =<< mapSOACM soacTransform soac
  where
    soacTransform =
      identitySOACMapper
        { mapOnSOACLambda = transformLambda,
          mapOnSOACExtLambda = transformLambda
        }
transformStmRecursively (Let pat aux e) =
  auxing aux $ letBind pat =<< mapExpM transform e
  where
    transform =
      identityMapper
        { mapOnBody = \scope -> localScope scope . transformBody,
          mapOnRetType = pure,
          mapOnBranchType = pure,
          mapOnFParam = pure,
          mapOnLParam = pure,
          mapOnOp = error "Unhandled Op in first order transform"
        }

-- Produce scratch "arrays" for the Map and Scan outputs of Screma.
-- "Arrays" is in quotes because some of those may be accumulators.
resultArray :: (Transformer m) => [VName] -> [Type] -> m [VName]
resultArray arrs ts = do
  arrs_ts <- mapM lookupType arrs
  let oneArray t@Acc {}
        | Just (v, _) <- find ((== t) . snd) (zip arrs arrs_ts) =
            pure v
      oneArray t =
        letExp "result" =<< eBlank t
  mapM oneArray ts

-- | Sequentialise a single FlatMap. The size of the nonuniform results produced
-- by the lambda is not known until it has been run, so each of them is
-- accumulated in a scratch buffer that is doubled whenever it runs out of
-- space, and finally truncated to the actual size. The value results need no
-- such treatment, as there is exactly one per iteration. The shape and offset
-- arrays are filled in as we go, and the flag array is then a scatter of the
-- segment starts.
transformFlatMap ::
  (Transformer m) =>
  Pat (LetDec (Rep m)) ->
  SubExp ->
  [VName] ->
  ExtLambda (Rep m) ->
  m ()
transformFlatMap pat w arrs lam = do
  let irreg_ts = flatMapRowTypes lam
      reg_ts = flatMapUniformTypes lam
  arrs_ts <- mapM lookupType arrs

  -- Loop parameters: the current filled size, the current capacity, the
  -- per-element shape and offset arrays, one scratch buffer per nonuniform
  -- result, and one array per uniform result.
  size_p <- newParam "flatmap_size" $ toDecl (Prim int64) Nonunique
  cap_p <- newParam "flatmap_cap" $ toDecl (Prim int64) Nonunique
  shape_p <- newParam "flatmap_shape" $ toDecl (arrayOfRow (Prim int64) w) Unique
  offset_p <- newParam "flatmap_offset" $ toDecl (arrayOfRow (Prim int64) w) Unique
  scratch_ps <-
    forM irreg_ts $ \et ->
      newParam "flatmap_res" $ toDecl (arrayOfRow et (Var (paramName cap_p))) Unique
  reg_ps <-
    forM reg_ts $ \rt ->
      newParam "flatmap_reg" $ toDecl (arrayOfRow rt w) Unique

  -- The capacity initially matches the input size.
  shape_init <- letExp "flatmap_shape" $ BasicOp $ Scratch int64 [w]
  offset_init <- letExp "flatmap_offset" $ BasicOp $ Scratch int64 [w]
  scratch_init <- forM irreg_ts $ \et -> letExp "flatmap_res" =<< eBlank (arrayOfRow et w)
  reg_init <- forM reg_ts $ \rt -> letExp "flatmap_reg" =<< eBlank (arrayOfRow rt w)

  let merge =
        (size_p, intConst Int64 0)
          : (cap_p, w)
          : (shape_p, Var shape_init)
          : (offset_p, Var offset_init)
          : zip scratch_ps (map Var scratch_init)
            <> zip reg_ps (map Var reg_init)
      merge_params = map fst merge

  i <- newVName "i"
  let loop_form = ForLoop i Int64 w
      loop_scope = scopeOfLoopForm loop_form <> scopeOfFParams merge_params
  loop_body <- runBodyBuilder . localScope loop_scope $ do
    -- Apply the lambda to the current elements.
    let arg arr arr_t = BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var i]
        size = Var $ paramName size_p
    lam_res <- map resSubExp <$> bindLambda lam (zipWith arg arrs arrs_ts)

    -- The lambda produces the common length of its segment results first.
    let (k, ys) =
          fromMaybe (error "transformFlatMap: malformed FlatMap.") $
            uncons lam_res
        (irreg_ys, reg_ys) = flatMapSplitValues lam ys
    new_size <-
      letSubExp "flatmap_new_size" . BasicOp $
        BinOp (Add Int64 OverflowUndef) size k
    grow <-
      letSubExp "flatmap_grow" . BasicOp $
        CmpOp (CmpSlt Int64) (Var (paramName cap_p)) new_size

    -- New capacity: double it (but at least fit) when it overflows.
    new_cap <-
      letSubExp "flatmap_new_cap"
        =<< eIf
          (eSubExp grow)
          ( buildBody_ $ do
              doubled <-
                letSubExp "doubled" . BasicOp $
                  BinOp (Mul Int64 OverflowUndef) (Var (paramName cap_p)) (intConst Int64 2)
              fmap (pure . subExpRes) . letSubExp "atleast" . BasicOp $
                BinOp (SMax Int64) doubled new_size
          )
          (buildBody_ $ pure [subExpRes $ Var (paramName cap_p)])

    let lowSlice t =
          fullSlice t [DimSlice (intConst Int64 0) size (intConst Int64 1)]

    -- Grow (and copy) each scratch buffer when necessary, then write the
    -- new elements at the end.
    scratch_res <- forM (zip3 scratch_ps irreg_ts irreg_ys) $ \(sp, et, ys_j) -> do
      let full_t = arrayOfRow et new_cap
      base <-
        letExp "flatmap_grown"
          =<< eIf
            (eSubExp grow)
            ( buildBody_ $ do
                fresh <-
                  letExp "flatmap_fresh" . BasicOp $
                    Scratch (elemType full_t) (arrayDims full_t)
                old_t <- lookupType $ paramName sp
                copied <-
                  letInPlace "flatmap_fresh" fresh (lowSlice full_t) $
                    BasicOp (Index (paramName sp) (lowSlice old_t))
                pure [varRes copied]
            )
            ( buildBody_ $
                fmap (pure . varRes) . letExp "flatmap_kept" $
                  shapeCoerce (arrayDims full_t) (paramName sp)
            )
      letInPlace "flatmap_res" base (fullSlice full_t [DimSlice size k (intConst Int64 1)]) $
        BasicOp (SubExp ys_j)

    -- The segment's size and its offset (the running total before it).
    shape' <-
      letInPlace "flatmap_shape" (paramName shape_p) (fullSlice (paramType shape_p) [DimFix $ Var i]) $
        BasicOp (SubExp k)
    offset' <-
      letInPlace "flatmap_offset" (paramName offset_p) (fullSlice (paramType offset_p) [DimFix $ Var i]) $
        BasicOp (SubExp size)

    -- The uniform results are simply written at this iteration's index.
    reg_res <- forM (zip reg_ps reg_ys) $ \(rp, reg_y) ->
      letInPlace "flatmap_reg" (paramName rp) (fullSlice (paramType rp) [DimFix $ Var i]) $
        BasicOp (SubExp reg_y)

    pure $
      subExpsRes [new_size, new_cap]
        <> varsRes (shape' : offset' : scratch_res <> reg_res)

  loop_res <- letTupExp "flatmap" $ Loop merge loop_form loop_body
  case (loop_res, patNames pat) of
    (size_res : _cap_res : shape_res : offset_res : value_res, m_pat : shape_pat : flag_pat : offset_pat : out_pats) -> do
      -- Bind the total size and the shape/offset arrays, then truncate each
      -- buffer. The uniform results are already of the right size.
      letBindNames [m_pat] $ BasicOp $ SubExp $ Var size_res
      letBindNames [shape_pat] $ BasicOp $ SubExp $ Var shape_res
      letBindNames [offset_pat] $ BasicOp $ SubExp $ Var offset_res
      let (scratch_res, reg_res) = splitAt (length irreg_ts) value_res
          (data_pats, reg_pats) = flatMapSplitValues lam out_pats
      forM_ (zip data_pats scratch_res) $ \(out, scratch) -> do
        scratch_t <- lookupType scratch
        letBindNames [out] . BasicOp . Index scratch $
          fullSlice scratch_t [DimSlice (intConst Int64 0) (Var m_pat) (intConst Int64 1)]
      forM_ (zip reg_pats reg_res) $ \(out, reg) ->
        letBindNames [out] $ BasicOp $ SubExp $ Var reg
      -- The flag array: scatter a 'true' at the offset of each non-empty
      -- segment, over an otherwise 'false' array.
      transformFlatMapFlags flag_pat w (Var m_pat) shape_res offset_res
    _ ->
      error "transformFlatMap: malformed FlatMap."

-- | Compute a 'FlatMap' flag array of length @m@: 'true' at the start of each
-- non-empty segment, 'false' elsewhere. Emitted as a sequential scatter loop.
transformFlatMapFlags ::
  (Transformer m) =>
  VName ->
  SubExp ->
  SubExp ->
  VName ->
  VName ->
  m ()
transformFlatMapFlags flag_pat w m shape offset = do
  let flag_t = arrayOfRow (Prim Bool) m
  flags_init <- letExp "flatmap_flags" $ BasicOp $ Replicate (Shape [m]) (constant False)
  flags_p <- newParam "flatmap_flags" $ toDecl flag_t Unique
  j <- newVName "j"
  let flag_form = ForLoop j Int64 w
  shape_t <- lookupType shape
  offset_t <- lookupType offset
  flag_body <- runBodyBuilder $
    localScope (scopeOfLoopForm flag_form <> scopeOfFParams [flags_p]) $ do
      sz <- letSubExp "flatmap_sz" $ BasicOp $ Index shape $ fullSlice shape_t [DimFix $ Var j]
      off <- letSubExp "flatmap_off" $ BasicOp $ Index offset $ fullSlice offset_t [DimFix $ Var j]
      nonempty <-
        letSubExp "flatmap_nonempty" . BasicOp $
          CmpOp (CmpSlt Int64) (intConst Int64 0) sz
      flags' <-
        letSubExp "flatmap_flags"
          =<< eIf
            (eSubExp nonempty)
            ( buildBody_
                $ fmap (pure . varRes)
                  . letInPlace "flatmap_flags" (paramName flags_p) (fullSlice flag_t [DimFix off])
                $ BasicOp (SubExp (constant True))
            )
            (buildBody_ $ pure [varRes $ paramName flags_p])
      pure [subExpRes flags']
  letBindNames [flag_pat] $ Loop [(flags_p, Var flags_init)] flag_form flag_body

-- | Sequentialise a single Screma.
transformScrema ::
  (Transformer m) =>
  Pat dec ->
  SubExp ->
  [VName] ->
  ScremaForm (Rep m) ->
  m ()
transformScrema pat w arrs form@(ScremaForm map_lam scans reds post_lam) = do
  -- See Note [Translation of Screma].
  --
  -- Start by combining all the reduction and scan parts into a single
  -- operator
  let Reduce _ red_lam red_nes = singleReduce reds
      Scan scan_lam scan_nes = singleScan scans
      (red_ts, post_ts) =
        splitAt (length red_nes) $ scremaType w form

  post_arrs <- resultArray arrs post_ts

  scanacc_params <- mapM (newParam "scanacc" . flip toDecl Nonunique) $ lambdaReturnType scan_lam
  redout_params <- mapM (newParam "redout" . flip toDecl Nonunique) $ lambdaReturnType red_lam
  out_params <- mapM (newParam "out" . flip toDecl Unique) post_ts

  arr_ts <- mapM lookupType arrs
  let paramForAcc (Acc c _ _ _) = find (f . paramType) out_params
        where
          f (Acc c2 _ _ _) = c == c2
          f _ = False
      paramForAcc _ = Nothing

  let merge =
        concat
          [ zip scanacc_params scan_nes,
            zip redout_params red_nes,
            zip out_params $ map Var post_arrs
          ]
  i <- newVName "i"
  let loopform = ForLoop i Int64 w
      lam_cons = consumedByLambda $ Alias.analyseLambda mempty map_lam

  loop_body <- runBodyBuilder
    . localScope (scopeOfFParams (map fst merge) <> scopeOfLoopForm loopform)
    $ do
      -- Bind the parameters to the lambda.
      forM_ (zip3 (lambdaParams map_lam) arrs arr_ts) $ \(p, arr, arr_t) ->
        case paramForAcc arr_t of
          Just acc_out_p ->
            letBindNames [paramName p] . BasicOp $
              SubExp $
                Var $
                  paramName acc_out_p
          Nothing
            | paramName p `nameIn` lam_cons -> do
                p' <-
                  letExp (baseName (paramName p)) . BasicOp $
                    Index arr $
                      fullSlice arr_t [DimFix $ Var i]
                letBindNames [paramName p] $ BasicOp $ Replicate mempty $ Var p'
            | otherwise ->
                letBindNames [paramName p] . BasicOp . Index arr $
                  fullSlice arr_t [DimFix $ Var i]

      -- Insert the statements of the lambda.  We have taken care to
      -- ensure that the parameters are bound at this point.
      mapM_ addStm $ bodyStms $ lambdaBody map_lam
      -- Split into scan results, reduce results, and map results.
      let (scan_res, red_res, map_res) =
            splitAt3 (length scan_nes) (length red_nes) $
              bodyResult $
                lambdaBody map_lam

      scan_res' <-
        eLambda scan_lam $
          map (pure . BasicOp . SubExp) $
            map (Var . paramName) scanacc_params ++ map resSubExp scan_res
      red_res' <-
        eLambda red_lam $
          map (pure . BasicOp . SubExp) $
            map (Var . paramName) redout_params ++ map resSubExp red_res

      let res = scan_res' <> map_res
          param_bind = resSubExp <$> res
          certs = resCerts <$> res
      forM_ (zip3 (paramName <$> lambdaParams post_lam) param_bind certs) $
        \(par, v, cs) -> do
          certifying cs $ letBindNames [par] $ BasicOp $ SubExp v

      mapM_ addStm $ bodyStms $ lambdaBody post_lam

      let post_res = bodyResult $ lambdaBody post_lam
      outarrs <-
        certifying (foldMap resCerts post_res) $
          letwith (map paramName out_params) (Var i) $
            map resSubExp post_res

      pure . concat $
        [ scan_res',
          red_res',
          varsRes outarrs
        ]

  discards <- replicateM (length scanacc_params) (newVName "discard")

  -- Screma requires alias-free results, so reduction results are bound to fresh
  -- names, so that we can copy the array-typed ones.
  let (red_pat_names, post_pat_names) = splitAt (length red_nes) $ patNames pat
  red_names <- mapM newName red_pat_names

  letBindNames (discards ++ red_names ++ post_pat_names) $
    Loop merge loopform loop_body

  forM_ (zip3 red_pat_names red_names red_ts) $ \(to, from, t) ->
    letBindNames [to] . BasicOp $
      case t of
        Array {} -> Replicate mempty $ Var from
        _ -> SubExp $ Var from

-- | Transform a single 'SOAC' into a do-loop.  The body of the lambda
-- is untouched, and may or may not contain further 'SOAC's depending
-- on the given rep.
transformSOAC ::
  (Transformer m) =>
  Pat (LetDec (Rep m)) ->
  SOAC (Rep m) ->
  m ()
transformSOAC _ JVP {} =
  error "transformSOAC: unhandled JVP"
transformSOAC _ VJP {} =
  error "transformSOAC: unhandled VJP"
transformSOAC _ WithVJP {} =
  error "transformSOAC: unhandled WithVJP"
transformSOAC pat (FlatMap w arrs lam) =
  transformFlatMap pat w arrs lam
transformSOAC pat (Screma w arrs form) =
  transformScrema pat w arrs form
transformSOAC pat (Stream w arrs nes lam) = do
  -- Create a loop that repeatedly applies the lambda body to a
  -- chunksize of 1.  Hopefully this will lead to this outer loop
  -- being the only one, as all the innermost one can be simplified
  -- away (as they will have one iteration each).
  let (chunk_size_param, fold_params, chunk_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam
      mapout_ts = map (`setOuterSize` w) $ drop (length nes) $ lambdaReturnType lam

  mapout_initial <- resultArray arrs mapout_ts
  mapout_params <- forM mapout_ts $ \t ->
    newParam "stream_mapout" $ toDecl t Unique
  let mapout_merge = zip mapout_params $ map Var mapout_initial

  let paramForAcc (Acc c _ _ _) = find (f . paramType) mapout_params
        where
          f (Acc c2 _ _ _) = c == c2
          f _ = False
      paramForAcc _ = Nothing

  -- We need to copy the neutral elements because they may be consumed
  -- in the body of the Stream.
  let copyIfArray se = do
        se_t <- subExpType se
        case (se_t, se) of
          (Array {}, Var v) ->
            letSubExp (baseName v) $ BasicOp $ Replicate mempty se
          _ -> pure se
  nes' <- mapM copyIfArray nes

  let onType t = t `toDecl` Unique
      merge = zip (map (fmap onType) fold_params) nes' ++ mapout_merge
      merge_params = map fst merge

  i <- newVName "i"

  let loop_form = ForLoop i Int64 w

  letBindNames [paramName chunk_size_param] . BasicOp . SubExp $
    intConst Int64 1

  arrs_ts <- mapM lookupType arrs
  loop_body <- runBodyBuilder $
    localScope (scopeOfLoopForm loop_form <> scopeOfFParams merge_params) $ do
      let slice = [DimSlice (Var i) (Var (paramName chunk_size_param)) (intConst Int64 1)]
      forM_ (zip3 chunk_params arrs arrs_ts) $ \(p, arr, arr_t) ->
        case paramForAcc arr_t of
          Just acc_out_p ->
            letBindNames [paramName p] . BasicOp . SubExp $
              Var (paramName acc_out_p)
          Nothing ->
            letBindNames [paramName p] . BasicOp $
              Index arr (fullSlice (paramType p) slice)

      (res, mapout_res) <- splitAt (length nes) <$> bodyBind (lambdaBody lam)

      res' <- mapM (copyIfArray . resSubExp) res

      mapout_res' <- forM (zip mapout_params mapout_res) $ \(p, SubExpRes cs se) ->
        certifying cs . letSubExp "mapout_res" . BasicOp $
          if isAcc (paramType p)
            then SubExp se
            else Update Unsafe (paramName p) (fullSlice (paramType p) slice) se

      pure $ subExpsRes $ res' ++ mapout_res'

  letBind pat $ Loop merge loop_form loop_body
transformSOAC pat (Hist len imgs ops bucket_fun) = do
  iter <- newVName "iter"

  -- Bind arguments to parameters for the merge-variables.
  hists_ts <- mapM lookupType $ concatMap histDest ops
  hists_out <- mapM (newIdent "dests") hists_ts
  let merge = loopMerge hists_out $ concatMap (map Var . histDest) ops

  -- Bind lambda-bodies for operators.
  let iter_scope = M.insert iter (IndexName Int64) $ scopeOfFParams $ map fst merge
  loopBody <- runBodyBuilder . localScope iter_scope $ do
    -- Bind images to parameters of bucket function.
    imgs' <- forM imgs $ \img -> do
      img_t <- lookupType img
      letSubExp "pixel" $ BasicOp $ Index img $ fullSlice img_t [DimFix $ Var iter]
    imgs'' <- map resSubExp <$> bindLambda bucket_fun (map (BasicOp . SubExp) imgs')

    -- Split out values from bucket function.
    let lens = sum $ map (shapeRank . histShape) ops
        ops_inds = chunks (map (shapeRank . histShape) ops) (take lens imgs'')
        vals = chunks (map (length . lambdaReturnType . histOp) ops) $ drop lens imgs''
        hists_out' =
          chunks (map (length . lambdaReturnType . histOp) ops) $
            map identName hists_out

    hists_out'' <- forM (zip4 hists_out' ops ops_inds vals) $ \(hist, op, idxs, val) -> do
      -- Check whether the indexes are in-bound.  If they are not, we
      -- return the histograms unchanged.
      let outside_bounds_branch = buildBody_ $ pure $ varsRes hist
          oob = case hist of
            [] -> eSubExp $ constant True
            arr : _ -> eOutOfBounds arr $ map eSubExp idxs

      letTupExp "new_histo" <=< eIf oob outside_bounds_branch $
        buildBody_ $ do
          -- Read values from histogram.
          h_val <- forM hist $ \arr -> do
            arr_t <- lookupType arr
            letSubExp "read_hist" $ BasicOp $ Index arr $ fullSlice arr_t $ map DimFix idxs

          -- Apply operator.
          h_val' <- bindLambda (histOp op) $ map (BasicOp . SubExp) $ h_val ++ val

          -- Write values back to histograms.
          hist' <- forM (zip hist h_val') $ \(arr, SubExpRes cs v) -> do
            arr_t <- lookupType arr
            certifying cs . letInPlace "hist_out" arr (fullSlice arr_t $ map DimFix idxs) $
              BasicOp $
                SubExp v

          pure $ varsRes hist'

    pure $ varsRes $ concat hists_out''

  -- Wrap up the above into a for-loop.
  letBind pat $ Loop merge (ForLoop iter Int64 len) loopBody

-- | Recursively first-order-transform a lambda.
transformLambda ::
  ( MonadFreshNames m,
    Buildable rep,
    BuilderOps rep,
    LocalScope somerep m,
    SameScope somerep rep,
    LetDec rep ~ LetDec SOACS,
    Alias.AliasableRep rep
  ) =>
  GLambda SOACS t ->
  m (AST.GLambda rep t)
transformLambda (Lambda params rettype body) = do
  body' <-
    fmap fst . runBuilder $
      localScope (scopeOfLParams params) $
        transformBody body
  pure $ Lambda params rettype body'

letwith :: (Transformer m) => [VName] -> SubExp -> [SubExp] -> m [VName]
letwith ks i vs = do
  let update k v = do
        k_t <- lookupType k
        case k_t of
          Acc {} ->
            letExp "lw_acc" $ BasicOp $ SubExp v
          _ ->
            letInPlace "lw_dest" k (fullSlice k_t [DimFix i]) $ BasicOp $ SubExp v
  zipWithM update ks vs

bindLambda ::
  (Transformer m) =>
  AST.GLambda (Rep m) t ->
  [AST.Exp (Rep m)] ->
  m Result
bindLambda (Lambda params _ body) args = do
  forM_ (zip params args) $ \(param, arg) ->
    if primType $ paramType param
      then letBindNames [paramName param] arg
      else letBindNames [paramName param] =<< eCopy (pure arg)
  bodyBind body

loopMerge :: [Ident] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge vars = loopMerge' $ map (,Unique) vars

loopMerge' :: [(Ident, Uniqueness)] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge' vars vals =
  [ (Param mempty pname $ toDecl ptype u, val)
  | ((Ident pname ptype, u), val) <- zip vars vals
  ]

-- Note [Translation of Screma]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Screma is the most general SOAC.  It is translated by constructing
-- a loop that contains several groups of parameters, in this order:
--
-- (0) Scan accumulator, initialised with neutral element.
-- (1) Scan results, initialised with Scratch.
-- (2) Reduce results (also functioning as accumulators),
--     initialised with neutral element.
-- (3) Map results, mostly initialised with Scratch.
--
-- However, category (3) is a little more tricky in the case where one
-- of the results is an Acc.  In that case, the result is not an
-- array, but another Acc.  Any Acc result of a Map must correspond to
-- an Acc that is an input to the map, and the result is initialised
-- to be that input.  This requires a 1:1 relationship between Acc
-- inputs and Acc outputs, which the type checker should enforce.
-- There is no guarantee that the map results appear in any particular
-- order (e.g. accumulator results before non-accumulator results), so
-- we need to do a little sleuthing to establish the relationship.
--
-- Inside the loop, the non-Acc parameters to map_lam become for-in
-- parameters.  Acc parameters refer to the loop parameters for the
-- corresponding Map result instead.
--
-- Intuitively, a Screma(w,
--                       (scan_op, scan_ne),
--                       (red_op, red_ne),
--                       map_fn,
--                       {acc_input, arr_input})
--
-- then becomes
--
-- loop (scan_acc, scan_arr, red_acc, map_acc, map_arr) =
--   for i < w, x in arr_input do
--     let (a,b,map_acc',d) = map_fn(map_acc, x)
--     let scan_acc' = scan_op(scan_acc, a)
--     let scan_arr[i] = scan_acc'
--     let red_acc' = red_op(red_acc, b)
--     let map_arr[i] = d
--     in (scan_acc', scan_arr', red_acc', map_acc', map_arr)
--
-- A similar operation is done for Stream.
