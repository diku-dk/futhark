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
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (find, zip4)
import Data.Map.Strict qualified as M
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
    soacTransform = identitySOACMapper {mapOnSOACLambda = transformLambda}
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
transformSOAC pat (Screma w arrs (ScremaForm map_lam scans reds post_lam)) = do
  -- See Note [Translation of Screma].
  --
  -- Start by combining all the reduction and scan parts into a single
  -- operator
  let Reduce _ red_lam red_nes = singleReduce reds
      Scan scan_lam scan_nes = singleScan scans
      ts = lambdaReturnType post_lam

  scanacc_params <- mapM (newParam "scanacc" . flip toDecl Nonunique) $ lambdaReturnType scan_lam
  redout_params <- mapM (newParam "redout" . flip toDecl Nonunique) $ lambdaReturnType red_lam
  out_params <- mapM (newParam "out" . flip toDecl Unique) ts

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
            zip out_params $ map Var arrs
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
                  letExp (baseString (paramName p)) . BasicOp $
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
      forM_ (zip (paramName <$> lambdaParams post_lam) param_bind) $
        \(par, v) -> do
          letBindNames [par] $ BasicOp $ SubExp v

      mapM_ addStm $ bodyStms $ lambdaBody post_lam

      outarrs <-
        certifying (foldMap resCerts res) $
          letwith (map paramName out_params) (Var i) $
            map resSubExp res

      pure . concat $
        [ scan_res',
          red_res',
          varsRes outarrs
        ]

  -- We need to discard the final scan accumulators, as they are not
  -- bound in the original pattern.
  names <-
    (++ patNames pat)
      <$> replicateM (length scanacc_params) (newVName "discard")
  letBindNames names $ Loop merge loopform loop_body
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
            letSubExp (baseString v) $ BasicOp $ Replicate mempty se
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
  Lambda SOACS ->
  m (AST.Lambda rep)
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
  AST.Lambda (Rep m) ->
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
