{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive,
-- and - it's certainly worth considering when we can express such
-- transformations in-place.
module Futhark.Transform.FirstOrderTransform
  ( transformFunDef,
    transformConsts,
    FirstOrderLore,
    Transformer,
    transformStmRecursively,
    transformLambda,
    transformSOAC,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Function ((&))
import Data.List (zip4)
import qualified Data.Map.Strict as M
import qualified Futhark.IR as AST
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Util (chunks, splitAt3)

-- | The constraints that must hold for a lore in order to be the
-- target of first-order transformation.
type FirstOrderLore lore =
  ( Bindable lore,
    BinderOps lore,
    LetDec SOACS ~ LetDec lore,
    LParamInfo SOACS ~ LParamInfo lore
  )

-- | First-order-transform a single function, with the given scope
-- provided by top-level constants.
transformFunDef ::
  (MonadFreshNames m, FirstOrderLore tolore) =>
  Scope tolore ->
  FunDef SOACS ->
  m (AST.FunDef tolore)
transformFunDef consts_scope (FunDef entry attrs fname rettype params body) = do
  (body', _) <- modifyNameSource $ runState $ runBinderT m consts_scope
  return $ FunDef entry attrs fname rettype params body'
  where
    m = localScope (scopeOfFParams params) $ insertStmsM $ transformBody body

-- | First-order-transform these top-level constants.
transformConsts ::
  (MonadFreshNames m, FirstOrderLore tolore) =>
  Stms SOACS ->
  m (AST.Stms tolore)
transformConsts stms =
  fmap snd $ modifyNameSource $ runState $ runBinderT m mempty
  where
    m = mapM_ transformStmRecursively stms

-- | The constraints that a monad must uphold in order to be used for
-- first-order transformation.
type Transformer m =
  ( MonadBinder m,
    LocalScope (Lore m) m,
    Bindable (Lore m),
    BinderOps (Lore m),
    LParamInfo SOACS ~ LParamInfo (Lore m)
  )

transformBody ::
  (Transformer m, LetDec (Lore m) ~ LetDec SOACS) =>
  Body ->
  m (AST.Body (Lore m))
transformBody (Body () bnds res) = insertStmsM $ do
  mapM_ transformStmRecursively bnds
  return $ resultBody res

-- | First transform any nested t'Body' or t'Lambda' elements, then
-- apply 'transformSOAC' if the expression is a SOAC.
transformStmRecursively ::
  (Transformer m, LetDec (Lore m) ~ LetDec SOACS) =>
  Stm ->
  m ()
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
          mapOnRetType = return,
          mapOnBranchType = return,
          mapOnFParam = return,
          mapOnLParam = return,
          mapOnOp = error "Unhandled Op in first order transform"
        }

-- | Transform a single 'SOAC' into a do-loop.  The body of the lambda
-- is untouched, and may or may not contain further 'SOAC's depending
-- on the given lore.
transformSOAC ::
  Transformer m =>
  AST.Pattern (Lore m) ->
  SOAC (Lore m) ->
  m ()
transformSOAC pat (Screma w form@(ScremaForm scans reds map_lam) arrs) = do
  -- Start by combining all the reduction parts into a single operator
  let Reduce _ red_lam red_nes = singleReduce reds
      Scan scan_lam scan_nes = singleScan scans
      (scan_arr_ts, _red_ts, map_arr_ts) =
        splitAt3 (length scan_nes) (length red_nes) $ scremaType w form
  scan_arrs <- resultArray scan_arr_ts
  map_arrs <- resultArray map_arr_ts

  -- We construct a loop that contains several groups of merge
  -- parameters:
  --
  -- (0) scan accumulator.
  -- (1) scan results.
  -- (2) reduce results (and accumulator).
  -- (3) map results.
  --
  -- Inside the loop, the parameters to map_lam become for-in
  -- parameters.

  scanacc_params <- mapM (newParam "scanacc" . flip toDecl Nonunique) $ lambdaReturnType scan_lam
  scanout_params <- mapM (newParam "scanout" . flip toDecl Unique) scan_arr_ts
  redout_params <- mapM (newParam "redout" . flip toDecl Nonunique) $ lambdaReturnType red_lam
  mapout_params <- mapM (newParam "mapout" . flip toDecl Unique) map_arr_ts

  let merge =
        concat
          [ zip scanacc_params scan_nes,
            zip scanout_params $ map Var scan_arrs,
            zip redout_params red_nes,
            zip mapout_params $ map Var map_arrs
          ]
  i <- newVName "i"
  let loopform = ForLoop i Int64 w []

  loop_body <- runBodyBinder $
    localScope (scopeOfFParams $ map fst merge) $
      inScopeOf loopform $ do
        forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
          arr_t <- lookupType arr
          letBindNames [paramName p] $
            BasicOp $
              Index arr $
                fullSlice arr_t [DimFix $ Var i]

        -- Insert the statements of the lambda.  We have taken care to
        -- ensure that the parameters are bound at this point.
        mapM_ addStm $ bodyStms $ lambdaBody map_lam
        -- Split into scan results, reduce results, and map results.
        let (scan_res, red_res, map_res) =
              splitAt3 (length scan_nes) (length red_nes) $
                bodyResult $ lambdaBody map_lam

        scan_res' <-
          eLambda scan_lam $
            map (pure . BasicOp . SubExp) $
              map (Var . paramName) scanacc_params ++ scan_res
        red_res' <-
          eLambda red_lam $
            map (pure . BasicOp . SubExp) $
              map (Var . paramName) redout_params ++ red_res

        -- Write the scan accumulator to the scan result arrays.
        scan_outarrs <-
          letwith (map paramName scanout_params) (pexp (Var i)) $
            map (BasicOp . SubExp) scan_res'

        -- Write the map results to the map result arrays.
        map_outarrs <-
          letwith (map paramName mapout_params) (pexp (Var i)) $
            map (BasicOp . SubExp) map_res

        return $
          resultBody $
            concat
              [ scan_res',
                map Var scan_outarrs,
                red_res',
                map Var map_outarrs
              ]

  -- We need to discard the final scan accumulators, as they are not
  -- bound in the original pattern.
  names <-
    (++ patternNames pat)
      <$> replicateM (length scanacc_params) (newVName "discard")
  letBindNames names $ DoLoop [] merge loopform loop_body
transformSOAC pat (Stream w stream_form lam nes arrs) = do
  -- Create a loop that repeatedly applies the lambda body to a
  -- chunksize of 1.  Hopefully this will lead to this outer loop
  -- being the only one, as all the innermost one can be simplified
  -- array (as they will have one iteration each).
  let (chunk_size_param, fold_params, chunk_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

  mapout_merge <- forM (drop (length nes) $ lambdaReturnType lam) $ \t ->
    let t' = t `setOuterSize` w
        scratch = BasicOp $ Scratch (elemType t') (arrayDims t')
     in (,)
          <$> newParam "stream_mapout" (toDecl t' Unique)
          <*> letSubExp "stream_mapout_scratch" scratch

  let merge =
        zip (map (fmap (`toDecl` Nonunique)) fold_params) nes
          ++ mapout_merge
      merge_params = map fst merge
      mapout_params = map fst mapout_merge

  i <- newVName "i"

  let loop_form = ForLoop i Int64 w []

  letBindNames [paramName chunk_size_param] $
    BasicOp $ SubExp $ intConst Int64 1

  loop_body <- runBodyBinder $
    localScope
      ( scopeOf loop_form
          <> scopeOfFParams merge_params
      )
      $ do
        let slice =
              [DimSlice (Var i) (Var (paramName chunk_size_param)) (intConst Int64 1)]
        forM_ (zip chunk_params arrs) $ \(p, arr) ->
          letBindNames [paramName p] $
            BasicOp $
              Index arr $
                fullSlice (paramType p) slice

        (res, mapout_res) <- splitAt (length nes) <$> bodyBind (lambdaBody lam)

        mapout_res' <- forM (zip mapout_params mapout_res) $ \(p, se) ->
          letSubExp "mapout_res" $
            BasicOp $
              Update
                (paramName p)
                (fullSlice (paramType p) slice)
                se

        resultBodyM $ res ++ mapout_res'

  letBind pat $ DoLoop [] merge loop_form loop_body
transformSOAC pat (Scatter len lam ivs as) = do
  iter <- newVName "write_iter"

  let (as_ws, as_ns, as_vs) = unzip3 as
  ts <- mapM lookupType as_vs
  asOuts <- mapM (newIdent "write_out") ts

  let ivsLen = zipWith (*) as_ns $ map length as_ws

  -- Scatter is in-place, so we use the input array as the output array.
  let merge = loopMerge asOuts $ map Var as_vs
  loopBody <- runBodyBinder $
    localScope
      ( M.insert iter (IndexName Int64) $
          scopeOfFParams $ map fst merge
      )
      $ do
        ivs' <- forM ivs $ \iv -> do
          iv_t <- lookupType iv
          letSubExp "write_iv" $ BasicOp $ Index iv $ fullSlice iv_t [DimFix $ Var iter]
        ivs'' <- bindLambda lam (map (BasicOp . SubExp) ivs')

        let indexes =
              take (sum ivsLen) ivs''
                & chunks (concat $ zipWith (\ws n -> replicate n (length ws)) as_ws as_ns)
                & chunks as_ns
            values = chunks as_ns $ drop (sum ivsLen) ivs''

        ress <- forM (zip3 indexes values (map identName asOuts)) $ \(indexes', values', arr) -> do
          let saveInArray arr' (indexCur, valueCur) =
                letExp "write_out" =<< eWriteArray arr' (map eSubExp indexCur) (eSubExp valueCur)

          foldM saveInArray arr $ zip indexes' values'
        return $ resultBody (map Var ress)
  letBind pat $ DoLoop [] merge (ForLoop iter Int64 len []) loopBody
transformSOAC pat (Hist len ops bucket_fun imgs) = do
  iter <- newVName "iter"

  -- Bind arguments to parameters for the merge-variables.
  hists_ts <- mapM lookupType $ concatMap histDest ops
  hists_out <- mapM (newIdent "dests") hists_ts
  let merge = loopMerge hists_out $ concatMap (map Var . histDest) ops

  -- Bind lambda-bodies for operators.
  loopBody <- runBodyBinder $
    localScope
      ( M.insert iter (IndexName Int64) $
          scopeOfFParams $ map fst merge
      )
      $ do
        -- Bind images to parameters of bucket function.
        imgs' <- forM imgs $ \img -> do
          img_t <- lookupType img
          letSubExp "pixel" $ BasicOp $ Index img $ fullSlice img_t [DimFix $ Var iter]
        imgs'' <- bindLambda bucket_fun $ map (BasicOp . SubExp) imgs'

        -- Split out values from bucket function.
        let lens = length ops
            inds = take lens imgs''
            vals = chunks (map (length . lambdaReturnType . histOp) ops) $ drop lens imgs''
            hists_out' =
              chunks (map (length . lambdaReturnType . histOp) ops) $
                map identName hists_out

        hists_out'' <- forM (zip4 hists_out' ops inds vals) $ \(hist, op, idx, val) -> do
          -- Check whether the indexes are in-bound.  If they are not, we
          -- return the histograms unchanged.
          let outside_bounds_branch = insertStmsM $ resultBodyM $ map Var hist
              oob = case hist of
                [] -> eSubExp $ constant True
                arr : _ -> eOutOfBounds arr [eSubExp idx]

          letTupExp "new_histo"
            <=< eIf oob outside_bounds_branch
            $ do
              -- Read values from histogram.
              h_val <- forM hist $ \arr -> do
                arr_t <- lookupType arr
                letSubExp "read_hist" $ BasicOp $ Index arr $ fullSlice arr_t [DimFix idx]

              -- Apply operator.
              h_val' <-
                bindLambda (histOp op) $
                  map (BasicOp . SubExp) $ h_val ++ val

              -- Write values back to histograms.
              hist' <- forM (zip hist h_val') $ \(arr, v) -> do
                arr_t <- lookupType arr
                letInPlace "hist_out" arr (fullSlice arr_t [DimFix idx]) $
                  BasicOp $ SubExp v

              return $ resultBody $ map Var hist'

        return $ resultBody $ map Var $ concat hists_out''

  -- Wrap up the above into a for-loop.
  letBind pat $ DoLoop [] merge (ForLoop iter Int64 len []) loopBody

-- | Recursively first-order-transform a lambda.
transformLambda ::
  ( MonadFreshNames m,
    Bindable lore,
    BinderOps lore,
    LocalScope somelore m,
    SameScope somelore lore,
    LetDec lore ~ LetDec SOACS
  ) =>
  Lambda ->
  m (AST.Lambda lore)
transformLambda (Lambda params body rettype) = do
  body' <-
    runBodyBinder $
      localScope (scopeOfLParams params) $
        transformBody body
  return $ Lambda params body' rettype

resultArray :: Transformer m => [Type] -> m [VName]
resultArray = mapM oneArray
  where
    oneArray t = letExp "result" $ BasicOp $ Scratch (elemType t) (arrayDims t)

letwith ::
  Transformer m =>
  [VName] ->
  m (AST.Exp (Lore m)) ->
  [AST.Exp (Lore m)] ->
  m [VName]
letwith ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  let update k v = do
        k_t <- lookupType k
        letInPlace "lw_dest" k (fullSlice k_t [DimFix i']) $ BasicOp $ SubExp v
  zipWithM update ks vs'

pexp :: Applicative f => SubExp -> f (AST.Exp lore)
pexp = pure . BasicOp . SubExp

bindLambda ::
  Transformer m =>
  AST.Lambda (Lore m) ->
  [AST.Exp (Lore m)] ->
  m [SubExp]
bindLambda (Lambda params body _) args = do
  forM_ (zip params args) $ \(param, arg) ->
    if primType $ paramType param
      then letBindNames [paramName param] arg
      else letBindNames [paramName param] =<< eCopy (pure arg)
  bodyBind body

loopMerge :: [Ident] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge vars = loopMerge' $ zip vars $ repeat Unique

loopMerge' :: [(Ident, Uniqueness)] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge' vars vals =
  [ (Param pname $ toDecl ptype u, val)
    | ((Ident pname ptype, u), val) <- zip vars vals
  ]
