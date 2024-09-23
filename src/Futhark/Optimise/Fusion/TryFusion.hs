{-# LANGUAGE TypeFamilies #-}

-- | Facilities for fusing two SOACs.
--
-- When the fusion algorithm decides that it's worth fusing two SOAC
-- statements, this is the module that tries to see if that's
-- possible.  May involve massaging either producer or consumer in
-- various ways.
module Futhark.Optimise.Fusion.TryFusion
  ( FusedSOAC (..),
    Mode (..),
    attemptFusion,
  )
where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, tails, (\\))
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.MapNest qualified as MapNest
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as Futhark
import Futhark.Optimise.Fusion.Composing
import Futhark.Pass.ExtractKernels.ISRWIM (rwimPossible)
import Futhark.Transform.Rename (renameLambda)
import Futhark.Transform.Substitute
import Futhark.Util (splitAt3)

newtype TryFusion a
  = TryFusion
      ( ReaderT
          (Scope SOACS)
          (StateT VNameSource Maybe)
          a
      )
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadFail,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

tryFusion ::
  (MonadFreshNames m) =>
  TryFusion a ->
  Scope SOACS ->
  m (Maybe a)
tryFusion (TryFusion m) types = modifyNameSource $ \src ->
  case runStateT (runReaderT m types) src of
    Just (x, src') -> (Just x, src')
    Nothing -> (Nothing, src)

liftMaybe :: Maybe a -> TryFusion a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = pure x

type SOAC = SOAC.SOAC SOACS

type MapNest = MapNest.MapNest SOACS

inputToOutput :: SOAC.Input -> Maybe (SOAC.ArrayTransform, SOAC.Input)
inputToOutput (SOAC.Input ts ia iat) =
  case SOAC.viewf ts of
    t SOAC.:< ts' -> Just (t, SOAC.Input ts' ia iat)
    SOAC.EmptyF -> Nothing

-- | A fused SOAC contains a bit of extra information.
data FusedSOAC = FusedSOAC
  { -- | The actual SOAC.
    fsSOAC :: SOAC,
    -- | A transformation to be applied to *all* results of the SOAC.
    fsOutputTransform :: SOAC.ArrayTransforms,
    -- | The outputs of the SOAC (i.e. the names in the pattern that
    -- the result of this SOAC should be bound to).
    fsOutNames :: [VName]
  }
  deriving (Show)

inputs :: FusedSOAC -> [SOAC.Input]
inputs = SOAC.inputs . fsSOAC

setInputs :: [SOAC.Input] -> FusedSOAC -> FusedSOAC
setInputs inps ker = ker {fsSOAC = inps `SOAC.setInputs` fsSOAC ker}

tryOptimizeSOAC ::
  Mode ->
  Names ->
  [VName] ->
  SOAC ->
  FusedSOAC ->
  TryFusion FusedSOAC
tryOptimizeSOAC mode unfus_nms outVars soac ker = do
  (soac', ots) <- optimizeSOAC Nothing soac mempty
  let ker' = map (addInitialTransformIfRelevant ots) (inputs ker) `setInputs` ker
      outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
      ker'' = fixInputTypes outIdents ker'
  applyFusionRules mode unfus_nms outVars soac' ker''
  where
    addInitialTransformIfRelevant ots inp
      | SOAC.inputArray inp `elem` outVars =
          SOAC.addInitialTransforms ots inp
      | otherwise =
          inp

tryOptimizeKernel ::
  Mode ->
  Names ->
  [VName] ->
  SOAC ->
  FusedSOAC ->
  TryFusion FusedSOAC
tryOptimizeKernel mode unfus_nms outVars soac ker = do
  ker' <- optimizeKernel (Just outVars) ker
  applyFusionRules mode unfus_nms outVars soac ker'

tryExposeInputs ::
  Mode ->
  Names ->
  [VName] ->
  SOAC ->
  FusedSOAC ->
  TryFusion FusedSOAC
tryExposeInputs mode unfus_nms outVars soac ker = do
  (ker', ots) <- exposeInputs outVars ker
  if SOAC.nullTransforms ots
    then fuseSOACwithKer mode unfus_nms outVars soac ker'
    else do
      guard $ unfus_nms == mempty
      (soac', ots') <- pullOutputTransforms soac ots
      let outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
          ker'' = fixInputTypes outIdents ker'
      if SOAC.nullTransforms ots'
        then applyFusionRules mode unfus_nms outVars soac' ker''
        else fail "tryExposeInputs could not pull SOAC transforms"

fixInputTypes :: [Ident] -> FusedSOAC -> FusedSOAC
fixInputTypes outIdents ker =
  ker {fsSOAC = fixInputTypes' $ fsSOAC ker}
  where
    fixInputTypes' soac =
      map fixInputType (SOAC.inputs soac) `SOAC.setInputs` soac
    fixInputType (SOAC.Input ts v _)
      | Just v' <- find ((== v) . identName) outIdents =
          SOAC.Input ts v $ identType v'
    fixInputType inp = inp

applyFusionRules ::
  Mode ->
  Names ->
  [VName] ->
  SOAC ->
  FusedSOAC ->
  TryFusion FusedSOAC
applyFusionRules mode unfus_nms outVars soac ker =
  tryOptimizeSOAC mode unfus_nms outVars soac ker
    <|> tryOptimizeKernel mode unfus_nms outVars soac ker
    <|> fuseSOACwithKer mode unfus_nms outVars soac ker
    <|> tryExposeInputs mode unfus_nms outVars soac ker

-- | Whether we are doing horizontal or vertical fusion.  Note that
-- vertical also includes "diagonal" fusion, where some producer
-- results are also produced by the final SOAC.
data Mode = Horizontal | Vertical

-- | Attempt fusing the producer into the consumer.
attemptFusion ::
  (HasScope SOACS m, MonadFreshNames m) =>
  Mode ->
  -- | Outputs of the producer that should still be output by the
  -- fusion result (corresponding to "diagonal fusion").
  Names ->
  -- | The outputs of the SOAC.
  [VName] ->
  SOAC ->
  FusedSOAC ->
  m (Maybe FusedSOAC)
attemptFusion mode unfus_nms outVars soac ker = do
  scope <- askScope
  tryFusion (applyFusionRules mode unfus_nms outVars soac ker) scope

-- | Check that the consumer does not use any scan or reduce results.
scremaFusionOK :: ([VName], [VName]) -> FusedSOAC -> Bool
scremaFusionOK (nonmap_outs, _map_outs) ker =
  all (`notElem` nonmap_outs) $ mapMaybe SOAC.isVarishInput (inputs ker)

-- | Check that the consumer uses all the outputs of the producer unmodified.
mapWriteFusionOK :: [VName] -> FusedSOAC -> Bool
mapWriteFusionOK outVars ker = all (`elem` inpIds) outVars
  where
    inpIds = mapMaybe SOAC.isVarishInput (inputs ker)

-- | The brain of this module: Fusing a SOAC with a Kernel.
fuseSOACwithKer ::
  Mode ->
  Names ->
  [VName] ->
  SOAC ->
  FusedSOAC ->
  TryFusion FusedSOAC
fuseSOACwithKer mode unfus_set outVars soac_p ker = do
  -- We are fusing soac_p into soac_c, i.e, the output of soac_p is going
  -- into soac_c.
  let soac_c = fsSOAC ker
      inp_p_arr = SOAC.inputs soac_p
      inp_c_arr = SOAC.inputs soac_c
      lam_p = SOAC.lambda soac_p
      lam_c = SOAC.lambda soac_c
      w = SOAC.width soac_p
      returned_outvars = filter (`nameIn` unfus_set) outVars
      success res_outnms res_soac = do
        -- Avoid name duplication, because the producer lambda is not
        -- removed from the program until much later.
        uniq_lam <- renameLambda $ SOAC.lambda res_soac
        pure $
          ker
            { fsSOAC = uniq_lam `SOAC.setLambda` res_soac,
              fsOutNames = res_outnms
            }

  -- Can only fuse SOACs with same width.
  guard $ SOAC.width soac_p == SOAC.width soac_c

  -- If we are getting rid of a producer output, then it must be used
  -- exclusively without any transformations.
  let ker_inputs = map SOAC.inputArray (inputs ker)
      okInput v inp = v /= SOAC.inputArray inp || isJust (SOAC.isVarishInput inp)
      inputOrUnfus v = all (okInput v) (inputs ker) || v `notElem` ker_inputs

  guard $ all inputOrUnfus outVars

  outPairs <- forM (zip outVars $ map rowType $ SOAC.typeOf soac_p) $ \(outVar, t) -> do
    outVar' <- newVName $ baseString outVar ++ "_elem"
    pure (outVar, Ident outVar' t)

  let mapLikeFusionCheck =
        let (res_lam, new_inp) = fuseMaps unfus_set lam_p inp_p_arr outPairs lam_c inp_c_arr
            (extra_nms, extra_rtps) =
              unzip $
                filter ((`nameIn` unfus_set) . fst) $
                  zip outVars $
                    map (stripArray 1) $
                      SOAC.typeOf soac_p
            res_lam' = res_lam {lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps}
         in (extra_nms, res_lam', new_inp)

  case (soac_c, soac_p, mode) of
    _ | SOAC.width soac_p /= SOAC.width soac_c -> fail "SOAC widths must match."
    (_, _, Horizontal)
      | not (SOAC.nullTransforms $ fsOutputTransform ker) ->
          fail "Horizontal fusion is invalid in the presence of output transforms."
    (_, _, Vertical)
      | unfus_set /= mempty,
        not (SOAC.nullTransforms $ fsOutputTransform ker) ->
          fail "Cannot perform diagonal fusion in the presence of output transforms."
    ( SOAC.Screma _ _ (ScremaForm scans_c reds_c _),
      SOAC.Screma _ _ (ScremaForm scans_p reds_p _),
      _
      )
        | scremaFusionOK (splitAt (Futhark.scanResults scans_p + Futhark.redResults reds_p) outVars) ker -> do
            let red_nes_p = concatMap redNeutral reds_p
                red_nes_c = concatMap redNeutral reds_c
                scan_nes_p = concatMap scanNeutral scans_p
                scan_nes_c = concatMap scanNeutral scans_c
                (res_lam', new_inp) =
                  fuseRedomap
                    unfus_set
                    outVars
                    lam_p
                    scan_nes_p
                    red_nes_p
                    inp_p_arr
                    outPairs
                    lam_c
                    scan_nes_c
                    red_nes_c
                    inp_c_arr
                (soac_p_scanout, soac_p_redout, _soac_p_mapout) =
                  splitAt3 (length scan_nes_p) (length red_nes_p) outVars
                (soac_c_scanout, soac_c_redout, soac_c_mapout) =
                  splitAt3 (length scan_nes_c) (length red_nes_c) $ fsOutNames ker
                unfus_arrs = returned_outvars \\ (soac_p_scanout ++ soac_p_redout)
            success
              ( soac_p_scanout
                  ++ soac_c_scanout
                  ++ soac_p_redout
                  ++ soac_c_redout
                  ++ soac_c_mapout
                  ++ unfus_arrs
              )
              $ SOAC.Screma
                w
                new_inp
                (ScremaForm (scans_p ++ scans_c) (reds_p ++ reds_c) res_lam')

    ------------------
    -- Scatter fusion --
    ------------------

    -- Map-Scatter fusion.
    --
    -- The 'inplace' mechanism for kernels already takes care of
    -- checking that the Scatter is not writing to any array used in
    -- the Map.
    ( SOAC.Scatter _len _ivs dests _lam,
      SOAC.Screma _ _ form,
      _
      )
        | isJust $ isMapSOAC form,
          -- 1. all arrays produced by the map are ONLY used (consumed)
          --    by the scatter, i.e., not used elsewhere.
          all (`notNameIn` unfus_set) outVars,
          -- 2. all arrays produced by the map are input to the scatter.
          mapWriteFusionOK outVars ker -> do
            let (extra_nms, res_lam', new_inp) = mapLikeFusionCheck
            success (fsOutNames ker ++ extra_nms) $
              SOAC.Scatter w new_inp dests res_lam'

    -- Map-Hist fusion.
    --
    -- The 'inplace' mechanism for kernels already takes care of
    -- checking that the Hist is not writing to any array used in
    -- the Map.
    ( SOAC.Hist _ _ ops _,
      SOAC.Screma _ _ form,
      _
      )
        | isJust $ isMapSOAC form,
          -- 1. all arrays produced by the map are ONLY used (consumed)
          --    by the hist, i.e., not used elsewhere.
          all (`notNameIn` unfus_set) outVars,
          -- 2. all arrays produced by the map are input to the scatter.
          mapWriteFusionOK outVars ker -> do
            let (extra_nms, res_lam', new_inp) = mapLikeFusionCheck
            success (fsOutNames ker ++ extra_nms) $
              SOAC.Hist w new_inp ops res_lam'

    -- Hist-Hist fusion
    ( SOAC.Hist _ _ ops_c _,
      SOAC.Hist _ _ ops_p _,
      Horizontal
      ) -> do
        let p_num_buckets = length ops_p
            c_num_buckets = length ops_c
            (body_p, body_c) = (lambdaBody lam_p, lambdaBody lam_c)
            body' =
              Body
                { bodyDec = bodyDec body_p, -- body_p and body_c have the same decorations
                  bodyStms = bodyStms body_p <> bodyStms body_c,
                  bodyResult =
                    take c_num_buckets (bodyResult body_c)
                      ++ take p_num_buckets (bodyResult body_p)
                      ++ drop c_num_buckets (bodyResult body_c)
                      ++ drop p_num_buckets (bodyResult body_p)
                }
            lam' =
              Lambda
                { lambdaParams = lambdaParams lam_c ++ lambdaParams lam_p,
                  lambdaBody = body',
                  lambdaReturnType =
                    replicate (c_num_buckets + p_num_buckets) (Prim int64)
                      ++ drop c_num_buckets (lambdaReturnType lam_c)
                      ++ drop p_num_buckets (lambdaReturnType lam_p)
                }
        success (fsOutNames ker ++ returned_outvars) $
          SOAC.Hist w (inp_c_arr <> inp_p_arr) (ops_c <> ops_p) lam'

    -- Scatter-write fusion.
    ( SOAC.Scatter _w_c ivs_c as_c _lam_c,
      SOAC.Scatter _w_p ivs_p as_p _lam_p,
      Horizontal
      ) -> do
        let zipW as_xs xs as_ys ys = xs_indices ++ ys_indices ++ xs_vals ++ ys_vals
              where
                (xs_indices, xs_vals) = splitScatterResults as_xs xs
                (ys_indices, ys_vals) = splitScatterResults as_ys ys
        let (body_p, body_c) = (lambdaBody lam_p, lambdaBody lam_c)
        let body' =
              Body
                { bodyDec = bodyDec body_p, -- body_p and body_c have the same decorations
                  bodyStms = bodyStms body_p <> bodyStms body_c,
                  bodyResult = zipW as_c (bodyResult body_c) as_p (bodyResult body_p)
                }
        let lam' =
              Lambda
                { lambdaParams = lambdaParams lam_c ++ lambdaParams lam_p,
                  lambdaBody = body',
                  lambdaReturnType = zipW as_c (lambdaReturnType lam_c) as_p (lambdaReturnType lam_p)
                }
        success (fsOutNames ker ++ returned_outvars) $
          SOAC.Scatter w (ivs_c ++ ivs_p) (as_c ++ as_p) lam'
    (SOAC.Scatter {}, _, _) ->
      fail "Cannot fuse a scatter with anything else than a scatter or a map"
    (_, SOAC.Scatter {}, _) ->
      fail "Cannot fuse a scatter with anything else than a scatter or a map"
    ----------------------------
    -- Stream-Stream Fusions: --
    ----------------------------
    (SOAC.Stream {}, SOAC.Stream {}, _) -> do
      -- fuse two SEQUENTIAL streams
      (res_nms, res_stream) <- fuseStreamHelper (fsOutNames ker) unfus_set outVars outPairs soac_c soac_p
      success res_nms res_stream
    -------------------------------------------------------------------
    --- If one is a stream, translate the other to a stream as well.---
    --- This does not get in trouble (infinite computation) because ---
    ---   scan's translation to Stream introduces a hindrance to    ---
    ---   (horizontal fusion), hence repeated application is for the---
    ---   moment impossible. However, if with a dependence-graph rep---
    ---   we could run in an infinite recursion, i.e., repeatedly   ---
    ---   fusing map o scan into an infinity of Stream levels!      ---
    -------------------------------------------------------------------
    (SOAC.Stream {}, _, _) -> do
      -- If this rule is matched then soac_p is NOT a stream.
      -- To fuse a stream kernel, we transform soac_p to a stream, which
      -- borrows the sequential/parallel property of the soac_c Stream,
      -- and recursively perform stream-stream fusion.
      (soac_p', newacc_ids) <- SOAC.soacToStream soac_p
      fuseSOACwithKer
        mode
        (namesFromList (map identName newacc_ids) <> unfus_set)
        (map identName newacc_ids ++ outVars)
        soac_p'
        ker
    (_, SOAC.Screma _ _ form, _) | Just _ <- Futhark.isScanomapSOAC form -> do
      -- A Scan soac can be currently only fused as a (sequential) stream,
      -- hence it is first translated to a (sequential) Stream and then
      -- fusion with a kernel is attempted.
      (soac_p', newacc_ids) <- SOAC.soacToStream soac_p
      if soac_p' /= soac_p
        then
          fuseSOACwithKer
            mode
            (namesFromList (map identName newacc_ids) <> unfus_set)
            (map identName newacc_ids ++ outVars)
            soac_p'
            ker
        else fail "SOAC could not be turned into stream."
    (_, SOAC.Stream {}, _) -> do
      -- If it reached this case then soac_c is NOT a Stream kernel,
      -- hence transform the kernel's soac to a stream and attempt
      -- stream-stream fusion recursivelly.
      -- The newly created stream corresponding to soac_c borrows the
      -- sequential/parallel property of the soac_p stream.
      (soac_c', newacc_ids) <- SOAC.soacToStream soac_c
      if soac_c' /= soac_c
        then
          fuseSOACwithKer
            mode
            (namesFromList (map identName newacc_ids) <> unfus_set)
            outVars
            soac_p
            $ ker {fsSOAC = soac_c', fsOutNames = map identName newacc_ids ++ fsOutNames ker}
        else fail "SOAC could not be turned into stream."

    ---------------------------------
    --- DEFAULT, CANNOT FUSE CASE ---
    ---------------------------------
    _ -> fail "Cannot fuse"

fuseStreamHelper ::
  [VName] ->
  Names ->
  [VName] ->
  [(VName, Ident)] ->
  SOAC ->
  SOAC ->
  TryFusion ([VName], SOAC)
fuseStreamHelper
  out_kernms
  unfus_set
  outVars
  outPairs
  (SOAC.Stream w2 inp2_arr nes2 lam2)
  (SOAC.Stream _ inp1_arr nes1 lam1) = do
    -- very similar to redomap o redomap composition, but need
    -- to remove first the `chunk' parameters of streams'
    -- lambdas and put them in the resulting stream lambda.
    let chunk1 = head $ lambdaParams lam1
        chunk2 = head $ lambdaParams lam2
        hmnms = M.fromList [(paramName chunk2, paramName chunk1)]
        lam20 = substituteNames hmnms lam2
        lam1' = lam1 {lambdaParams = tail $ lambdaParams lam1}
        lam2' = lam20 {lambdaParams = tail $ lambdaParams lam20}
        (res_lam', new_inp) =
          fuseRedomap
            unfus_set
            outVars
            lam1'
            []
            nes1
            inp1_arr
            outPairs
            lam2'
            []
            nes2
            inp2_arr
        res_lam'' = res_lam' {lambdaParams = chunk1 : lambdaParams res_lam'}
        unfus_accs = take (length nes1) outVars
        unfus_arrs = filter (`notElem` unfus_accs) $ filter (`nameIn` unfus_set) outVars
    pure
      ( unfus_accs ++ out_kernms ++ unfus_arrs,
        SOAC.Stream w2 new_inp (nes1 ++ nes2) res_lam''
      )
fuseStreamHelper _ _ _ _ _ _ = fail "Cannot Fuse Streams!"

-- Here follows optimizations and transforms to expose fusability.

optimizeKernel :: Maybe [VName] -> FusedSOAC -> TryFusion FusedSOAC
optimizeKernel inp ker = do
  (soac, resTrans) <- optimizeSOAC inp (fsSOAC ker) (fsOutputTransform ker)
  pure $ ker {fsSOAC = soac, fsOutputTransform = resTrans}

optimizeSOAC ::
  Maybe [VName] ->
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)
optimizeSOAC inp soac os = do
  res <- foldM comb (False, soac, os) optimizations
  case res of
    (False, _, _) -> fail "No optimisation applied"
    (True, soac', os') -> pure (soac', os')
  where
    comb (changed, soac', os') f =
      do
        (soac'', os'') <- f inp soac' os
        pure (True, soac'', os'')
        <|> pure (changed, soac', os')

type Optimization =
  Maybe [VName] ->
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)

optimizations :: [Optimization]
optimizations = [iswim]

iswim ::
  Maybe [VName] ->
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)
iswim _ (SOAC.Screma w arrs form) ots
  | Just [Futhark.Scan scan_fun nes] <- Futhark.isScanSOAC form,
    Just (map_pat, map_cs, map_w, map_fun) <- rwimPossible scan_fun,
    Just nes_names <- mapM subExpVar nes = do
      let nes_idents = zipWith Ident nes_names $ lambdaReturnType scan_fun
          map_nes = map SOAC.identInput nes_idents
          map_arrs' = map_nes ++ map (SOAC.transposeInput 0 1) arrs
          (scan_acc_params, scan_elem_params) =
            splitAt (length arrs) $ lambdaParams scan_fun
          map_params =
            map removeParamOuterDim scan_acc_params
              ++ map (setParamOuterDimTo w) scan_elem_params
          map_rettype = map (`setOuterSize` w) $ lambdaReturnType scan_fun

          scan_params = lambdaParams map_fun
          scan_body = lambdaBody map_fun
          scan_rettype = lambdaReturnType map_fun
          scan_fun' = Lambda scan_params scan_rettype scan_body
          nes' = map Var $ take (length map_nes) $ map paramName map_params
          arrs' = drop (length map_nes) $ map paramName map_params

      scan_form <- scanSOAC [Futhark.Scan scan_fun' nes']

      let map_body =
            mkBody
              ( oneStm $
                  Let (setPatOuterDimTo w map_pat) (defAux ()) . Op $
                    Futhark.Screma w arrs' scan_form
              )
              $ varsRes
              $ patNames map_pat
          map_fun' = Lambda map_params map_rettype map_body
          perm = case lambdaReturnType scan_fun of -- instead of map_fun
            [] -> []
            t : _ -> 1 : 0 : [2 .. arrayRank t]

      pure
        ( SOAC.Screma map_w map_arrs' (ScremaForm [] [] map_fun'),
          ots SOAC.|> SOAC.Rearrange map_cs perm
        )
iswim _ _ _ =
  fail "ISWIM does not apply."

removeParamOuterDim :: LParam SOACS -> LParam SOACS
removeParamOuterDim param =
  let t = rowType $ paramType param
   in param {paramDec = t}

setParamOuterDimTo :: SubExp -> LParam SOACS -> LParam SOACS
setParamOuterDimTo w param =
  let t = paramType param `setOuterSize` w
   in param {paramDec = t}

setPatOuterDimTo :: SubExp -> Pat Type -> Pat Type
setPatOuterDimTo w = fmap (`setOuterSize` w)

-- Now for fiddling with transpositions...

commonTransforms ::
  [VName] ->
  [SOAC.Input] ->
  (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms interesting inps = commonTransforms' inps'
  where
    inps' =
      [ (SOAC.inputArray inp `elem` interesting, inp)
        | inp <- inps
      ]

commonTransforms' :: [(Bool, SOAC.Input)] -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms' inps =
  case foldM inspect (Nothing, []) inps of
    Just (Just mot, inps') -> first (mot SOAC.<|) $ commonTransforms' $ reverse inps'
    _ -> (SOAC.noTransforms, map snd inps)
  where
    inspect (mot, prev) (True, inp) =
      case (mot, inputToOutput inp) of
        (Nothing, Just (ot, inp')) -> Just (Just ot, (True, inp') : prev)
        (Just ot1, Just (ot2, inp'))
          | ot1 == ot2 -> Just (Just ot2, (True, inp') : prev)
        _ -> Nothing
    inspect (mot, prev) inp = Just (mot, inp : prev)

mapDepth :: MapNest -> Int
mapDepth (MapNest.MapNest _ lam levels _) =
  min resDims (length levels) + 1
  where
    resDims = minDim $ case levels of
      [] -> lambdaReturnType lam
      nest : _ -> MapNest.nestingReturnType nest
    minDim [] = 0
    minDim (t : ts) = foldl min (arrayRank t) $ map arrayRank ts

pullRearrange ::
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)
pullRearrange soac ots = do
  nest <- liftMaybe =<< MapNest.fromSOAC soac
  SOAC.Rearrange cs perm SOAC.:< ots' <- pure $ SOAC.viewf ots
  if rearrangeReach perm <= mapDepth nest
    then do
      let -- Expand perm to cover the full extent of the input dimensionality
          perm' inp = take r perm ++ [length perm .. r - 1]
            where
              r = SOAC.inputRank inp
          addPerm inp = SOAC.addTransform (SOAC.Rearrange cs $ perm' inp) inp
          inputs' = map addPerm $ MapNest.inputs nest
      soac' <-
        MapNest.toSOAC $
          inputs' `MapNest.setInputs` rearrangeReturnTypes nest perm
      pure (soac', ots')
    else fail "Cannot pull transpose"

pullIndex ::
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)
pullIndex (SOAC.Screma _ inps form) ots
  | SOAC.Index cs slice@(Slice (ds@(DimSlice _ w' _) : inner_ds))
      SOAC.:< ots' <-
      SOAC.viewf ots,
    Just lam <- isMapSOAC form = do
      let sliceInput inp =
            SOAC.addTransform
              (SOAC.Index cs (fullSlice (SOAC.inputType inp) [ds]))
              inp
          sliceRes (SubExpRes rcs (Var v)) =
            certifying rcs
              . fmap subExpRes
              . letSubExp (baseString v <> "_sliced")
              $ BasicOp (Index v (Slice inner_ds))
          sliceRes r = pure r
          inner_changed =
            any
              ((/= stripDims 1 (sliceShape slice)) . arrayShape)
              (lambdaReturnType lam)
      lam' <-
        if not inner_changed
          then pure lam
          else
            runLambdaBuilder (lambdaParams lam) $
              mapM sliceRes =<< bodyBind (lambdaBody lam)
      pure (SOAC.Screma w' (map sliceInput inps) (mapSOAC lam'), ots')
pullIndex _ _ = fail "Cannot pull index"

pushRearrange ::
  [VName] ->
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)
pushRearrange inpIds soac ots = do
  nest <- liftMaybe =<< MapNest.fromSOAC soac
  (perm, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest
  if rearrangeReach perm <= mapDepth nest
    then do
      let invertRearrange = SOAC.Rearrange mempty $ rearrangeInverse perm
      soac' <-
        MapNest.toSOAC $
          inputs'
            `MapNest.setInputs` rearrangeReturnTypes nest perm
      pure (soac', invertRearrange SOAC.<| ots)
    else fail "Cannot push transpose"

-- | Actually also rearranges indices.
rearrangeReturnTypes :: MapNest -> [Int] -> MapNest
rearrangeReturnTypes nest@(MapNest.MapNest w body nestings inps) perm =
  MapNest.MapNest
    w
    body
    ( zipWith
        setReturnType
        nestings
        $ drop 1
        $ iterate (map rowType) ts
    )
    inps
  where
    origts = MapNest.typeOf nest
    -- The permutation may be deeper than the rank of the type,
    -- but it is required that it is an identity permutation
    -- beyond that.  This is supposed to be checked as an
    -- invariant by whoever calls rearrangeReturnTypes.
    rearrangeType' t = rearrangeType (take (arrayRank t) perm) t
    ts = map rearrangeType' origts

    setReturnType nesting t' =
      nesting {MapNest.nestingReturnType = t'}

fixupInputs :: [VName] -> [SOAC.Input] -> Maybe ([Int], [SOAC.Input])
fixupInputs inpIds inps =
  case mapMaybe inputRearrange $ filter exposable inps of
    perm : _ -> do
      inps' <- mapM (fixupInput (rearrangeReach perm) perm) inps
      pure (perm, inps')
    _ -> Nothing
  where
    exposable = (`elem` inpIds) . SOAC.inputArray

    inputRearrange (SOAC.Input ts _ _)
      | _ SOAC.:> SOAC.Rearrange _ perm <- SOAC.viewl ts = Just perm
    inputRearrange _ = Nothing

    fixupInput d perm inp
      | r <- SOAC.inputRank inp,
        r >= d =
          Just $ SOAC.addTransform (SOAC.Rearrange mempty $ take r perm) inp
      | otherwise = Nothing

pullReshape :: SOAC -> SOAC.ArrayTransforms -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullReshape (SOAC.Screma _ inps form) ots
  | Just maplam <- Futhark.isMapSOAC form,
    SOAC.Reshape cs k shape SOAC.:< ots' <- SOAC.viewf ots,
    all primType $ lambdaReturnType maplam = do
      let mapw' = case reverse $ shapeDims shape of
            [] -> intConst Int64 0
            d : _ -> d
          trInput inp
            | arrayRank (SOAC.inputType inp) == 1 =
                SOAC.addTransform (SOAC.Reshape cs k shape) inp
            | otherwise =
                SOAC.addTransform (SOAC.ReshapeOuter cs k shape) inp
          inputs' = map trInput inps
          inputTypes = map SOAC.inputType inputs'

      let outersoac ::
            ([SOAC.Input] -> SOAC) ->
            (SubExp, [SubExp]) ->
            TryFusion ([SOAC.Input] -> SOAC)
          outersoac inner (w, outershape) = do
            let addDims t = arrayOf t (Shape outershape) NoUniqueness
                retTypes = map addDims $ lambdaReturnType maplam

            ps <- forM inputTypes $ \inpt ->
              newParam "pullReshape_param" $
                stripArray (length shape - length outershape) inpt

            inner_body <-
              runBodyBuilder $
                varsRes
                  <$> (letTupExp "x" <=< SOAC.toExp $ inner $ map (SOAC.identInput . paramIdent) ps)
            let inner_fun =
                  Lambda
                    { lambdaParams = ps,
                      lambdaReturnType = retTypes,
                      lambdaBody = inner_body
                    }
            pure $ flip (SOAC.Screma w) $ Futhark.mapSOAC inner_fun

      op' <-
        foldM outersoac (flip (SOAC.Screma mapw') $ Futhark.mapSOAC maplam) $
          zip (drop 1 $ reverse $ shapeDims shape) $
            drop 1 . reverse . drop 1 . tails $
              shapeDims shape
      pure (op' inputs', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs ::
  [VName] ->
  FusedSOAC ->
  TryFusion (FusedSOAC, SOAC.ArrayTransforms)
exposeInputs inpIds ker =
  (exposeInputs' =<< pushRearrange')
    <|> (exposeInputs' =<< pullRearrange')
    <|> (exposeInputs' =<< pullIndex')
    <|> exposeInputs' ker
  where
    ot = fsOutputTransform ker

    pushRearrange' = do
      (soac', ot') <- pushRearrange inpIds (fsSOAC ker) ot
      pure
        ker
          { fsSOAC = soac',
            fsOutputTransform = ot'
          }

    pullRearrange' = do
      (soac', ot') <- pullRearrange (fsSOAC ker) ot
      unless (SOAC.nullTransforms ot') $
        fail "pullRearrange was not enough"
      pure
        ker
          { fsSOAC = soac',
            fsOutputTransform = SOAC.noTransforms
          }

    pullIndex' = do
      (soac', ot') <- pullIndex (fsSOAC ker) ot
      unless (SOAC.nullTransforms ot') $
        fail "pullIndex was not enough"
      pure
        ker
          { fsSOAC = soac',
            fsOutputTransform = SOAC.noTransforms
          }

    exposeInputs' ker' =
      case commonTransforms inpIds $ inputs ker' of
        (ot', inps')
          | all exposed inps' ->
              pure (ker' {fsSOAC = inps' `SOAC.setInputs` fsSOAC ker'}, ot')
        _ -> fail "Cannot expose"

    exposed (SOAC.Input ts _ _)
      | SOAC.nullTransforms ts = True
    exposed inp = SOAC.inputArray inp `notElem` inpIds

outputTransformPullers ::
  [ SOAC ->
    SOAC.ArrayTransforms ->
    TryFusion (SOAC, SOAC.ArrayTransforms)
  ]
outputTransformPullers = [pullRearrange, pullReshape, pullIndex]

pullOutputTransforms ::
  SOAC ->
  SOAC.ArrayTransforms ->
  TryFusion (SOAC, SOAC.ArrayTransforms)
pullOutputTransforms = attempt outputTransformPullers
  where
    attempt [] _ _ = fail "Cannot pull anything"
    attempt (p : ps) soac ots =
      do
        (soac', ots') <- p soac ots
        if SOAC.nullTransforms ots'
          then pure (soac', SOAC.noTransforms)
          else pullOutputTransforms soac' ots' <|> pure (soac', ots')
        <|> attempt ps soac ots
