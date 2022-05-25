{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.Fusion.LoopKernel
  ( FusedKer (..),
    attemptFusion,
  )
where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, tails, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Futhark.Analysis.HORep.MapNest as MapNest
import qualified Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
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
  MonadFreshNames m =>
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

data FusedKer = FusedKer
  { -- | the SOAC expression, e.g., mapT( f(a,b), x, y )
    fsoac :: SOAC,
    -- | The names in scope at the kernel.
    kernelScope :: Scope SOACS,
    outputTransform :: SOAC.ArrayTransforms,
    outNames :: [VName]
  }
  deriving (Show)

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker {fsoac = inps `SOAC.setInputs` fsoac ker}

tryOptimizeSOAC ::
  Names ->
  [VName] ->
  SOAC ->
  FusedKer ->
  TryFusion FusedKer
tryOptimizeSOAC unfus_nms outVars soac ker = do
  (soac', ots) <- optimizeSOAC Nothing soac mempty
  let ker' = map (addInitialTransformIfRelevant ots) (inputs ker) `setInputs` ker
      outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
      ker'' = fixInputTypes outIdents ker'
  applyFusionRules unfus_nms outVars soac' ker''
  where
    addInitialTransformIfRelevant ots inp
      | SOAC.inputArray inp `elem` outVars =
          SOAC.addInitialTransforms ots inp
      | otherwise =
          inp

tryOptimizeKernel ::
  Names ->
  [VName] ->
  SOAC ->
  FusedKer ->
  TryFusion FusedKer
tryOptimizeKernel unfus_nms outVars soac ker = do
  ker' <- optimizeKernel (Just outVars) ker
  applyFusionRules unfus_nms outVars soac ker'

tryExposeInputs ::
  Names ->
  [VName] ->
  SOAC ->
  FusedKer ->
  TryFusion FusedKer
tryExposeInputs unfus_nms outVars soac ker = do
  (ker', ots) <- exposeInputs outVars ker
  if SOAC.nullTransforms ots
    then fuseSOACwithKer unfus_nms outVars soac ker'
    else do
      guard $ unfus_nms == mempty
      (soac', ots') <- pullOutputTransforms soac ots
      let outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
          ker'' = fixInputTypes outIdents ker'
      if SOAC.nullTransforms ots'
        then applyFusionRules unfus_nms outVars soac' ker''
        else fail "tryExposeInputs could not pull SOAC transforms"

fixInputTypes :: [Ident] -> FusedKer -> FusedKer
fixInputTypes outIdents ker =
  ker {fsoac = fixInputTypes' $ fsoac ker}
  where
    fixInputTypes' soac =
      map fixInputType (SOAC.inputs soac) `SOAC.setInputs` soac
    fixInputType (SOAC.Input ts v _)
      | Just v' <- find ((== v) . identName) outIdents =
          SOAC.Input ts v $ identType v'
    fixInputType inp = inp

applyFusionRules ::
  Names ->
  [VName] ->
  SOAC ->
  FusedKer ->
  TryFusion FusedKer
applyFusionRules unfus_nms outVars soac ker =
  tryOptimizeSOAC unfus_nms outVars soac ker
    <|> tryOptimizeKernel unfus_nms outVars soac ker
    <|> fuseSOACwithKer unfus_nms outVars soac ker
    <|> tryExposeInputs unfus_nms outVars soac ker

attemptFusion ::
  MonadFreshNames m =>
  Names ->
  [VName] ->
  SOAC ->
  FusedKer ->
  m (Maybe FusedKer)
attemptFusion unfus_nms outVars soac ker =
  tryFusion
    (applyFusionRules unfus_nms outVars soac ker)
    (kernelScope ker)

-- | Check that the consumer does not use any scan or reduce results.
scremaFusionOK :: ([VName], [VName]) -> FusedKer -> Bool
scremaFusionOK (nonmap_outs, _map_outs) ker =
  all (`notElem` nonmap_outs) $ mapMaybe SOAC.isVarishInput (inputs ker)

-- | Check that the consumer uses all the outputs of the producer unmodified.
mapWriteFusionOK :: [VName] -> FusedKer -> Bool
mapWriteFusionOK outVars ker = all (`elem` inpIds) outVars
  where
    inpIds = mapMaybe SOAC.isVarishInput (inputs ker)

-- | The brain of this module: Fusing a SOAC with a Kernel.
fuseSOACwithKer ::
  Names ->
  [VName] ->
  SOAC ->
  FusedKer ->
  TryFusion FusedKer
fuseSOACwithKer unfus_set outVars soac_p ker = do
  -- We are fusing soac_p into soac_c, i.e, the output of soac_p is going
  -- into soac_c.
  let soac_c = fsoac ker
      inp_p_arr = SOAC.inputs soac_p
      horizFuse = unfus_set /= mempty
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
            { fsoac = uniq_lam `SOAC.setLambda` res_soac,
              outNames = res_outnms
            }

  -- Can only fuse SOACs with same width.
  guard $ SOAC.width soac_p == SOAC.width soac_c

  -- If we are getting rid of a producer output, then it must be used
  -- without any transformation.
  let bare_inputs = mapMaybe SOAC.isVarishInput (inputs ker)
      ker_inputs = map SOAC.inputArray (inputs ker)
      inputOrUnfus v = v `elem` bare_inputs || v `notElem` ker_inputs

  guard $ all inputOrUnfus outVars

  outPairs <- forM (zip outVars $ map rowType $ SOAC.typeOf soac_p) $ \(outVar, t) -> do
    outVar' <- newVName $ baseString outVar ++ "_elem"
    pure (outVar, Ident outVar' t)

  let mapLikeFusionCheck =
        let (res_lam, new_inp) = fuseMaps unfus_set lam_p inp_p_arr outPairs lam_c inp_c_arr
            (extra_nms, extra_rtps) =
              unzip $
                filter ((`nameIn` unfus_set) . fst) $
                  zip outVars $ map (stripArray 1) $ SOAC.typeOf soac_p
            res_lam' = res_lam {lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps}
         in (extra_nms, res_lam', new_inp)

  when (horizFuse && not (SOAC.nullTransforms $ outputTransform ker)) $
    fail "Horizontal fusion is invalid in the presence of output transforms."

  case (soac_c, soac_p) of
    _ | SOAC.width soac_p /= SOAC.width soac_c -> fail "SOAC widths must match."
    ( SOAC.Screma _ (ScremaForm scans_c reds_c _) _,
      SOAC.Screma _ (ScremaForm scans_p reds_p _) _
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
                  splitAt3 (length scan_nes_c) (length red_nes_c) $ outNames ker
                unfus_arrs = returned_outvars \\ (soac_p_scanout ++ soac_p_redout)
            success
              ( soac_p_scanout ++ soac_c_scanout
                  ++ soac_p_redout
                  ++ soac_c_redout
                  ++ soac_c_mapout
                  ++ unfus_arrs
              )
              $ SOAC.Screma
                w
                (ScremaForm (scans_p ++ scans_c) (reds_p ++ reds_c) res_lam')
                new_inp

    ------------------
    -- Scatter fusion --
    ------------------

    -- Map-Scatter fusion.
    --
    -- The 'inplace' mechanism for kernels already takes care of
    -- checking that the Scatter is not writing to any array used in
    -- the Map.
    ( SOAC.Scatter _len _lam _ivs dests,
      SOAC.Screma _ form _
      )
        | isJust $ isMapSOAC form,
          -- 1. all arrays produced by the map are ONLY used (consumed)
          --    by the scatter, i.e., not used elsewhere.
          not (any (`nameIn` unfus_set) outVars),
          -- 2. all arrays produced by the map are input to the scatter.
          mapWriteFusionOK outVars ker -> do
            let (extra_nms, res_lam', new_inp) = mapLikeFusionCheck
            success (outNames ker ++ extra_nms) $
              SOAC.Scatter w res_lam' new_inp dests

    -- Map-Hist fusion.
    --
    -- The 'inplace' mechanism for kernels already takes care of
    -- checking that the Hist is not writing to any array used in
    -- the Map.
    ( SOAC.Hist _ ops _ _,
      SOAC.Screma _ form _
      )
        | isJust $ isMapSOAC form,
          -- 1. all arrays produced by the map are ONLY used (consumed)
          --    by the hist, i.e., not used elsewhere.
          not (any (`nameIn` unfus_set) outVars),
          -- 2. all arrays produced by the map are input to the scatter.
          mapWriteFusionOK outVars ker -> do
            let (extra_nms, res_lam', new_inp) = mapLikeFusionCheck
            success (outNames ker ++ extra_nms) $
              SOAC.Hist w ops res_lam' new_inp

    -- Hist-Hist fusion
    ( SOAC.Hist _ ops_c _ _,
      SOAC.Hist _ ops_p _ _
      )
        | horizFuse -> do
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
            success (outNames ker ++ returned_outvars) $
              SOAC.Hist w (ops_c <> ops_p) lam' (inp_c_arr <> inp_p_arr)

    -- Scatter-write fusion.
    ( SOAC.Scatter _len_c _lam_c ivs_c as_c,
      SOAC.Scatter _len_p _lam_p ivs_p as_p
      )
        | horizFuse -> do
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
            success (outNames ker ++ returned_outvars) $
              SOAC.Scatter w lam' (ivs_c ++ ivs_p) (as_c ++ as_p)
    (SOAC.Scatter {}, _) ->
      fail "Cannot fuse a write with anything else than a write or a map"
    (_, SOAC.Scatter {}) ->
      fail "Cannot fuse a write with anything else than a write or a map"
    ----------------------------
    -- Stream-Stream Fusions: --
    ----------------------------
    (SOAC.Stream _ Sequential _ _ _, SOAC.Stream _ Sequential _ _ _) -> do
      -- fuse two SEQUENTIAL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_set outVars outPairs soac_c soac_p
      success res_nms res_stream
    (SOAC.Stream {}, SOAC.Stream {}) -> do
      -- fuse two PARALLEL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_set outVars outPairs soac_c soac_p
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
    (SOAC.Stream _ form2 _ _ _, _) -> do
      -- If this rule is matched then soac_p is NOT a stream.
      -- To fuse a stream kernel, we transform soac_p to a stream, which
      -- borrows the sequential/parallel property of the soac_c Stream,
      -- and recursively perform stream-stream fusion.
      (soac_p', newacc_ids) <- SOAC.soacToStream soac_p
      soac_p'' <- case form2 of
        Sequential {} -> maybe (fail "not a stream") pure (toSeqStream soac_p')
        _ -> pure soac_p'
      if soac_p' == soac_p
        then fail "SOAC could not be turned into stream."
        else
          fuseSOACwithKer
            (namesFromList (map identName newacc_ids) <> unfus_set)
            (map identName newacc_ids ++ outVars)
            soac_p''
            ker
    (_, SOAC.Screma _ form _) | Just _ <- Futhark.isScanomapSOAC form -> do
      -- A Scan soac can be currently only fused as a (sequential) stream,
      -- hence it is first translated to a (sequential) Stream and then
      -- fusion with a kernel is attempted.
      (soac_p', newacc_ids) <- SOAC.soacToStream soac_p
      if soac_p' /= soac_p
        then
          fuseSOACwithKer
            (namesFromList (map identName newacc_ids) <> unfus_set)
            (map identName newacc_ids ++ outVars)
            soac_p'
            ker
        else fail "SOAC could not be turned into stream."
    (_, SOAC.Stream _ form_p _ _ _) -> do
      -- If it reached this case then soac_c is NOT a Stream kernel,
      -- hence transform the kernel's soac to a stream and attempt
      -- stream-stream fusion recursivelly.
      -- The newly created stream corresponding to soac_c borrows the
      -- sequential/parallel property of the soac_p stream.
      (soac_c', newacc_ids) <- SOAC.soacToStream soac_c
      when (soac_c' == soac_c) $ fail "SOAC could not be turned into stream."
      soac_c'' <- case form_p of
        Sequential -> maybe (fail "not a stream") pure (toSeqStream soac_c')
        _ -> pure soac_c'

      fuseSOACwithKer
        (namesFromList (map identName newacc_ids) <> unfus_set)
        outVars
        soac_p
        $ ker {fsoac = soac_c'', outNames = map identName newacc_ids ++ outNames ker}

    ---------------------------------
    --- DEFAULT, CANNOT FUSE CASE ---
    ---------------------------------
    _ -> fail "Cannot fuse"

getStreamOrder :: StreamForm rep -> StreamOrd
getStreamOrder (Parallel o _ _) = o
getStreamOrder Sequential = InOrder

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
  (SOAC.Stream w2 form2 lam2 nes2 inp2_arr)
  (SOAC.Stream _ form1 lam1 nes1 inp1_arr) =
    if getStreamOrder form2 /= getStreamOrder form1
      then fail "fusion conditions not met!"
      else do
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
        let res_form = mergeForms form2 form1
        pure
          ( unfus_accs ++ out_kernms ++ unfus_arrs,
            SOAC.Stream w2 res_form res_lam'' (nes1 ++ nes2) new_inp
          )
fuseStreamHelper _ _ _ _ _ _ = fail "Cannot Fuse Streams!"

mergeForms :: StreamForm SOACS -> StreamForm SOACS -> StreamForm SOACS
mergeForms Sequential Sequential = Sequential
mergeForms (Parallel _ comm2 lam2r) (Parallel o1 comm1 lam1r) =
  Parallel o1 (comm1 <> comm2) (mergeReduceOps lam1r lam2r)
mergeForms _ _ = error "Fusing sequential to parallel stream disallowed!"

-- | If a Stream is passed as argument then it converts it to a
--   Sequential Stream.
toSeqStream :: SOAC -> Maybe SOAC
toSeqStream s@(SOAC.Stream _ Sequential _ _ _) = Just s
toSeqStream (SOAC.Stream w Parallel {} l acc inps) =
  Just $ SOAC.Stream w Sequential l acc inps
toSeqStream _ = Nothing

-- Here follows optimizations and transforms to expose fusability.

optimizeKernel :: Maybe [VName] -> FusedKer -> TryFusion FusedKer
optimizeKernel inp ker = do
  (soac, resTrans) <- optimizeSOAC inp (fsoac ker) startTrans
  pure $
    ker
      { fsoac = soac,
        outputTransform = resTrans
      }
  where
    startTrans = outputTransform ker

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
iswim _ (SOAC.Screma w form arrs) ots
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
          scan_fun' = Lambda scan_params scan_body scan_rettype
          nes' = map Var $ take (length map_nes) $ map paramName map_params
          arrs' = drop (length map_nes) $ map paramName map_params

      scan_form <- scanSOAC [Futhark.Scan scan_fun' nes']

      let map_body =
            mkBody
              ( oneStm $
                  Let (setPatOuterDimTo w map_pat) (defAux ()) $
                    Op $ Futhark.Screma w arrs' scan_form
              )
              $ varsRes $ patNames map_pat
          map_fun' = Lambda map_params map_body map_rettype
          perm = case lambdaReturnType scan_fun of -- instead of map_fun
            [] -> []
            t : _ -> 1 : 0 : [2 .. arrayRank t]

      pure
        ( SOAC.Screma map_w (ScremaForm [] [] map_fun') map_arrs',
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
        $ drop 1 $ iterate (map rowType) ts
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
pullReshape (SOAC.Screma _ form inps) ots
  | Just maplam <- Futhark.isMapSOAC form,
    SOAC.Reshape cs shape SOAC.:< ots' <- SOAC.viewf ots,
    all primType $ lambdaReturnType maplam = do
      let mapw' = case reverse $ newDims shape of
            [] -> intConst Int64 0
            d : _ -> d
          inputs' = map (SOAC.addTransform $ SOAC.ReshapeOuter cs shape) inps
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
                eBody [SOAC.toExp $ inner $ map (SOAC.identInput . paramIdent) ps]
            let inner_fun =
                  Lambda
                    { lambdaParams = ps,
                      lambdaReturnType = retTypes,
                      lambdaBody = inner_body
                    }
            pure $ SOAC.Screma w $ Futhark.mapSOAC inner_fun

      op' <-
        foldM outersoac (SOAC.Screma mapw' $ Futhark.mapSOAC maplam) $
          zip (drop 1 $ reverse $ newDims shape) $
            drop 1 $ reverse $ drop 1 $ tails $ newDims shape
      pure (op' inputs', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs ::
  [VName] ->
  FusedKer ->
  TryFusion (FusedKer, SOAC.ArrayTransforms)
exposeInputs inpIds ker =
  (exposeInputs' =<< pushRearrange')
    <|> (exposeInputs' =<< pullRearrange')
    <|> exposeInputs' ker
  where
    ot = outputTransform ker

    pushRearrange' = do
      (soac', ot') <- pushRearrange inpIds (fsoac ker) ot
      pure
        ker
          { fsoac = soac',
            outputTransform = ot'
          }

    pullRearrange' = do
      (soac', ot') <- pullRearrange (fsoac ker) ot
      unless (SOAC.nullTransforms ot') $
        fail "pullRearrange was not enough"
      pure
        ker
          { fsoac = soac',
            outputTransform = SOAC.noTransforms
          }

    exposeInputs' ker' =
      case commonTransforms inpIds $ inputs ker' of
        (ot', inps')
          | all exposed inps' ->
              pure (ker' {fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
        _ -> fail "Cannot expose"

    exposed (SOAC.Input ts _ _)
      | SOAC.nullTransforms ts = True
    exposed inp = SOAC.inputArray inp `notElem` inpIds

outputTransformPullers :: [SOAC -> SOAC.ArrayTransforms -> TryFusion (SOAC, SOAC.ArrayTransforms)]
outputTransformPullers = [pullRearrange, pullReshape]

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
