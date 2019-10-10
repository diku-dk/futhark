{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Optimise.Fusion.LoopKernel
  ( FusedKer(..)
  , newKernel
  , inputs
  , setInputs
  , arrInputs
  , kernelType
  , transformOutput
  , attemptFusion
  , SOAC
  , MapNest
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

import Futhark.Representation.SOACS hiding (SOAC(..))
import qualified Futhark.Representation.SOACS as Futhark
import Futhark.Transform.Rename (renameLambda)
import Futhark.Transform.Substitute
import Futhark.MonadFreshNames
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import qualified Futhark.Analysis.HORepresentation.MapNest as MapNest
import Futhark.Pass.ExtractKernels.ISRWIM (rwimPossible)
import Futhark.Optimise.Fusion.TryFusion
import Futhark.Optimise.Fusion.Composing
import Futhark.Construct
import Futhark.Util (splitAt3)

type SOAC = SOAC.SOAC SOACS
type MapNest = MapNest.MapNest SOACS

-- XXX: This function is very gross.
transformOutput :: SOAC.ArrayTransforms -> [VName] -> [Ident]
                -> Binder SOACS ()
transformOutput ts names = descend ts
  where descend ts' validents =
          case SOAC.viewf ts' of
            SOAC.EmptyF ->
              forM_ (zip names validents) $ \(k, valident) ->
              letBindNames [k] $ BasicOp $ SubExp $ Var $ identName valident
            t SOAC.:< ts'' -> do
              let (es,css) = unzip $ map (applyTransform t) validents
                  mkPat (Ident nm tp) = Pattern [] [PatElem nm tp]
              opts <- concat <$> mapM primOpType es
              newIds <- forM (zip names opts) $ \(k, opt) ->
                newIdent (baseString k) opt
              forM_ (zip3 css newIds es) $ \(cs,ids,e) ->
                certifying cs $ letBind (mkPat ids) (BasicOp e)
              descend ts'' newIds

applyTransform :: SOAC.ArrayTransform -> Ident -> (BasicOp, Certificates)
applyTransform (SOAC.Rearrange cs perm) v =
  (Rearrange perm' $ identName v, cs)
  where perm' = perm ++ drop (length perm) [0..arrayRank (identType v)-1]
applyTransform (SOAC.Reshape cs shape) v =
  (Reshape shape $ identName v, cs)
applyTransform (SOAC.ReshapeOuter cs shape) v =
  let shapes = reshapeOuter shape 1 $ arrayShape $ identType v
  in (Reshape shapes $ identName v, cs)
applyTransform (SOAC.ReshapeInner cs shape) v =
  let shapes = reshapeInner shape 1 $ arrayShape $ identType v
  in (Reshape shapes $ identName v, cs)
applyTransform (SOAC.Replicate cs n) v =
  (Replicate n $ Var $ identName v, cs)

inputToOutput :: SOAC.Input -> Maybe (SOAC.ArrayTransform, SOAC.Input)
inputToOutput (SOAC.Input ts ia iat) =
  case SOAC.viewf ts of
    t SOAC.:< ts' -> Just (t, SOAC.Input ts' ia iat)
    SOAC.EmptyF   -> Nothing

data FusedKer = FusedKer {
    fsoac      :: SOAC
  -- ^ the SOAC expression, e.g., mapT( f(a,b), x, y )

  , inplace    :: Names
  -- ^ Variables used in in-place updates in the kernel itself, as
  -- well as on the path to the kernel from the current position.
  -- This is used to avoid fusion that would violate in-place
  -- restrictions.

  , fusedVars :: [VName]
  -- ^ whether at least a fusion has been performed.

  , fusedConsumed :: Names
  -- ^ The set of variables that were consumed by the SOACs
  -- contributing to this kernel.  Note that, by the type rules, the
  -- final SOAC may actually consume _more_ than its original
  -- contributors, which implies the need for 'Copy' expressions.

  , kernelScope :: Scope SOACS
  -- ^ The names in scope at the kernel.

  , outputTransform :: SOAC.ArrayTransforms
  , outNames :: [VName]
  , certificates :: Certificates
  }
                deriving (Show)

newKernel :: Certificates -> SOAC -> Names -> [VName] -> Scope SOACS -> FusedKer
newKernel cs soac consumed out_nms scope =
  FusedKer { fsoac = soac
           , inplace = consumed
           , fusedVars = []
           , fusedConsumed = consumed
           , outputTransform = SOAC.noTransforms
           , outNames = out_nms
           , kernelScope = scope
           , certificates = cs
           }

arrInputs :: FusedKer -> S.Set VName
arrInputs = S.fromList . map SOAC.inputArray . inputs

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker { fsoac = inps `SOAC.setInputs` fsoac ker }

kernelType :: FusedKer -> [Type]
kernelType = SOAC.typeOf . fsoac

tryOptimizeSOAC :: Names -> [VName] -> SOAC -> Names -> FusedKer
                -> TryFusion FusedKer
tryOptimizeSOAC unfus_nms outVars soac consumed ker = do
  (soac', ots) <- optimizeSOAC Nothing soac mempty
  let ker' = map (addInitialTransformIfRelevant ots) (inputs ker) `setInputs` ker
      outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
      ker'' = fixInputTypes outIdents ker'
  applyFusionRules unfus_nms outVars soac' consumed ker''
  where addInitialTransformIfRelevant ots inp
          | SOAC.inputArray inp `elem` outVars =
              SOAC.addInitialTransforms ots inp
          | otherwise =
              inp

tryOptimizeKernel :: Names -> [VName] -> SOAC -> Names -> FusedKer
                  -> TryFusion FusedKer
tryOptimizeKernel unfus_nms outVars soac consumed ker = do
  ker' <- optimizeKernel (Just outVars) ker
  applyFusionRules unfus_nms outVars soac consumed ker'

tryExposeInputs :: Names -> [VName] -> SOAC -> Names -> FusedKer
                -> TryFusion FusedKer
tryExposeInputs unfus_nms outVars soac consumed ker = do
  (ker', ots) <- exposeInputs outVars ker
  if SOAC.nullTransforms ots
  then fuseSOACwithKer unfus_nms outVars soac consumed ker'
  else do
    (soac', ots') <- pullOutputTransforms soac ots
    let outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
        ker'' = fixInputTypes outIdents ker'
    if SOAC.nullTransforms ots'
    then applyFusionRules unfus_nms outVars soac' consumed ker''
    else fail "tryExposeInputs could not pull SOAC transforms"

fixInputTypes :: [Ident] -> FusedKer -> FusedKer
fixInputTypes outIdents ker =
  ker { fsoac = fixInputTypes' $ fsoac ker }
  where fixInputTypes' soac =
          map fixInputType (SOAC.inputs soac) `SOAC.setInputs` soac
        fixInputType (SOAC.Input ts v _)
          | Just v' <- find ((==v) . identName) outIdents =
            SOAC.Input ts v $ identType v'
        fixInputType inp = inp

applyFusionRules :: Names -> [VName] -> SOAC -> Names -> FusedKer
                 -> TryFusion FusedKer
applyFusionRules    unfus_nms outVars soac consumed ker =
  tryOptimizeSOAC   unfus_nms outVars soac consumed ker <|>
  tryOptimizeKernel unfus_nms outVars soac consumed ker <|>
  fuseSOACwithKer   unfus_nms outVars soac consumed ker <|>
  tryExposeInputs   unfus_nms outVars soac consumed ker

attemptFusion :: MonadFreshNames m =>
                 Names -> [VName] -> SOAC -> Names -> FusedKer
              -> m (Maybe FusedKer)
attemptFusion unfus_nms outVars soac consumed ker =
  fmap removeUnusedParamsFromKer <$>
    tryFusion (applyFusionRules unfus_nms outVars soac consumed ker)
    (kernelScope ker)

removeUnusedParamsFromKer :: FusedKer -> FusedKer
removeUnusedParamsFromKer ker =
  case soac of SOAC.Screma {} -> ker { fsoac = soac' }
               _                -> ker
  where soac = fsoac ker
        l = SOAC.lambda soac
        inps = SOAC.inputs soac
        (l', inps') = removeUnusedParams l inps
        soac' = l' `SOAC.setLambda`
                (inps' `SOAC.setInputs` soac)

removeUnusedParams :: Lambda -> [SOAC.Input] -> (Lambda, [SOAC.Input])
removeUnusedParams l inps =
  (l { lambdaParams = ps' }, inps')
  where pInps = zip (lambdaParams l) inps
        (ps', inps') = case (unzip $ filter (used . fst) pInps, pInps) of
                         (([], []), (p,inp):_) -> ([p], [inp])
                         ((ps_, inps_), _)     -> (ps_, inps_)
        used p = paramName p `nameIn` freeVars
        freeVars = freeIn $ lambdaBody l

-- | Check that the consumer uses at least one output of the producer
-- unmodified.
mapFusionOK :: [VName] -> FusedKer -> Bool
mapFusionOK outVars ker = any (`elem` inpIds) outVars
  where inpIds = mapMaybe SOAC.isVarishInput (inputs ker)

-- | Check that the consumer uses all the outputs of the producer unmodified.
mapWriteFusionOK :: [VName] -> FusedKer -> Bool
mapWriteFusionOK outVars ker = all (`elem` inpIds) outVars
  where inpIds = mapMaybe SOAC.isVarishInput (inputs ker)

-- | The brain of this module: Fusing a SOAC with a Kernel.
fuseSOACwithKer :: Names -> [VName] -> SOAC -> Names -> FusedKer
                -> TryFusion FusedKer
fuseSOACwithKer unfus_set outVars soac_p soac_p_consumed ker = do
  -- We are fusing soac_p into soac_c, i.e, the output of soac_p is going
  -- into soac_c.
  let soac_c    = fsoac ker
      inp_p_arr = SOAC.inputs soac_p
      horizFuse= unfus_set /= mempty &&
                 SOAC.width soac_p == SOAC.width soac_c
      inp_c_arr = SOAC.inputs soac_c
      lam_p     = SOAC.lambda soac_p
      lam_c     = SOAC.lambda soac_c
      w        = SOAC.width soac_p
      returned_outvars = filter (`nameIn` unfus_set) outVars
      success res_outnms res_soac = do
        let fusedVars_new = fusedVars ker++outVars
        -- Avoid name duplication, because the producer lambda is not
        -- removed from the program until much later.
        uniq_lam <- renameLambda $ SOAC.lambda res_soac
        return $ ker { fsoac = uniq_lam `SOAC.setLambda` res_soac
                     , fusedVars = fusedVars_new
                     , inplace = inplace ker <> soac_p_consumed
                     , fusedConsumed = fusedConsumed ker <> soac_p_consumed
                     , outNames = res_outnms
                     }

  outPairs <- forM (zip outVars $ map rowType $ SOAC.typeOf soac_p) $ \(outVar, t) -> do
                outVar' <- newVName $ baseString outVar ++ "_elem"
                return (outVar, Ident outVar' t)

  let mapLikeFusionCheck =
        let (res_lam, new_inp) = fuseMaps unfus_set lam_p inp_p_arr outPairs lam_c inp_c_arr
            (extra_nms,extra_rtps) = unzip $ filter ((`nameIn` unfus_set) . fst) $
              zip outVars $ map (stripArray 1) $ SOAC.typeOf soac_p
            res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
        in (extra_nms, res_lam', new_inp)

  when (horizFuse && not (SOAC.nullTransforms $ outputTransform ker)) $
    fail "Horizontal fusion is invalid in the presence of output transforms."

  case (soac_c, soac_p) of
    _ | SOAC.width soac_p /= SOAC.width soac_c -> fail "SOAC widths must match."

    (SOAC.Screma _ (ScremaForm (scan_lam_c, scan_nes_c) reds_c _) _,
     SOAC.Screma _ (ScremaForm (scan_lam_p, scan_nes_p) reds_p _) _)
      | mapFusionOK (drop (length scan_nes_p+Futhark.redResults reds_p) outVars) ker
        || horizFuse -> do
      let red_nes_p = concatMap redNeutral reds_p
          red_nes_c = concatMap redNeutral reds_c
          (res_lam', new_inp) = fuseRedomap unfus_set outVars
                                            lam_p scan_nes_p red_nes_p inp_p_arr
                                            outPairs
                                            lam_c scan_nes_c red_nes_c inp_c_arr
          (soac_p_scanout, soac_p_redout, _soac_p_mapout) =
            splitAt3 (length scan_nes_p) (length red_nes_p) outVars
          (soac_c_scanout, soac_c_redout, soac_c_mapout) =
            splitAt3 (length scan_nes_c) (length red_nes_c) $ outNames ker
          unfus_arrs  = returned_outvars \\ (soac_p_scanout++soac_p_redout)
          scan_lam'   = mergeReduceOps scan_lam_p scan_lam_c
      success (soac_p_scanout ++ soac_c_scanout ++
               soac_p_redout ++ soac_c_redout ++
               soac_c_mapout ++ unfus_arrs) $
        SOAC.Screma w (ScremaForm (scan_lam', scan_nes_p++scan_nes_c) (reds_p ++ reds_c) res_lam')
        new_inp

    ------------------
    -- Scatter fusion --
    ------------------

    -- Map-Scatter fusion.
    --
    -- The 'inplace' mechanism for kernels already takes care of
    -- checking that the Scatter is not writing to any array used in
    -- the Map.
    (SOAC.Scatter _len _lam _ivs dests,
     SOAC.Screma _ form _)
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
    (SOAC.Hist _ ops _ _,
     SOAC.Screma _ form _)
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
    (SOAC.Hist _ ops_c _ _,
     SOAC.Hist _ ops_p _ _)
      | horizFuse -> do
          let p_num_buckets = length ops_p
              c_num_buckets = length ops_c
              (body_p, body_c) = (lambdaBody lam_p, lambdaBody lam_c)
              body' =
                Body { bodyAttr = bodyAttr body_p -- body_p and body_c have the same lores
                     , bodyStms = bodyStms body_p <> bodyStms body_c
                     , bodyResult = take c_num_buckets (bodyResult body_c) ++
                                    take p_num_buckets (bodyResult body_p) ++
                                    drop c_num_buckets (bodyResult body_c) ++
                                    drop p_num_buckets (bodyResult body_p)
                     }
              lam' =
                Lambda { lambdaParams = lambdaParams lam_c ++ lambdaParams lam_p
                       , lambdaBody = body'
                       , lambdaReturnType = replicate (c_num_buckets+p_num_buckets) (Prim int32) ++
                                            drop c_num_buckets (lambdaReturnType lam_c) ++
                                            drop p_num_buckets (lambdaReturnType lam_p)
                       }
          success (outNames ker ++ returned_outvars) $
            SOAC.Hist w (ops_c <> ops_p) lam' (inp_c_arr <> inp_p_arr)

    -- Scatter-write fusion.
    (SOAC.Scatter _len2 _lam_c ivs2 as2,
     SOAC.Scatter _len_p _lam_p ivs_p as_p)
      | horizFuse -> do
          let zipW xs ys = ys_p ++ xs_p ++ ys2 ++ xs2
                where lenx = length xs `div` 2
                      xs_p  = take lenx xs
                      xs2  = drop lenx xs
                      leny = length ys `div` 2
                      ys_p  = take leny ys
                      ys2  = drop leny ys
          let (body_p, body2) = (lambdaBody lam_p, lambdaBody lam_c)
          let body' = Body { bodyAttr = bodyAttr body_p -- body_p and body2 have the same lores
                           , bodyStms = bodyStms body_p <> bodyStms body2
                           , bodyResult = zipW (bodyResult body_p) (bodyResult body2)
                           }
          let lam' = Lambda { lambdaParams = lambdaParams lam_p ++ lambdaParams lam_c
                            , lambdaBody = body'
                            , lambdaReturnType = zipW (lambdaReturnType lam_p) (lambdaReturnType lam_c)
                            }
          success (outNames ker ++ returned_outvars) $
            SOAC.Scatter w lam' (ivs_p ++ ivs2) (as2 ++ as_p)

    (SOAC.Scatter {}, _) ->
      fail "Cannot fuse a write with anything else than a write or a map"
    (_, SOAC.Scatter {}) ->
      fail "Cannot fuse a write with anything else than a write or a map"

    ----------------------------
    -- Stream-Stream Fusions: --
    ----------------------------
    (SOAC.Stream _ Sequential{} _ _, SOAC.Stream _ form_p@Sequential{} _ _)
     | mapFusionOK (drop (length $ getStreamAccums form_p) outVars) ker || horizFuse -> do
      -- fuse two SEQUENTIAL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_set outVars outPairs soac_c soac_p
      success res_nms res_stream

    (SOAC.Stream _ Sequential{} _ _, SOAC.Stream _ Sequential{} _ _) ->
      fail "Fusion conditions not met for two SEQ streams!"

    (SOAC.Stream _ Sequential{} _ _, SOAC.Stream{}) ->
      fail "Cannot fuse a parallel with a sequential Stream!"

    (SOAC.Stream{}, SOAC.Stream _ Sequential{} _ _) ->
      fail "Cannot fuse a parallel with a sequential Stream!"

    (SOAC.Stream{}, SOAC.Stream _ form_p _ _)
     | mapFusionOK (drop (length $ getStreamAccums form_p) outVars) ker || horizFuse -> do
      -- fuse two PARALLEL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_set outVars outPairs soac_c soac_p
      success res_nms res_stream

    (SOAC.Stream{}, SOAC.Stream {}) ->
      fail "Fusion conditions not met for two PAR streams!"

    -------------------------------------------------------------------
    --- If one is a stream, translate the other to a stream as well.---
    --- This does not get in trouble (infinite computation) because ---
    ---   scan's translation to Stream introduces a hindrance to    ---
    ---   (horizontal fusion), hence repeated application is for the---
    ---   moment impossible. However, if with a dependence-graph rep---
    ---   we could run in an infinite recursion, i.e., repeatedly   ---
    ---   fusing map o scan into an infinity of Stream levels!      ---
    -------------------------------------------------------------------
    (SOAC.Stream _ form2 _ _, _) -> do
      -- If this rule is matched then soac_p is NOT a stream.
      -- To fuse a stream kernel, we transform soac_p to a stream, which
      -- borrows the sequential/parallel property of the soac_c Stream,
      -- and recursively perform stream-stream fusion.
      (soac_p', newacc_ids) <- SOAC.soacToStream soac_p
      soac_p'' <- case form2 of
                    Sequential{} -> toSeqStream soac_p'
                    _            -> return soac_p'
      if soac_p' == soac_p
        then fail "SOAC could not be turned into stream."
        else fuseSOACwithKer unfus_set (map identName newacc_ids++outVars) soac_p'' soac_p_consumed ker

    (_, SOAC.Screma _ form _) | Just _ <- Futhark.isScanSOAC form -> do
      -- A Scan soac can be currently only fused as a (sequential) stream,
      -- hence it is first translated to a (sequential) Stream and then
      -- fusion with a kernel is attempted.
      (soac_p', newacc_ids) <- SOAC.soacToStream soac_p
      if soac_p' /= soac_p then
        fuseSOACwithKer unfus_set (map identName newacc_ids++outVars) soac_p' soac_p_consumed ker
        else fail "SOAC could not be turned into stream."

    (_, SOAC.Stream _ form_p _ _) -> do
      -- If it reached this case then soac_c is NOT a Stream kernel,
      -- hence transform the kernel's soac to a stream and attempt
      -- stream-stream fusion recursivelly.
      -- The newly created stream corresponding to soac_c borrows the
      -- sequential/parallel property of the soac_p stream.
      (soac_c', newacc_ids) <- SOAC.soacToStream soac_c
      when (soac_c' == soac_c) $ fail "SOAC could not be turned into stream."
      soac_c'' <- case form_p of
                    Sequential _ -> toSeqStream soac_c'
                    _            -> return soac_c'

      fuseSOACwithKer unfus_set outVars soac_p soac_p_consumed $
        ker { fsoac = soac_c'', outNames = map identName newacc_ids ++ outNames ker }

    ---------------------------------
    --- DEFAULT, CANNOT FUSE CASE ---
    ---------------------------------
    _ -> fail "Cannot fuse"

fuseStreamHelper :: [VName] -> Names -> [VName] -> [(VName,Ident)]
                 -> SOAC -> SOAC -> TryFusion ([VName], SOAC)
fuseStreamHelper out_kernms unfus_set outVars outPairs
                 (SOAC.Stream w2 form2 lam2 inp2_arr)
                 (SOAC.Stream _ form1 lam1 inp1_arr) =
  if getStreamOrder form2 /= getStreamOrder form1
  then fail "fusion conditions not met!"
  else do -- very similar to redomap o redomap composition, but need
          -- to remove first the `chunk' parameters of streams'
          -- lambdas and put them in the resulting stream lambda.
          let nes1    = getStreamAccums form1
              chunk1  = head $ lambdaParams lam1
              chunk2  = head $ lambdaParams lam2
              hmnms = M.fromList [(paramName chunk2, paramName chunk1)]
              lam20 = substituteNames hmnms lam2
              lam1' = lam1  { lambdaParams = tail $ lambdaParams lam1  }
              lam2' = lam20 { lambdaParams = tail $ lambdaParams lam20 }
              (res_lam', new_inp) = fuseRedomap unfus_set outVars
                                                lam1' [] nes1
                                                inp1_arr outPairs
                                                lam2' [] (getStreamAccums form2)
                                                inp2_arr
              res_lam'' = res_lam' { lambdaParams = chunk1 : lambdaParams res_lam' }
              unfus_accs  = take (length nes1) outVars
              unfus_arrs  = filter (`nameIn` unfus_set) outVars
          res_form <- mergeForms form2 form1
          return (unfus_accs ++ out_kernms ++ unfus_arrs,
                  SOAC.Stream w2 res_form res_lam'' new_inp )
  where mergeForms (Sequential acc2) (Sequential acc1) = return $ Sequential (acc1++acc2)
        mergeForms (Parallel _ comm2 lam2r acc2) (Parallel o1 comm1 lam1r acc1) =
            return $ Parallel o1 (comm1<>comm2) (mergeReduceOps lam1r lam2r) (acc1++acc2)
        mergeForms _ _ = fail "Fusing sequential to parallel stream disallowed!"
fuseStreamHelper _ _ _ _ _ _ = fail "Cannot Fuse Streams!"

-- | If a Stream is passed as argument then it converts it to a
--   Sequential Stream; Otherwise it FAILS!
toSeqStream :: SOAC -> TryFusion SOAC
toSeqStream s@(SOAC.Stream _ (Sequential _) _ _) = return s
toSeqStream (SOAC.Stream w (Parallel _ _ _ acc) l inps) =
    return $ SOAC.Stream w (Sequential acc) l inps
toSeqStream _ = fail "toSeqStream expects a stream, but given a SOAC."

-- Here follows optimizations and transforms to expose fusability.

optimizeKernel :: Maybe [VName] -> FusedKer -> TryFusion FusedKer
optimizeKernel inp ker = do
  (soac, resTrans) <- optimizeSOAC inp (fsoac ker) startTrans
  return $ ker { fsoac = soac
               , outputTransform = resTrans
               }
  where startTrans = outputTransform ker

optimizeSOAC :: Maybe [VName] -> SOAC -> SOAC.ArrayTransforms
             -> TryFusion (SOAC, SOAC.ArrayTransforms)
optimizeSOAC inp soac os = do
  res <- foldM comb (False, soac, os) optimizations
  case res of
    (False, _, _)      -> fail "No optimisation applied"
    (True, soac', os') -> return (soac', os')
  where comb (changed, soac', os') f = do
          (soac'', os'') <- f inp soac' os
          return (True, soac'', os'')
          <|> return (changed, soac', os')

type Optimization = Maybe [VName]
                    -> SOAC
                    -> SOAC.ArrayTransforms
                    -> TryFusion (SOAC, SOAC.ArrayTransforms)

optimizations :: [Optimization]
optimizations = [iswim]

iswim :: Maybe [VName] -> SOAC -> SOAC.ArrayTransforms
      -> TryFusion (SOAC, SOAC.ArrayTransforms)
iswim _ (SOAC.Screma w form arrs) ots
  | Just (scan_fun, nes) <- Futhark.isScanSOAC form,
    Just (map_pat, map_cs, map_w, map_fun) <- rwimPossible scan_fun,
    Just nes_names <- mapM subExpVar nes = do

      let nes_idents = zipWith Ident nes_names $ lambdaReturnType scan_fun
          map_nes = map SOAC.identInput nes_idents
          map_arrs' = map_nes ++ map (SOAC.transposeInput 0 1) arrs
          (scan_acc_params, scan_elem_params) =
            splitAt (length arrs) $ lambdaParams scan_fun
          map_params = map removeParamOuterDim scan_acc_params ++
                       map (setParamOuterDimTo w) scan_elem_params
          map_rettype = map (`setOuterSize` w) $ lambdaReturnType scan_fun

          scan_params = lambdaParams map_fun
          scan_body = lambdaBody map_fun
          scan_rettype = lambdaReturnType map_fun
          scan_fun' = Lambda scan_params scan_body scan_rettype
          nes' = map Var $ take (length map_nes) $ map paramName map_params
          arrs' = drop (length map_nes) $ map paramName map_params

      id_map_lam <- mkIdentityLambda $ lambdaReturnType scan_fun'

      let map_body = mkBody (oneStm $
                              Let (setPatternOuterDimTo w map_pat) (defAux ()) $
                              Op $ Futhark.Screma w (ScremaForm (scan_fun', nes') [] id_map_lam) arrs') $
                            map Var $ patternNames map_pat
          map_fun' = Lambda map_params map_body map_rettype
          perm = case lambdaReturnType map_fun of
                   []  -> []
                   t:_ -> 1 : 0 : [2..arrayRank t]

      return (SOAC.Screma map_w (ScremaForm (nilFn, mempty) [] map_fun') map_arrs',
              ots SOAC.|> SOAC.Rearrange map_cs perm)

iswim _ _ _ =
  fail "ISWIM does not apply."

removeParamOuterDim :: LParam -> LParam
removeParamOuterDim param =
  let t = rowType $ paramType param
  in param { paramAttr = t }

setParamOuterDimTo :: SubExp -> LParam -> LParam
setParamOuterDimTo w param =
  let t = paramType param `setOuterSize` w
  in param { paramAttr = t }

setPatternOuterDimTo :: SubExp -> Pattern -> Pattern
setPatternOuterDimTo w = fmap (`setOuterSize` w)

-- Now for fiddling with transpositions...

commonTransforms :: [VName] -> [SOAC.Input]
                 -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms interesting inps = commonTransforms' inps'
  where inps' = [ (SOAC.inputArray inp `elem` interesting, inp)
                | inp <- inps ]

commonTransforms' :: [(Bool, SOAC.Input)] -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms' inps =
  case foldM inspect (Nothing, []) inps of
    Just (Just mot, inps') -> first (mot SOAC.<|) $ commonTransforms' $ reverse inps'
    _                      -> (SOAC.noTransforms, map snd inps)
  where inspect (mot, prev) (True, inp) =
          case (mot, inputToOutput inp) of
           (Nothing,  Just (ot, inp'))  -> Just (Just ot, (True, inp') : prev)
           (Just ot1, Just (ot2, inp'))
             | ot1 == ot2 -> Just (Just ot2, (True, inp') : prev)
           _              -> Nothing
        inspect (mot, prev) inp = Just (mot,inp:prev)

mapDepth :: MapNest -> Int
mapDepth (MapNest.MapNest _ lam levels _) =
  min resDims (length levels) + 1
  where resDims = minDim $ case levels of
                    [] -> lambdaReturnType lam
                    nest:_ -> MapNest.nestingReturnType nest
        minDim [] = 0
        minDim (t:ts) = foldl min (arrayRank t) $ map arrayRank ts

pullRearrange :: SOAC -> SOAC.ArrayTransforms
              -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullRearrange soac ots = do
  nest <- liftMaybe =<< MapNest.fromSOAC soac
  SOAC.Rearrange cs perm SOAC.:< ots' <- return $ SOAC.viewf ots
  if rearrangeReach perm <= mapDepth nest then do
    let -- Expand perm to cover the full extent of the input dimensionality
        perm' inp = take r perm ++ [length perm..r-1]
          where r = SOAC.inputRank inp
        addPerm inp = SOAC.addTransform (SOAC.Rearrange cs $ perm' inp) inp
        inputs' = map addPerm $ MapNest.inputs nest
    soac' <- MapNest.toSOAC $
      inputs' `MapNest.setInputs` rearrangeReturnTypes nest perm
    return (soac', ots')
  else fail "Cannot pull transpose"

pushRearrange :: [VName] -> SOAC -> SOAC.ArrayTransforms
              -> TryFusion (SOAC, SOAC.ArrayTransforms)
pushRearrange inpIds soac ots = do
  nest <- liftMaybe =<< MapNest.fromSOAC soac
  (perm, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest
  if rearrangeReach perm <= mapDepth nest then do
    let invertRearrange = SOAC.Rearrange mempty $ rearrangeInverse perm
    soac' <- MapNest.toSOAC $
      inputs' `MapNest.setInputs`
      rearrangeReturnTypes nest perm
    return (soac', invertRearrange SOAC.<| ots)
  else fail "Cannot push transpose"

-- | Actually also rearranges indices.
rearrangeReturnTypes :: MapNest -> [Int] -> MapNest
rearrangeReturnTypes nest@(MapNest.MapNest w body nestings inps) perm =
  MapNest.MapNest w
  body
  (zipWith setReturnType
   nestings $
   drop 1 $ iterate (map rowType) ts)
  inps
  where origts = MapNest.typeOf nest
        -- The permutation may be deeper than the rank of the type,
        -- but it is required that it is an identity permutation
        -- beyond that.  This is supposed to be checked as an
        -- invariant by whoever calls rearrangeReturnTypes.
        rearrangeType' t = rearrangeType (take (arrayRank t) perm) t
        ts = map rearrangeType' origts

        setReturnType nesting t' =
          nesting { MapNest.nestingReturnType = t' }

fixupInputs :: [VName] -> [SOAC.Input] -> Maybe ([Int], [SOAC.Input])
fixupInputs inpIds inps =
  case mapMaybe inputRearrange $ filter exposable inps of
    perm:_ -> do inps' <- mapM (fixupInput (rearrangeReach perm) perm) inps
                 return (perm, inps')
    _    -> Nothing
  where exposable = (`elem` inpIds) . SOAC.inputArray

        inputRearrange (SOAC.Input ts _ _)
          | _ SOAC.:> SOAC.Rearrange _ perm <- SOAC.viewl ts = Just perm
        inputRearrange _                                     = Nothing

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
        []  -> intConst Int32 0
        d:_ -> d
      inputs' = map (SOAC.addTransform $ SOAC.ReshapeOuter cs shape) inps
      inputTypes = map SOAC.inputType inputs'

  let outersoac :: ([SOAC.Input] -> SOAC) -> (SubExp, [SubExp])
                -> TryFusion ([SOAC.Input] -> SOAC)
      outersoac inner (w, outershape) = do
        let addDims t = arrayOf t (Shape outershape) NoUniqueness
            retTypes = map addDims $ lambdaReturnType maplam

        ps <- forM inputTypes $ \inpt ->
          newParam "pullReshape_param" $
            stripArray (length shape-length outershape) inpt

        inner_body <- runBodyBinder $
          eBody [SOAC.toExp $ inner $ map (SOAC.identInput . paramIdent) ps]
        let inner_fun = Lambda { lambdaParams = ps
                               , lambdaReturnType = retTypes
                               , lambdaBody = inner_body
                               }
        return $ SOAC.Screma w $ Futhark.mapSOAC inner_fun

  op' <- foldM outersoac (SOAC.Screma mapw' $ Futhark.mapSOAC maplam) $
         zip (drop 1 $ reverse $ newDims shape) $
         drop 1 $ reverse $ drop 1 $ tails $ newDims shape
  return (op' inputs', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [VName] -> FusedKer
             -> TryFusion (FusedKer, SOAC.ArrayTransforms)
exposeInputs inpIds ker =
  (exposeInputs' =<< pushRearrange') <|>
  (exposeInputs' =<< pullRearrange') <|>
  exposeInputs' ker
  where ot = outputTransform ker

        pushRearrange' = do
          (soac', ot') <- pushRearrange inpIds (fsoac ker) ot
          return ker { fsoac = soac'
                     , outputTransform = ot'
                     }

        pullRearrange' = do
          (soac',ot') <- pullRearrange (fsoac ker) ot
          unless (SOAC.nullTransforms ot') $
            fail "pullRearrange was not enough"
          return ker { fsoac = soac'
                     , outputTransform = SOAC.noTransforms
                     }

        exposeInputs' ker' =
          case commonTransforms inpIds $ inputs ker' of
            (ot', inps') | all exposed inps' ->
              return (ker' { fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
            _ -> fail "Cannot expose"

        exposed (SOAC.Input ts _ _)
          | SOAC.nullTransforms ts = True
        exposed inp = SOAC.inputArray inp `notElem` inpIds

outputTransformPullers :: [SOAC -> SOAC.ArrayTransforms -> TryFusion (SOAC, SOAC.ArrayTransforms)]
outputTransformPullers = [pullRearrange, pullReshape]

pullOutputTransforms :: SOAC -> SOAC.ArrayTransforms
                     -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullOutputTransforms = attempt outputTransformPullers
  where attempt [] _ _ = fail "Cannot pull anything"
        attempt (p:ps) soac ots = do
          (soac',ots') <- p soac ots
          if SOAC.nullTransforms ots' then return (soac', SOAC.noTransforms)
          else pullOutputTransforms soac' ots' <|> return (soac', ots')
          <|> attempt ps soac ots
