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
  , toNestedSeqStream --not used!
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Monoid
import Data.List

import Prelude

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

type SOAC = SOAC.SOAC SOACS
type MapNest = MapNest.MapNest SOACS

-- XXX: This function is very gross.
transformOutput :: SOAC.ArrayTransforms -> [VName] -> SOAC
                -> Binder SOACS ()
transformOutput ts names soac = do
  validents <- zipWithM newIdent (map baseString names) $ SOAC.typeOf soac
  e <- SOAC.toExp soac
  letBind_ (basicPattern' [] validents) e
  descend ts validents
  where descend ts' validents =
          case SOAC.viewf ts' of
            SOAC.EmptyF ->
              forM_ (zip names validents) $ \(k, valident) ->
              letBindNames' [k] $ PrimOp $ SubExp $ Var $ identName valident
            t SOAC.:< ts'' -> do
              let es = map (applyTransform t) validents
                  mkPat (Ident nm tp) = Pattern [] [PatElem nm BindVar tp]
              opts <- concat <$> mapM primOpType es
              newIds <- forM (zip names opts) $ \(k, opt) ->
                newIdent (baseString k) opt
              zipWithM_ letBind (map mkPat newIds) $ map PrimOp es
              descend ts'' newIds

applyTransform :: SOAC.ArrayTransform -> Ident -> PrimOp
applyTransform (SOAC.Rearrange cs perm) v =
  Rearrange cs perm $ identName v
applyTransform (SOAC.Reshape cs shape) v =
  Reshape cs shape $ identName v
applyTransform (SOAC.ReshapeOuter cs shape) v =
  let shapes = reshapeOuter shape 1 $ arrayShape $ identType v
  in Reshape cs shapes $ identName v
applyTransform (SOAC.ReshapeInner cs shape) v =
  let shapes = reshapeInner shape 1 $ arrayShape $ identType v
  in Reshape cs shapes $ identName v
applyTransform (SOAC.Replicate n) v =
  Replicate n $ Var $ identName v

inputToOutput :: SOAC.Input -> Maybe (SOAC.ArrayTransform, SOAC.Input)
inputToOutput (SOAC.Input ts ia iat) =
  case SOAC.viewf ts of
    t SOAC.:< ts' -> Just (t, SOAC.Input ts' ia iat)
    SOAC.EmptyF   -> Nothing

data FusedKer = FusedKer {
    fsoac      :: SOAC
  -- ^ the SOAC expression, e.g., mapT( f(a,b), x, y )

  , inplace    :: Names
  -- ^ every kernel maintains a set of variables
  -- that alias vars used in in-place updates,
  -- such that fusion is prevented to move
  -- a use of an

  , fusedVars :: [VName]
  -- ^ whether at least a fusion has been performed.

  , kernelScope :: Scope SOACS
  -- ^ The names in scope at the kernel.

  , outputTransform :: SOAC.ArrayTransforms
  , outNames :: [VName]
  }
                deriving (Show)

newKernel :: SOAC -> [VName] -> Scope SOACS -> FusedKer
newKernel soac out_nms scope =
  FusedKer { fsoac = soac
           , inplace = HS.empty
           , fusedVars = []
           , outputTransform = SOAC.noTransforms
           , outNames = out_nms
           , kernelScope = scope
           }

arrInputs :: FusedKer -> HS.HashSet VName
arrInputs = HS.fromList . map SOAC.inputArray . inputs

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker { fsoac = inps `SOAC.setInputs` fsoac ker }

kernelType :: FusedKer -> [Type]
kernelType = SOAC.typeOf . fsoac

tryOptimizeSOAC :: Names -> [VName] -> SOAC -> FusedKer
                -> TryFusion FusedKer
tryOptimizeSOAC unfus_nms outVars soac ker = do
  (soac', ots) <- optimizeSOAC Nothing soac mempty
  let ker' = map (SOAC.addTransforms ots) (inputs ker) `setInputs` ker
      outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
      ker'' = fixInputTypes outIdents ker'
  applyFusionRules unfus_nms outVars soac' ker''

tryOptimizeKernel :: Names -> [VName] -> SOAC -> FusedKer
                  -> TryFusion FusedKer
tryOptimizeKernel unfus_nms outVars soac ker = do
  ker' <- optimizeKernel (Just outVars) ker
  applyFusionRules unfus_nms outVars soac ker'

tryExposeInputs :: Names -> [VName] -> SOAC -> FusedKer
                -> TryFusion FusedKer
tryExposeInputs unfus_nms outVars soac ker = do
  (ker', ots) <- exposeInputs outVars ker
  if SOAC.nullTransforms ots
  then fuseSOACwithKer unfus_nms outVars soac ker'
  else do
    (soac', ots') <- pullOutputTransforms soac ots
    let outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
        ker'' = fixInputTypes outIdents ker'
    if SOAC.nullTransforms ots'
    then applyFusionRules unfus_nms outVars soac' ker''
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

applyFusionRules :: Names -> [VName] -> SOAC -> FusedKer
                 -> TryFusion FusedKer
applyFusionRules    unfus_nms outVars soac ker =
  tryOptimizeSOAC   unfus_nms outVars soac ker <|>
  tryOptimizeKernel unfus_nms outVars soac ker <|>
  tryExposeInputs   unfus_nms outVars soac ker <|>
  fuseSOACwithKer   unfus_nms outVars soac ker

attemptFusion :: MonadFreshNames m =>
                 Names -> [VName] -> SOAC -> FusedKer
              -> m (Maybe FusedKer)
attemptFusion unfus_nms outVars soac ker =
  fmap removeUnusedParamsFromKer <$>
    tryFusion (applyFusionRules unfus_nms outVars soac ker)
    (kernelScope ker)

removeUnusedParamsFromKer :: FusedKer -> FusedKer
removeUnusedParamsFromKer ker =
  case soac of
    SOAC.Map {}     -> ker { fsoac = soac' }
    SOAC.Redomap {} -> ker { fsoac = soac' }
    SOAC.Scanomap {} -> ker { fsoac = soac' }
    _               -> ker
  where soac = fsoac ker
        l = SOAC.lambda soac
        inps = SOAC.inputs soac
        (l', inps') = removeUnusedParams l inps
        soac' = l' `SOAC.setLambda`
                (inps' `SOAC.setInputs` soac)

removeUnusedParams :: Lambda -> [SOAC.Input] -> (Lambda, [SOAC.Input])
removeUnusedParams l inps =
  (l { lambdaParams = accParams ++ ps' }, inps')
  where allParams = lambdaParams l
        (accParams, arrParams) =
          splitAt (length allParams - length inps) allParams
        pInps = zip arrParams inps
        (ps', inps') = case (unzip $ filter (used . fst) pInps, pInps) of
                         (([], []), (p,inp):_) -> ([p], [inp])
                         ((ps_, inps_), _)     -> (ps_, inps_)
        used p = paramName p `HS.member` freeVars
        freeVars = freeInBody $ lambdaBody l

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
fuseSOACwithKer :: Names -> [VName] -> SOAC -> FusedKer
                -> TryFusion FusedKer
fuseSOACwithKer unfus_set outVars soac1 ker = do
  -- We are fusing soac1 into soac2, i.e, the output of soac1 is going
  -- into soac2.
  let soac2    = fsoac ker
      cs1      = SOAC.certificates soac1
      cs2      = SOAC.certificates soac2
      inp1_arr = SOAC.inputs soac1
      horizFuse= not (HS.null unfus_set) &&
                 SOAC.width soac1 == SOAC.width soac2
      inp2_arr = SOAC.inputs soac2
      lam1     = SOAC.lambda soac1
      lam2     = SOAC.lambda soac2
      w        = SOAC.width soac1
      returned_outvars = filter (`HS.member` unfus_set) outVars
      success res_outnms res_soac = do
        let fusedVars_new = fusedVars ker++outVars
        -- Avoid name duplication, because the producer lambda is not
        -- removed from the program until much later.
        uniq_lam <- renameLambda $ SOAC.lambda res_soac
        return $ ker { fsoac = uniq_lam `SOAC.setLambda` res_soac
                     , fusedVars = fusedVars_new
                     , outNames = res_outnms
                     }

  outPairs <- forM (zip outVars $ SOAC.typeOf soac1) $ \(outVar, t) -> do
                outVar' <- newVName $ baseString outVar ++ "_elem"
                return (outVar, Ident outVar' t)

  let mapLikeFusionCheck =
        let (res_lam, new_inp) = fuseMaps unfus_set lam1 inp1_arr outPairs lam2 inp2_arr
            (extra_nms,extra_rtps) = unzip $ filter ((`HS.member` unfus_set) . fst) $
              zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
            res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
        in (extra_nms, res_lam', new_inp)

  case (soac2, soac1) of
    ------------------------------
    -- Redomap-Redomap Fusions: --
    ------------------------------
    (SOAC.Map {}, SOAC.Map    {})
      | mapFusionOK outVars ker || horizFuse -> do
          let (extra_nms, res_lam', new_inp) = mapLikeFusionCheck
          success (outNames ker ++ extra_nms) $
            SOAC.Map (cs1++cs2) w res_lam' new_inp

    (SOAC.Map {}, SOAC.Redomap _ _ comm1 lam11 _ nes _)
      | mapFusionOK (drop (length nes) outVars) ker || horizFuse -> do
      let (res_lam', new_inp) = fuseRedomap unfus_set outVars nes lam1 inp1_arr
                                            outPairs lam2 inp2_arr
          unfus_accs  = take (length nes) outVars
          unfus_arrs  = returned_outvars \\ unfus_accs
      success (unfus_accs ++ outNames ker ++ unfus_arrs) $
              SOAC.Redomap (cs1++cs2) w comm1 lam11 res_lam' nes new_inp

    (SOAC.Redomap _ _ comm2 lam2r _ nes2 _, SOAC.Redomap _ _ comm1 lam1r _ nes1 _)
      | mapFusionOK (drop (length nes1) outVars) ker || horizFuse -> do
      let (res_lam', new_inp) = fuseRedomap unfus_set outVars nes1 lam1 inp1_arr
                                            outPairs lam2 inp2_arr
          unfus_accs  = take (length nes1) outVars
          unfus_arrs  = returned_outvars \\ unfus_accs
          lamr        = mergeReduceOps lam1r lam2r
      success (unfus_accs ++ outNames ker ++ unfus_arrs) $
              SOAC.Redomap (cs1++cs2) w (comm1<>comm2) lamr res_lam' (nes1++nes2) new_inp

    (SOAC.Redomap _ _ comm2 lam21 _ nes _, SOAC.Map {})
      | mapFusionOK outVars ker || horizFuse -> do
      let (res_lam, new_inp) = fuseMaps unfus_set lam1 inp1_arr outPairs lam2 inp2_arr
          (_,extra_rtps) = unzip $ filter ((`HS.member` unfus_set) . fst) $
                           zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
      success (outNames ker ++ returned_outvars) $
              SOAC.Redomap (cs1++cs2) w comm2 lam21 res_lam' nes new_inp

    ----------------------------
    -- Scanomap Fusions:      --
    ----------------------------

    (SOAC.Scanomap _ _ lam2r _ nes2 _, SOAC.Scanomap _ _  lam1r _ nes1 _)
      | horizFuse -> do
          let (res_lam', new_inp) = fuseRedomap unfus_set outVars nes1 lam1 inp1_arr outPairs lam2 inp2_arr
              lamr        = mergeReduceOps lam1r lam2r
              unfus_arrs  = returned_outvars \\ unfus_accs
              unfus_accs  = take (length nes1) outVars
          success (unfus_accs ++ outNames ker ++ unfus_arrs) $
              SOAC.Scanomap (cs1++cs2) w  lamr res_lam' (nes1++nes2) new_inp

    -- Map -> Scanomap Fusion
    (SOAC.Scanomap _ _ lam21 _ nes _, SOAC.Map {})
      | mapFusionOK outVars ker || horizFuse -> do
      -- Create new inner reduction function
      let (res_lam, new_inp) = fuseMaps unfus_set lam1 inp1_arr outPairs lam2 inp2_arr
          -- Get the lists from soac1 that still need to be returned
          (_,extra_rtps) = unzip $ filter (\(nm,_)->nm `HS.member` unfus_set) $
                           zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
      success (outNames ker ++ returned_outvars) $
              SOAC.Scanomap (cs1++cs2) w lam21 res_lam' nes new_inp

    ------------------
    -- Write fusion --
    ------------------

    -- Map-write fusion.
    (SOAC.Write _cs _len _lam _ivs as,
     SOAC.Map {})
      | mapWriteFusionOK (outVars ++ map snd as) ker -> do
          let (extra_nms, res_lam', new_inp) = mapLikeFusionCheck
          success (outNames ker ++ extra_nms) $
            SOAC.Write (cs1++cs2) w res_lam' new_inp as

    -- Write-write fusion.
    (SOAC.Write _cs2 _len2 _lam2 ivs2 as2,
     SOAC.Write _cs1 _len1 _lam1 ivs1 as1)
      | horizFuse -> do
          let zipW xs ys = ys1 ++ xs1 ++ ys2 ++ xs2
                where len = length xs `div` 2 -- same as with ys
                      xs1 = take len xs
                      xs2 = drop len xs
                      ys1 = take len ys
                      ys2 = drop len ys
          let (body1, body2) = (lambdaBody lam1, lambdaBody lam2)
          let body' = Body { bodyLore = bodyLore body1 -- body1 and body2 have the same lores
                           , bodyBindings = bodyBindings body1 ++ bodyBindings body2
                           , bodyResult = zipW (bodyResult body1) (bodyResult body2)
                           }
          let lam' = Lambda { lambdaParams = lambdaParams lam1 ++ lambdaParams lam2
                            , lambdaBody = body'
                            , lambdaReturnType = zipW (lambdaReturnType lam1) (lambdaReturnType lam2)
                            }
          success (outNames ker ++ returned_outvars) $
            SOAC.Write (cs1 ++ cs2) w lam' (ivs1 ++ ivs2) (as2 ++ as1)

    (SOAC.Write {}, _) ->
      fail "Cannot fuse a write with anything else than a write or a map"
    (_, SOAC.Write {}) ->
      fail "Cannot fuse a write with anything else than a write or a map"

    ----------------------------
    -- Stream-Stream Fusions: --
    ----------------------------
    (SOAC.Stream _ _ Sequential{} _ _, SOAC.Stream _ _ form1@Sequential{} _ _)
     | mapFusionOK (drop (length $ getStreamAccums form1) outVars) ker || horizFuse -> do
      -- fuse two SEQUENTIAL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_set outVars outPairs soac2 soac1
      success res_nms res_stream

    (SOAC.Stream _ _ Sequential{} _ _, SOAC.Stream _ _ Sequential{} _ _) ->
      fail "Fusion conditions not met for two SEQ streams!"

    (SOAC.Stream _ _ Sequential{} _ _, SOAC.Stream{}) ->
      fail "Cannot fuse a parallel with a sequential Stream!"

    (SOAC.Stream{}, SOAC.Stream _ _ Sequential{} _ _) ->
      fail "Cannot fuse a parallel with a sequential Stream!"

    (SOAC.Stream{}, SOAC.Stream _ _ form1 _ _)
     | mapFusionOK (drop (length $ getStreamAccums form1) outVars) ker || horizFuse -> do
      -- fuse two PARALLEL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_set outVars outPairs soac2 soac1
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
    (SOAC.Stream _ _ form2 _ _, _) -> do
      -- If this rule is matched then soac1 is NOT a stream.
      -- To fuse a stream kernel, we transform soac1 to a stream, which
      -- borrows the sequential/parallel property of the soac2 Stream,
      -- and recursively perform stream-stream fusion.
      (soac1', newacc_ids) <- SOAC.soacToStream soac1
      soac1'' <- case form2 of
                    Sequential{} -> toSeqStream soac1'
                    _            -> return soac1'
      fuseSOACwithKer unfus_set (map identName newacc_ids++outVars) soac1'' ker

    (_, SOAC.Scan  {}) -> do
      -- A Scan soac can be currently only fused as a (sequential) stream,
      -- hence it is first translated to a (sequential) Stream and then
      -- fusion with a kernel is attempted.
      (soac1', newacc_ids) <- SOAC.soacToStream soac1
      fuseSOACwithKer unfus_set (map identName newacc_ids++outVars) soac1' ker

    (_, SOAC.Stream _ _ form1 _ _) -> do
      -- If it reached this case then soac2 is NOT a Stream kernel,
      -- hence transform the kernel's soac to a stream and attempt
      -- stream-stream fusion recursivelly.
      -- The newly created stream corresponding to soac2 borrows the
      -- sequential/parallel property of the soac1 stream.
      (soac2', newacc_ids) <- SOAC.soacToStream soac2
      soac2'' <- case form1 of
                    Sequential _ -> toSeqStream soac2'
                    _            -> return soac2'
      fuseSOACwithKer unfus_set outVars soac1 $
        ker { fsoac = soac2'', outNames = map identName newacc_ids ++ outNames ker }

    ---------------------------------
    --- DEFAULT, CANNOT FUSE CASE ---
    ---------------------------------
    _ -> fail "Cannot fuse"

fuseStreamHelper :: [VName] -> Names -> [VName] -> [(VName,Ident)]
                 -> SOAC -> SOAC -> TryFusion ([VName], SOAC)
fuseStreamHelper out_kernms unfus_set outVars outPairs
                 (SOAC.Stream cs2 w2 form2 lam2 inp2_arr)
                 (SOAC.Stream cs1 _ form1 lam1 inp1_arr) =
  if getStreamOrder form2 /= getStreamOrder form1
  then fail "fusion conditions not met!"
  else do -- very similar to redomap o redomap composition,
          -- but need to remove first the `chunk' and `i'
          -- parameters of streams' lambdas and put them
          -- lab in the resulting stream lambda.
          let nes1    = getStreamAccums form1
              chunk1  = head $ lambdaParams lam1
              chunk2  = head $ lambdaParams lam2
              hmnms = HM.fromList [(paramName chunk2, paramName chunk1)]
              lam20 = substituteNames hmnms lam2
              lam1' = lam1  { lambdaParams = tail $ lambdaParams lam1  }
              lam2' = lam20 { lambdaParams = tail $ lambdaParams lam20 }
              (res_lam', new_inp) = fuseRedomap unfus_set outVars nes1 lam1'
                                                inp1_arr outPairs lam2' inp2_arr
              res_lam'' = res_lam' { lambdaParams = chunk1 : lambdaParams res_lam' }
              unfus_accs  = take (length nes1) outVars
              unfus_arrs  = filter (`HS.member` unfus_set) outVars
          res_form <- mergeForms form2 form1
          return (  unfus_accs ++ out_kernms ++ unfus_arrs,
                    SOAC.Stream (cs1++cs2) w2 res_form res_lam'' new_inp )
  where mergeForms (MapLike _) (MapLike o ) = return $ MapLike o
        mergeForms (MapLike _) (RedLike o comm lam0 acc0) = return $ RedLike o comm lam0 acc0
        mergeForms (RedLike o comm lam0 acc0) (MapLike _) = return $ RedLike o comm lam0 acc0
        mergeForms (Sequential acc2) (Sequential acc1) = return $ Sequential (acc1++acc2)
        mergeForms (RedLike _ comm2 lam2r acc2) (RedLike o1 comm1 lam1r acc1) =
            return $ RedLike o1 (comm1<>comm2) (mergeReduceOps lam1r lam2r) (acc1++acc2)
        mergeForms _ _ = fail "Fusing sequential to parallel stream disallowed!"
fuseStreamHelper _ _ _ _ _ _ = fail "Cannot Fuse Streams!"

-- | If a Stream is passed as argument then it converts it to a
--   Sequential Stream; Otherwise it FAILS!
toSeqStream :: SOAC -> TryFusion SOAC
toSeqStream s@(SOAC.Stream _ _ (Sequential _) _ _) = return s
toSeqStream (SOAC.Stream cs w (MapLike _) l inps) =
    return $ SOAC.Stream cs w (Sequential  []) l inps
toSeqStream (SOAC.Stream cs w (RedLike _ _ _ acc) l inps) =
    return $ SOAC.Stream cs w (Sequential acc) l inps
toSeqStream _ = fail "toSeqStream expects a string, but given a SOAC."

-- | This is not currently used, but it might be useful in the future,
--   so I am going to export it in order not to complain about it.
toNestedSeqStream :: SOAC -> TryFusion SOAC
--toNestedSeqStream s@(SOAC.Stream _ (Sequential _) _ _ _) = return s
toNestedSeqStream   (SOAC.Stream cs w form lam arrs) = do
  innerlam      <- renameLambda lam
  instrm_resids <- mapM (newIdent "res_instream") $ lambdaReturnType lam
  let inner_extlam = ExtLambda (lambdaParams innerlam)
                               (lambdaBody   innerlam)
                               (staticShapes $ lambdaReturnType innerlam)
      nes      = getStreamAccums form
      instrm_inarrs = drop (1 + length nes) $ map paramName $ lambdaParams lam
      insoac   = Futhark.Stream cs w form inner_extlam instrm_inarrs
      lam_bind = mkLet' [] instrm_resids $ Op insoac
      lam_body = mkBody [lam_bind] $ map (Futhark.Var . identName) instrm_resids
      lam' = lam { lambdaBody = lam_body }
  return $ SOAC.Stream cs w (Sequential nes) lam' arrs
toNestedSeqStream _ = fail "In toNestedSeqStream: Input paramter not a stream"

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
optimizations = [iswim, scanToScanomap]

iswim :: Maybe [VName] -> SOAC -> SOAC.ArrayTransforms
      -> TryFusion (SOAC, SOAC.ArrayTransforms)
iswim _ (SOAC.Scan cs w scan_fun scan_input) ots
  | Just (map_pat, map_cs, map_w, map_fun) <- rwimPossible scan_fun,
    (nes, arrs) <- unzip scan_input,
    Just nes_names <- mapM subExpVar nes = do

      let nes_idents = zipWith Ident nes_names $ lambdaReturnType scan_fun
          nes' = map SOAC.identInput nes_idents
          map_arrs' = nes' ++ map (SOAC.transposeInput 0 1) arrs
          (scan_acc_params, scan_elem_params) =
            splitAt (length arrs) $ lambdaParams scan_fun
          map_params = map removeParamOuterDim scan_acc_params ++
                       map (setParamOuterDimTo w) scan_elem_params
          map_rettype = map (setOuterDimTo w) $ lambdaReturnType scan_fun
          map_fun' = Lambda map_params map_body map_rettype

          scan_params = lambdaParams map_fun
          scan_body = lambdaBody map_fun
          scan_rettype = lambdaReturnType map_fun
          scan_fun' = Lambda scan_params scan_body scan_rettype
          scan_input' = map (first Var) $
                        uncurry zip $ splitAt (length nes') $ map paramName map_params

          map_body = mkBody [Let (setPatternOuterDimTo w map_pat) () $
                             Op $ Futhark.Scan cs w scan_fun' scan_input'] $
                            map Var $ patternNames map_pat

      let perm = case lambdaReturnType map_fun of
            []  -> []
            t:_ -> 1 : 0 : [2..arrayRank t]
      return (SOAC.Map map_cs map_w map_fun' map_arrs',
              ots SOAC.|> SOAC.Rearrange map_cs perm)

iswim _ _ _ =
  fail "ISWIM does not apply."

scanToScanomap :: Maybe [VName] -> SOAC -> SOAC.ArrayTransforms
               -> TryFusion (SOAC, SOAC.ArrayTransforms)
scanToScanomap _ (SOAC.Scan cs w scan_fun scan_input) ots = do
       let (nes, array_inputs) =  unzip scan_input
       return (SOAC.Scanomap cs w scan_fun scan_fun nes array_inputs, ots)
scanToScanomap _ _ _ =
  fail "Only turn scan into scanomaps"


removeParamOuterDim :: LParam -> LParam
removeParamOuterDim param =
  let t = rowType $ paramType param
  in param { paramAttr = t }

setParamOuterDimTo :: SubExp -> LParam -> LParam
setParamOuterDimTo w param =
  let t = setOuterDimTo w $ paramType param
  in param { paramAttr = t }

setIdentOuterDimTo :: SubExp -> Ident -> Ident
setIdentOuterDimTo w ident =
  let t = setOuterDimTo w $ identType ident
  in ident { identType = t }

setOuterDimTo :: SubExp -> Type -> Type
setOuterDimTo w t =
  arrayOfRow (rowType t) w

setPatternOuterDimTo :: SubExp -> Pattern -> Pattern
setPatternOuterDimTo w pat =
  basicPattern' [] $ map (setIdentOuterDimTo w) $ patternValueIdents pat

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
mapDepth (MapNest.MapNest _ _ lam levels _) =
  min resDims (length levels) + 1
  where resDims = minDim $ case levels of
                    [] -> lambdaReturnType lam
                    nest:_ -> MapNest.nestingReturnType nest
        minDim [] = 0
        minDim (t:ts) = foldl min (arrayRank t) $ map arrayRank ts

pullRearrange :: SOAC -> SOAC.ArrayTransforms
              -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullRearrange soac ots = do
  nest <- join $ liftMaybe <$> MapNest.fromSOAC soac
  SOAC.Rearrange cs perm SOAC.:< ots' <- return $ SOAC.viewf ots
  if rearrangeReach perm <= mapDepth nest then do
    let -- Expand perm to cover the full extent of the input dimensionality
        perm' inp = perm ++ [length perm..SOAC.inputRank inp-1]
        addPerm inp = SOAC.addTransform (SOAC.Rearrange cs $ perm' inp) inp
        inputs' = map addPerm $ MapNest.inputs nest
    soac' <- MapNest.toSOAC $
      inputs' `MapNest.setInputs` rearrangeReturnTypes nest perm
    return (soac', ots')
  else fail "Cannot pull transpose"

pushRearrange :: [VName] -> SOAC -> SOAC.ArrayTransforms
              -> TryFusion (SOAC, SOAC.ArrayTransforms)
pushRearrange inpIds soac ots = do
  nest <- join $ liftMaybe <$> MapNest.fromSOAC soac
  (perm, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest
  if rearrangeReach perm <= mapDepth nest then do
    let invertRearrange = SOAC.Rearrange [] $ rearrangeInverse perm
    soac' <- MapNest.toSOAC $
      inputs' `MapNest.setInputs`
      rearrangeReturnTypes nest perm
    return (soac', ots SOAC.|> invertRearrange)
  else fail "Cannot push transpose"

-- | Actually also rearranges indices.
rearrangeReturnTypes :: MapNest -> [Int] -> MapNest
rearrangeReturnTypes nest@(MapNest.MapNest cs w body nestings inps) perm =
  MapNest.MapNest cs w
  body
  (zipWith setReturnType
   nestings $
   drop 1 $ iterate (map rowType) ts)
  inps
  where origts = MapNest.typeOf nest
        ts =  map (rearrangeType perm) origts

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
          | SOAC.inputRank inp >= d =
              Just $ SOAC.addTransform (SOAC.Rearrange [] $ rearrangeInverse perm) inp
          | otherwise = Nothing

pullReshape :: SOAC -> SOAC.ArrayTransforms -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullReshape (SOAC.Map mapcs _ maplam inps) ots
  | SOAC.Reshape cs shape SOAC.:< ots' <- SOAC.viewf ots,
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
        return $ SOAC.Map [] w inner_fun

  op' <- foldM outersoac (SOAC.Map mapcs mapw' maplam) $
         zip (drop 1 $ reverse $ newDims shape) $
         drop 1 $ reverse $ drop 1 $ tails $ newDims shape
  return (op' inputs', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- We can make a Replicate output-transform part of a map SOAC simply
-- by adding another dimension to the SOAC.
pullReplicate :: SOAC -> SOAC.ArrayTransforms -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullReplicate soac@SOAC.Map{} ots
  | SOAC.Replicate n SOAC.:< ots' <- SOAC.viewf ots = do
      let rettype = SOAC.typeOf soac
      body <- runBodyBinder $ do
        names <- letTupExp "pull_replicate" =<< SOAC.toExp soac
        resultBodyM $ map Var names
      let lam = Lambda { lambdaReturnType = rettype
                       , lambdaBody = body
                       , lambdaParams = []
                       }
      return (SOAC.Map [] n lam [], ots')
pullReplicate _ _ = fail "Cannot pull replicate"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [VName] -> FusedKer
             -> TryFusion (FusedKer, SOAC.ArrayTransforms)
exposeInputs inpIds ker = do
  let soac = fsoac ker
  (exposeInputs' =<< pushRearrange' soac) <|>
    (exposeInputs' =<< pullRearrange' soac) <|>
    exposeInputs' ker
  where ot = outputTransform ker

        pushRearrange' soac = do
          (soac', ot') <- pushRearrange inpIds soac ot
          return ker { fsoac = soac'
                     , outputTransform = ot'
                     }

        pullRearrange' soac = do
          (soac',ot') <- pullRearrange soac ot
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
outputTransformPullers = [pullRearrange, pullReshape, pullReplicate]

pullOutputTransforms :: SOAC -> SOAC.ArrayTransforms
                     -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullOutputTransforms = attempt outputTransformPullers
  where attempt [] _ _ = fail "Cannot pull anything"
        attempt (p:ps) soac ots = do
          (soac',ots') <- p soac ots
          if SOAC.nullTransforms ots' then return (soac', SOAC.noTransforms)
          else pullOutputTransforms soac' ots' <|> return (soac', ots')
          <|> attempt ps soac ots
