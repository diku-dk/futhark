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
  , SOACNest
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
import qualified Futhark.Analysis.HORepresentation.SOACNest as Nest
import qualified Futhark.Analysis.HORepresentation.MapNest as MapNest
import Futhark.Optimise.Fusion.TryFusion
import Futhark.Optimise.Fusion.Composing
import Futhark.Construct
import qualified Futhark.Analysis.ScalExp as SE

type SOAC = SOAC.SOAC SOACS
type SOACNest = Nest.SOACNest SOACS
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
  (soac', ots) <- optimizeSOAC Nothing soac
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

attemptFusion :: (MonadFreshNames m, HasScope SOACS m) =>
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
      unfus_nms= HS.toList unfus_set
      w        = SOAC.width soac1
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
  case (soac2, soac1) of
    ------------------------------
    -- Redomap-Redomap Fusions: --
    ------------------------------
    (SOAC.Map {}, SOAC.Map    {})
      | mapFusionOK outVars ker || horizFuse -> do
      let (res_lam, new_inp) = fuseMaps unfus_nms lam1 inp1_arr outPairs lam2 inp2_arr
          (extra_nms,extra_rtps) = unzip $ filter (\(nm,_)->elem nm unfus_nms) $
                                   zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
      success (outNames ker ++ extra_nms) $
              SOAC.Map (cs1++cs2) w res_lam' new_inp

    (SOAC.Map {}, SOAC.Redomap _ _ comm1 lam11 _ nes _)
      | mapFusionOK (drop (length nes) outVars) ker || horizFuse -> do
      let (res_lam', new_inp) = fuseRedomap unfus_nms outVars nes lam1 inp1_arr
                                            outPairs lam2 inp2_arr
          unfus_accs  = take (length nes) outVars
          unfus_arrs  = unfus_nms \\ unfus_accs
      success (unfus_accs ++ outNames ker ++ unfus_arrs) $
              SOAC.Redomap (cs1++cs2) w comm1 lam11 res_lam' nes new_inp

    (SOAC.Redomap _ _ comm2 lam2r _ nes2 _, SOAC.Redomap _ _ comm1 lam1r _ nes1 _)
      | mapFusionOK (drop (length nes1) outVars) ker || horizFuse -> do
      let (res_lam', new_inp) = fuseRedomap unfus_nms outVars nes1 lam1 inp1_arr
                                            outPairs lam2 inp2_arr
          unfus_accs  = take (length nes1) outVars
          unfus_arrs  = unfus_nms \\ unfus_accs
          lamr        = mergeReduceOps lam1r lam2r
      success (unfus_accs ++ outNames ker ++ unfus_arrs) $
              SOAC.Redomap (cs1++cs2) w (comm1<>comm2) lamr res_lam' (nes1++nes2) new_inp

    (SOAC.Redomap _ _ comm2 lam21 _ nes _, SOAC.Map {})
      | mapFusionOK outVars ker || horizFuse -> do
      let (res_lam, new_inp) = fuseMaps unfus_nms lam1 inp1_arr outPairs lam2 inp2_arr
          (_,extra_rtps) = unzip $ filter (\(nm,_)->elem nm unfus_nms) $
                           zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
      success (outNames ker ++ unfus_nms) $
              SOAC.Redomap (cs1++cs2) w comm2 lam21 res_lam' nes new_inp
    ----------------------------
    -- Stream-Stream Fusions: --
    ----------------------------
    (SOAC.Stream _ _ Sequential{} _ _, SOAC.Stream _ _ form1@Sequential{} _ _)
     | mapFusionOK (drop (length $ getStreamAccums form1) outVars) ker || horizFuse -> do
      -- fuse two SEQUENTIAL streams
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_nms outVars outPairs soac2 soac1
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
      (res_nms, res_stream) <- fuseStreamHelper (outNames ker) unfus_nms outVars outPairs soac2 soac1
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

fuseStreamHelper :: [VName] -> [VName] -> [VName] -> [(VName,Ident)]
                 -> SOAC -> SOAC -> TryFusion ([VName], SOAC)
fuseStreamHelper out_kernms unfus_nms outVars outPairs
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
              (res_lam', new_inp) = fuseRedomap unfus_nms outVars nes1 lam1'
                                                inp1_arr outPairs lam2' inp2_arr
              res_lam'' = res_lam' { lambdaParams = chunk1 : lambdaParams res_lam' }
              unfus_accs  = take (length nes1) outVars
              unfus_arrs  = unfus_nms \\ unfus_accs
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
  let inner_extlam = ExtLambda (lambdaIndex innerlam)
                               (lambdaParams innerlam)
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
  startNest <- Nest.fromSOAC $ fsoac ker
  (resNest, resTrans) <- optimizeSOACNest inp startNest startTrans
  soac <- Nest.toSOAC resNest
  return $ ker { fsoac = soac
               , outputTransform = resTrans
               }
  where startTrans = outputTransform ker

optimizeSOAC :: Maybe [VName] -> SOAC -> TryFusion (SOAC, SOAC.ArrayTransforms)
optimizeSOAC inp soac = do
  nest <- Nest.fromSOAC soac
  (nest', ots) <- optimizeSOACNest inp nest SOAC.noTransforms
  soac' <- Nest.toSOAC nest'
  return (soac', ots)

optimizeSOACNest :: Maybe [VName] -> SOACNest -> SOAC.ArrayTransforms
                 -> TryFusion (SOACNest, SOAC.ArrayTransforms)
optimizeSOACNest inp soac os = do
  res <- foldM comb (False, soac, os) $ reverse optimizations
  case res of
    (False, _, _)      -> fail "No optimisation applied"
    (True, soac', os') -> return (soac', os')
  where comb (changed, soac', os') f = do
          (soac'', os'') <- f inp soac' os
          return (True, soac'', os'')
          <|> return (changed, soac', os')

type Optimization = Maybe [VName]
                    -> SOACNest
                    -> SOAC.ArrayTransforms
                    -> TryFusion (SOACNest, SOAC.ArrayTransforms)

optimizations :: [Optimization]
optimizations = [iswim]

iswim :: Maybe [VName] -> SOACNest -> SOAC.ArrayTransforms
      -> TryFusion (SOACNest, SOAC.ArrayTransforms)
iswim _ nest ots
  | Nest.Scan cs1 w1 (Nest.NewNest lvl nn) es <- Nest.operation nest,
    Nest.Map cs2 w2 mb <- nn,
    Just es' <- mapM Nest.inputFromTypedSubExp es,
    Nest.Nesting i paramIds mapArrs bndIds retTypes <- lvl,
    mapM isVarInput mapArrs == Just paramIds = do
    let newInputs :: [SOAC.Input]
        newInputs = es' ++ map (SOAC.transposeInput 0 1) (Nest.inputs nest)
        inputTypes = map SOAC.inputType newInputs
        (accsizes, arrsizes) =
          splitAt (length es) $ map rowType inputTypes
        innerAccParams = zipWith Nest.TypedSubExp
                         (map Var $ take (length es) paramIds) accsizes
        innerArrParams = zipWith Ident (drop (length es) paramIds) arrsizes
    let innerScan = Nest.Scan cs2 w1 mb innerAccParams
        scanNest = Nest.Nesting {
                     Nest.nestingInputs = map SOAC.identInput innerArrParams
                   , Nest.nestingReturnType = zipWith setOuterSize retTypes $
                                              map (arraySize 0) arrsizes
                   , Nest.nestingResult = bndIds
                   , Nest.nestingIndex = i
                   , Nest.nestingParamNames = paramIds
                   }
        perm = case retTypes of []  -> []
                                t:_ -> transposeIndex 0 1 [0..arrayRank t]
        nest' = Nest.SOACNest
                newInputs
                (Nest.Map cs1 w2 (Nest.NewNest scanNest innerScan))
    return (nest',
            ots SOAC.|> SOAC.Rearrange cs2 perm)
iswim _ _ _ = fail "ISWIM does not apply"

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
mapDepth (MapNest.MapNest _ _ body levels _) =
  -- XXX: The restriction to pure nests is conservative, but we cannot
  -- know whether an arbitrary postbody is dependent on the exact size
  -- of the nesting result.
  min resDims (length levels) + 1
  where resDims = minDim $ case levels of
                    [] -> case body of Nest.Fun lam ->
                                         lambdaReturnType lam
                                       Nest.NewNest nest _ ->
                                         Nest.nestingReturnType nest
                    nest:_ -> MapNest.nestingReturnType nest
        minDim [] = 0
        minDim (t:ts) = foldl min (arrayRank t) $ map arrayRank ts

pullRearrange :: SOACNest -> SOAC.ArrayTransforms
              -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pullRearrange nest ots = do
  nest' <- join $ liftMaybe <$> MapNest.fromSOACNest nest
  SOAC.Rearrange cs perm SOAC.:< ots' <- return $ SOAC.viewf ots
  if rearrangeReach perm <= mapDepth nest' then
    let -- Expand perm to cover the full extent of the input dimensionality
        perm' inp = perm ++ [length perm..SOAC.inputRank inp-1]
        addPerm inp = SOAC.addTransform (SOAC.Rearrange cs $ perm' inp) inp
        inputs' = map addPerm $ MapNest.inputs nest'
    in return (MapNest.toSOACNest $
               inputs' `MapNest.setInputs`
               rearrangeReturnTypes nest' perm,
               ots')
  else fail "Cannot pull transpose"

pushRearrange :: [VName] -> SOACNest -> SOAC.ArrayTransforms
              -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pushRearrange inpIds nest ots = do
  nest' <- join $ liftMaybe <$> MapNest.fromSOACNest nest
  (perm, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest'
  if rearrangeReach perm <= mapDepth nest' then do
    let invertRearrange = SOAC.Rearrange [] $ rearrangeInverse perm
        nest'' = MapNest.toSOACNest $
                 inputs' `MapNest.setInputs`
                 rearrangeReturnTypes nest' perm
    return (nest'',
            ots SOAC.|> invertRearrange)
  else fail "Cannot push transpose"

-- | Actually also rearranges indices.
rearrangeReturnTypes :: MapNest -> [Int] -> MapNest
rearrangeReturnTypes nest@(MapNest.MapNest cs w body nestings inps) perm =
  MapNest.MapNest cs w
  (inner_index `Nest.setNestBodyIndex` body)
  (zipWith setIndex
   (zipWith setReturnType
    nestings $
    drop 1 $ iterate (map rowType) ts)
   outer_indices)
  inps
  where origts = MapNest.typeOf nest
        ts =  map (rearrangeType perm) origts

        orig_indices =
          map MapNest.nestingIndex nestings ++ [Nest.nestBodyIndex body]

        (outer_indices,inner_index) =
          case reverse $
               rearrangeShape (take (length orig_indices) perm) orig_indices of
           i:is -> (reverse is, i)
           []   -> ([], Nest.nestBodyIndex body)

        setReturnType nesting t' =
          nesting { MapNest.nestingReturnType = t' }
        setIndex nesting index =
          nesting { MapNest.nestingIndex = index }

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

pullReshape :: SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pullReshape nest ots
  | SOAC.Reshape cs shape SOAC.:< ots' <- SOAC.viewf ots,
    op@(Nest.Map _ _ mapbody) <- Nest.operation nest,
    all primType $ Nest.returnType op = do
  let mapw' = case reverse $ newDims shape of
        []  -> intConst Int32 0
        d:_ -> d
      inputs' = map (SOAC.addTransform $ SOAC.ReshapeOuter cs shape) $
                Nest.inputs nest
      inputTypes = map SOAC.inputType inputs'
      innermost_input_ts = map (stripArray (length shape - 1)) inputTypes
  new_index <- newVName "pull_reshape_inner_index"
  new_indices <- replicateM (length (newDims shape) - 1) $
                 newVName "pull_reshape_index"
  maplam <- Nest.bodyToLambda innermost_input_ts mapbody
  let old_index = lambdaIndex maplam
      indices_scope = HM.fromList $ zip (new_index:new_indices) $ repeat IndexInfo
      flat_idx = flattenIndex
                 (map SE.intSubExpToScalExp $ newDims shape)
                 (map (SE.intSubExpToScalExp . Var) $ new_indices++[new_index])
  compute_old_index_bnds <- runBinder_ $ localScope indices_scope $
    letBindNames'_ [old_index] =<< SE.fromScalExp flat_idx

  let mapbody' = Nest.Fun maplam { lambdaIndex = new_index
                                 , lambdaBody =
                                     insertBindings compute_old_index_bnds $
                                     lambdaBody maplam
                                 }
      outernest inner (i, w, outershape) = do
        let addDims t = arrayOf t (Shape outershape) NoUniqueness
            retTypes = map addDims $ Nest.returnType op

        ps <- forM inputTypes $ \inpt -> do
          let t = rowType (stripArray (length outershape-2) inpt)
          newIdent "pullReshape_param" t

        bnds <- forM retTypes $ \_ ->
                  newNameFromString "pullReshape_bnd"

        let nesting = Nest.Nesting {
                        Nest.nestingIndex = i
                      , Nest.nestingParamNames = map identName ps
                      , Nest.nestingInputs = map SOAC.identInput ps
                      , Nest.nestingResult = bnds
                      , Nest.nestingReturnType = retTypes
                      }
        return $ Nest.Map [] w (Nest.NewNest nesting inner)
  -- Only have the certificates on the outermost loop nest.  This
  -- only has the significance of making the generated code look
  -- very slightly neater.
  op' <- foldM outernest (Nest.Map [] mapw' mapbody') $
         zip3 new_indices (drop 1 $ reverse $ newDims shape) $
         drop 1 $ reverse $ drop 1 $ tails $ newDims shape
  let nest'   = Nest.SOACNest {
                  Nest.inputs    = inputs'
                , Nest.operation =
                  Nest.combCertificates op `Nest.setCombCertificates` op'
                }
  return (nest', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [VName] -> FusedKer
             -> TryFusion (FusedKer, SOAC.ArrayTransforms)
exposeInputs inpIds ker = do
  nest <- Nest.fromSOAC $ fsoac ker
  (exposeInputs' =<< pushRearrange' nest) <|>
    (exposeInputs' =<< pullRearrange' nest) <|>
    exposeInputs' ker
  where ot = outputTransform ker

        pushRearrange' nest = do
          (nest', ot') <- pushRearrange inpIds nest ot
          soac         <- Nest.toSOAC nest'
          return ker { fsoac = soac, outputTransform = ot' }

        pullRearrange' nest = do
          (nest',ot') <- pullRearrange nest ot
          unless (SOAC.nullTransforms ot') $
            fail "pullRearrange was not enough"
          soac        <- Nest.toSOAC nest'
          return ker { fsoac = soac,
                       outputTransform = SOAC.noTransforms
                     }

        exposeInputs' ker' =
          case commonTransforms inpIds $ inputs ker' of
            (ot', inps') | all exposed inps' ->
              return (ker' { fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
            _ -> fail "Cannot expose"

        exposed (SOAC.Input ts _ _)
          | SOAC.nullTransforms ts = True
        exposed inp = SOAC.inputArray inp `notElem` inpIds

outputTransformPullers :: [SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)]
outputTransformPullers = [pullRearrange, pullReshape]

pullOutputTransforms :: SOAC -> SOAC.ArrayTransforms
                     -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullOutputTransforms soac origOts = do
  nest <- Nest.fromSOAC soac
  (nest', ots') <- attemptAll nest origOts
  soac' <- Nest.toSOAC nest'
  return (soac', ots')
  where attemptAll nest ots = attempt nest ots outputTransformPullers
        attempt _ _ [] = fail "Cannot pull anything"
        attempt nest ots (p:ps) = do
          (nest',ots') <- p nest ots
          if SOAC.nullTransforms ots' then return (nest', SOAC.noTransforms)
          else attemptAll nest' ots' <|> return (nest', ots')
          <|> attempt nest ots ps
