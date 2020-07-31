module Futhark.CodeGen.ImpGen.Multicore.SegHist
  (compileSegHist
  )
  where

import Control.Monad
import Data.Maybe
import Data.List
import Prelude hiding (quot, rem)

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.Util (chunks, splitFromEnd, takeLast, maybeNth)
import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem

import Futhark.Util.IntegralExp (rem)
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.MonadFreshNames



onOpCas :: HistOp MCMem -> MulticoreGen ([VName] -> [Imp.Exp] -> MulticoreGen())
onOpCas op = do

  atomics <- hostAtomics <$> askEnv
  let lambda = histOp op
      do_op = atomicUpdateLocking atomics lambda
  case do_op of
    AtomicPrim f -> return f
    AtomicCAS f -> return f
    AtomicLocking f -> do
      let num_locks = 100151
          dims = map (toExp' int32) $
                 shapeDims (histShape op) ++ [histWidth op]
      locks <-
        sStaticArray "hist_locks" DefaultSpace int32 $
        Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 (pure . (`rem` fromIntegral num_locks) . flattenIndex dims)
      return $ f l'


nonsegmentedHist :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> VName
                -> MulticoreGen Imp.Code
nonsegmentedHist pat space histops kbody nsubtasks = do
  emit $ Imp.DebugPrint "nonsegmented segHist" Nothing
  -- let ns = map snd $ unSegSpace space
  -- ns' <- mapM toExp ns
  -- num_threads <- getNumThreads
  -- num_threads' <- toExp $ Var num_threads
  -- hist_width <- toExp $ histWidth $ head histops
  -- TODO we should find a proper condition
  -- let use_small_dest_histogram =  (num_threads' * hist_width) .<=. product ns'
  -- histops' <- renameHistOpLambda histops

  collect $ do
    -- flat_idx <- dPrim "iter" int32
    -- sIf use_small_dest_histogram
    -- smallDestHistogram pat flat_idx space histops num_threads kbody
    casHistogram pat space histops kbody nsubtasks



-- |
-- A different version of segHist
-- Takes sequential version and simpley chunks is
-- Implementation does currently not ensure amotic updates
casHistogram :: Pattern MCMem
             -> SegSpace
             -> [HistOp MCMem]
             -> KernelBody MCMem
             -> VName
             -> MulticoreGen ()
casHistogram pat space histops kbody nsubtasks = do
  emit $ Imp.DebugPrint "largeDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  atomicOps <- mapM onOpCas histops

  idx <- dPrim "iter" int32
  emit $ Imp.DebugPrint "CAS segHist body" Nothing
  body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 idx
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
          (buckets, vs) = splitAt (length histops) red_res
          perOp = chunks $ map (length . histDest) histops

      let pes_per_op = chunks (map (length . histDest) histops) all_red_pes
      forM_ (zip5 histops (perOp vs) buckets atomicOps pes_per_op) $
         \(HistOp dest_w _ _ _ shape lam, vs', bucket, do_op, dest_res) -> do

           let (_is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
               bucket'   = toExp' int32 $ kernelResultSubExp bucket
               dest_w'   = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

           sComment "save map-out results" $
             forM_ (zip map_pes map_res) $ \(pe, res) ->
               copyDWIMFix (patElemName pe) (map Imp.vi32 is) (kernelResultSubExp res) []

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             let bucket_is = map Imp.vi32 (init is) ++ [bucket']
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, res) ->
                 copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
               do_op (map patElemName dest_res) (bucket_is ++ is')


  free_params <- freeParams body (segFlat space : [idx])

  let sched = Imp.Static -- decideScheduling body
  emit $ Imp.Op $ Imp.MCFunc idx mempty body free_params $
      Imp.MulticoreInfo nsubtasks sched (segFlat space)


-- | Locking strategy used for an atomic update.
data Locking =
  Locking { lockingArray :: VName
            -- ^ Array containing the lock.
          , lockingIsUnlocked :: Imp.Exp
            -- ^ Value for us to consider the lock free.
          , lockingToLock :: Imp.Exp
            -- ^ What to write when we lock it.
          , lockingToUnlock :: Imp.Exp
            -- ^ What to write when we unlock it.
          , lockingMapping :: [Imp.Exp] -> [Imp.Exp]
            -- ^ A transformation from the logical lock index to the
            -- physical position in the array.  This can also be used
            -- to make the lock array smaller.
          }

-- | A function for generating code for an atomic update.  Assumes
-- that the bucket is in-bounds.
type DoAtomicUpdate lore r =
  [VName] -> [Imp.Exp] -> MulticoreGen ()

-- | The mechanism that will be used for performing the atomic update.
-- Approximates how efficient it will be.  Ordered from most to least
-- efficient.
data AtomicUpdate lore r
  = AtomicPrim (DoAtomicUpdate lore r)
  | AtomicCAS (DoAtomicUpdate lore r)
    -- ^ Can be done by efficient swaps.
  | AtomicLocking (Locking -> DoAtomicUpdate lore r)
    -- ^ Requires explicit locking.



atomicUpdateLocking :: AtomicBinOp -> Lambda MCMem
                    -> AtomicUpdate MCMem ()
atomicUpdateLocking atomicBinOp lam
  | Just ops_and_ts <- splitOp lam,
    all (\(_, t, _, _) -> supportedPrims(primBitSize t)) ops_and_ts =
    primOrCas ops_and_ts $ \arrs bucket ->
  -- If the operator is a vectorised binary operator on 32-bit values,
  -- we can use a particularly efficient implementation. If the
  -- operator has an atomic implementation we use that, otherwise it
  -- is still a binary operator which can be implemented by atomic
  -- compare-and-swap if 32 bits.
  forM_ (zip arrs ops_and_ts) $ \(a, (op, t, x, y)) -> do

  -- Common variables.
  old <- dPrim "old" t

  (arr', _a_space, bucket_offset) <- fullyIndexArray a bucket

  case opHasAtomicSupport old arr' bucket_offset op of
    Just f -> sOp $ f $ Imp.var y t
    Nothing -> atomicUpdateCAS t a old bucket x $
      x <-- Imp.BinOpExp op (Imp.var x t) (Imp.var y t)

  where opHasAtomicSupport old arr' bucket' bop = do
          let atomic f = Imp.Atomic . f old arr' bucket'
          atomic <$> atomicBinOp bop

        primOrCas ops
          | all isPrim ops = AtomicPrim
          | otherwise      = AtomicCAS

        isPrim (op, _, _, _) = isJust $ atomicBinOp op

atomicUpdateLocking _ op
  | [Prim t] <- lambdaReturnType op,
    [xp, _] <- lambdaParams op,
    supportedPrims (primBitSize t) = AtomicCAS $ \[arr] bucket -> do
      old <- dPrim "old" t
      atomicUpdateCAS t arr old bucket (paramName xp) $
        compileBody' [xp] $ lambdaBody op

atomicUpdateLocking _ op = AtomicLocking $ \locking arrs bucket -> do
  old <- dPrim "old" int32
  continue <- newVName "continue"
  dPrimVol_ continue int32
  continue <-- 0

  -- Correctly index into locks.
  (locks', _locks_space, locks_offset) <-
    fullyIndexArray (lockingArray locking) $ lockingMapping locking bucket

  -- Critical section
  let try_acquire_lock = do
        old <-- 0
        sOp $ Imp.Atomic $
          Imp.AtomicCmpXchg int32 old locks' locks_offset
          continue (lockingToLock locking)
      lock_acquired = Imp.var continue int32 -- .==. lockingIsUnlocked locking
      -- Even the releasing is done with an atomic rather than a
      -- simple write, for memory coherency reasons.
      release_lock = do
        old <-- lockingToLock locking
        sOp $ Imp.Atomic $
          Imp.AtomicCmpXchg int32 old locks' locks_offset
          continue (lockingToUnlock locking)

  -- Preparing parameters. It is assumed that the caller has already
  -- filled the arr_params. We copy the current value to the
  -- accumulator parameters.
  let (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op
      bind_acc_params =
        everythingVolatile $
        sComment "bind lhs" $
        forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
        copyDWIMFix (paramName acc_p) [] (Var arr) bucket

  let op_body = sComment "execute operation" $
                compileBody' acc_params $ lambdaBody op

      do_hist =
        everythingVolatile $
        sComment "update global result" $
        zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params


  -- While-loop: Try to insert your value
  sWhile (Imp.var continue int32 .==. 0) $ do
    try_acquire_lock
    sWhen lock_acquired $ do
      dLParams acc_params
      bind_acc_params
      op_body
      do_hist
      release_lock
      -- break_loop
  where writeArray bucket arr val = copyDWIMFix arr bucket val []


atomicUpdateCAS :: PrimType
                -> VName -> VName
                -> [Imp.Exp] -> VName
                -> MulticoreGen ()
                -> MulticoreGen ()
atomicUpdateCAS t arr old bucket x do_op = do
  -- Code generation target:
  --
  -- old = d_his[idx];
  -- do {
  --   assumed = old;
  --   x = do_op(assumed, y);
  --   old = atomicCAS(&d_his[idx], assumed, tmp);
  -- } while(assumed != old);
  run_loop <- dPrimV "run_loop" 0
  everythingVolatile $ copyDWIMFix old [] (Var arr) bucket
  (arr', _a_space, bucket_offset) <- fullyIndexArray arr bucket

  bytes <- toIntegral $ primBitSize t
  (to, from) <- getBitConvertFunc $ primBitSize t
  -- While-loop: Try to insert your value
  let (toBits, _fromBits) =
        case t of FloatType _ ->
                    (\v -> Imp.FunExp to [v] bytes,
                     \v -> Imp.FunExp from [v] t)
                  _           -> (id, id)

  sWhile (Imp.var run_loop int32 .==. 0) $ do
    x <-- Imp.var old t
    do_op -- Writes result into x
    sOp $ Imp.Atomic $
      Imp.AtomicCmpXchg bytes old arr' bucket_offset
      run_loop (toBits (Imp.var x t))

-- | Horizontally fission a lambda that models a binary operator.
splitOp :: ASTLore lore => Lambda lore -> Maybe [(BinOp, PrimType, VName, VName)]
splitOp lam = mapM splitStm $ bodyResult $ lambdaBody lam
  where n = length $ lambdaReturnType lam
        splitStm (Var res) = do
          Let (Pattern [] [pe]) _ (BasicOp (BinOp op (Var x) (Var y))) <-
            find (([res]==) . patternNames . stmPattern) $
            stmsToList $ bodyStms $ lambdaBody lam
          i <- Var res `elemIndex` bodyResult (lambdaBody lam)
          xp <- maybeNth i $ lambdaParams lam
          yp <- maybeNth (n+i) $ lambdaParams lam
          guard $ paramName xp == x
          guard $ paramName yp == y
          Prim t <- Just $ patElemType pe
          return (op, t, paramName xp, paramName yp)
        splitStm _ = Nothing


getBitConvertFunc :: Int -> MulticoreGen (String, String)
-- getBitConvertFunc 8 = ("to_bits8, from_bits8")
-- getBitConvertFunc 16 = ("to_bits8, from_bits8")
getBitConvertFunc 32 = return  ("to_bits32", "from_bits32")
getBitConvertFunc 64 = return  ("to_bits64", "from_bits64")
getBitConvertFunc b = error $ "number of bytes is supported " ++ pretty b


supportedPrims :: Int -> Bool
supportedPrims 8  = True
supportedPrims 16 = True
supportedPrims 32 = True
supportedPrims 64 = True
supportedPrims _  = False

-- Supported bytes lengths by GCC (and clang) compiler
toIntegral :: Int -> MulticoreGen PrimType
toIntegral 8  =  return int8
toIntegral 16 =  return int16
toIntegral 32 =  return int32
toIntegral 64 = return int64
toIntegral b  = error $ "number of bytes is supported for CAS - " ++ pretty b

segmentedHist :: Pattern MCMem
              -> SegSpace
              -> [HistOp MCMem]
              -> KernelBody MCMem
              -> VName
              -> MulticoreGen Imp.Code
segmentedHist pat space histops kbody ntasks = do
  emit $ Imp.DebugPrint "Segmented segHist" Nothing

  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int32
  collect $ do
    par_body <- compileSegHistBody n_segments pat space histops kbody
    free_params <- freeParams par_body [segFlat space, n_segments]
    let sched = decideScheduling par_body
    emit $ Imp.Op $ Imp.MCFunc n_segments mempty par_body free_params $
      Imp.MulticoreInfo ntasks sched (segFlat space)


-- nonsegmentedHist :: Pattern MCMem
--                 -> SegSpace
--                 -> [HistOp MCMem]
--                 -> KernelBody MCMem
--                 -> VName
--                 -> MulticoreGen Imp.Code
-- nonsegmentedHist pat space histops kbody ntasks  = do
--   emit $ Imp.DebugPrint "nonsegmented segHist" Nothing
--   -- let ns = map snd $ unSegSpace space
--   -- ns' <- mapM toExp ns

--   -- num_threads' <- toExp $ Var num_threads
--   -- hist_width <- toExp $ histWidth $ head histops
--   -- TODO we should find a proper condition
--   -- let use_small_dest_histogram =  (num_threads' * hist_width) .<=. product ns'
--   -- histops' <- renameHistOpLambda histops

--   collect $ do
--     flat_idx <- dPrim "iter" int32
--     -- sIf use_small_dest_histogram
--     smallDestHistogram pat flat_idx space histops ntasks kbody
--      -- (largeDestHistogram pat space histops' kbody)


-- Generates num_threads sub histograms of the size
-- of the destination histogram
-- Then for each chunk of the input each subhistogram
-- is computed and finally merged through a reduction
-- across the histogram indicies.
-- This is expected to be fast if len(histDest) is small
smallDestHistogram :: Pattern MCMem
                   -> VName
                   -> SegSpace
                   -> [HistOp MCMem]
                   -> VName
                   -> KernelBody MCMem
                   -> MulticoreGen ()
smallDestHistogram pat flat_idx space histops num_histos kbody = do
  emit $ Imp.DebugPrint "smallDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns


  num_histos' <- toExp $ Var num_histos
  let pes = patternElements pat
      num_red_res = length histops + sum (map (length . histNeutral) histops)
      (_all_red_pes, map_pes) = splitAt num_red_res pes

  let per_red_pes = segHistOpChunks histops $ patternValueElements pat
  hist_M <- dPrimV "hist_M" num_histos'

  init_histograms <-
    prepareIntermediateArrays hist_M histops

  hist_H_chks <- forM (map histWidth histops) $ \w -> do
    w' <- toExp w
    dPrimV "hist_H_chk" w'

  tid' <- toExp $ Var $ segFlat space

  -- Actually allocate subhistograms
  histograms <- forM (zip init_histograms hist_H_chks) $
                \(init_local_subhistos, hist_H_chk) -> do
    (local_subhistos, do_op) <- init_local_subhistos (Var hist_H_chk)
    return (local_subhistos, hist_H_chk, do_op)

  prebody <- collect $ do
    emit $ Imp.DebugPrint "nonsegmented segHist stage 1" Nothing
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
    forM_ (zip4 per_red_pes histograms hist_H_chks histops) $ \(pes', (hists, _, _), hist_H_chk, histop) ->
      forM_ (zip3 pes' hists (histNeutral histop)) $ \(pe, hist, ne) -> do
        hist_H_chk' <- toExp $ Var hist_H_chk

        -- First thread initializes histrogram wtih dest vals
        -- Others initialize with neutral element
        let is_first_tid = tid' .==. 0
        sFor "i" hist_H_chk' $ \i ->
          sIf is_first_tid
              (copyDWIMFix hist (tid' : [i])
                           (Var $ patElemName pe) (map Imp.vi32 (init is) ++ [i]))
              (sLoopNest (histShape histop) $ \is' ->
                 copyDWIMFix hist (tid' : i : is') ne [])


  -- Generate loop body of parallel function
  body <- collect $ do
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
     compileStms mempty (kernelBodyStms kbody) $ do
       let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
           (buckets, vs) = splitAt (length histops) red_res
           perOp = chunks $ map (length . histDest) histops

       sComment "save map-out results" $
          forM_ (zip map_pes map_res) $ \(pe, res) ->
          copyDWIMFix (patElemName pe)
          (map Imp.vi32 is) (kernelResultSubExp res) []

       forM_ (zip4 histops histograms buckets (perOp vs)) $
         \(HistOp dest_w _ _ _ shape lam,
           (_, _hist_H_chk, do_op), bucket, vs') -> do

           let bucket' = toExp' int32 $ kernelResultSubExp bucket
               dest_w' = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
               vs_params = takeLast (length vs') $ lambdaParams lam
               bucket_is = [tid', bucket']

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, res) ->
                 copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
               do_op (bucket_is ++ is')

  free_params <- freeParams (prebody <> body) (segFlat space : [flat_idx])
  let sched = Imp.Static

  emit $ Imp.Op $ Imp.MCFunc flat_idx prebody body free_params $
      Imp.MulticoreInfo num_histos sched (segFlat space)


  emit $ Imp.DebugPrint "nonsegmented hist stage 2"  Nothing
  forM_ (zip3 per_red_pes histograms histops) $ \(red_pes, (hists,_,_),  op) -> do
    bucket_id <- newVName "bucket_id"
    subhistogram_id <- newVName "subhistogram_id"

    -- let unitHistoCase =
    --    -- This is OK because the memory blocks are at least as
    --    -- large as the ones we are supposed to use for the result.
    --      forM_ (zip red_pes hists) $ \(pe, subhisto) -> do
    --        emit $ Imp.DebugPrint "unitHistoCase" Nothing
    --        pe_mem <- memLocationName . entryArrayLocation <$>
    --                  lookupArray (patElemName pe)
    --        subhisto_mem <- memLocationName . entryArrayLocation <$>
    --                        lookupArray subhisto
    --        emit $ Imp.SetMem pe_mem subhisto_mem DefaultSpace

    -- sIf (Imp.var num_histos int32 .==. 1) unitHistoCase $ do

    emit $ Imp.DebugPrint "multiHistocase" Nothing

    let num_buckets = histWidth op

    let segred_space =
          SegSpace (segFlat space) $
          segment_dims ++
          [(bucket_id, num_buckets)] ++
          [(subhistogram_id, Var num_histos)]

    let segred_op = SegBinOp Noncommutative (histOp op) (histNeutral op) (histShape op)
        ns_red = map snd $ unSegSpace segred_space
    ns_red' <- mapM toExp ns_red

    let iterations = case unSegSpace segred_space of
                       [_] -> product ns_red'
                       _   -> product $ init ns_red' -- Segmented reduction is over the inner most dimension
    let retvals = map patElemName red_pes
    retvals_ts <- mapM lookupType retvals
    retval_params <- zipWithM toParam retvals retvals_ts
    let retval_names = map Imp.paramName retval_params

    nsubtasks_red <- dPrim "num_tasks" $ IntType Int32
    red_code <- compileSegRed' (Pattern [] red_pes) segred_space [segred_op] nsubtasks_red $ \red_cont ->
      red_cont $ flip map hists $ \subhisto ->
            (Var subhisto, map Imp.vi32 $
              map fst segment_dims ++ [subhistogram_id, bucket_id])

    free_params_red <- freeParams red_code ([segFlat space, nsubtasks_red] ++ retval_names )
    emit $ Imp.Op $ Imp.Task free_params_red iterations red_code Nothing (segFlat space) nsubtasks_red retval_params Imp.Static

   where segment_dims = init $ unSegSpace space


prepareIntermediateArrays :: VName
                          -> [HistOp MCMem]
                          -> MulticoreGen [InitLocalHistograms]
prepareIntermediateArrays num_subhistos_per_group =
  mapM (onOp num_subhistos_per_group)

compileSegHist  :: Pattern MCMem
                -> SegSpace
                -> [HistOp MCMem]
                -> KernelBody MCMem
                -> VName
                -> MulticoreGen Imp.Code
compileSegHist pat space histops kbody
  | [_] <- unSegSpace space =
      nonsegmentedHist pat space histops kbody
  | otherwise =
      segmentedHist pat space histops kbody

-- | Split some list into chunks equal to the number of values
-- returned by each 'SegBinOp'
segHistOpChunks :: [HistOp lore] -> [a] -> [[a]]
segHistOpChunks = chunks . map (length . histNeutral)


compileSegHistBody :: VName
                   -> Pattern MCMem
                   -> SegSpace
                   -> [HistOp MCMem]
                   -> KernelBody MCMem
                   -> MulticoreGen Imp.Code
compileSegHistBody idx pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (_all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat
      per_red_pes = segHistOpChunks histops $ patternValueElements pat

  idx' <- toExp $ Var idx
  collect $ do
    emit $ Imp.DebugPrint "Segmented segHist" Nothing
    let inner_bound = last ns'

    sFor "i" inner_bound $ \i -> do
      zipWithM_ dPrimV_ (init is) $ unflattenIndex (init ns') idx'
      dPrimV_ (last is) i

      compileStms mempty (kernelBodyStms kbody) $ do
        let (_red_res, map_res) = splitFromEnd (length map_pes) $
                                 map kernelResultSubExp $ kernelBodyResult kbody
            (buckets, vs) = splitAt (length histops) _red_res
            perOp = chunks $ map (length . histDest) histops

        forM_ (zip4 per_red_pes histops (perOp vs) buckets) $
           \(red_res, HistOp dest_w _ _ _ shape lam, vs', bucket) -> do

             let (is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
                 bucket'   = toExp' int32 bucket
                 dest_w'   = toExp' int32 dest_w
                 bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

             sComment "save map-out results" $
               forM_ (zip map_pes map_res) $ \(pe, res) ->
                 copyDWIMFix (patElemName pe) (map Imp.vi32 is) res []

             sComment "perform updates" $
               sWhen bucket_in_bounds $ do
               dLParams $ lambdaParams lam
               sLoopNest shape $ \is' -> do
                 -- Index
                 buck <- toExp bucket
                 forM_ (zip red_res is_params) $ \(pe, p) ->
                   copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 (init is)  ++ [buck] ++ is')
                 -- Value at index
                 forM_ (zip vs_params vs') $ \(p, v) ->
                   copyDWIMFix (paramName p) [] v is'
                 compileStms mempty (bodyStms $ lambdaBody lam) $
                   forM_ (zip red_res  $ bodyResult $ lambdaBody lam) $
                   \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ [buck] ++ is') se []

type InitLocalHistograms = SubExp -> MulticoreGen ([VName], [Imp.Exp] -> MulticoreGen ())

onOp :: VName -> HistOp MCMem -> MulticoreGen InitLocalHistograms
onOp num_subhistos_per_group op = do
  let mk_op arrs bucket = do
        let acc_params = take (length arrs) $ lambdaParams $ histOp op
            bind_acc_params =
              sComment "bind lhs" $
                forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
                  copyDWIMFix (paramName acc_p) [] (Var arr) bucket
            op_body = sComment "execute operation" $
                      compileBody' acc_params $ lambdaBody $ histOp op
            do_hist =
              sComment "update sub hist result" $
              zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

        sComment "Start of body" $ do
          dLParams acc_params
          bind_acc_params
          op_body
          do_hist

  -- Initialise local-memory sub-histograms.  These are
  -- represented as two-dimensional arrays.
  let init_local_subhistos hist_H_chk = do
        local_subhistos <-
          forM (histType op) $ \t -> do
            let sub_local_shape = Shape [Var num_subhistos_per_group] <>
                                  (arrayShape t `setOuterDim` hist_H_chk)
            sAllocArray "subhistogram" (elemType t) sub_local_shape DefaultSpace

        return (local_subhistos, mk_op local_subhistos)
  return init_local_subhistos

  where
    writeArray bucket arr val = copyDWIMFix arr bucket val []
