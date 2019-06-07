{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.CodeGen.ImpGen.Kernels.Base
  ( KernelConstants (..)
  , inKernelOperations
  , keyWithEntryPoint
  , CallKernelGen
  , InKernelGen
  , computeThreadChunkSize
  , simpleKernelConstants
  , kernelInitialisation
  , kernelInitialisationSimple
  , kernelInitialisationSetSpace
  , setSpaceIndices
  , makeAllMemoryGlobal
  , allThreads
  , compileKernelStms
  , groupReduce
  , groupScan
  , isActive
  , sKernel
  , sReplicate
  , sIota
  , sCopy
  , compileKernelResult
  , virtualiseGroups

  , atomicUpdate
  , atomicUpdateLocking
  , Locking(..)
  , AtomicUpdate
  )
  where

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

import Prelude hiding (quot, rem)

import Futhark.Error
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import Futhark.CodeGen.ImpGen
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem, IntegralExp)
import Futhark.Util (splitAt3, maybeNth, takeLast)

type CallKernelGen = ImpM ExplicitMemory Imp.HostOp
type InKernelGen = ImpM InKernel Imp.KernelOp

data KernelConstants = KernelConstants
                       { kernelOuterVTable :: VTable ExplicitMemory -- XXX
                       , kernelGlobalThreadId :: Imp.Exp
                       , kernelLocalThreadId :: Imp.Exp
                       , kernelGroupId :: Imp.Exp
                       , kernelGlobalThreadIdVar :: VName
                       , kernelLocalThreadIdVar :: VName
                       , kernelGroupIdVar :: VName
                       , kernelGroupSize :: Imp.Exp
                       , kernelNumGroups :: Imp.Exp
                       , kernelNumThreads :: Imp.Exp
                       , kernelWaveSize :: Imp.Exp
                       , kernelDimensions :: [(VName, Imp.Exp)]
                       , kernelThreadActive :: Imp.Exp
                       , kernelStreamed :: [(VName, Imp.DimSize)]
                       -- ^ Chunk sizes and their maximum size.  Hint
                       -- for unrolling.
                       }

inKernelOperations :: KernelConstants -> Operations InKernel Imp.KernelOp
inKernelOperations constants =
  (defaultOperations $ compileInKernelOp constants)
  { opsCopyCompiler = inKernelCopy
  , opsExpCompiler = inKernelExpCompiler
  , opsStmsCompiler = \_ -> compileKernelStms constants
  , opsAllocCompilers =
      M.fromList [ (Space "local", allocLocal)
                 , (Space "private", allocPrivate) ]
  }
  where allocLocal :: AllocCompiler InKernel Imp.KernelOp
        allocLocal mem size = do
          size' <- localMemSize (kernelOuterVTable constants) size
          sOp $ Imp.LocalAlloc mem size'
        allocPrivate mem size =
          sOp $ Imp.PrivateAlloc mem size

keyWithEntryPoint :: Name -> Name -> Name
keyWithEntryPoint fname key =
  nameFromString $ nameToString fname ++ "." ++ nameToString key

-- | We have no bulk copy operation (e.g. memmove) inside kernels, so
-- turn any copy into a loop.
inKernelCopy :: CopyCompiler InKernel Imp.KernelOp
inKernelCopy = copyElementWise

compileInKernelOp :: KernelConstants -> Pattern InKernel -> Op InKernel
                  -> InKernelGen ()
compileInKernelOp _ (Pattern _ [mem]) (Alloc size (Space "private")) = do
  size' <- toExp size
  sOp $ Imp.PrivateAlloc (patElemName mem) $ Imp.bytes size'
compileInKernelOp constants (Pattern _ [mem]) (Alloc size (Space "local")) = do
  size' <- localMemSize (kernelOuterVTable constants) . Imp.bytes =<<
           toExp size
  sOp $ Imp.LocalAlloc (patElemName mem) size'
compileInKernelOp _ (Pattern _ [mem]) Alloc{} =
  compilerLimitationS $ "Cannot allocate memory block " ++ pretty mem ++ " in kernel."
compileInKernelOp _ dest Alloc{} =
  compilerBugS $ "Invalid target for in-kernel allocation: " ++ show dest
compileInKernelOp constants pat (Inner op) =
  compileKernelExp constants pat op

inKernelExpCompiler :: ExpCompiler InKernel Imp.KernelOp
inKernelExpCompiler _ (BasicOp (Assert _ _ (loc, locs))) =
  compilerLimitationS $
  unlines [ "Cannot compile assertion at " ++
            intercalate " -> " (reverse $ map locStr $ loc:locs) ++
            " inside parallel kernel."
          , "As a workaround, surround the expression with 'unsafe'."]
-- The static arrays stuff does not work inside kernels.
inKernelExpCompiler (Pattern _ [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0..] es) $ \(i,e) ->
  copyDWIM (patElemName dest) [fromIntegral (i::Int32)] e []
inKernelExpCompiler dest e =
  defCompileExp dest e

compileKernelExp :: KernelConstants -> Pattern InKernel -> KernelExp InKernel
                 -> InKernelGen ()

compileKernelExp _ pat (Barrier ses) = do
  forM_ (zip (patternNames pat) ses) $ \(d, se) ->
    copyDWIM d [] se []
  sOp Imp.LocalBarrier

compileKernelExp _ (Pattern [] [size]) (SplitSpace o w i elems_per_thread) = do
  num_elements <- Imp.elements <$> toExp w
  i' <- toExp i
  elems_per_thread' <- Imp.elements <$> toExp elems_per_thread
  computeThreadChunkSize o i' elems_per_thread' num_elements (patElemName size)

compileKernelExp constants pat (Combine (CombineSpace scatter cspace) _ aspace body) = do
  -- First we compute how many times we have to iterate to cover
  -- cspace with our group size.  It is a fairly common case that
  -- we statically know that this requires 1 iteration, so we
  -- could detect it and not generate a loop in that case.
  -- However, it seems to have no impact on performance (an extra
  -- conditional jump), so for simplicity we just always generate
  -- the loop.
  let cspace_dims = map (streamBounded . snd) cspace
      num_iters
        | cspace_dims == [kernelGroupSize constants] = 1
        | otherwise = product cspace_dims `quotRoundingUp`
                      kernelGroupSize constants

  iter <- newVName "comb_iter"

  sFor iter Int32 num_iters $ do
    mapM_ ((`dPrim_` int32) . fst) cspace
    -- Compute the *flat* array index.
    cid <- dPrimV "flat_comb_id" $
      Imp.var iter int32 * kernelGroupSize constants +
      kernelLocalThreadId constants

    -- Turn it into a nested array index.
    zipWithM_ (<--) (map fst cspace) $ unflattenIndex cspace_dims (Imp.var cid int32)

    -- Construct the body.  This is mostly about the book-keeping
    -- for the scatter-like part.
    let (scatter_ws, scatter_ns, _scatter_vs) = unzip3 scatter
        scatter_ws_repl = concat $ zipWith replicate scatter_ns scatter_ws
        (scatter_pes, normal_pes) =
          splitAt (sum scatter_ns) $ patternElements pat
        (res_is, res_vs, res_normal) =
          splitAt3 (sum scatter_ns) (sum scatter_ns) $ bodyResult body

    -- Execute the body if we are within bounds.
    sWhen (isActive cspace .&&. isActive aspace) $ allThreads constants $
      compileStms (freeIn $ bodyResult body) (bodyStms body) $ do

      forM_ (zip4 scatter_ws_repl res_is res_vs scatter_pes) $
        \(w, res_i, res_v, scatter_pe) -> do
          let res_i' = toExp' int32 res_i
              w'     = toExp' int32 w
              -- We have to check that 'res_i' is in-bounds wrt. an array of size 'w'.
              in_bounds = 0 .<=. res_i' .&&. res_i' .<. w'
          sWhen in_bounds $ copyDWIM (patElemName scatter_pe) [res_i'] res_v []

      forM_ (zip normal_pes res_normal) $ \(pe, res) ->
        copyDWIM (patElemName pe) local_index res []

  sOp Imp.LocalBarrier

  where streamBounded (Var v)
          | Just x <- lookup v $ kernelStreamed constants =
              Imp.sizeToExp x
        streamBounded se = toExp' int32 se

        local_index = map (toExp' int32 . Var . fst) cspace

compileKernelExp constants (Pattern _ dests) (GroupReduce w lam input) = do
  w' <- toExp w
  groupReduce constants w' lam $ map snd input

  sOp Imp.LocalBarrier

  -- The final result will be stored in element 0 of the local memory array.
  forM_ (zip dests input) $ \(dest, (_, arr)) ->
    copyDWIM (patElemName dest) [] (Var arr) [0]

compileKernelExp constants _ (GroupScan w lam input) = do
  w' <- toExp w
  groupScan constants Nothing w' lam $ map snd input

compileKernelExp constants (Pattern _ final) (GroupStream w maxchunk lam accs _arrs) = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      block_offset' = Imp.var block_offset int32
  w' <- toExp w
  max_block_size <- toExp maxchunk

  dLParams (acc_params++arr_params)
  forM_ (zip acc_params accs) $ \(p, acc) ->
    copyDWIM (paramName p) [] acc []
  dPrim_ block_size int32

  -- If the GroupStream is morally just a do-loop, generate simpler code.
  case mapM isSimpleThreadInSpace $ stmsToList $ bodyStms body of
    Just stms' | ValueExp x <- max_block_size, oneIsh x -> do
      let body' = body { bodyStms = stmsFromList stms' }
          body'' = allThreads constants $
                   compileLoopBody acc_params body'
      block_size <-- 1

      -- Check if loop is candidate for unrolling.
      let loop =
            case w of
              Var w_var | Just w_bound <- lookup w_var $ kernelStreamed constants,
                          w_bound /= Imp.ConstSize 1 ->
                          -- Candidate for unrolling, so generate two loops.
                          sIf (w' .==. Imp.sizeToExp w_bound)
                          (sFor block_offset Int32 (Imp.sizeToExp w_bound) body'')
                          (sFor block_offset Int32 w' body'')
              _ -> sFor block_offset Int32 w' body''

      if kernelThreadActive constants == Imp.ValueExp (BoolValue True)
        then loop
        else sWhen (kernelThreadActive constants) loop

    _ -> do
      dPrim_ block_offset int32
      let body' = streaming constants block_size maxchunk $
                  compileBody' acc_params body

      block_offset <-- 0

      let not_at_end = block_offset' .<. w'
          set_block_size =
            sIf (w' - block_offset' .<. max_block_size)
            (block_size <-- (w' - block_offset'))
            (block_size <-- max_block_size)
          increase_offset =
            block_offset <-- block_offset' + max_block_size

      -- Three cases to consider for simpler generated code based
      -- on max block size: (0) if full input size, do not
      -- generate a loop; (1) if one, generate for-loop (2)
      -- otherwise, generate chunked while-loop.
      if max_block_size == w' then
        (block_size <-- w') >> body'
      else if max_block_size == Imp.ValueExp (value (1::Int32)) then do
             block_size <-- w'
             sFor block_offset Int32 w' body'
           else
             sWhile not_at_end $
             set_block_size >> body' >> increase_offset

  forM_ (zip final acc_params) $ \(pe, p) ->
    copyDWIM (patElemName pe) [] (Var $ paramName p) []

  where isSimpleThreadInSpace (Let _ _ Op{}) = Nothing
        isSimpleThreadInSpace bnd = Just bnd

compileKernelExp _ _ (GroupGenReduce w arrs op bucket values locks) = do
  -- Check if bucket is in-bounds
  bucket' <- mapM toExp bucket
  w' <- mapM toExp w
  num_locks <- toExp' int32 . arraySize 0 <$> lookupType locks
  let locking = Locking locks 0 1 0 $ (`rem` num_locks) . sum
      values_params = takeLast (length values) $ lambdaParams op

  sWhen (indexInBounds bucket' w') $ do
    forM_ (zip values_params values) $ \(p, v) ->
      copyDWIM (paramName p) [] v []
    atomicUpdate DefaultSpace arrs bucket' op locking
  where indexInBounds inds bounds =
          foldl1 (.&&.) $ zipWith checkBound inds bounds
          where checkBound ind bound = 0 .<=. ind .&&. ind .<. bound

compileKernelExp _ dest e =
  compilerBugS $ unlines ["Invalid target", "  " ++ show dest,
                          "for kernel expression", "  " ++ pretty e]

streaming :: KernelConstants -> VName -> SubExp -> InKernelGen () -> InKernelGen ()
streaming constants chunksize bound m = do
  bound' <- subExpToDimSize bound
  let constants' =
        constants { kernelStreamed = (chunksize, bound') : kernelStreamed constants }
  emit =<< subImpM_ (inKernelOperations constants') m

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
          , lockingMapping :: [Imp.Exp] -> Imp.Exp
            -- ^ A transformation from the logical lock index to the
            -- physical position in the array.  This can also be used
            -- to make the lock array smaller.
          }

-- | A function for generating code for an atomic update.  Assumes
-- that the bucket is in-bounds.
type AtomicUpdate lore =
  Space -> [VName] -> [Imp.Exp] -> ImpM lore Imp.KernelOp ()

atomicUpdate :: ExplicitMemorish lore =>
                Space -> [VName] -> [Imp.Exp] -> Lambda lore -> Locking
             -> ImpM lore Imp.KernelOp ()
atomicUpdate space arrs bucket lam locking =
  case atomicUpdateLocking lam of
    Left f -> f space arrs bucket
    Right f -> f locking space arrs bucket

-- | 'atomicUpdate', but where it is explicitly visible whether a
-- locking strategy is necessary.
atomicUpdateLocking :: ExplicitMemorish lore =>
                       Lambda lore
                    -> Either (AtomicUpdate lore) (Locking -> AtomicUpdate lore)

atomicUpdateLocking lam
  | Just ops_and_ts <- splitOp lam,
    all (\(_, t, _, _) -> primBitSize t == 32) ops_and_ts = Left $ \space arrs bucket ->
  -- If the operator is a vectorised binary operator on 32-bit values,
  -- we can use a particularly efficient implementation. If the
  -- operator has an atomic implementation we use that, otherwise it
  -- is still a binary operator which can be implemented by atomic
  -- compare-and-swap if 32 bits.
  forM_ (zip arrs ops_and_ts) $ \(a, (op, t, x, y)) -> do

  -- Common variables.
  old <- dPrim "old" t

  (arr', _a_space, bucket_offset) <- fullyIndexArray a bucket

  case opHasAtomicSupport space old arr' bucket_offset op of
    Just f -> sOp $ f $ Imp.var y t
    Nothing -> atomicUpdateCAS space t a old bucket x $
      x <-- Imp.BinOpExp op (Imp.var x t) (Imp.var y t)

  where opHasAtomicSupport space old arr' bucket' bop = do
          let atomic f = Imp.Atomic space . f old arr' bucket'
          atomic <$> Imp.atomicBinOp bop

-- If the operator functions purely on single 32-bit values, we can
-- use an implementation based on CAS, no matter what the operator
-- does.
atomicUpdateLocking op
  | [Prim t] <- lambdaReturnType op,
    [xp, _] <- lambdaParams op,
    primBitSize t == 32 = Left $ \space [arr] bucket -> do
      old <- dPrim "old" t
      atomicUpdateCAS space t arr old bucket (paramName xp) $
        compileBody' [xp] $ lambdaBody op

atomicUpdateLocking op = Right $ \locking space arrs bucket -> do
  old <- dPrim "old" int32
  continue <- dPrimV "continue" true

  -- Correctly index into locks.
  (locks', _locks_space, locks_offset) <-
    fullyIndexArray (lockingArray locking) [lockingMapping locking bucket]

  -- Critical section
  let try_acquire_lock =
        sOp $ Imp.Atomic space $
        Imp.AtomicCmpXchg old locks' locks_offset (lockingIsUnlocked locking) (lockingToLock locking)
      lock_acquired = Imp.var old int32 .==. lockingIsUnlocked locking
      -- Even the releasing is done with an atomic rather than a
      -- simple write, for memory coherency reasons.
      release_lock =
        sOp $ Imp.Atomic space $
        Imp.AtomicCmpXchg old locks' locks_offset (lockingToLock locking) (lockingToUnlock locking)
      break_loop = continue <-- false

  -- Preparing parameters. It is assumed that the caller has already
  -- filled the arr_params. We copy the current value to the
  -- accumulator parameters.
  --
  -- Note the use of 'everythingVolatile' when reading and writing the
  -- buckets.  This was necessary to ensure correct execution on a
  -- newer NVIDIA GPU (RTX 2080).  The 'volatile' modifiers likely
  -- make the writes pass through the (SM-local) L1 cache, which is
  -- necessary here, because we are really doing device-wide
  -- synchronisation without atomics (naughty!).
  let (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op
      bind_acc_params =
        everythingVolatile $
        sComment "bind lhs" $
        forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
        copyDWIM (paramName acc_p) [] (Var arr) bucket

  let op_body = sComment "execute operation" $
                compileBody' acc_params $ lambdaBody op

      do_gen_reduce =
        everythingVolatile $
        sComment "update global result" $
        zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

      fence = case space of Space "local" -> sOp Imp.MemFenceLocal
                            _             -> sOp Imp.MemFenceGlobal


  -- While-loop: Try to insert your value
  sWhile (Imp.var continue Bool) $ do
    try_acquire_lock
    sWhen lock_acquired $ do
      dLParams acc_params
      bind_acc_params
      op_body
      do_gen_reduce
      fence
      release_lock
      break_loop
    fence
  where writeArray bucket arr val = copyDWIM arr bucket val []

atomicUpdateCAS :: Space -> PrimType
                -> VName -> VName
                -> [Imp.Exp] -> VName
                -> ImpM lore Imp.KernelOp ()
                -> ImpM lore Imp.KernelOp ()
atomicUpdateCAS space t arr old bucket x do_op = do
  -- Code generation target:
  --
  -- old = d_his[idx];
  -- do {
  --   assumed = old;
  --   x = do_op(assumed, y);
  --   old = atomicCAS(&d_his[idx], assumed, tmp);
  -- } while(assumed != old);
  assumed <- dPrim "assumed" t
  run_loop <- dPrimV "run_loop" 1
  copyDWIM old [] (Var arr) bucket

  (arr', _a_space, bucket_offset) <- fullyIndexArray arr bucket

  -- While-loop: Try to insert your value
  let (toBits, fromBits) =
        case t of FloatType Float32 -> (\v -> Imp.FunExp "to_bits32" [v] int32,
                                        \v -> Imp.FunExp "from_bits32" [v] t)
                  _                 -> (id, id)
  sWhile (Imp.var run_loop int32) $ do
    assumed <-- Imp.var old t
    x <-- Imp.var assumed t
    do_op
    old_bits <- dPrim "old_bits" int32
    sOp $ Imp.Atomic space $
      Imp.AtomicCmpXchg old_bits arr' bucket_offset
      (toBits (Imp.var assumed t)) (toBits (Imp.var x t))
    old <-- fromBits (Imp.var old_bits int32)
    sWhen (toBits (Imp.var assumed t) .==. Imp.var old_bits int32)
      (run_loop <-- 0)

-- | Horizontally fission a lambda that models a binary operator.
splitOp :: Attributes lore => Lambda lore -> Maybe [(BinOp, PrimType, VName, VName)]
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

computeKernelUses :: FreeIn a =>
                     a -> [VName]
                  -> CallKernelGen [Imp.KernelUse]
computeKernelUses kernel_body bound_in_kernel = do
  let actually_free = freeIn kernel_body `S.difference` S.fromList bound_in_kernel
  -- Compute the variables that we need to pass to the kernel.
  nub <$> readsFromSet actually_free

readsFromSet :: Names -> CallKernelGen [Imp.KernelUse]
readsFromSet free =
  fmap catMaybes $
  forM (S.toList free) $ \var -> do
    t <- lookupType var
    vtable <- getVTable
    case t of
      Array {} -> return Nothing
      Mem (Space "local") -> return Nothing
      Mem {} -> return $ Just $ Imp.MemoryUse var
      Prim bt ->
        isConstExp vtable (Imp.var var bt) >>= \case
          Just ce -> return $ Just $ Imp.ConstUse var ce
          Nothing | bt == Cert -> return Nothing
                  | otherwise  -> return $ Just $ Imp.ScalarUse var bt

localMemSize :: VTable ExplicitMemory -> Imp.Count Imp.Bytes
             -> ImpM lore op (Either (Imp.Count Imp.Bytes) Imp.KernelConstExp)
localMemSize vtable e = isConstExp vtable (Imp.innerExp e) >>= \case
  Just e' | isStaticExp e' -> return $ Right e'
  _ -> return $ Left e

isConstExp :: VTable ExplicitMemory -> Imp.Exp
           -> ImpM lore op (Maybe Imp.KernelConstExp)
isConstExp vtable size = do
  fname <- asks envFunction
  let onLeaf (Imp.ScalarVar name) _ = lookupConstExp name
      onLeaf (Imp.SizeOf pt) _ = Just $ primByteSize pt
      onLeaf Imp.Index{} _ = Nothing
      lookupConstExp name =
        constExp =<< hasExp =<< M.lookup name vtable
      constExp (Op (Inner (GetSize key _))) =
        Just $ LeafExp (Imp.SizeConst $ keyWithEntryPoint fname key) int32
      constExp e = primExpFromExp lookupConstExp e
  return $ replaceInPrimExpM onLeaf size
  where hasExp (ArrayVar e _) = e
        hasExp (ScalarVar e _) = e
        hasExp (MemVar e _) = e

-- | Only some constant expressions qualify as *static* expressions,
-- which we can use for static memory allocation.  This is a bit of a
-- hack, as it is primarly motivated by what you can put as the size
-- when declaring an array in C.
isStaticExp :: Imp.KernelConstExp -> Bool
isStaticExp LeafExp{} = True
isStaticExp ValueExp{} = True
isStaticExp (ConvOpExp ZExt{} x) = isStaticExp x
isStaticExp (ConvOpExp SExt{} x) = isStaticExp x
isStaticExp (BinOpExp Add{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Sub{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Mul{} x y) = isStaticExp x && isStaticExp y
isStaticExp _ = False

computeThreadChunkSize :: SplitOrdering
                       -> Imp.Exp
                       -> Imp.Count Imp.Elements
                       -> Imp.Count Imp.Elements
                       -> VName
                       -> ImpM lore op ()
computeThreadChunkSize (SplitStrided stride) thread_index elements_per_thread num_elements chunk_var = do
  stride' <- toExp stride
  chunk_var <--
    Imp.BinOpExp (SMin Int32)
    (Imp.innerExp elements_per_thread)
    ((Imp.innerExp num_elements - thread_index) `quotRoundingUp` stride')

computeThreadChunkSize SplitContiguous thread_index elements_per_thread num_elements chunk_var = do
  starting_point <- dPrimV "starting_point" $
    thread_index * Imp.innerExp elements_per_thread
  remaining_elements <- dPrimV "remaining_elements" $
    Imp.innerExp num_elements - Imp.var starting_point int32

  let no_remaining_elements = Imp.var remaining_elements int32 .<=. 0
      beyond_bounds = Imp.innerExp num_elements .<=. Imp.var starting_point int32

  sIf (no_remaining_elements .||. beyond_bounds)
    (chunk_var <-- 0)
    (sIf is_last_thread
       (chunk_var <-- Imp.innerExp last_thread_elements)
       (chunk_var <-- Imp.innerExp elements_per_thread))
  where last_thread_elements =
          num_elements - Imp.elements thread_index * elements_per_thread
        is_last_thread =
          Imp.innerExp num_elements .<.
          (thread_index + 1) * Imp.innerExp elements_per_thread

kernelInitialisationSimple :: Imp.Exp -> Imp.Exp
                           -> Maybe (VName, VName, VName)
                           -> CallKernelGen (KernelConstants, ImpM InKernel Imp.KernelOp ())
kernelInitialisationSimple num_groups group_size names = do
  (global_tid, local_tid, group_id) <-
    case names of Nothing ->
                    (,,)
                    <$> newVName "global_tid"
                    <*> newVName "local_tid"
                    <*> newVName "group_id"
                  Just (global_tid, local_tid, group_id) ->
                    return (global_tid, local_tid, group_id)
  wave_size <- newVName "wave_size"
  inner_group_size <- newVName "group_size"
  vtable <- getVTable
  let constants =
        KernelConstants vtable
        (Imp.var global_tid int32)
        (Imp.var local_tid int32)
        (Imp.var group_id int32)
        global_tid local_tid group_id
        group_size num_groups (group_size*num_groups)
        (Imp.var wave_size int32) []
        true mempty

  let set_constants = do
        dPrim_ global_tid int32
        dPrim_ local_tid int32
        dPrim_ inner_group_size int32
        dPrim_ wave_size int32
        dPrim_ group_id int32

        sOp (Imp.GetGlobalId global_tid 0)
        sOp (Imp.GetLocalId local_tid 0)
        sOp (Imp.GetLocalSize inner_group_size 0)
        sOp (Imp.GetLockstepWidth wave_size)
        sOp (Imp.GetGroupId group_id 0)

  return (constants, set_constants)

kernelInitialisationSetSpace :: KernelSpace -> InKernelGen ()
                             -> CallKernelGen (KernelConstants, ImpM InKernel Imp.KernelOp ())
kernelInitialisationSetSpace space set_space = do
  group_size <- toExp $ spaceGroupSize space
  num_groups <- toExp $ spaceNumGroups space

  (constants, set_constants) <-
    kernelInitialisationSimple num_groups group_size $
    Just (spaceGlobalId space, spaceLocalId space, spaceGroupId space)

  let set_constants' = do
        set_constants
        case spaceStructure space of
          FlatThreadSpace is_and_dims ->
            mapM_ ((`dPrim_` int32) . fst) is_and_dims
          NestedThreadSpace is_and_dims -> do
            let (gtids, _, ltids, _) = unzip4 is_and_dims
            mapM_ (`dPrim_` int32) $ gtids ++ ltids
        set_space

  let (space_is, space_dims) = unzip $ spaceDimensions space
  space_dims' <- mapM toExp space_dims

  return (constants { kernelThreadActive =
                        if null $ spaceDimensions space
                        then true
                        else isActive $ spaceDimensions space
                    , kernelDimensions =
                        zip space_is space_dims'
                    },
          set_constants')

kernelInitialisation :: KernelSpace
                     -> CallKernelGen (KernelConstants, ImpM InKernel Imp.KernelOp ())
kernelInitialisation space =
  kernelInitialisationSetSpace space $
  setSpaceIndices (Imp.var (spaceGlobalId space) int32) space

setSpaceIndices :: Imp.Exp -> KernelSpace -> InKernelGen ()
setSpaceIndices gtid space =
  case spaceStructure space of
    FlatThreadSpace is_and_dims ->
      flatSpaceWith gtid is_and_dims
    NestedThreadSpace is_and_dims -> do
      let (gtids, gdims, ltids, ldims) = unzip4 is_and_dims
      gdims' <- mapM toExp gdims
      ldims' <- mapM toExp ldims
      let (gtid_es, ltid_es) = unzip $ unflattenNestedIndex gdims' ldims' gtid
      zipWithM_ (<--) gtids gtid_es
      zipWithM_ (<--) ltids ltid_es
  where flatSpaceWith base is_and_dims = do
          let (is, dims) = unzip is_and_dims
          dims' <- mapM toExp dims
          zipWithM_ (<--) is $ unflattenIndex dims' base

isActive :: [(VName, SubExp)] -> Imp.Exp
isActive limit = case actives of
                    [] -> Imp.ValueExp $ BoolValue True
                    x:xs -> foldl (.&&.) x xs
  where (is, ws) = unzip limit
        actives = zipWith active is $ map (toExp' Bool) ws
        active i = (Imp.var i int32 .<.)

unflattenNestedIndex :: IntegralExp num => [num] -> [num] -> num -> [(num,num)]
unflattenNestedIndex global_dims group_dims global_id =
  zip global_is local_is
  where num_groups_dims = zipWith quotRoundingUp global_dims group_dims
        group_size = product group_dims
        group_id = global_id `Futhark.Util.IntegralExp.quot` group_size
        local_id = global_id `Futhark.Util.IntegralExp.rem` group_size

        group_is = unflattenIndex num_groups_dims group_id
        local_is = unflattenIndex group_dims local_id
        global_is = zipWith (+) local_is $ zipWith (*) group_is group_dims


-- | Change every memory block to be in the global address space,
-- except those who are in the local memory space.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and dared as variables in the
-- kernel).
makeAllMemoryGlobal :: CallKernelGen a -> CallKernelGen a
makeAllMemoryGlobal =
  local (\env -> env { envDefaultSpace = Imp.Space "global" }) .
  localVTable (M.map globalMemory)
  where globalMemory (MemVar _ entry)
          | entryMemSpace entry /= Space "local" =
              MemVar Nothing entry { entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

allThreads :: KernelConstants -> InKernelGen () -> InKernelGen ()
allThreads constants = emit <=< subImpM_ (inKernelOperations constants')
  where constants' =
          constants { kernelThreadActive = Imp.ValueExp (BoolValue True) }



writeParamToLocalMemory :: Typed (MemBound u) =>
                           Imp.Exp -> (VName, t) -> Param (MemBound u)
                        -> ImpM lore op ()
writeParamToLocalMemory i (mem, _) param
  | Prim t <- paramType param =
      emit $
      Imp.Write mem (bytes i') bt (Space "local") Imp.Volatile $
      Imp.var (paramName param) t
  | otherwise =
      return ()
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

readParamFromLocalMemory :: Typed (MemBound u) =>
                            Imp.Exp -> Param (MemBound u) -> (VName, t)
                         -> ImpM lore op ()
readParamFromLocalMemory i param (l_mem, _)
  | Prim _ <- paramType param =
      paramName param <--
      Imp.index l_mem (bytes i') bt (Space "local") Imp.Volatile
  | otherwise = return ()
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

groupReduce :: ExplicitMemorish lore =>
               KernelConstants
            -> Imp.Exp
            -> Lambda lore
            -> [VName]
            -> ImpM lore Imp.KernelOp ()
groupReduce constants w lam arrs = do
  offset <- dPrim "offset" int32
  groupReduceWithOffset constants offset w lam arrs

groupReduceWithOffset :: ExplicitMemorish lore =>
                         KernelConstants
                      -> VName
                      -> Imp.Exp
                      -> Lambda lore
                      -> [VName]
                      -> ImpM lore Imp.KernelOp ()
groupReduceWithOffset constants offset w lam arrs = do
  let (reduce_acc_params, reduce_arr_params) = splitAt (length arrs) $ lambdaParams lam

  skip_waves <- dPrim "skip_waves" int32
  dLParams $ lambdaParams lam

  offset <-- 0

  comment "participating threads read initial accumulator" $
    sWhen (local_tid .<. w) $
    zipWithM_ readReduceArgument reduce_acc_params arrs

  let do_reduce = do comment "read array element" $
                       zipWithM_ readReduceArgument reduce_arr_params arrs
                     comment "apply reduction operation" $
                       compileBody' reduce_acc_params $ lambdaBody lam
                     comment "write result of operation" $
                       zipWithM_ writeReduceOpResult reduce_acc_params arrs
      in_wave_reduce = everythingVolatile do_reduce

      wave_size = kernelWaveSize constants
      group_size = kernelGroupSize constants
      wave_id = local_tid `quot` wave_size
      in_wave_id = local_tid - wave_id * wave_size
      num_waves = (group_size + wave_size - 1) `quot` wave_size
      arg_in_bounds = local_tid + Imp.var offset int32 .<. w

      doing_in_wave_reductions =
        Imp.var offset int32 .<. wave_size
      apply_in_in_wave_iteration =
        (in_wave_id .&. (2 * Imp.var offset int32 - 1)) .==. 0
      in_wave_reductions = do
        offset <-- 1
        sWhile doing_in_wave_reductions $ do
          sWhen (arg_in_bounds .&&. apply_in_in_wave_iteration)
            in_wave_reduce
          offset <-- Imp.var offset int32 * 2

      doing_cross_wave_reductions =
        Imp.var skip_waves int32 .<. num_waves
      is_first_thread_in_wave =
        in_wave_id .==. 0
      wave_not_skipped =
        (wave_id .&. (2 * Imp.var skip_waves int32 - 1)) .==. 0
      apply_in_cross_wave_iteration =
        arg_in_bounds .&&. is_first_thread_in_wave .&&. wave_not_skipped
      cross_wave_reductions = do
        skip_waves <-- 1
        sWhile doing_cross_wave_reductions $ do
          barrier
          offset <-- Imp.var skip_waves int32 * wave_size
          sWhen apply_in_cross_wave_iteration
            do_reduce
          skip_waves <-- Imp.var skip_waves int32 * 2

  in_wave_reductions
  cross_wave_reductions
  where local_tid = kernelLocalThreadId constants
        global_tid = kernelGlobalThreadId constants

        barrier
          | all primType $ lambdaReturnType lam = sOp Imp.LocalBarrier
          | otherwise                           = sOp Imp.GlobalBarrier

        readReduceArgument param arr
          | Prim _ <- paramType param = do
              let i = local_tid + Imp.vi32 offset
              copyDWIM (paramName param) [] (Var arr) [i]
          | otherwise = do
              let i = global_tid + Imp.vi32 offset
              copyDWIM (paramName param) [] (Var arr) [i]

        writeReduceOpResult param arr
          | Prim _ <- paramType param =
              copyDWIM arr [local_tid] (Var $ paramName param) []
          | otherwise =
              return ()

groupScan :: KernelConstants
          -> Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)
          -> Imp.Exp
          -> Lambda InKernel
          -> [VName]
          -> ImpM InKernel Imp.KernelOp ()
groupScan constants seg_flag w lam arrs = do
  when (any (not . primType . paramType) $ lambdaParams lam) $
    compilerLimitationS "Cannot compile parallel scans with array element type."

  renamed_lam <- renameLambda lam

  acc_local_mem <- flip zip (repeat ()) <$>
                   mapM (fmap (memLocationName . entryArrayLocation) .
                         lookupArray) arrs

  let ltid = kernelLocalThreadId constants
      (x_params, y_params) = splitAt (length arrs) $ lambdaParams lam

  dLParams (lambdaParams lam++lambdaParams renamed_lam)

  -- The scan works by splitting the group into blocks, which are
  -- scanned separately.  Typically, these blocks are smaller than
  -- the lockstep width, which enables barrier-free execution inside
  -- them.
  --
  -- We hardcode the block size here.  The only requirement is that
  -- it should not be less than the square root of the group size.
  -- With 32, we will work on groups of size 1024 or smaller, which
  -- fits every device Troels has seen.  Still, it would be nicer if
  -- it were a runtime parameter.  Some day.
  let block_size = Imp.ValueExp $ IntValue $ Int32Value 32
      simd_width = kernelWaveSize constants
      block_id = ltid `quot` block_size
      in_block_id = ltid - block_id * block_size
      doInBlockScan seg_flag' active = inBlockScan seg_flag' simd_width block_size active ltid acc_local_mem
      ltid_in_bounds = ltid .<. w

  doInBlockScan seg_flag ltid_in_bounds lam
  sOp Imp.LocalBarrier

  let last_in_block = in_block_id .==. block_size - 1
  sComment "last thread of block 'i' writes its result to offset 'i'" $
    sWhen (last_in_block .&&. ltid_in_bounds) $
    zipWithM_ (writeParamToLocalMemory block_id) acc_local_mem y_params

  sOp Imp.LocalBarrier

  let is_first_block = block_id .==. 0
      first_block_seg_flag = do
        flag_true <- seg_flag
        Just $ \from to ->
          flag_true (from*block_size+block_size-1) (to*block_size+block_size-1)
  comment
    "scan the first block, after which offset 'i' contains carry-in for warp 'i+1'" $
    doInBlockScan first_block_seg_flag (is_first_block .&&. ltid_in_bounds) renamed_lam

  sOp Imp.LocalBarrier

  let read_carry_in =
        zipWithM_ (readParamFromLocalMemory (block_id - 1))
        x_params acc_local_mem

  let op_to_y
        | Nothing <- seg_flag =
            compileBody' y_params $ lambdaBody lam
        | Just flag_true <- seg_flag =
            sUnless (flag_true (block_id*block_size-1) ltid) $
              compileBody' y_params $ lambdaBody lam
      write_final_result =
        zipWithM_ (writeParamToLocalMemory ltid) acc_local_mem y_params

  sComment "carry-in for every block except the first" $
    sUnless (is_first_block .||. Imp.UnOpExp Not ltid_in_bounds) $ do
    sComment "read operands" read_carry_in
    sComment "perform operation" op_to_y
    sComment "write final result" write_final_result

  sOp Imp.LocalBarrier

  sComment "restore correct values for first block" $
    sWhen is_first_block write_final_result

  sOp Imp.LocalBarrier

inBlockScan :: Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)
            -> Imp.Exp
            -> Imp.Exp
            -> Imp.Exp
            -> Imp.Exp
            -> [(VName, t)]
            -> Lambda InKernel
            -> InKernelGen ()
inBlockScan seg_flag lockstep_width block_size active ltid acc_local_mem scan_lam = everythingVolatile $ do
  skip_threads <- dPrim "skip_threads" int32
  let in_block_thread_active =
        Imp.var skip_threads int32 .<=. in_block_id
      actual_params = lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
      read_operands =
        zipWithM_ (readParamFromLocalMemory $ ltid - Imp.var skip_threads int32)
        x_params acc_local_mem

  -- Set initial y values
  sWhen active $
    zipWithM_ (readParamFromLocalMemory ltid) y_params acc_local_mem

  let op_to_y
        | Nothing <- seg_flag =
            compileBody' y_params $ lambdaBody scan_lam
        | Just flag_true <- seg_flag =
            sUnless (flag_true (ltid-Imp.var skip_threads int32) ltid) $
              compileBody' y_params $ lambdaBody scan_lam
      write_operation_result =
        zipWithM_ (writeParamToLocalMemory ltid) acc_local_mem y_params
      maybeLocalBarrier = sWhen (lockstep_width .<=. Imp.var skip_threads int32) $
                          sOp Imp.LocalBarrier

  sComment "in-block scan (hopefully no barriers needed)" $ do
    skip_threads <-- 1
    sWhile (Imp.var skip_threads int32 .<. block_size) $ do
      sWhen (in_block_thread_active .&&. active) $ do
        sComment "read operands" read_operands
        sComment "perform operation" op_to_y

      maybeLocalBarrier

      sWhen (in_block_thread_active .&&. active) $
        sComment "write result" write_operation_result

      maybeLocalBarrier

      skip_threads <-- Imp.var skip_threads int32 * 2

  where block_id = ltid `quot` block_size
        in_block_id = ltid - block_id * block_size

compileKernelStms :: KernelConstants -> Stms InKernel
                  -> InKernelGen a
                  -> InKernelGen a
compileKernelStms constants ungrouped_bnds m =
  compileGroupedKernelStms' $ groupStmsByGuard constants $ stmsToList ungrouped_bnds
  where compileGroupedKernelStms' [] = m
        compileGroupedKernelStms' ((g, bnds):rest_bnds) = do
          dScopes (map ((Just . stmExp) &&& (castScope . scopeOf)) bnds)
          protect g $ mapM_ compileKernelStm bnds
          compileGroupedKernelStms' rest_bnds

        protect Nothing body_m =
          body_m
        protect (Just (Imp.ValueExp (BoolValue True))) body_m =
          body_m
        protect (Just g) body_m =
          sWhen g $ allThreads constants body_m

        compileKernelStm (Let pat _ e) = compileExp pat e

groupStmsByGuard :: KernelConstants
                     -> [Stm InKernel]
                     -> [(Maybe Imp.Exp, [Stm InKernel])]
groupStmsByGuard constants bnds =
  map collapse $ groupBy sameGuard $ zip (map bindingGuard bnds) bnds
  where bindingGuard (Let _ _ Op{}) = Nothing
        bindingGuard _ = Just $ kernelThreadActive constants

        sameGuard (g1, _) (g2, _) = g1 == g2

        collapse [] =
          (Nothing, [])
        collapse l@((g,_):_) =
          (g, map snd l)

computeMapKernelGroups :: Imp.Exp -> CallKernelGen (Imp.Exp, Imp.Exp)
computeMapKernelGroups kernel_size = do
  group_size <- dPrim "group_size" int32
  fname <- asks envFunction
  let group_size_var = Imp.var group_size int32
      group_size_key = keyWithEntryPoint fname $ nameFromString $ pretty group_size
  sOp $ Imp.GetSize group_size group_size_key Imp.SizeGroup
  num_groups <- dPrimV "num_groups" $ kernel_size `quotRoundingUp` Imp.ConvOpExp (SExt Int32 Int32) group_size_var
  return (Imp.var group_size int32, Imp.var num_groups int32)

simpleKernelConstants :: Imp.Exp -> String
                      -> CallKernelGen (KernelConstants, ImpM InKernel Imp.KernelOp ())
simpleKernelConstants kernel_size desc = do
  thread_gtid <- newVName $ desc ++ "_gtid"
  thread_ltid <- newVName $ desc ++ "_ltid"
  group_id <- newVName $ desc ++ "_gid"
  (group_size, num_groups) <- computeMapKernelGroups kernel_size
  let set_constants = do
        dPrim_ thread_gtid int32
        dPrim_ thread_ltid int32
        dPrim_ group_id int32
        sOp (Imp.GetGlobalId thread_gtid 0)
        sOp (Imp.GetLocalId thread_ltid 0)
        sOp (Imp.GetGroupId group_id 0)


  vtable <- getVTable
  return (KernelConstants vtable
          (Imp.var thread_gtid int32) (Imp.var thread_ltid int32) (Imp.var group_id int32)
          thread_gtid thread_ltid group_id
          group_size num_groups (group_size*num_groups) 0
          [] (Imp.var thread_gtid int32 .<. kernel_size) mempty,

          set_constants)

-- | For many kernels, we may not have enough physical groups to cover
-- the logical iteration space.  Some groups thus have to perform
-- double duty; we put an outer loop to accomplish this.  The
-- advantage over just launching a bazillion threads is that the cost
-- of memory expansion should be proportional to the number of
-- *physical* threads (hardware parallelism), not the amount of
-- application parallelism.
virtualiseGroups :: KernelConstants
                 -> Imp.Exp
                 -> (VName -> InKernelGen ())
                 -> InKernelGen ()
virtualiseGroups constants required_groups m
  | kernelNumGroups constants == required_groups =
      m $ kernelGroupIdVar constants
  | otherwise = do
  phys_group_id <- dPrim "phys_group_id" int32
  sOp $ Imp.GetGroupId phys_group_id 0
  let iterations = (required_groups - Imp.vi32 phys_group_id) `quotRoundingUp`
                   kernelNumGroups constants
  i <- newVName "i"
  sFor i Int32 iterations $
    m =<< dPrimV "virt_group_id" (Imp.vi32 phys_group_id + Imp.vi32 i * kernelNumGroups constants)

sKernel :: KernelConstants -> String -> ImpM InKernel Imp.KernelOp a -> CallKernelGen ()
sKernel constants name m = do
  body <- makeAllMemoryGlobal $ subImpM_ (inKernelOperations constants) m
  uses <- computeKernelUses body mempty

  emit $ Imp.Op $ Imp.CallKernel Imp.Kernel
    { Imp.kernelBody = body
    , Imp.kernelUses = uses
    , Imp.kernelNumGroups = [kernelNumGroups constants]
    , Imp.kernelGroupSize = [kernelGroupSize constants]
    , Imp.kernelName =
        nameFromString $ name ++ "_" ++ show tag
    }
  where tag = baseTag $ kernelGlobalThreadIdVar constants

-- | Perform a Replicate with a kernel.
sReplicate :: VName -> Shape -> SubExp
           -> CallKernelGen ()
sReplicate arr (Shape ds) se = do
  t <- subExpType se

  dims <- mapM toExp $ ds ++ arrayDims t
  (constants, set_constants) <-
    simpleKernelConstants (product dims) "replicate"

  let is' = unflattenIndex dims $ kernelGlobalThreadId constants

  sKernel constants "replicate" $ do
    set_constants
    sWhen (kernelThreadActive constants) $
      copyDWIM arr is' se $ drop (length ds) is'

-- | Perform an Iota with a kernel.
sIota :: VName -> Imp.Exp -> Imp.Exp -> Imp.Exp -> IntType
      -> CallKernelGen ()
sIota arr n x s et = do
  destloc <- entryArrayLocation <$> lookupArray arr
  (constants, set_constants) <- simpleKernelConstants n "iota"

  sKernel constants "iota" $ do
    set_constants
    let gtid = kernelGlobalThreadId constants
    sWhen (kernelThreadActive constants) $ do
      (destmem, destspace, destidx) <-
        fullyIndexArray' destloc [gtid] (IntType et)

      emit $
        Imp.Write destmem destidx (IntType et) destspace Imp.Nonvolatile $
        Imp.ConvOpExp (SExt Int32 et) gtid * s + x

sCopy :: PrimType
      -> MemLocation
      -> MemLocation
      -> Imp.Count Imp.Elements
      -> CallKernelGen ()
sCopy bt
  destloc@(MemLocation destmem _ _)
  srcloc@(MemLocation srcmem srcshape _)
  n = do
  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map Imp.sizeToExp srcshape
      shape_se = map (Imp.innerExp . dimSizeToExp) srcshape
      kernel_size = Imp.innerExp n * product (drop 1 shape)

  (constants, set_constants) <- simpleKernelConstants kernel_size "copy"

  sKernel constants "copy" $ do
    set_constants

    let gtid = kernelGlobalThreadId constants
        dest_is = unflattenIndex shape_se gtid
        src_is = dest_is

    (_, destspace, destidx) <- fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- fullyIndexArray' srcloc src_is bt

    sWhen (gtid .<. kernel_size) $ emit $
      Imp.Write destmem destidx bt destspace Imp.Nonvolatile $
      Imp.index srcmem srcidx bt srcspace Imp.Nonvolatile


compileKernelResult :: KernelConstants -> PatElem InKernel -> KernelResult
                    -> InKernelGen ()

compileKernelResult constants pe (GroupsReturn what) = do
  i <- newVName "i"

  in_local_memory <- arrayInLocalMemory what
  let me = kernelLocalThreadId constants

  if not in_local_memory then do
    who' <- toExp $ intConst Int32 0
    sWhen (me .==. who') $
      copyDWIM (patElemName pe) [kernelGroupId constants] what []
    else do
      -- If the result of the group is an array in local memory, we
      -- store it by collective copying among all the threads of the
      -- group.  TODO: also do this if the array is in global memory
      -- (but this is a bit more tricky, synchronisation-wise).
      --
      -- We do the reads/writes multidimensionally, but the loop is
      -- single-dimensional.
      ws <- mapM toExp . arrayDims =<< subExpType what
      -- Compute how many elements this thread is responsible for.
      -- Formula: (w - ltid) / group_size (rounded up).
      let w = product ws
          ltid = kernelLocalThreadId constants
          group_size = kernelGroupSize constants
          to_write = (w - ltid) `quotRoundingUp` group_size
          is = unflattenIndex ws $ Imp.vi32 i * group_size + ltid

      sFor i Int32 to_write $
        copyDWIM (patElemName pe) (kernelGroupId constants : is) what is

compileKernelResult constants pe (ThreadsReturn what) = do
  let is = map (Imp.vi32 . fst) $ kernelDimensions constants
  sWhen (kernelThreadActive constants) $ copyDWIM (patElemName pe) is what []

compileKernelResult constants pe (ConcatReturns SplitContiguous _ per_thread_elems moffset what) = do
  dest_loc <- entryArrayLocation <$> lookupArray (patElemName pe)
  let dest_loc_offset = offsetArray dest_loc offset
      dest' = arrayDestination dest_loc_offset
  copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> toExp' int32 per_thread_elems *
                              kernelGlobalThreadId constants
                   Just se -> toExp' int32 se

compileKernelResult constants pe (ConcatReturns (SplitStrided stride) _ _ moffset what) = do
  dest_loc <- entryArrayLocation <$> lookupArray (patElemName pe)
  let dest_loc' = strideArray
                  (offsetArray dest_loc offset) $
                  toExp' int32 stride
      dest' = arrayDestination dest_loc'
  copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> kernelGlobalThreadId constants
                   Just se -> toExp' int32 se

compileKernelResult constants pe (WriteReturn rws _arr dests) = do
  rws' <- mapM toExp rws
  forM_ dests $ \(is, e) -> do
    is' <- mapM toExp is
    let condInBounds i rw = 0 .<=. i .&&. i .<. rw
        write = foldl (.&&.) (kernelThreadActive constants) $
                zipWith condInBounds is' rws'
    sWhen write $ copyDWIM (patElemName pe) (map (toExp' int32) is) e []

arrayInLocalMemory :: SubExp -> InKernelGen Bool
arrayInLocalMemory (Var name) = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry ->
      (Space "local"==) . entryMemSpace <$>
      lookupMemory (memLocationName (entryArrayLocation entry))
    _ -> return False
arrayInLocalMemory Constant{} = return False
