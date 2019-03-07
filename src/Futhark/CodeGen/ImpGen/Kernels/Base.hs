{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.CodeGen.ImpGen.Kernels.Base
  ( KernelConstants (..)
  , inKernelOperations
  , computeKernelUses
  , keyWithEntryPoint
  , CallKernelGen
  , InKernelGen
  , computeThreadChunkSize
  , kernelInitialisation
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
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen ((<--),
                               sFor, sWhile, sComment, sIf, sWhen, sUnless,
                               sOp,
                               dPrim, dPrim_, dPrimV)
import Futhark.Tools (partitionChunkedKernelLambdaParameters)
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem, IntegralExp)
import Futhark.Util (splitAt3, maybeNth, takeLast)

type CallKernelGen = ImpGen.ImpM ExplicitMemory Imp.HostOp
type InKernelGen = ImpGen.ImpM InKernel Imp.KernelOp

data KernelConstants = KernelConstants
                       { kernelGlobalThreadId :: Imp.Exp
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

inKernelOperations :: KernelConstants -> ImpGen.Operations InKernel Imp.KernelOp
inKernelOperations constants = (ImpGen.defaultOperations $ compileInKernelOp constants)
                               { ImpGen.opsCopyCompiler = inKernelCopy
                               , ImpGen.opsExpCompiler = inKernelExpCompiler
                               , ImpGen.opsStmsCompiler = \_ -> compileKernelStms constants
                               }

keyWithEntryPoint :: Name -> Name -> Name
keyWithEntryPoint fname key =
  nameFromString $ nameToString fname ++ "." ++ nameToString key

-- | We have no bulk copy operation (e.g. memmove) inside kernels, so
-- turn any copy into a loop.
inKernelCopy :: ImpGen.CopyCompiler InKernel Imp.KernelOp
inKernelCopy = ImpGen.copyElementWise

compileInKernelOp :: KernelConstants -> Pattern InKernel -> Op InKernel
                  -> InKernelGen ()
compileInKernelOp _ (Pattern _ [mem]) Alloc{} =
  compilerLimitationS $ "Cannot allocate memory block " ++ pretty mem ++ " in kernel."
compileInKernelOp _ dest Alloc{} =
  compilerBugS $ "Invalid target for in-kernel allocation: " ++ show dest
compileInKernelOp constants pat (Inner op) =
  compileKernelExp constants pat op

inKernelExpCompiler :: ImpGen.ExpCompiler InKernel Imp.KernelOp
inKernelExpCompiler _ (BasicOp (Assert _ _ (loc, locs))) =
  compilerLimitationS $
  unlines [ "Cannot compile assertion at " ++
            intercalate " -> " (reverse $ map locStr $ loc:locs) ++
            " inside parallel kernel."
          , "As a workaround, surround the expression with 'unsafe'."]
-- The static arrays stuff does not work inside kernels.
inKernelExpCompiler (Pattern _ [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0..] es) $ \(i,e) ->
  ImpGen.copyDWIM (patElemName dest) [fromIntegral (i::Int32)] e []
inKernelExpCompiler dest e =
  ImpGen.defCompileExp dest e

compileKernelExp :: KernelConstants -> Pattern InKernel -> KernelExp InKernel
                 -> InKernelGen ()

compileKernelExp _ pat (Barrier ses) = do
  forM_ (zip (patternNames pat) ses) $ \(d, se) ->
    ImpGen.copyDWIM d [] se []
  sOp Imp.LocalBarrier

compileKernelExp _ (Pattern [] [size]) (SplitSpace o w i elems_per_thread) = do
  num_elements <- Imp.elements <$> ImpGen.compileSubExp w
  i' <- ImpGen.compileSubExp i
  elems_per_thread' <- Imp.elements <$> ImpGen.compileSubExp elems_per_thread
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
      ImpGen.compileStms (freeIn $ bodyResult body) (stmsToList $ bodyStms body) $ do

      forM_ (zip4 scatter_ws_repl res_is res_vs scatter_pes) $
        \(w, res_i, res_v, scatter_pe) -> do
          let res_i' = ImpGen.compileSubExpOfType int32 res_i
              w'     = ImpGen.compileSubExpOfType int32 w
              -- We have to check that 'res_i' is in-bounds wrt. an array of size 'w'.
              in_bounds = 0 .<=. res_i' .&&. res_i' .<. w'
          sWhen in_bounds $ ImpGen.copyDWIM (patElemName scatter_pe) [res_i'] res_v []

      forM_ (zip normal_pes res_normal) $ \(pe, res) ->
        ImpGen.copyDWIM (patElemName pe) local_index res []

  sOp Imp.LocalBarrier

  where streamBounded (Var v)
          | Just x <- lookup v $ kernelStreamed constants =
              Imp.sizeToExp x
        streamBounded se = ImpGen.compileSubExpOfType int32 se

        local_index = map (ImpGen.compileSubExpOfType int32 . Var . fst) cspace

compileKernelExp constants (Pattern _ dests) (GroupReduce w lam input) = do
  let [my_index_param, offset_param] = take 2 $ lambdaParams lam
      lam' = lam { lambdaParams = drop 2 $ lambdaParams lam }

  dPrim_ (paramName my_index_param) int32
  dPrim_ (paramName offset_param) int32
  paramName my_index_param <-- kernelGlobalThreadId constants
  w' <- ImpGen.compileSubExp w
  groupReduceWithOffset constants (paramName offset_param) w' lam' $ map snd input

  sOp Imp.LocalBarrier

  -- The final result will be stored in element 0 of the local memory array.
  forM_ (zip dests input) $ \(dest, (_, arr)) ->
    ImpGen.copyDWIM (patElemName dest) [] (Var arr) [0]

compileKernelExp constants _ (GroupScan w lam input) = do
  w' <- ImpGen.compileSubExp w
  groupScan constants Nothing w' lam $ map snd input

compileKernelExp constants (Pattern _ final) (GroupStream w maxchunk lam accs _arrs) = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      block_offset' = Imp.var block_offset int32
  w' <- ImpGen.compileSubExp w
  max_block_size <- ImpGen.compileSubExp maxchunk

  ImpGen.dLParams (acc_params++arr_params)
  zipWithM_ ImpGen.compileSubExpTo (map paramName acc_params) accs
  dPrim_ block_size int32

  -- If the GroupStream is morally just a do-loop, generate simpler code.
  case mapM isSimpleThreadInSpace $ stmsToList $ bodyStms body of
    Just stms' | ValueExp x <- max_block_size, oneIsh x -> do
      let body' = body { bodyStms = stmsFromList stms' }
          body'' = allThreads constants $
                   ImpGen.compileLoopBody (map paramName acc_params) body'
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
                  ImpGen.compileBody' acc_params body

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
    ImpGen.copyDWIM (patElemName pe) [] (Var $ paramName p) []

  where isSimpleThreadInSpace (Let _ _ Op{}) = Nothing
        isSimpleThreadInSpace bnd = Just bnd

compileKernelExp _ _ (GroupGenReduce w arrs op bucket values locks) = do
  -- Check if bucket is in-bounds
  bucket' <- mapM ImpGen.compileSubExp bucket
  w' <- mapM ImpGen.compileSubExp w
  num_locks <- ImpGen.compileSubExpOfType int32 . arraySize 0 <$> lookupType locks
  let locking = Locking locks 0 1 0 $ (`rem` num_locks) . sum
      values_params = takeLast (length values) $ lambdaParams op

  sWhen (indexInBounds bucket' w') $ do
    forM_ (zip values_params values) $ \(p, v) ->
      ImpGen.copyDWIM (paramName p) [] v []
    atomicUpdate arrs bucket' op locking
  where indexInBounds inds bounds =
          foldl1 (.&&.) $ zipWith checkBound inds bounds
          where checkBound ind bound = 0 .<=. ind .&&. ind .<. bound

compileKernelExp _ dest e =
  compilerBugS $ unlines ["Invalid target", "  " ++ show dest,
                          "for kernel expression", "  " ++ pretty e]

streaming :: KernelConstants -> VName -> SubExp -> InKernelGen () -> InKernelGen ()
streaming constants chunksize bound m = do
  bound' <- ImpGen.subExpToDimSize bound
  let constants' =
        constants { kernelStreamed = (chunksize, bound') : kernelStreamed constants }
  ImpGen.emit =<< ImpGen.subImpM_ (inKernelOperations constants') m

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
  [VName] -> [Imp.Exp] -> ImpGen.ImpM lore Imp.KernelOp ()

atomicUpdate :: ExplicitMemorish lore =>
                [VName] -> [Imp.Exp] -> Lambda lore -> Locking
             -> ImpGen.ImpM lore Imp.KernelOp ()
atomicUpdate arrs bucket lam locking =
  case atomicUpdateLocking lam of
    Left f -> f arrs bucket
    Right f -> f locking arrs bucket

-- | 'atomicUpdate', but where it is explicitly visible whether a
-- locking strategy is necessary.
atomicUpdateLocking :: ExplicitMemorish lore =>
                       Lambda lore
                    -> Either (AtomicUpdate lore) (Locking -> AtomicUpdate lore)

atomicUpdateLocking lam
  | Just ops_and_ts <- splitOp lam,
    all (\(_, t, _) -> primBitSize t == 32) ops_and_ts = Left $ \arrs bucket ->
  -- If the operator is a vectorised binary operator on 32-bit values,
  -- we can use a particularly efficient implementation. If the
  -- operator has an atomic implementation we use that, otherwise it
  -- is still a binary operator which can be implemented by atomic
  -- compare-and-swap if 32 bits.
  forM_ (zip arrs ops_and_ts) $ \(a, (op, t, val)) -> do

  -- Common variables.
  old <- dPrim "old" t

  (arr', _a_space, bucket_offset) <- ImpGen.fullyIndexArray a bucket

  case opHasAtomicSupport old arr' bucket_offset op of
    Just f -> sOp $ f val

    Nothing -> do
      -- Code generation target:
      --
      -- old = d_his[idx];
      -- do {
      --   assumed = old;
      --   tmp = OP::apply(val, assumed);
      --   old = atomicCAS(&d_his[idx], assumed, tmp);
      -- } while(assumed != old);
      assumed <- dPrim "assumed" t
      run_loop <- dPrimV "run_loop" 1
      ImpGen.copyDWIM old [] (Var a) bucket

      -- Critical section
      x <- dPrim "x" t
      y <- dPrim "y" t

      -- While-loop: Try to insert your value
      let (toBits, fromBits) =
            case t of FloatType Float32 -> (\v -> Imp.FunExp "to_bits32" [v] int32,
                                            \v -> Imp.FunExp "from_bits32" [v] t)
                      _                 -> (id, id)
      sWhile (Imp.var run_loop int32) $ do
        assumed <-- Imp.var old t
        x <-- val
        y <-- Imp.var assumed t
        x <-- Imp.BinOpExp op (Imp.var x t) (Imp.var y t)
        old_bits <- dPrim "old_bits" int32
        sOp $ Imp.Atomic $
          Imp.AtomicCmpXchg old_bits arr' bucket_offset
          (toBits (Imp.var assumed t)) (toBits (Imp.var x t))
        old <-- fromBits (Imp.var old_bits int32)
        sWhen (toBits (Imp.var assumed t) .==. Imp.var old_bits int32)
          (run_loop <-- 0)

  where opHasAtomicSupport old arr' bucket' bop = do
          let atomic f = Imp.Atomic . f old arr' bucket'
          atomic <$> Imp.atomicBinOp bop

atomicUpdateLocking op = Right $ \locking arrs bucket -> do
  old <- dPrim "old" int32
  continue <- dPrimV "continue" true

  -- Correctly index into locks.
  (locks', _locks_space, locks_offset) <-
    ImpGen.fullyIndexArray (lockingArray locking) [lockingMapping locking bucket]

  -- Critical section
  let try_acquire_lock =
        sOp $ Imp.Atomic $
        Imp.AtomicCmpXchg old locks' locks_offset (lockingIsUnlocked locking) (lockingToLock locking)
      lock_acquired = Imp.var old int32 .==. lockingIsUnlocked locking
      -- Even the releasing is done with an atomic rather than a
      -- simple write, for memory coherency reasons.
      release_lock =
        sOp $ Imp.Atomic $
        Imp.AtomicCmpXchg old locks' locks_offset (lockingToLock locking) (lockingToUnlock locking)
      break_loop = continue <-- false

  -- Preparing parameters. It is assumed that the caller has already
  -- filled the arr_params. We copy the current value to the
  -- accumulator parameters.
  let (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op
      bind_acc_params =
        ImpGen.sComment "bind lhs" $
        forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
        ImpGen.copyDWIM (paramName acc_p) [] (Var arr) bucket

  let op_body = ImpGen.sComment "execute operation" $
                ImpGen.compileBody' acc_params $ lambdaBody op

      do_gen_reduce = ImpGen.sComment "update global result" $
                      zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

  -- While-loop: Try to insert your value
  sWhile (Imp.var continue Bool) $ do
    try_acquire_lock
    sWhen lock_acquired $ do
      ImpGen.dLParams acc_params
      bind_acc_params
      op_body
      do_gen_reduce
      release_lock
      break_loop
    sOp Imp.MemFence
  where writeArray bucket arr val = ImpGen.copyDWIM arr bucket val []

-- | Horizontally fission a lambda that models a binary operator.
splitOp :: Attributes lore => Lambda lore -> Maybe [(BinOp, PrimType, Imp.Exp)]
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
          return (op, t, Imp.var (paramName yp) t)
        splitStm _ = Nothing

computeKernelUses :: FreeIn a =>
                     a -> [VName]
                  -> CallKernelGen ([Imp.KernelUse], [Imp.LocalMemoryUse])
computeKernelUses kernel_body bound_in_kernel = do
  let actually_free = freeIn kernel_body `S.difference` S.fromList bound_in_kernel

  -- Compute the variables that we need to pass to the kernel.
  reads_from <- readsFromSet actually_free

  -- Are we using any local memory?
  local_memory <- computeLocalMemoryUse actually_free
  return (nub reads_from, nub local_memory)

readsFromSet :: Names -> CallKernelGen [Imp.KernelUse]
readsFromSet free =
  fmap catMaybes $
  forM (S.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Array {} -> return Nothing
      Mem _ (Space "local") -> return Nothing
      Mem {} -> return $ Just $ Imp.MemoryUse var
      Prim bt ->
        isConstExp var >>= \case
          Just ce -> return $ Just $ Imp.ConstUse var ce
          Nothing | bt == Cert -> return Nothing
                  | otherwise  -> return $ Just $ Imp.ScalarUse var bt

computeLocalMemoryUse :: Names -> CallKernelGen [Imp.LocalMemoryUse]
computeLocalMemoryUse free =
  fmap catMaybes $
  forM (S.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Mem memsize (Space "local") -> do
        memsize' <- localMemSize =<< ImpGen.subExpToDimSize memsize
        return $ Just (var, memsize')
      _ -> return Nothing

localMemSize :: Imp.MemSize -> CallKernelGen (Either Imp.MemSize Imp.KernelConstExp)
localMemSize (Imp.ConstSize x) =
  return $ Right $ ValueExp $ IntValue $ Int64Value x
localMemSize (Imp.VarSize v) = isConstExp v >>= \case
  Just e | isStaticExp e -> return $ Right e
  _ -> return $ Left $ Imp.VarSize v

isConstExp :: VName -> CallKernelGen (Maybe Imp.KernelConstExp)
isConstExp v = do
  vtable <- ImpGen.getVTable
  fname <- asks ImpGen.envFunction
  let lookupConstExp name = constExp =<< hasExp =<< M.lookup name vtable
      constExp (Op (Inner (GetSize key _))) =
        Just $ LeafExp (Imp.SizeConst $ keyWithEntryPoint fname key) int32
      constExp e = primExpFromExp lookupConstExp e
  return $ lookupConstExp v
  where hasExp (ImpGen.ArrayVar e _) = e
        hasExp (ImpGen.ScalarVar e _) = e
        hasExp (ImpGen.MemVar e _) = e

-- | Only some constant expressions quality as *static* expressions,
-- which we can use for static memory allocation.  This is a bit of a
-- hack, as it is primarly motivated by what you can put as the size
-- when daring an array in C.
isStaticExp :: Imp.KernelConstExp -> Bool
isStaticExp LeafExp{} = True
isStaticExp ValueExp{} = True
isStaticExp (BinOpExp Add{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Sub{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Mul{} x y) = isStaticExp x && isStaticExp y
isStaticExp _ = False

computeThreadChunkSize :: SplitOrdering
                       -> Imp.Exp
                       -> Imp.Count Imp.Elements
                       -> Imp.Count Imp.Elements
                       -> VName
                       -> ImpGen.ImpM lore op ()
computeThreadChunkSize (SplitStrided stride) thread_index elements_per_thread num_elements chunk_var = do
  stride' <- ImpGen.compileSubExp stride
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

kernelInitialisationSetSpace :: KernelSpace -> InKernelGen ()
                             -> ImpGen.ImpM lore op (KernelConstants, ImpGen.ImpM InKernel Imp.KernelOp ())
kernelInitialisationSetSpace space set_space = do
  group_size' <- ImpGen.compileSubExp $ spaceGroupSize space
  num_threads' <- ImpGen.compileSubExp $ spaceNumThreads space
  num_groups <- ImpGen.compileSubExp $ spaceNumGroups space

  let global_tid = spaceGlobalId space
      local_tid = spaceLocalId space
      group_id = spaceGroupId space
  wave_size <- newVName "wave_size"
  inner_group_size <- newVName "group_size"

  let (space_is, space_dims) = unzip $ spaceDimensions space
  space_dims' <- mapM ImpGen.compileSubExp space_dims
  let constants =
        KernelConstants
        (Imp.var global_tid int32)
        (Imp.var local_tid int32)
        (Imp.var group_id int32)
        global_tid local_tid group_id
        group_size' num_groups num_threads'
        (Imp.var wave_size int32) (zip space_is space_dims')
        (if null (spaceDimensions space)
         then true else isActive (spaceDimensions space)) mempty

  let set_constants = do
        dPrim_ wave_size int32
        dPrim_ inner_group_size int32
        ImpGen.dScope Nothing (scopeOfKernelSpace space)

        sOp (Imp.GetGlobalId global_tid 0)
        sOp (Imp.GetLocalId local_tid 0)
        sOp (Imp.GetLocalSize inner_group_size 0)
        sOp (Imp.GetLockstepWidth wave_size)
        sOp (Imp.GetGroupId group_id 0)

        set_space

  return (constants, set_constants)

kernelInitialisation :: KernelSpace
                     -> ImpGen.ImpM lore op (KernelConstants, ImpGen.ImpM InKernel Imp.KernelOp ())
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
      gdims' <- mapM ImpGen.compileSubExp gdims
      ldims' <- mapM ImpGen.compileSubExp ldims
      let (gtid_es, ltid_es) = unzip $ unflattenNestedIndex gdims' ldims' gtid
      zipWithM_ (<--) gtids gtid_es
      zipWithM_ (<--) ltids ltid_es
  where flatSpaceWith base is_and_dims = do
          let (is, dims) = unzip is_and_dims
          dims' <- mapM ImpGen.compileSubExp dims
          let index_expressions = unflattenIndex dims' base
          zipWithM_ (<--) is index_expressions

isActive :: [(VName, SubExp)] -> Imp.Exp
isActive limit = case actives of
                    [] -> Imp.ValueExp $ BoolValue True
                    x:xs -> foldl (.&&.) x xs
  where (is, ws) = unzip limit
        actives = zipWith active is $ map (ImpGen.compileSubExpOfType Bool) ws
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
  local (\env -> env { ImpGen.envDefaultSpace = Imp.Space "global" }) .
  ImpGen.localVTable (M.map globalMemory)
  where globalMemory (ImpGen.MemVar _ entry)
          | ImpGen.entryMemSpace entry /= Space "local" =
              ImpGen.MemVar Nothing entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

allThreads :: KernelConstants -> InKernelGen () -> InKernelGen ()
allThreads constants = ImpGen.emit <=< ImpGen.subImpM_ (inKernelOperations constants')
  where constants' =
          constants { kernelThreadActive = Imp.ValueExp (BoolValue True) }



writeParamToLocalMemory :: Typed (MemBound u) =>
                           Imp.Exp -> (VName, t) -> Param (MemBound u)
                        -> ImpGen.ImpM lore op ()
writeParamToLocalMemory i (mem, _) param
  | Prim t <- paramType param =
      ImpGen.emit $
      Imp.Write mem (bytes i') bt (Space "local") Imp.Volatile $
      Imp.var (paramName param) t
  | otherwise =
      return ()
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

readParamFromLocalMemory :: Typed (MemBound u) =>
                            VName -> Imp.Exp -> Param (MemBound u) -> (VName, t)
                         -> ImpGen.ImpM lore op ()
readParamFromLocalMemory index i param (l_mem, _)
  | Prim _ <- paramType param =
      paramName param <--
      Imp.index l_mem (bytes i') bt (Space "local") Imp.Volatile
  | otherwise = index <-- i
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

groupReduce :: ExplicitMemorish lore =>
               KernelConstants
            -> Imp.Exp
            -> Lambda lore
            -> [VName]
            -> ImpGen.ImpM lore Imp.KernelOp ()
groupReduce constants w lam arrs = do
  offset <- dPrim "offset" int32
  groupReduceWithOffset constants offset w lam arrs

groupReduceWithOffset :: ExplicitMemorish lore =>
                         KernelConstants
                      -> VName
                      -> Imp.Exp
                      -> Lambda lore
                      -> [VName]
                      -> ImpGen.ImpM lore Imp.KernelOp ()
groupReduceWithOffset constants offset w lam arrs = do
  let (reduce_acc_params, reduce_arr_params) = splitAt (length arrs) $ lambdaParams lam

  skip_waves <- dPrim "skip_waves" int32
  ImpGen.dLParams $ lambdaParams lam

  offset <-- 0

  ImpGen.comment "participating threads read initial accumulator" $
    sWhen (local_tid .<. w) $
    zipWithM_ readReduceArgument reduce_acc_params arrs

  let do_reduce = do ImpGen.comment "read array element" $
                       zipWithM_ readReduceArgument reduce_arr_params arrs
                     ImpGen.comment "apply reduction operation" $
                       ImpGen.compileBody' reduce_acc_params $ lambdaBody lam
                     ImpGen.comment "write result of operation" $
                       zipWithM_ writeReduceOpResult reduce_acc_params arrs
      in_wave_reduce = ImpGen.everythingVolatile do_reduce

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
              let i = local_tid + ImpGen.varIndex offset
              ImpGen.copyDWIM (paramName param) [] (Var arr) [i]
          | otherwise = do
              let i = global_tid + ImpGen.varIndex offset
              ImpGen.copyDWIM (paramName param) [] (Var arr) [i]

        writeReduceOpResult param arr
          | Prim _ <- paramType param =
              ImpGen.copyDWIM arr [local_tid] (Var $ paramName param) []
          | otherwise =
              return ()

groupScan :: KernelConstants
          -> Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)
          -> Imp.Exp
          -> Lambda InKernel
          -> [VName]
          -> ImpGen.ImpM InKernel Imp.KernelOp ()
groupScan constants seg_flag w lam arrs = do
  when (any (not . primType . paramType) $ lambdaParams lam) $
    compilerLimitationS "Cannot compile parallel scans with array element type."

  renamed_lam <- renameLambda lam

  acc_local_mem <- flip zip (repeat ()) <$>
                   mapM (fmap (ImpGen.memLocationName . ImpGen.entryArrayLocation) .
                         ImpGen.lookupArray) arrs

  let ltid = kernelLocalThreadId constants
      (lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (x_params, y_params) = splitAt (length arrs) actual_params

  ImpGen.dLParams (lambdaParams lam++lambdaParams renamed_lam)
  lam_i <-- ltid

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
  ImpGen.comment
    "scan the first block, after which offset 'i' contains carry-in for warp 'i+1'" $
    doInBlockScan first_block_seg_flag (is_first_block .&&. ltid_in_bounds) renamed_lam

  sOp Imp.LocalBarrier

  let read_carry_in =
        zipWithM_ (readParamFromLocalMemory
                   (paramName other_index_param) (block_id - 1))
        x_params acc_local_mem

  let op_to_y
        | Nothing <- seg_flag =
            ImpGen.compileBody' y_params $ lambdaBody lam
        | Just flag_true <- seg_flag =
            sUnless (flag_true (block_id*block_size-1) ltid) $
              ImpGen.compileBody' y_params $ lambdaBody lam
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

inBlockScan :: Maybe (Imp.Exp -> Imp.Exp -> Imp.Exp)
            -> Imp.Exp
            -> Imp.Exp
            -> Imp.Exp
            -> Imp.Exp
            -> [(VName, t)]
            -> Lambda InKernel
            -> InKernelGen ()
inBlockScan seg_flag lockstep_width block_size active ltid acc_local_mem scan_lam = ImpGen.everythingVolatile $ do
  skip_threads <- dPrim "skip_threads" int32
  let in_block_thread_active =
        Imp.var skip_threads int32 .<=. in_block_id
      (scan_lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
      read_operands =
        zipWithM_ (readParamFromLocalMemory (paramName other_index_param) $
                   ltid - Imp.var skip_threads int32)
        x_params acc_local_mem

  -- Set initial y values
  sWhen active $
    zipWithM_ (readParamFromLocalMemory scan_lam_i ltid)
    y_params acc_local_mem

  let op_to_y
        | Nothing <- seg_flag =
            ImpGen.compileBody' y_params $ lambdaBody scan_lam
        | Just flag_true <- seg_flag =
            sUnless (flag_true (ltid-Imp.var skip_threads int32) ltid) $
              ImpGen.compileBody' y_params $ lambdaBody scan_lam
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

compileKernelStms :: KernelConstants -> [Stm InKernel]
                  -> InKernelGen a
                  -> InKernelGen a
compileKernelStms constants ungrouped_bnds m =
  compileGroupedKernelStms' $ groupStmsByGuard constants ungrouped_bnds
  where compileGroupedKernelStms' [] = m
        compileGroupedKernelStms' ((g, bnds):rest_bnds) = do
          ImpGen.dScopes (map ((Just . stmExp) &&& (castScope . scopeOf)) bnds)
          protect g $ mapM_ compileKernelStm bnds
          compileGroupedKernelStms' rest_bnds

        protect Nothing body_m =
          body_m
        protect (Just (Imp.ValueExp (BoolValue True))) body_m =
          body_m
        protect (Just g) body_m =
          sWhen g $ allThreads constants body_m

        compileKernelStm (Let pat _ e) = ImpGen.compileExp pat e

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
  fname <- asks ImpGen.envFunction
  let group_size_var = Imp.var group_size int32
      group_size_key = keyWithEntryPoint fname $ nameFromString $ pretty group_size
  sOp $ Imp.GetSize group_size group_size_key Imp.SizeGroup
  num_groups <- dPrimV "num_groups" $ kernel_size `quotRoundingUp` Imp.ConvOpExp (SExt Int32 Int32) group_size_var
  return (Imp.var group_size int32, Imp.var num_groups int32)

simpleKernelConstants :: Imp.Exp -> String
                      -> CallKernelGen (KernelConstants, ImpGen.ImpM InKernel Imp.KernelOp ())
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

  return (KernelConstants
          (Imp.var thread_gtid int32) (Imp.var thread_ltid int32) (Imp.var group_id int32)
          thread_gtid thread_ltid group_id
          group_size num_groups (group_size*num_groups) 0
          [] (Imp.var thread_gtid int32 .<. kernel_size) mempty,

          set_constants)

sKernel :: KernelConstants -> String -> ImpGen.ImpM InKernel Imp.KernelOp a -> CallKernelGen ()
sKernel constants name m = do
  body <- makeAllMemoryGlobal $
          ImpGen.subImpM_ (inKernelOperations constants) m
  (uses, local_memory) <- computeKernelUses body mempty
  ImpGen.emit $ Imp.Op $ Imp.CallKernel Imp.Kernel
    { Imp.kernelBody = body
    , Imp.kernelLocalMemory = local_memory
    , Imp.kernelUses = uses
    , Imp.kernelNumGroups = [kernelNumGroups constants]
    , Imp.kernelGroupSize = [kernelGroupSize constants]
    , Imp.kernelName =
        nameFromString $ name ++ "_" ++ show (baseTag $ kernelGlobalThreadIdVar constants)
    }

-- | Perform a Replicate with a kernel.
sReplicate :: VName -> Shape -> SubExp
           -> CallKernelGen ()
sReplicate arr (Shape ds) se = do
  t <- subExpType se

  dims <- mapM ImpGen.compileSubExp $ ds ++ arrayDims t
  (constants, set_constants) <-
    simpleKernelConstants (product dims) "replicate"

  let is' = unflattenIndex dims $ kernelGlobalThreadId constants

  sKernel constants "replicate" $ do
    set_constants
    sWhen (kernelThreadActive constants) $
      ImpGen.copyDWIM arr is' se $ drop (length ds) is'

-- | Perform an Iota with a kernel.
sIota :: VName -> Imp.Exp -> Imp.Exp -> Imp.Exp -> IntType
      -> CallKernelGen ()
sIota arr n x s et = do
  destloc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray arr
  (constants, set_constants) <- simpleKernelConstants n "iota"

  sKernel constants "iota" $ do
    set_constants
    let gtid = kernelGlobalThreadId constants
    sWhen (kernelThreadActive constants) $ do
      (destmem, destspace, destidx) <-
        ImpGen.fullyIndexArray' destloc [gtid] (IntType et)

      ImpGen.emit $
        Imp.Write destmem destidx (IntType et) destspace Imp.Nonvolatile $
        Imp.ConvOpExp (SExt Int32 et) gtid * s + x

sCopy :: PrimType
      -> ImpGen.MemLocation
      -> ImpGen.MemLocation
      -> Imp.Count Imp.Elements
      -> CallKernelGen ()
sCopy bt
  destloc@(ImpGen.MemLocation destmem _ _)
  srcloc@(ImpGen.MemLocation srcmem srcshape _)
  n = do
  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map Imp.sizeToExp srcshape
      shape_se = map (Imp.innerExp . ImpGen.dimSizeToExp) srcshape
      kernel_size = Imp.innerExp n * product (drop 1 shape)

  (constants, set_constants) <- simpleKernelConstants kernel_size "copy"

  sKernel constants "copy" $ do
    set_constants

    let gtid = kernelGlobalThreadId constants
        dest_is = unflattenIndex shape_se gtid
        src_is = dest_is

    (_, destspace, destidx) <- ImpGen.fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- ImpGen.fullyIndexArray' srcloc src_is bt

    sWhen (gtid .<. kernel_size) $ ImpGen.emit $
      Imp.Write destmem destidx bt destspace Imp.Nonvolatile $
      Imp.index srcmem srcidx bt srcspace Imp.Nonvolatile
