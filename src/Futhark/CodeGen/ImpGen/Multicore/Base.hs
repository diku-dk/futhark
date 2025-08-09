module Futhark.CodeGen.ImpGen.Multicore.Base
  ( extractAllocations,
    compileThreadResult,
    Locks (..),
    HostEnv (..),
    AtomicBinOp,
    MulticoreGen,
    decideScheduling,
    decideScheduling',
    renameSegBinOp,
    freeParams,
    renameHistOpLambda,
    atomicUpdateLocking,
    AtomicUpdate (..),
    DoAtomicUpdate,
    Locking (..),
    getSpace,
    getLoopBounds,
    getIterationDomain,
    getReturnParams,
    segOpString,
    ChunkLoopVectorization (..),
    generateChunkLoop,
    generateUniformizeLoop,
    extractVectorLane,
    inISPC,
    toParam,
    sLoopNestVectorized,
    renameSegPostOp,
    taskProvenance,
  )
where

import Control.Monad
import Data.Map qualified as M
import Data.Maybe
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Error
import Futhark.IR.MCMem
import Futhark.Transform.Rename
import Prelude hiding (quot, rem)

-- | Is there an atomic t'BinOp' corresponding to this t'BinOp'?
type AtomicBinOp =
  BinOp ->
  Maybe (VName -> VName -> Imp.Count Imp.Elements (Imp.TExp Int32) -> Imp.Exp -> Imp.AtomicOp)

-- | Information about the locks available for accumulators.
data Locks = Locks
  { locksArray :: VName,
    locksCount :: Int
  }

data HostEnv = HostEnv
  { hostAtomics :: AtomicBinOp,
    hostLocks :: M.Map VName Locks
  }

type MulticoreGen = ImpM MCMem HostEnv Imp.Multicore

segOpString :: SegOp () MCMem -> MulticoreGen String
segOpString SegMap {} = pure "segmap"
segOpString SegRed {} = pure "segred"
segOpString SegScan {} = pure "segscan"
segOpString SegHist {} = pure "seghist"

arrParam :: VName -> MulticoreGen Imp.Param
arrParam arr = do
  name_entry <- lookupVar arr
  case name_entry of
    ArrayVar _ (ArrayEntry (MemLoc mem _ _) _) ->
      pure $ Imp.MemParam mem DefaultSpace
    _ -> error $ "arrParam: could not handle array " ++ show arr

toParam :: VName -> TypeBase shape u -> MulticoreGen [Imp.Param]
toParam name (Prim pt) = pure [Imp.ScalarParam name pt]
toParam name (Mem space) = pure [Imp.MemParam name space]
toParam name Array {} = pure <$> arrParam name
toParam _name Acc {} = pure [] -- FIXME?  Are we sure this works?

getSpace :: SegOp () MCMem -> SegSpace
getSpace (SegHist _ space _ _ _) = space
getSpace (SegRed _ space _ _ _) = space
getSpace (SegScan _ space _ _ _ _) = space
getSpace (SegMap _ space _ _) = space

getLoopBounds :: MulticoreGen (Imp.TExp Int64, Imp.TExp Int64)
getLoopBounds = do
  start <- dPrim "start"
  end <- dPrim "end"
  emit $ Imp.Op $ Imp.GetLoopBounds (tvVar start) (tvVar end)
  pure (tvExp start, tvExp end)

getIterationDomain :: SegOp () MCMem -> SegSpace -> MulticoreGen (Imp.TExp Int64)
getIterationDomain SegMap {} space = do
  let ns = map snd $ unSegSpace space
      ns_64 = map pe64 ns
  pure $ product ns_64
getIterationDomain _ space = do
  let ns = map snd $ unSegSpace space
      ns_64 = map pe64 ns
  case unSegSpace space of
    [_] -> pure $ product ns_64
    -- A segmented SegOp is over the segments
    -- so we drop the last dimension, which is
    -- executed sequentially
    _ -> pure $ product $ init ns_64

-- When the SegRed's return value is a scalar
-- we perform a call by value-result in the segop function
getReturnParams :: Pat LetDecMem -> SegOp () MCMem -> MulticoreGen [Imp.Param]
getReturnParams pat SegRed {} =
  -- It's a good idea to make sure any prim values are initialised, as
  -- we will load them (redundantly) in the task code, and
  -- uninitialised values are UB.
  fmap concat . forM (patElems pat) $ \pe -> do
    case patElemType pe of
      Prim pt -> patElemName pe <~~ ValueExp (blankPrimValue pt)
      _ -> pure ()
    toParam (patElemName pe) (patElemType pe)
getReturnParams _ _ = pure mempty

renameSegBinOp :: [SegBinOp MCMem] -> MulticoreGen [SegBinOp MCMem]
renameSegBinOp segbinops =
  forM segbinops $ \(SegBinOp comm lam ne shape) -> do
    lam' <- renameLambda lam
    pure $ SegBinOp comm lam' ne shape

renameSegPostOp :: SegPostOp MCMem -> MulticoreGen (SegPostOp MCMem)
renameSegPostOp (SegPostOp lam) = do
  lam' <- renameLambda lam
  pure $ SegPostOp lam'

compileThreadResult ::
  SegSpace ->
  PatElem LetDecMem ->
  KernelResult ->
  MulticoreGen ()
compileThreadResult space pe (Returns _ _ what) = do
  let is = map (Imp.le64 . fst) $ unSegSpace space
  copyDWIMFix (patElemName pe) is what []
compileThreadResult _ _ TileReturns {} =
  compilerBugS "compileThreadResult: TileReturns unhandled."
compileThreadResult _ _ RegTileReturns {} =
  compilerBugS "compileThreadResult: RegTileReturns unhandled."

freeParams :: (FreeIn a) => a -> MulticoreGen [Imp.Param]
freeParams code = do
  let free = namesToList $ freeIn code
  ts <- mapM lookupType free
  concat <$> zipWithM toParam free ts

isLoadBalanced :: Imp.MCCode -> Bool
isLoadBalanced (a Imp.:>>: b) = isLoadBalanced a && isLoadBalanced b
isLoadBalanced (Imp.For _ _ a) = isLoadBalanced a
isLoadBalanced (Imp.If _ a b) = isLoadBalanced a && isLoadBalanced b
isLoadBalanced Imp.While {} = False
isLoadBalanced (Imp.Op (Imp.ParLoop _ code _)) = isLoadBalanced code
isLoadBalanced (Imp.Op (Imp.ForEachActive _ a)) = isLoadBalanced a
isLoadBalanced (Imp.Op (Imp.ForEach _ _ _ a)) = isLoadBalanced a
isLoadBalanced (Imp.Op (Imp.ISPCKernel a _)) = isLoadBalanced a
isLoadBalanced _ = True

decideScheduling' :: SegOp () rep -> Imp.MCCode -> Imp.Scheduling
decideScheduling' SegHist {} _ = Imp.Static
decideScheduling' SegScan {} _ = Imp.Static
decideScheduling' SegRed {} _ = Imp.Static
decideScheduling' SegMap {} code = decideScheduling code

decideScheduling :: Imp.MCCode -> Imp.Scheduling
decideScheduling code =
  if isLoadBalanced code
    then Imp.Static
    else Imp.Dynamic

-- | Try to extract invariant allocations.  If we assume that the
-- given 'Imp.MCCode' is the body of a 'SegOp', then it is always safe
-- to move the immediate allocations to the prebody.
extractAllocations :: Imp.MCCode -> (Imp.MCCode, Imp.MCCode)
extractAllocations segop_code = f segop_code
  where
    declared = Imp.declaredIn segop_code
    f (Imp.DeclareMem name space) =
      -- Hoisting declarations out is always safe.
      (Imp.DeclareMem name space, mempty)
    f (Imp.Allocate name size space)
      | not $ freeIn size `namesIntersect` declared =
          (Imp.Allocate name size space, mempty)
    f (x Imp.:>>: y) = f x <> f y
    f (Imp.While cond body) =
      (mempty, Imp.While cond body)
    f (Imp.For i bound body) =
      (mempty, Imp.For i bound body)
    f Imp.Free {} =
      mempty
    f (Imp.If cond tcode fcode) =
      let (ta, tcode') = f tcode
          (fa, fcode') = f fcode
       in (ta <> fa, Imp.If cond tcode' fcode')
    f (Imp.Op (Imp.ParLoop s body free)) =
      let (body_allocs, body') = extractAllocations body
          (free_allocs, here_allocs) = f body_allocs
          free' =
            filter
              ( (`notNameIn` Imp.declaredIn body_allocs) . Imp.paramName
              )
              free
       in ( free_allocs,
            here_allocs <> Imp.Op (Imp.ParLoop s body' free')
          )
    f code =
      (mempty, code)

-- | Indicates whether to vectorize a chunk loop or keep it sequential.
-- We use this to allow falling back to sequential chunk loops in cases
-- we don't care about trying to vectorize.
data ChunkLoopVectorization = Vectorized | Scalar

-- | Emit code for the chunk loop, given an action that generates code
-- for a single iteration.
--
-- The action is called with the (symbolic) index of the current
-- iteration.
generateChunkLoop ::
  String ->
  ChunkLoopVectorization ->
  (Imp.TExp Int64 -> MulticoreGen ()) ->
  MulticoreGen ()
generateChunkLoop desc Scalar m = do
  (start, end) <- getLoopBounds
  n <- dPrimVE "n" $ end - start
  i <- newVName (desc <> "_i")
  (body_allocs, body) <- fmap extractAllocations $
    collect $ do
      addLoopVar i Int64
      m $ start + Imp.le64 i
  emit body_allocs
  -- Emit either foreach or normal for loop
  let bound = untyped n
  emit $ Imp.For i bound body
generateChunkLoop desc Vectorized m = do
  (start, end) <- getLoopBounds
  n <- dPrimVE "n" $ end - start
  i <- newVName (desc <> "_i")
  (body_allocs, body) <- fmap extractAllocations $
    collect $ do
      addLoopVar i Int64
      m $ Imp.le64 i
  emit body_allocs
  -- Emit either foreach or normal for loop
  let from = untyped start
  let bound = untyped (start + n)
  emit $ Imp.Op $ Imp.ForEach i from bound body

-- | Emit code for a sequential loop over each vector lane, given
-- and action that generates code for a single iteration. The action
-- is called with the symbolic index of the current iteration.
generateUniformizeLoop :: (Imp.TExp Int64 -> MulticoreGen ()) -> MulticoreGen ()
generateUniformizeLoop m = do
  i <- newVName "uni_i"
  body <- collect $ do
    addLoopVar i Int64
    m $ Imp.le64 i
  emit $ Imp.Op $ Imp.ForEachActive i body

-- | Given a piece of code, if that code performs an assignment, turn
-- that assignment into an extraction of element from a vector on the
-- right hand side, using a passed index for the extraction. Other code
-- is left as is.
extractVectorLane :: Imp.TExp Int64 -> MulticoreGen Imp.MCCode -> MulticoreGen ()
extractVectorLane j code = do
  let ut_exp = untyped j
  code' <- code
  case code' of
    Imp.SetScalar vname e -> do
      typ <- lookupType vname
      case typ of
        -- ISPC v1.17 does not support extract on f16 yet..
        -- Thus we do this stupid conversion to f32
        Prim (FloatType Float16) -> do
          tv :: TV Float <- dPrim "hack_extract_f16"
          emit $ Imp.SetScalar (tvVar tv) e
          emit $ Imp.Op $ Imp.ExtractLane vname (untyped $ tvExp tv) ut_exp
        _ -> emit $ Imp.Op $ Imp.ExtractLane vname e ut_exp
    _ ->
      emit code'

-- | Given an action that may generate some code, put that code
-- into an ISPC kernel.
inISPC :: MulticoreGen () -> MulticoreGen ()
inISPC code = do
  code' <- collect code
  free <- freeParams code'
  emit $ Imp.Op $ Imp.ISPCKernel code' free

-------------------------------
------- SegRed helpers  -------
-------------------------------
sForVectorized' :: VName -> Imp.Exp -> MulticoreGen () -> MulticoreGen ()
sForVectorized' i bound body = do
  let it = case primExpType bound of
        IntType bound_t -> bound_t
        t -> error $ "sFor': bound " ++ prettyString bound ++ " is of type " ++ prettyString t
  addLoopVar i it
  body' <- collect body
  emit $ Imp.Op $ Imp.ForEach i (Imp.ValueExp $ blankPrimValue $ Imp.IntType Imp.Int64) bound body'

sForVectorized :: String -> Imp.TExp t -> (Imp.TExp t -> MulticoreGen ()) -> MulticoreGen ()
sForVectorized i bound body = do
  i' <- newVName i
  sForVectorized' i' (untyped bound) $
    body $
      TPrimExp $
        Imp.var i' $
          primExpType $
            untyped bound

-- | Like sLoopNest, but puts a vectorized loop at the innermost layer.
sLoopNestVectorized ::
  Shape ->
  ([Imp.TExp Int64] -> MulticoreGen ()) ->
  MulticoreGen ()
sLoopNestVectorized = sLoopNest' [] . shapeDims
  where
    sLoopNest' is [] f = f $ reverse is
    sLoopNest' is [d] f =
      sForVectorized "nest_i" (pe64 d) $ \i -> sLoopNest' (i : is) [] f
    sLoopNest' is (d : ds) f =
      sFor "nest_i" (pe64 d) $ \i -> sLoopNest' (i : is) ds f

-------------------------------
------- SegHist helpers -------
-------------------------------
renameHistOpLambda :: [HistOp MCMem] -> MulticoreGen [HistOp MCMem]
renameHistOpLambda hist_ops =
  forM hist_ops $ \(HistOp w rf dest neutral shape lam) -> do
    lam' <- renameLambda lam
    pure $ HistOp w rf dest neutral shape lam'

-- | Locking strategy used for an atomic update.
data Locking = Locking
  { -- | Array containing the lock.
    lockingArray :: VName,
    -- | Value for us to consider the lock free.
    lockingIsUnlocked :: Imp.TExp Int32,
    -- | What to write when we lock it.
    lockingToLock :: Imp.TExp Int32,
    -- | What to write when we unlock it.
    lockingToUnlock :: Imp.TExp Int32,
    -- | A transformation from the logical lock index to the
    -- physical position in the array.  This can also be used
    -- to make the lock array smaller.
    lockingMapping :: [Imp.TExp Int64] -> [Imp.TExp Int64]
  }

-- | A function for generating code for an atomic update.  Assumes
-- that the bucket is in-bounds.
type DoAtomicUpdate rep r =
  [VName] -> [Imp.TExp Int64] -> MulticoreGen ()

-- | The mechanism that will be used for performing the atomic update.
-- Approximates how efficient it will be.  Ordered from most to least
-- efficient.
data AtomicUpdate rep r
  = AtomicPrim (DoAtomicUpdate rep r)
  | -- | Can be done by efficient swaps.
    AtomicCAS (DoAtomicUpdate rep r)
  | -- | Requires explicit locking.
    AtomicLocking (Locking -> DoAtomicUpdate rep r)

atomicUpdateLocking ::
  AtomicBinOp ->
  Lambda MCMem ->
  AtomicUpdate MCMem ()
atomicUpdateLocking atomicBinOp lam
  | Just ops_and_ts <- lamIsBinOp lam,
    all (\(_, t, _, _) -> supportedPrims $ primBitSize t) ops_and_ts =
      primOrCas ops_and_ts $ \arrs bucket ->
        -- If the operator is a vectorised binary operator on 32-bit values,
        -- we can use a particularly efficient implementation. If the
        -- operator has an atomic implementation we use that, otherwise it
        -- is still a binary operator which can be implemented by atomic
        -- compare-and-swap if 32 bits.
        forM_ (zip arrs ops_and_ts) $ \(a, (op, t, x, y)) -> do
          -- Common variables.
          old <- dPrimS "old" t

          (arr', _a_space, bucket_offset) <- fullyIndexArray a bucket

          case opHasAtomicSupport old arr' (sExt32 <$> bucket_offset) op of
            Just f -> sOp $ f $ Imp.var y t
            Nothing ->
              atomicUpdateCAS t a old bucket x $
                x <~~ Imp.BinOpExp op (Imp.var x t) (Imp.var y t)
  where
    opHasAtomicSupport old arr' bucket' bop = do
      let atomic f = Imp.Atomic . f old arr' bucket'
      atomic <$> atomicBinOp bop

    primOrCas ops
      | all isPrim ops = AtomicPrim
      | otherwise = AtomicCAS

    isPrim (op, _, _, _) = isJust $ atomicBinOp op
atomicUpdateLocking _ op
  | [Prim t] <- lambdaReturnType op,
    [xp, _] <- lambdaParams op,
    supportedPrims (primBitSize t) = AtomicCAS $ \[arr] bucket -> do
      old <- dPrimS "old" t
      atomicUpdateCAS t arr old bucket (paramName xp) $
        compileBody' [xp] $
          lambdaBody op
atomicUpdateLocking _ op = AtomicLocking $ \locking arrs bucket -> do
  old <- dPrim "old"
  continue <- dPrimVol "continue" int32 (0 :: Imp.TExp Int32)

  -- Correctly index into locks.
  (locks', _locks_space, locks_offset) <-
    fullyIndexArray (lockingArray locking) $ lockingMapping locking bucket

  -- Critical section
  let try_acquire_lock = do
        old <-- (0 :: Imp.TExp Int32)
        sOp . Imp.Atomic $
          Imp.AtomicCmpXchg
            int32
            (tvVar old)
            locks'
            (sExt32 <$> locks_offset)
            (tvVar continue)
            (untyped (lockingToLock locking))
      lock_acquired = tvExp continue
      -- Even the releasing is done with an atomic rather than a
      -- simple write, for memory coherency reasons.
      release_lock = do
        old <-- lockingToLock locking
        sOp . Imp.Atomic $
          Imp.AtomicCmpXchg
            int32
            (tvVar old)
            locks'
            (sExt32 <$> locks_offset)
            (tvVar continue)
            (untyped (lockingToUnlock locking))

  -- Preparing parameters. It is assumed that the caller has already
  -- filled the arr_params. We copy the current value to the
  -- accumulator parameters.
  let (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op
      bind_acc_params =
        everythingVolatile $
          sComment "bind lhs" $
            forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
              copyDWIMFix (paramName acc_p) [] (Var arr) bucket

  let op_body =
        sComment "execute operation" $
          compileBody' acc_params $
            lambdaBody op

      do_hist =
        everythingVolatile $
          sComment "update global result" $
            zipWithM_ (writeArray bucket) arrs $
              map (Var . paramName) acc_params

  -- While-loop: Try to insert your value
  sWhile (tvExp continue .==. 0) $ do
    try_acquire_lock
    sUnless (lock_acquired .==. 0) $ do
      dLParams acc_params
      bind_acc_params
      op_body
      do_hist
      release_lock
  where
    writeArray bucket arr val = copyDWIMFix arr bucket val []

atomicUpdateCAS ::
  PrimType ->
  VName ->
  VName ->
  [Imp.TExp Int64] ->
  VName ->
  MulticoreGen () ->
  MulticoreGen ()
atomicUpdateCAS t arr old bucket x do_op = do
  run_loop <- dPrimV "run_loop" (0 :: Imp.TExp Int32)
  (arr', _a_space, bucket_offset) <- fullyIndexArray arr bucket

  bytes <- toIntegral $ primBitSize t
  let (toBits, fromBits) =
        case t of
          FloatType ft ->
            ( Imp.ConvOpExp (FPToBits ft),
              Imp.ConvOpExp (BitsToFP ft)
            )
          _ -> (id, id)

      int
        | primBitSize t == 16 = int16
        | primBitSize t == 32 = int32
        | otherwise = int64

  everythingVolatile $ copyDWIMFix old [] (Var arr) bucket

  old_bits_v <- dPrimS "old_bits" int
  old_bits_v <~~ toBits (Imp.var old t)
  let old_bits = Imp.var old_bits_v int

  -- While-loop: Try to insert your value
  sWhile (tvExp run_loop .==. 0) $ do
    x <~~ Imp.var old t
    do_op -- Writes result into x
    sOp . Imp.Atomic $
      Imp.AtomicCmpXchg
        bytes
        old_bits_v
        arr'
        (sExt32 <$> bucket_offset)
        (tvVar run_loop)
        (toBits (Imp.var x t))
    old <~~ fromBits old_bits

supportedPrims :: Int -> Bool
supportedPrims 8 = True
supportedPrims 16 = True
supportedPrims 32 = True
supportedPrims 64 = True
supportedPrims _ = False

-- Supported bytes lengths by GCC (and clang) compiler
toIntegral :: Int -> MulticoreGen PrimType
toIntegral 8 = pure int8
toIntegral 16 = pure int16
toIntegral 32 = pure int32
toIntegral 64 = pure int64
toIntegral b = error $ "number of bytes is not supported for CAS - " ++ prettyString b

-- | Find the provenance of a task given its body. This is done by folding the
-- provenance of the code in the body.
taskProvenance :: Imp.MCCode -> Provenance
taskProvenance = Imp.foldProvenances onOp
  where
    onOp (Imp.ParLoop _ code _) = taskProvenance code
    onOp (Imp.SegOp _ _ task1 task2 _ _) =
      onTask task1 <> maybe mempty onTask task2
    onOp (Imp.ISPCKernel code _) = taskProvenance code
    onOp (Imp.ForEach _ _ _ code) = taskProvenance code
    onOp (Imp.ForEachActive _ code) = taskProvenance code
    onOp _ = mempty
    onTask (Imp.ParallelTask code) = taskProvenance code
