{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | The bulk of the short-circuiting implementation.
module Futhark.Optimise.ArrayShortCircuiting.ArrayCoalescing
  ( mkCoalsTab,
    CoalsTab,
    mkCoalsTabGPU,
    mkCoalsTabMC,
  )
where

import Control.Exception.Base qualified as Exc
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Function ((&))
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence (Seq (..))
import Data.Set qualified as S
import Futhark.Analysis.LastUse
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.GPUMem as GPU
import Futhark.IR.MCMem as MC
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Optimise.ArrayShortCircuiting.MemRefAggreg
import Futhark.Optimise.ArrayShortCircuiting.TopdownAnalysis
import Futhark.Util

-- | A helper type describing representations that can be short-circuited.
type Coalesceable rep inner =
  ( Mem rep inner,
    ASTRep rep,
    CanBeAliased inner,
    AliasableRep rep,
    Op rep ~ MemOp inner rep,
    HasMemBlock (Aliases rep),
    LetDec rep ~ LetDecMem,
    TopDownHelper (inner (Aliases rep))
  )

type ComputeScalarTable rep op =
  ScopeTab rep -> op -> ScalarTableM rep (M.Map VName (PrimExp VName))

-- Helper type for computing scalar tables on ops.
newtype ComputeScalarTableOnOp rep = ComputeScalarTableOnOp
  { scalarTableOnOp :: ComputeScalarTable rep (Op (Aliases rep))
  }

type ScalarTableM rep a = Reader (ComputeScalarTableOnOp rep) a

data ShortCircuitReader rep = ShortCircuitReader
  { onOp ::
      LUTabFun ->
      Pat (VarAliases, LetDecMem) ->
      Certs ->
      Op (Aliases rep) ->
      TopdownEnv rep ->
      BotUpEnv ->
      ShortCircuitM rep BotUpEnv,
    ssPointFromOp ::
      LUTabFun ->
      TopdownEnv rep ->
      ScopeTab rep ->
      Pat (VarAliases, LetDecMem) ->
      Certs ->
      Op (Aliases rep) ->
      Maybe [SSPointInfo]
  }

newtype ShortCircuitM rep a = ShortCircuitM (ReaderT (ShortCircuitReader rep) (State VNameSource) a)
  deriving (Functor, Applicative, Monad, MonadReader (ShortCircuitReader rep), MonadState VNameSource)

instance MonadFreshNames (ShortCircuitM rep) where
  putNameSource = put
  getNameSource = get

emptyTopdownEnv :: TopdownEnv rep
emptyTopdownEnv =
  TopdownEnv
    { alloc = mempty,
      scope = mempty,
      inhibited = mempty,
      v_alias = mempty,
      m_alias = mempty,
      nonNegatives = mempty,
      scalarTable = mempty,
      knownLessThan = mempty,
      td_asserts = mempty
    }

emptyBotUpEnv :: BotUpEnv
emptyBotUpEnv =
  BotUpEnv
    { scals = mempty,
      activeCoals = mempty,
      successCoals = mempty,
      inhibit = mempty
    }

--------------------------------------------------------------------------------
--- Main Coalescing Transformation computes a successful coalescing table    ---
--------------------------------------------------------------------------------

-- | Given a 'Prog' in 'SegMem' representation, compute the coalescing table
-- by folding over each function.
mkCoalsTab :: (MonadFreshNames m) => Prog (Aliases SeqMem) -> m (M.Map Name CoalsTab)
mkCoalsTab prog =
  mkCoalsTabProg
    (lastUseSeqMem prog)
    (ShortCircuitReader shortCircuitSeqMem genSSPointInfoSeqMem)
    (ComputeScalarTableOnOp $ const $ const $ pure mempty)
    prog

-- | Given a 'Prog' in 'GPUMem' representation, compute the coalescing table
-- by folding over each function.
mkCoalsTabGPU :: (MonadFreshNames m) => Prog (Aliases GPUMem) -> m (M.Map Name CoalsTab)
mkCoalsTabGPU prog =
  mkCoalsTabProg
    (lastUseGPUMem prog)
    (ShortCircuitReader shortCircuitGPUMem genSSPointInfoGPUMem)
    (ComputeScalarTableOnOp (computeScalarTableMemOp computeScalarTableGPUMem))
    prog

-- | Given a 'Prog' in 'MCMem' representation, compute the coalescing table
-- by folding over each function.
mkCoalsTabMC :: (MonadFreshNames m) => Prog (Aliases MCMem) -> m (M.Map Name CoalsTab)
mkCoalsTabMC prog =
  mkCoalsTabProg
    (lastUseMCMem prog)
    (ShortCircuitReader shortCircuitMCMem genSSPointInfoMCMem)
    (ComputeScalarTableOnOp (computeScalarTableMemOp computeScalarTableMCMem))
    prog

-- | Given a function, compute the coalescing table
mkCoalsTabProg ::
  (MonadFreshNames m, Coalesceable rep inner) =>
  LUTabProg ->
  ShortCircuitReader rep ->
  ComputeScalarTableOnOp rep ->
  Prog (Aliases rep) ->
  m (M.Map Name CoalsTab)
mkCoalsTabProg (_, lutab_prog) r computeScalarOnOp =
  fmap M.fromList . mapM onFun . progFuns
  where
    onFun fun@(FunDef _ _ fname _ fpars body) = do
      -- First compute last-use information
      let unique_mems = getUniqueMemFParam fpars
          lutab = lutab_prog M.! fname
          scalar_table =
            runReader
              ( concatMapM
                  (computeScalarTable $ scopeOf fun <> scopeOf (bodyStms body))
                  (stmsToList $ bodyStms body)
              )
              computeScalarOnOp
          topenv =
            emptyTopdownEnv
              { scope = scopeOfFParams fpars,
                alloc = unique_mems,
                scalarTable = scalar_table,
                nonNegatives = foldMap paramSizes fpars
              }
          ShortCircuitM m = fixPointCoalesce lutab fpars body topenv
      (fname,) <$> modifyNameSource (runState (runReaderT m r))

paramSizes :: Param FParamMem -> Names
paramSizes (Param _ _ (MemArray _ shp _ _)) = freeIn shp
paramSizes _ = mempty

-- | Short-circuit handler for a 'SeqMem' 'Op'.
--
-- Because 'SeqMem' don't have any special operation, simply return the input
-- 'BotUpEnv'.
shortCircuitSeqMem :: LUTabFun -> Pat (VarAliases, LetDecMem) -> Certs -> Op (Aliases SeqMem) -> TopdownEnv SeqMem -> BotUpEnv -> ShortCircuitM SeqMem BotUpEnv
shortCircuitSeqMem _ _ _ _ _ = pure

-- | Short-circuit handler for SegOp.
shortCircuitSegOp ::
  (Coalesceable rep inner) =>
  (lvl -> Bool) ->
  LUTabFun ->
  Pat (VarAliases, LetDecMem) ->
  Certs ->
  SegOp lvl (Aliases rep) ->
  TopdownEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep BotUpEnv
shortCircuitSegOp lvlOK lutab pat pat_certs (SegMap lvl space _ kernel_body) td_env bu_env =
  -- No special handling necessary for 'SegMap'. Just call the helper-function.
  shortCircuitSegOpHelper 0 lvlOK lvl lutab pat pat_certs space kernel_body td_env bu_env
shortCircuitSegOp lvlOK lutab pat pat_certs (SegRed lvl space binops _ kernel_body) td_env bu_env =
  -- When handling 'SegRed', we we first invalidate all active coalesce-entries
  -- where any of the variables in 'vartab' are also free in the list of
  -- 'SegBinOp'. In other words, anything that is used as part of the reduction
  -- step should probably not be coalesced.
  let to_fail = M.filter (\entry -> namesFromList (M.keys $ vartab entry) `namesIntersect` foldMap (freeIn . segBinOpLambda) binops) $ activeCoals bu_env
      (active, inh) =
        foldl markFailedCoal (activeCoals bu_env, inhibit bu_env) $ M.keys to_fail
      bu_env' = bu_env {activeCoals = active, inhibit = inh}
      num_reds = length red_ts
   in shortCircuitSegOpHelper num_reds lvlOK lvl lutab pat pat_certs space kernel_body td_env bu_env'
  where
    segment_dims = init $ segSpaceDims space
    red_ts = do
      op <- binops
      let shp = Shape segment_dims <> segBinOpShape op
      map (`arrayOfShape` shp) (lambdaReturnType $ segBinOpLambda op)
shortCircuitSegOp lvlOK lutab pat pat_certs (SegScan lvl space binops _ kernel_body) td_env bu_env =
  -- Like in the handling of 'SegRed', we do not want to coalesce anything that
  -- is used in the 'SegBinOp'
  let to_fail = M.filter (\entry -> namesFromList (M.keys $ vartab entry) `namesIntersect` foldMap (freeIn . segBinOpLambda) binops) $ activeCoals bu_env
      (active, inh) = foldl markFailedCoal (activeCoals bu_env, inhibit bu_env) $ M.keys to_fail
      bu_env' = bu_env {activeCoals = active, inhibit = inh}
   in shortCircuitSegOpHelper 0 lvlOK lvl lutab pat pat_certs space kernel_body td_env bu_env'
shortCircuitSegOp lvlOK lutab pat pat_certs (SegHist lvl space histops _ kernel_body) td_env bu_env = do
  -- Need to take zipped patterns and histDest (flattened) and insert transitive coalesces
  let to_fail = M.filter (\entry -> namesFromList (M.keys $ vartab entry) `namesIntersect` foldMap (freeIn . histOp) histops) $ activeCoals bu_env
      (active, inh) = foldl markFailedCoal (activeCoals bu_env, inhibit bu_env) $ M.keys to_fail
      bu_env' = bu_env {activeCoals = active, inhibit = inh}
  bu_env'' <- shortCircuitSegOpHelper 0 lvlOK lvl lutab pat pat_certs space kernel_body td_env bu_env'
  pure $
    foldl insertHistCoals bu_env'' $
      zip (patElems pat) $
        concatMap histDest histops
  where
    insertHistCoals acc (PatElem p _, hist_dest) =
      case ( getScopeMemInfo p $ scope td_env,
             getScopeMemInfo hist_dest $ scope td_env
           ) of
        (Just (MemBlock _ _ p_mem _), Just (MemBlock _ _ dest_mem _)) ->
          case M.lookup p_mem $ successCoals acc of
            Just entry ->
              -- Update this entry with an optdep for the memory block of hist_dest
              let entry' = entry {optdeps = M.insert p p_mem $ optdeps entry}
               in acc
                    { successCoals = M.insert p_mem entry' $ successCoals acc,
                      activeCoals = M.insert dest_mem entry $ activeCoals acc
                    }
            Nothing -> acc
        _ -> acc

-- | Short-circuit handler for 'GPUMem' 'Op'.
--
-- When the 'Op' is a 'SegOp', we handle it accordingly, otherwise we do
-- nothing.
shortCircuitGPUMem ::
  LUTabFun ->
  Pat (VarAliases, LetDecMem) ->
  Certs ->
  Op (Aliases GPUMem) ->
  TopdownEnv GPUMem ->
  BotUpEnv ->
  ShortCircuitM GPUMem BotUpEnv
shortCircuitGPUMem _ _ _ (Alloc _ _) _ bu_env = pure bu_env
shortCircuitGPUMem lutab pat certs (Inner (GPU.SegOp op)) td_env bu_env =
  shortCircuitSegOp isSegThread lutab pat certs op td_env bu_env
shortCircuitGPUMem lutab pat certs (Inner (GPU.GPUBody _ body)) td_env bu_env = do
  fresh1 <- newNameFromString "gpubody"
  fresh2 <- newNameFromString "gpubody"
  shortCircuitSegOpHelper
    0
    isSegThread
    -- Construct a 'SegLevel' corresponding to a single thread
    ( GPU.SegThread GPU.SegNoVirt $
        Just $
          GPU.KernelGrid
            (GPU.Count $ Constant $ IntValue $ Int64Value 1)
            (GPU.Count $ Constant $ IntValue $ Int64Value 1)
    )
    lutab
    pat
    certs
    (SegSpace fresh1 [(fresh2, Constant $ IntValue $ Int64Value 1)])
    (bodyToKernelBody body)
    td_env
    bu_env
shortCircuitGPUMem _ _ _ (Inner (GPU.SizeOp _)) _ bu_env = pure bu_env
shortCircuitGPUMem _ _ _ (Inner (GPU.OtherOp NoOp)) _ bu_env = pure bu_env

shortCircuitMCMem ::
  LUTabFun ->
  Pat (VarAliases, LetDecMem) ->
  Certs ->
  Op (Aliases MCMem) ->
  TopdownEnv MCMem ->
  BotUpEnv ->
  ShortCircuitM MCMem BotUpEnv
shortCircuitMCMem _ _ _ (Alloc _ _) _ bu_env = pure bu_env
shortCircuitMCMem _ _ _ (Inner (MC.OtherOp NoOp)) _ bu_env = pure bu_env
shortCircuitMCMem lutab pat certs (Inner (MC.ParOp (Just par_op) op)) td_env bu_env =
  shortCircuitSegOp (const True) lutab pat certs par_op td_env bu_env
    >>= shortCircuitSegOp (const True) lutab pat certs op td_env
shortCircuitMCMem lutab pat certs (Inner (MC.ParOp Nothing op)) td_env bu_env =
  shortCircuitSegOp (const True) lutab pat certs op td_env bu_env

dropLastSegSpace :: SegSpace -> SegSpace
dropLastSegSpace space = space {unSegSpace = init $ unSegSpace space}

isSegThread :: GPU.SegLevel -> Bool
isSegThread GPU.SegThread {} = True
isSegThread _ = False

-- | Computes the slice written at the end of a thread in a 'SegOp'.
threadSlice :: SegSpace -> KernelResult -> Maybe (Slice (TPrimExp Int64 VName))
threadSlice space Returns {} =
  Just $
    Slice $
      map (DimFix . TPrimExp . flip LeafExp (IntType Int64) . fst) $
        unSegSpace space
threadSlice space (RegTileReturns _ dims _) =
  Just
    $ Slice
    $ zipWith
      ( \(_, block_tile_size0, reg_tile_size0) (x0, _) ->
          let x = pe64 $ Var x0
              block_tile_size = pe64 block_tile_size0
              reg_tile_size = pe64 reg_tile_size0
           in DimSlice (x * block_tile_size * reg_tile_size) (block_tile_size * reg_tile_size) 1
      )
      dims
    $ unSegSpace space
threadSlice _ _ = Nothing

bodyToKernelBody :: Body (Aliases GPUMem) -> KernelBody (Aliases GPUMem)
bodyToKernelBody (Body dec stms res) =
  KernelBody dec stms $ map (\(SubExpRes cert subexps) -> Returns ResultNoSimplify cert subexps) res

-- | A helper for all the different kinds of 'SegOp'.
--
-- Consists of four parts:
--
-- 1. Create coalescing relations between the pattern elements and the kernel
-- body results using 'makeSegMapCoals'.
--
-- 2. Process the statements of the 'KernelBody'.
--
-- 3. Check the overlap between the different threads.
--
-- 4. Mark active coalescings as finished, since a 'SegOp' is an array creation
-- point.
shortCircuitSegOpHelper ::
  (Coalesceable rep inner) =>
  -- | The number of returns for which we should drop the last seg space
  Int ->
  -- | Whether we should look at a segop with this lvl.
  (lvl -> Bool) ->
  lvl ->
  LUTabFun ->
  Pat (VarAliases, LetDecMem) ->
  Certs ->
  SegSpace ->
  KernelBody (Aliases rep) ->
  TopdownEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep BotUpEnv
shortCircuitSegOpHelper num_reds lvlOK lvl lutab pat@(Pat ps0) pat_certs space0 kernel_body td_env bu_env = do
  -- We need to drop the last element of the 'SegSpace' for pattern elements
  -- that correspond to reductions.
  let ps_space_and_res =
        zip3 ps0 (replicate num_reds (dropLastSegSpace space0) <> repeat space0) $
          kernelBodyResult kernel_body
  -- Create coalescing relations between pattern elements and kernel body
  -- results
  let (actv0, inhibit0) =
        filterSafetyCond2and5
          (activeCoals bu_env)
          (inhibit bu_env)
          (scals bu_env)
          td_env
          (patElems pat)
      (actv_return, inhibit_return) =
        if num_reds > 0
          then (actv0, inhibit0)
          else foldl (makeSegMapCoals lvlOK lvl td_env kernel_body pat_certs) (actv0, inhibit0) ps_space_and_res

  -- Start from empty references, we'll update with aggregates later.
  let actv0' = M.map (\etry -> etry {memrefs = mempty}) $ actv0 <> actv_return
  -- Process kernel body statements
  bu_env' <-
    mkCoalsTabStms lutab (kernelBodyStms kernel_body) td_env $
      bu_env {activeCoals = actv0', inhibit = inhibit_return}

  let actv_coals_after =
        M.mapWithKey
          ( \k etry ->
              etry
                { memrefs = memrefs etry <> maybe mempty memrefs (M.lookup k $ actv0 <> actv_return)
                }
          )
          $ activeCoals bu_env'

  -- Check partial overlap.
  let checkPartialOverlap bu_env_f (k, entry) = do
        let sliceThreadAccess (p, space, res) =
              case M.lookup (patElemName p) $ vartab entry of
                Just (Coalesced _ (MemBlock _ _ _ ixf) _) ->
                  maybe
                    Undeterminable
                    ( ixfunToAccessSummary
                        . LMAD.slice ixf
                        . fullSlice (LMAD.shape ixf)
                    )
                    $ threadSlice space res
                Nothing -> mempty
            thread_writes = foldMap sliceThreadAccess ps_space_and_res
            source_writes = srcwrts (memrefs entry) <> thread_writes
        destination_uses <-
          case dstrefs (memrefs entry)
            `accessSubtract` dstrefs (maybe mempty memrefs $ M.lookup k $ activeCoals bu_env) of
            Set s ->
              concatMapM
                (aggSummaryMapPartial (scalarTable td_env) $ unSegSpace space0)
                (S.toList s)
            Undeterminable -> pure Undeterminable
        -- Do not allow short-circuiting from a segop-shared memory
        -- block (not in the topdown scope) to an outer memory block.
        if dstmem entry `M.member` scope td_env
          && noMemOverlap td_env destination_uses source_writes
          then pure bu_env_f
          else do
            let (ac, inh) = markFailedCoal (activeCoals bu_env_f, inhibit bu_env_f) k
            pure $ bu_env_f {activeCoals = ac, inhibit = inh}

  bu_env'' <-
    foldM
      checkPartialOverlap
      (bu_env' {activeCoals = actv_coals_after})
      $ M.toList actv_coals_after

  let updateMemRefs entry = do
        wrts <- aggSummaryMapTotal (scalarTable td_env) (unSegSpace space0) $ srcwrts $ memrefs entry
        uses <- aggSummaryMapTotal (scalarTable td_env) (unSegSpace space0) $ dstrefs $ memrefs entry

        -- Add destination uses from the pattern
        let uses' =
              foldMap
                ( \case
                    PatElem _ (_, MemArray _ _ _ (ArrayIn p_mem p_ixf))
                      | p_mem `nameIn` alsmem entry ->
                          ixfunToAccessSummary p_ixf
                    _ -> mempty
                )
                ps0

        pure $ entry {memrefs = MemRefs (uses <> uses') wrts}

  actv <- mapM updateMemRefs $ activeCoals bu_env''
  let bu_env''' = bu_env'' {activeCoals = actv}

  -- Process pattern and return values
  let mergee_writes =
        mapMaybe
          ( \(p, _, _) ->
              fmap (p,) $
                getDirAliasedIxfn' td_env (activeCoals bu_env''') $
                  patElemName p
          )
          ps_space_and_res

  -- Now, for each mergee write, we need to check that it doesn't overlap with any previous uses of the destination.
  let checkMergeeOverlap bu_env_f (p, (m_b, _, ixf)) =
        let as = ixfunToAccessSummary ixf
         in -- Should be @bu_env@ here, because we need to check overlap
            -- against previous uses.
            case M.lookup m_b $ activeCoals bu_env of
              Just coal_entry -> do
                let mrefs =
                      memrefs coal_entry
                    res = noMemOverlap td_env as $ dstrefs mrefs
                    fail_res =
                      let (ac, inh) = markFailedCoal (activeCoals bu_env_f, inhibit bu_env_f) m_b
                       in bu_env_f {activeCoals = ac, inhibit = inh}

                if res
                  then case M.lookup (patElemName p) $ vartab coal_entry of
                    Nothing -> pure bu_env_f
                    Just (Coalesced knd mbd@(MemBlock _ _ _ ixfn) _) -> pure $
                      case freeVarSubstitutions (scope td_env) (scalarTable td_env) ixfn of
                        Just fv_subst ->
                          let entry =
                                coal_entry
                                  { vartab =
                                      M.insert
                                        (patElemName p)
                                        (Coalesced knd mbd fv_subst)
                                        (vartab coal_entry)
                                  }
                              (ac, suc) =
                                markSuccessCoal (activeCoals bu_env_f, successCoals bu_env_f) m_b entry
                           in bu_env_f {activeCoals = ac, successCoals = suc}
                        Nothing ->
                          fail_res
                  else pure fail_res
              _ -> pure bu_env_f

  foldM checkMergeeOverlap bu_env''' mergee_writes

-- | Given a pattern element and the corresponding kernel result, try to put the
-- kernel result directly in the memory block of pattern element
makeSegMapCoals ::
  (Coalesceable rep inner) =>
  (lvl -> Bool) ->
  lvl ->
  TopdownEnv rep ->
  KernelBody (Aliases rep) ->
  Certs ->
  (CoalsTab, InhibitTab) ->
  (PatElem (VarAliases, LetDecMem), SegSpace, KernelResult) ->
  (CoalsTab, InhibitTab)
makeSegMapCoals lvlOK lvl td_env kernel_body pat_certs (active, inhb) (PatElem pat_name (_, MemArray _ _ _ (ArrayIn pat_mem pat_ixf)), space, Returns _ _ (Var return_name))
  | Just (MemBlock tp return_shp return_mem _) <-
      getScopeMemInfo return_name $ scope td_env <> scopeOf (kernelBodyStms kernel_body),
    lvlOK lvl,
    MemMem pat_space <- runReader (lookupMemInfo pat_mem) $ removeScopeAliases $ scope td_env,
    MemMem return_space <-
      scope td_env <> scopeOf (kernelBodyStms kernel_body) <> scopeOfSegSpace space
        & removeScopeAliases
        & runReader (lookupMemInfo return_mem),
    pat_space == return_space =
      case M.lookup pat_mem active of
        Nothing ->
          -- We are not in a transitive case
          case ( maybe False (pat_mem `nameIn`) (M.lookup return_mem inhb),
                 Coalesced
                   InPlaceCoal
                   (MemBlock tp return_shp pat_mem $ resultSlice pat_ixf)
                   mempty
                   & M.singleton return_name
                   & flip (addInvAliasesVarTab td_env) return_name
               ) of
            (False, Just vtab) ->
              ( active
                  <> M.singleton
                    return_mem
                    (CoalsEntry pat_mem pat_ixf (oneName pat_mem) vtab mempty mempty pat_certs),
                inhb
              )
            _ -> (active, inhb)
        Just trans ->
          case ( maybe False (dstmem trans `nameIn`) $ M.lookup return_mem inhb,
                 let Coalesced _ (MemBlock _ _ trans_mem trans_ixf) _ =
                       fromMaybe (error "Impossible") $ M.lookup pat_name $ vartab trans
                  in Coalesced
                       TransitiveCoal
                       (MemBlock tp return_shp trans_mem $ resultSlice trans_ixf)
                       mempty
                       & M.singleton return_name
                       & flip (addInvAliasesVarTab td_env) return_name
               ) of
            (False, Just vtab) ->
              let opts =
                    if dstmem trans == pat_mem
                      then mempty
                      else M.insert pat_name pat_mem $ optdeps trans
               in ( M.insert
                      return_mem
                      ( CoalsEntry
                          (dstmem trans)
                          (dstind trans)
                          (oneName pat_mem <> alsmem trans)
                          vtab
                          opts
                          mempty
                          (certs trans <> pat_certs)
                      )
                      active,
                    inhb
                  )
            _ -> (active, inhb)
  where
    thread_slice =
      unSegSpace space
        & map (DimFix . TPrimExp . flip LeafExp (IntType Int64) . fst)
        & Slice
    resultSlice ixf = LMAD.slice ixf $ fullSlice (LMAD.shape ixf) thread_slice
makeSegMapCoals _ _ td_env _ _ x (_, _, WriteReturns _ return_name _) =
  case getScopeMemInfo return_name $ scope td_env of
    Just (MemBlock _ _ return_mem _) -> markFailedCoal x return_mem
    Nothing -> error "Should not happen?"
makeSegMapCoals _ _ td_env _ _ x (_, _, result) =
  freeIn result
    & namesToList
    & mapMaybe (flip getScopeMemInfo $ scope td_env)
    & foldr (flip markFailedCoal . memName) x

fullSlice :: [TPrimExp Int64 VName] -> Slice (TPrimExp Int64 VName) -> Slice (TPrimExp Int64 VName)
fullSlice shp (Slice slc) =
  Slice $ slc ++ map (\d -> DimSlice 0 d 1) (drop (length slc) shp)

fixPointCoalesce ::
  (Coalesceable rep inner) =>
  LUTabFun ->
  [Param FParamMem] ->
  Body (Aliases rep) ->
  TopdownEnv rep ->
  ShortCircuitM rep CoalsTab
fixPointCoalesce lutab fpar bdy topenv = do
  buenv <- mkCoalsTabStms lutab (bodyStms bdy) topenv (emptyBotUpEnv {inhibit = inhibited topenv})
  let succ_tab = successCoals buenv
      actv_tab = activeCoals buenv
      inhb_tab = inhibit buenv
      -- Allow short-circuiting function parameters that are unique and have
      -- matching index functions, otherwise mark as failed
      handleFunctionParams (a, i, s) (_, u, MemBlock _ _ m ixf) =
        case (u, M.lookup m a) of
          (Unique, Just entry)
            | dstind entry == ixf,
              Set dst_uses <- dstrefs (memrefs entry),
              dst_uses == mempty ->
                let (a', s') = markSuccessCoal (a, s) m entry
                 in (a', i, s')
          _ ->
            let (a', i') = markFailedCoal (a, i) m
             in (a', i', s)
      (actv_tab', inhb_tab', succ_tab') =
        foldl
          handleFunctionParams
          (actv_tab, inhb_tab, succ_tab)
          $ getArrMemAssocFParam fpar

      (succ_tab'', failed_optdeps) = fixPointFilterDeps succ_tab' M.empty
      inhb_tab'' = M.unionWith (<>) failed_optdeps inhb_tab'
  if not $ M.null actv_tab'
    then error ("COALESCING ROOT: BROKEN INV, active not empty: " ++ show (M.keys actv_tab'))
    else
      if M.null $ inhb_tab'' `M.difference` inhibited topenv
        then pure succ_tab''
        else fixPointCoalesce lutab fpar bdy (topenv {inhibited = inhb_tab''})
  where
    fixPointFilterDeps :: CoalsTab -> InhibitTab -> (CoalsTab, InhibitTab)
    fixPointFilterDeps coaltab inhbtab =
      let (coaltab', inhbtab') = foldl filterDeps (coaltab, inhbtab) (M.keys coaltab)
       in if length (M.keys coaltab) == length (M.keys coaltab')
            then (coaltab', inhbtab')
            else fixPointFilterDeps coaltab' inhbtab'

    filterDeps (coal, inhb) mb
      | not (M.member mb coal) = (coal, inhb)
    filterDeps (coal, inhb) mb
      | Just coal_etry <- M.lookup mb coal =
          let failed = M.filterWithKey (failedOptDep coal) (optdeps coal_etry)
           in if M.null failed
                then (coal, inhb) -- all ok
                else -- optimistic dependencies failed for the current
                -- memblock; extend inhibited mem-block mergings.
                  markFailedCoal (coal, inhb) mb
    filterDeps _ _ = error "In ArrayCoalescing.hs, fun filterDeps, impossible case reached!"
    failedOptDep coal _ mr
      | not (mr `M.member` coal) = True
    failedOptDep coal r mr
      | Just coal_etry <- M.lookup mr coal = not $ r `M.member` vartab coal_etry
    failedOptDep _ _ _ = error "In ArrayCoalescing.hs, fun failedOptDep, impossible case reached!"

-- | Perform short-circuiting on 'Stms'.
mkCoalsTabStms ::
  (Coalesceable rep inner) =>
  LUTabFun ->
  Stms (Aliases rep) ->
  TopdownEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep BotUpEnv
mkCoalsTabStms lutab stms0 = traverseStms stms0
  where
    non_negs_in_pats = foldMap (nonNegativesInPat . stmPat) stms0
    traverseStms Empty _ bu_env = pure bu_env
    traverseStms (stm :<| stms) td_env bu_env = do
      -- Compute @td_env@ top down
      let td_env' = updateTopdownEnv td_env stm
      -- Compute @bu_env@ bottom up
      bu_env' <- traverseStms stms td_env' bu_env
      mkCoalsTabStm lutab stm (td_env' {nonNegatives = nonNegatives td_env' <> non_negs_in_pats}) bu_env'

-- | Array (register) coalescing can have one of three shapes:
--      a) @let y    = copy(b^{lu})@
--      b) @let y    = concat(a, b^{lu})@
--      c) @let y[i] = b^{lu}@
--   The intent is to use the memory block of the left-hand side
--     for the right-hand side variable, meaning to store @b@ in
--     @m_y@ (rather than @m_b@).
--   The following five safety conditions are necessary:
--      1. the right-hand side is lastly-used in the current statement
--      2. the allocation of @m_y@ dominates the creation of @b@
--         ^ relax it by hoisting the allocation of @m_y@
--      3. there is no use of the left-hand side memory block @m_y@
--           during the liveness of @b@, i.e., in between its last use
--           and its creation.
--         ^ relax it by pointwise/interval-based checking
--      4. @b@ is a newly created array, i.e., does not aliases anything
--         ^ relax it to support exitential memory blocks for if-then-else
--      5. the new index function of @b@ corresponding to memory block @m_y@
--           can be translated at the definition of @b@, and the
--           same for all variables aliasing @b@.
--   Observation: during the live range of @b@, @m_b@ can only be used by
--                variables aliased with @b@, because @b@ is newly created.
--                relax it: in case @m_b@ is existential due to an if-then-else
--                          then the checks should be extended to the actual
--                          array-creation points.
mkCoalsTabStm ::
  (Coalesceable rep inner) =>
  LUTabFun ->
  Stm (Aliases rep) ->
  TopdownEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep BotUpEnv
mkCoalsTabStm _ (Let (Pat [pe]) _ e) td_env bu_env
  | Just primexp <- primExpFromExp (vnameToPrimExp (scope td_env) (scals bu_env)) e =
      pure $ bu_env {scals = M.insert (patElemName pe) primexp (scals bu_env)}
mkCoalsTabStm lutab (Let patt _ (Match _ cases defbody _)) td_env bu_env = do
  let pat_val_elms = patElems patt
      -- ToDo: 1. we need to record existential memory blocks in alias table on the top-down pass.
      --       2. need to extend the scope table

      --  i) Filter @activeCoals@ by the 2ND AND 5th safety conditions:
      (activeCoals0, inhibit0) =
        filterSafetyCond2and5
          (activeCoals bu_env)
          (inhibit bu_env)
          (scals bu_env)
          td_env
          pat_val_elms

      -- ii) extend @activeCoals@ by transfering the pattern-elements bindings existent
      --     in @activeCoals@ to the body results of the then and else branches, but only
      --     if the current pattern element can be potentially coalesced and also
      --     if the current pattern element satisfies safety conditions 2 & 5.
      res_mem_def = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms defbody
      res_mem_cases = map (findMemBodyResult activeCoals0 (scope td_env) pat_val_elms . caseBody) cases

      subs_def = mkSubsTab patt $ map resSubExp $ bodyResult defbody
      subs_cases = map (mkSubsTab patt . map resSubExp . bodyResult . caseBody) cases

      actv_def_i = foldl (transferCoalsToBody subs_def) activeCoals0 res_mem_def
      actv_cases_i = zipWith (\subs res -> foldl (transferCoalsToBody subs) activeCoals0 res) subs_cases res_mem_cases

      -- eliminate the original pattern binding of the if statement,
      -- @let x = if y[0,0] > 0 then map (+y[0,0]) a else map (+1) b@
      -- @let y[0] = x@
      -- should succeed because @m_y@ is used before @x@ is created.
      aux ac (MemBodyResult m_b _ _ m_r) = if m_b == m_r then ac else M.delete m_b ac
      actv_def = foldl aux actv_def_i res_mem_def
      actv_cases = zipWith (foldl aux) actv_cases_i res_mem_cases

  -- iii) process the then and else bodies
  res_def <- mkCoalsTabStms lutab (bodyStms defbody) td_env (bu_env {activeCoals = actv_def})
  res_cases <- zipWithM (\c a -> mkCoalsTabStms lutab (bodyStms $ caseBody c) td_env (bu_env {activeCoals = a})) cases actv_cases
  let (actv_def0, succ_def0, inhb_def0) = (activeCoals res_def, successCoals res_def, inhibit res_def)

      -- iv) optimistically mark the pattern succesful:
      ((activeCoals1, inhibit1), successCoals1) =
        foldl
          ( foldfun
              ( (actv_def0, succ_def0)
                  : zip (map activeCoals res_cases) (map successCoals res_cases)
              )
          )
          ((activeCoals0, inhibit0), successCoals bu_env)
          (L.transpose $ res_mem_def : res_mem_cases)

      --  v) unify coalescing results of all branches by taking the union
      --     of all entries in the current/then/else success tables.

      actv_res = foldr (M.intersectionWith unionCoalsEntry) activeCoals1 $ actv_def0 : map activeCoals res_cases

      succ_res = foldr (M.unionWith unionCoalsEntry) successCoals1 $ succ_def0 : map successCoals res_cases

      -- vi) The step of filtering by 3rd safety condition is not
      --       necessary, because we perform index analysis of the
      --       source/destination uses, and they should have been
      --       filtered during the analysis of the then/else bodies.
      inhibit_res =
        M.unionsWith
          (<>)
          ( inhibit1
              : zipWith
                ( \actv inhb ->
                    let failed = M.difference actv $ M.intersectionWith unionCoalsEntry actv activeCoals0
                     in snd $ foldl markFailedCoal (failed, inhb) (M.keys failed)
                )
                (actv_def0 : map activeCoals res_cases)
                (inhb_def0 : map inhibit res_cases)
          )
  pure
    bu_env
      { activeCoals = actv_res,
        successCoals = succ_res,
        inhibit = inhibit_res
      }
  where
    foldfun _ _ [] =
      error "Imposible Case 1!!!"
    foldfun _ ((act, _), _) mem_body_results
      | Nothing <- M.lookup (patMem $ head mem_body_results) act =
          error "Imposible Case 2!!!"
    foldfun
      acc
      ((act, inhb), succc)
      mem_body_results@(MemBodyResult m_b _ _ _ : _)
        | Just info <- M.lookup m_b act,
          Just _ <- zipWithM (M.lookup . bodyMem) mem_body_results $ map snd acc =
            -- Optimistically promote to successful coalescing and append!
            let info' =
                  info
                    { optdeps =
                        foldr
                          (\mbr -> M.insert (bodyName mbr) (bodyMem mbr))
                          (optdeps info)
                          mem_body_results
                    }
                (act', succc') = markSuccessCoal (act, succc) m_b info'
             in ((act', inhb), succc')
    foldfun
      acc
      ((act, inhb), succc)
      mem_body_results@(MemBodyResult m_b _ _ _ : _)
        | Just info <- M.lookup m_b act,
          all ((==) m_b . bodyMem) mem_body_results,
          Just info' <- zipWithM (M.lookup . bodyMem) mem_body_results $ map fst acc =
            -- Treating special case resembling:
            -- @let x0 = map (+1) a                                  @
            -- @let x3 = if cond then let x1 = x0 with [0] <- 2 in x1@
            -- @                 else let x2 = x0 with [1] <- 3 in x2@
            -- @let z[1] = x3                                        @
            -- In this case the result active table should be the union
            -- of the @m_x@ entries of the then and else active tables.
            let info'' =
                  foldl unionCoalsEntry info info'
                act' = M.insert m_b info'' act
             in ((act', inhb), succc)
    foldfun _ ((act, inhb), succc) (mbr : _) =
      -- one of the branches has failed coalescing,
      -- hence remove the coalescing of the result.

      (markFailedCoal (act, inhb) (patMem mbr), succc)
mkCoalsTabStm lutab (Let pat _ (Loop arginis lform body)) td_env bu_env = do
  let pat_val_elms = patElems pat

      --  i) Filter @activeCoals@ by the 2nd, 3rd AND 5th safety conditions. In
      --  other words, for each active coalescing target, the creation of the
      --  array we're trying to merge should happen before the allocation of the
      --  merge target and the index function should be translateable.
      (actv0, inhibit0) =
        filterSafetyCond2and5
          (activeCoals bu_env)
          (inhibit bu_env)
          (scals bu_env)
          td_env
          pat_val_elms
      -- ii) Extend @activeCoals@ by transfering the pattern-elements bindings
      --     existent in @activeCoals@ to the loop-body results, but only if:
      --       (a) the pattern element is a candidate for coalescing,        &&
      --       (b) the pattern element satisfies safety conditions 2 & 5,
      --           (conditions (a) and (b) have already been checked above), &&
      --       (c) the memory block of the corresponding body result is
      --           allocated outside the loop, i.e., non-existential,        &&
      --       (d) the init name is lastly-used in the initialization
      --           of the loop variant.
      --     Otherwise fail and remove from active-coalescing table!
      bdy_ress = bodyResult body
      (patmems, argmems, inimems, resmems) =
        L.unzip4 $
          mapMaybe (mapmbFun actv0) (zip3 pat_val_elms arginis $ map resSubExp bdy_ress) -- td_env'

      -- remove the other pattern elements from the active coalescing table:
      coal_pat_names = namesFromList $ map fst patmems
      (actv1, inhibit1) =
        foldl
          ( \(act, inhb) (b, MemBlock _ _ m_b _) ->
              if b `nameIn` coal_pat_names
                then (act, inhb) -- ok
                else markFailedCoal (act, inhb) m_b -- remove from active
          )
          (actv0, inhibit0)
          (getArrMemAssoc pat)

      -- iii) Process the loop's body.
      --      If the memory blocks of the loop result and loop variant param differ
      --      then make the original memory block of the loop result conflict with
      --      the original memory block of the loop parameter. This is done in
      --      order to prevent the coalescing of @a1@, @a0@, @x@ and @db@ in the
      --      same memory block of @y@ in the example below:
      --      @loop(a1 = a0) = for i < n do @
      --      @    let x = map (stencil a1) (iota n)@
      --      @    let db = copy x          @
      --      @    in db                    @
      --      @let y[0] = a1                @
      --      Meaning the coalescing of @x@ in @let db = copy x@ should fail because
      --      @a1@ appears in the definition of @let x = map (stencil a1) (iota n)@.
      res_mem_bdy = zipWith (\(b, m_b) (r, m_r) -> MemBodyResult m_b b r m_r) patmems resmems
      res_mem_arg = zipWith (\(b, m_b) (r, m_r) -> MemBodyResult m_b b r m_r) patmems argmems
      res_mem_ini = zipWith (\(b, m_b) (r, m_r) -> MemBodyResult m_b b r m_r) patmems inimems

      actv2 =
        let subs_res = mkSubsTab pat $ map resSubExp $ bodyResult body
            actv11 = foldl (transferCoalsToBody subs_res) actv1 res_mem_bdy
            subs_arg = mkSubsTab pat $ map (Var . paramName . fst) arginis
            actv12 = foldl (transferCoalsToBody subs_arg) actv11 res_mem_arg
            subs_ini = mkSubsTab pat $ map snd arginis
         in foldl (transferCoalsToBody subs_ini) actv12 res_mem_ini

      -- The code below adds an aliasing relation to the loop-arg memory
      --   so that to prevent, e.g., the coalescing of an iterative stencil
      --   (you need a buffer for the result and a separate one for the stencil).
      -- @ let b =               @
      -- @    loop (a) for i<N do@
      -- @        stencil a      @
      -- @  ...                  @
      -- @  y[slc_y] = b         @
      -- This should fail coalescing because we are aliasing @m_a@ with
      --   the memory block of the result.
      insertMemAliases tab (MemBodyResult _ _ _ m_r, MemBodyResult _ _ _ m_a) =
        if m_r == m_a
          then tab
          else case M.lookup m_r tab of
            Nothing -> tab
            Just etry ->
              M.insert m_r (etry {alsmem = alsmem etry <> oneName m_a}) tab
      actv3 = foldl insertMemAliases actv2 (zip res_mem_bdy res_mem_arg)
      -- analysing the loop body starts from a null memory-reference set;
      --  the results of the loop body iteration are aggregated later
      actv4 = M.map (\etry -> etry {memrefs = mempty}) actv3
  res_env_body <-
    mkCoalsTabStms
      lutab
      (bodyStms body)
      td_env'
      ( bu_env
          { activeCoals = actv4,
            inhibit = inhibit1
          }
      )
  let scals_loop = scals res_env_body
      (res_actv0, res_succ0, res_inhb0) = (activeCoals res_env_body, successCoals res_env_body, inhibit res_env_body)
      -- iv) Aggregate memory references across loop and filter unsound coalescing
      -- a) Filter the active-table by the FIRST SOUNDNESS condition, namely:
      --     W_i does not overlap with Union_{j=i+1..n} U_j,
      --     where W_i corresponds to the Write set of src mem-block m_b,
      --     and U_j correspond to the uses of the destination
      --     mem-block m_y, in which m_b is coalesced into.
      --     W_i and U_j correspond to the accesses within the loop body.
      mb_loop_idx = mbLoopIndexRange lform
  res_actv1 <- filterMapM1 (loopSoundness1Entry scals_loop mb_loop_idx) res_actv0

  -- b) Update the memory-reference summaries across loop:
  --   W = Union_{i=0..n-1} W_i Union W_{before-loop}
  --   U = Union_{i=0..n-1} U_i Union U_{before-loop}
  res_actv2 <- mapM (aggAcrossLoopEntry (scope td_env' <> scopeOf (bodyStms body)) scals_loop mb_loop_idx) res_actv1

  -- c) check soundness of the successful promotions for:
  --      - the entries that have been promoted to success during the loop-body pass
  --      - for all the entries of active table
  --    Filter the entries by the SECOND SOUNDNESS CONDITION, namely:
  --      Union_{i=1..n-1} W_i does not overlap the before-the-loop uses
  --        of the destination memory block.
  let res_actv3 = M.filterWithKey (loopSoundness2Entry actv3) res_actv2

  let tmp_succ =
        M.filterWithKey (okLookup actv3) $
          M.difference res_succ0 (successCoals bu_env)
      ver_succ = M.filterWithKey (loopSoundness2Entry actv3) tmp_succ
  let suc_fail = M.difference tmp_succ ver_succ
      (res_succ, res_inhb1) = foldl markFailedCoal (res_succ0, res_inhb0) $ M.keys suc_fail
      --
      act_fail = M.difference res_actv0 res_actv3
      (_, res_inhb) = foldl markFailedCoal (res_actv0, res_inhb1) $ M.keys act_fail
      res_actv =
        M.mapWithKey (addBeforeLoop actv3) res_actv3

      -- v) optimistically mark the pattern succesful if there is any chance to succeed
      ((fin_actv1, fin_inhb1), fin_succ1) =
        foldl foldFunOptimPromotion ((res_actv, res_inhb), res_succ) $
          L.zip4 patmems argmems resmems inimems
      (fin_actv2, fin_inhb2) =
        M.foldlWithKey
          ( \acc k _ ->
              if k `nameIn` namesFromList (map (paramName . fst) arginis)
                then markFailedCoal acc k
                else acc
          )
          (fin_actv1, fin_inhb1)
          fin_actv1
  pure bu_env {activeCoals = fin_actv2, successCoals = fin_succ1, inhibit = fin_inhb2}
  where
    allocs_bdy = foldl getAllocs (alloc td_env') $ bodyStms body
    td_env_allocs = td_env' {alloc = allocs_bdy, scope = scope td_env' <> scopeOf (bodyStms body)}
    td_env' = updateTopdownEnvLoop td_env arginis lform
    getAllocs tab (Let (Pat [pe]) _ (Op (Alloc _ sp))) =
      M.insert (patElemName pe) sp tab
    getAllocs tab _ = tab
    okLookup tab m _
      | Just _ <- M.lookup m tab = True
    okLookup _ _ _ = False
    --
    mapmbFun actv0 (patel, (arg, ini), bdyres)
      | b <- patElemName patel,
        (_, MemArray _ _ _ (ArrayIn m_b _)) <- patElemDec patel,
        a <- paramName arg,
        -- Not safe to short-circuit if the index function of this
        -- parameter is variant to the loop.
        not $ any ((`nameIn` freeIn (paramDec arg)) . paramName . fst) arginis,
        Var a0 <- ini,
        Var r <- bdyres,
        Just coal_etry <- M.lookup m_b actv0,
        Just _ <- M.lookup b (vartab coal_etry),
        Just (MemBlock _ _ m_a _) <- getScopeMemInfo a (scope td_env_allocs),
        Just (MemBlock _ _ m_a0 _) <- getScopeMemInfo a0 (scope td_env_allocs),
        Just (MemBlock _ _ m_r _) <- getScopeMemInfo r (scope td_env_allocs),
        Just nms <- M.lookup a lutab,
        a0 `nameIn` nms,
        m_r `elem` M.keys (alloc td_env_allocs) =
          Just ((b, m_b), (a, m_a), (a0, m_a0), (r, m_r))
    mapmbFun _ (_patel, (_arg, _ini), _bdyres) = Nothing
    foldFunOptimPromotion ::
      ((CoalsTab, InhibitTab), CoalsTab) ->
      ((VName, VName), (VName, VName), (VName, VName), (VName, VName)) ->
      ((CoalsTab, InhibitTab), CoalsTab)
    foldFunOptimPromotion ((act, inhb), succc) ((b, m_b), (a, m_a), (_r, m_r), (b_i, m_i))
      | m_r == m_i,
        Just info <- M.lookup m_i act,
        Just vtab_i <- addInvAliasesVarTab td_env (vartab info) b_i =
          Exc.assert
            (m_r == m_b && m_a == m_b)
            ((M.insert m_b (info {vartab = vtab_i}) act, inhb), succc)
      | m_r == m_i =
          Exc.assert
            (m_r == m_b && m_a == m_b)
            (markFailedCoal (act, inhb) m_b, succc)
      | Just info_b0 <- M.lookup m_b act,
        Just info_a0 <- M.lookup m_a act,
        Just info_i <- M.lookup m_i act,
        M.member m_r succc,
        Just vtab_i <- addInvAliasesVarTab td_env (vartab info_i) b_i,
        [Just info_b, Just info_a] <- map translateIxFnInScope [(b, info_b0), (a, info_a0)] =
          let info_b' = info_b {optdeps = M.insert b_i m_i $ optdeps info_b}
              info_a' = info_a {optdeps = M.insert b_i m_i $ optdeps info_a}
              info_i' =
                info_i
                  { optdeps = M.insert b m_b $ optdeps info_i,
                    memrefs = mempty,
                    vartab = vtab_i
                  }
              act' = M.insert m_i info_i' act
              (act1, succc1) =
                foldl
                  (\acc (m, info) -> markSuccessCoal acc m info)
                  (act', succc)
                  [(m_b, info_b'), (m_a, info_a')]
           in -- ToDo: make sure that ixfun translates and update substitutions (?)
              ((act1, inhb), succc1)
    foldFunOptimPromotion ((act, inhb), succc) ((_, m_b), (_a, m_a), (_r, m_r), (_b_i, m_i)) =
      Exc.assert
        (m_r /= m_i)
        (foldl markFailedCoal (act, inhb) [m_b, m_a, m_r, m_i], succc)

    translateIxFnInScope (x, info)
      | Just (Coalesced knd mbd@(MemBlock _ _ _ ixfn) _subs0) <- M.lookup x (vartab info),
        isInScope td_env (dstmem info) =
          let scope_tab =
                scope td_env
                  <> scopeOfFParams (map fst arginis)
           in case freeVarSubstitutions scope_tab (scals bu_env) ixfn of
                Just fv_subst ->
                  Just $ info {vartab = M.insert x (Coalesced knd mbd fv_subst) (vartab info)}
                Nothing -> Nothing
    translateIxFnInScope _ = Nothing
    se0 = intConst Int64 0
    mbLoopIndexRange ::
      LoopForm ->
      Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName))
    mbLoopIndexRange (WhileLoop _) = Nothing
    mbLoopIndexRange (ForLoop inm _inttp seN) = Just (inm, (pe64 se0, pe64 seN))
    addBeforeLoop actv_bef m_b etry =
      case M.lookup m_b actv_bef of
        Nothing -> etry
        Just etry0 ->
          etry {memrefs = memrefs etry0 <> memrefs etry}
    aggAcrossLoopEntry scope_loop scal_tab idx etry = do
      wrts <-
        aggSummaryLoopTotal (scope td_env) scope_loop scal_tab idx $
          (srcwrts . memrefs) etry
      uses <-
        aggSummaryLoopTotal (scope td_env) scope_loop scal_tab idx $
          (dstrefs . memrefs) etry
      pure $ etry {memrefs = MemRefs uses wrts}
    loopSoundness1Entry scal_tab idx etry = do
      let wrt_i = (srcwrts . memrefs) etry
      use_p <-
        aggSummaryLoopPartial (scal_tab <> scalarTable td_env) idx $
          dstrefs $
            memrefs etry
      pure $ noMemOverlap td_env' wrt_i use_p
    loopSoundness2Entry :: CoalsTab -> VName -> CoalsEntry -> Bool
    loopSoundness2Entry old_actv m_b etry =
      case M.lookup m_b old_actv of
        Nothing -> True
        Just etry0 ->
          let uses_before = (dstrefs . memrefs) etry0
              write_loop = (srcwrts . memrefs) etry
           in noMemOverlap td_env write_loop uses_before

-- The case of in-place update:
--   @let x' = x with slice <- elm@
mkCoalsTabStm lutab stm@(Let pat@(Pat [x']) _ (BasicOp (Update safety x _ _elm))) td_env bu_env
  | [(_, MemBlock _ _ m_x _)] <- getArrMemAssoc pat = do
      -- (a) filter by the 3rd safety for @elm@ and @x'@
      let (actv, inhbt) = recordMemRefUses td_env bu_env stm
          -- (b) if @x'@ is in active coalesced table, then add an entry for @x@ as well
          (actv', inhbt') =
            case M.lookup m_x actv of
              Nothing -> (actv, inhbt)
              Just info ->
                case M.lookup (patElemName x') (vartab info) of
                  Nothing -> markFailedCoal (actv, inhbt) m_x
                  Just (Coalesced k mblk@(MemBlock _ _ _ x_indfun) _) ->
                    case freeVarSubstitutions (scope td_env) (scals bu_env) x_indfun of
                      Just fv_subs
                        | isInScope td_env (dstmem info) ->
                            let coal_etry_x = Coalesced k mblk fv_subs
                                info' =
                                  info
                                    { vartab =
                                        M.insert x coal_etry_x $
                                          M.insert (patElemName x') coal_etry_x (vartab info)
                                    }
                             in (M.insert m_x info' actv, inhbt)
                      _ ->
                        markFailedCoal (actv, inhbt) m_x

      -- (c) this stm is also a potential source for coalescing, so process it
      actv'' <-
        if safety == Unsafe
          then mkCoalsHelper3PatternMatch stm lutab td_env {inhibited = inhbt'} bu_env {activeCoals = actv'}
          else pure actv'
      pure $ bu_env {activeCoals = actv'', inhibit = inhbt'}

-- The case of flat in-place update:
--   @let x' = x with flat-slice <- elm@
mkCoalsTabStm lutab stm@(Let pat@(Pat [x']) _ (BasicOp (FlatUpdate x _ _elm))) td_env bu_env
  | [(_, MemBlock _ _ m_x _)] <- getArrMemAssoc pat = do
      -- (a) filter by the 3rd safety for @elm@ and @x'@
      let (actv, inhbt) = recordMemRefUses td_env bu_env stm
          -- (b) if @x'@ is in active coalesced table, then add an entry for @x@ as well
          (actv', inhbt') =
            case M.lookup m_x actv of
              Nothing -> (actv, inhbt)
              Just info ->
                case M.lookup (patElemName x') (vartab info) of
                  -- this case should not happen, but if it can that
                  -- just fail conservatively
                  Nothing -> markFailedCoal (actv, inhbt) m_x
                  Just (Coalesced k mblk@(MemBlock _ _ _ x_indfun) _) ->
                    case freeVarSubstitutions (scope td_env) (scals bu_env) x_indfun of
                      Just fv_subs
                        | isInScope td_env (dstmem info) ->
                            let coal_etry_x = Coalesced k mblk fv_subs
                                info' =
                                  info
                                    { vartab =
                                        M.insert x coal_etry_x $
                                          M.insert (patElemName x') coal_etry_x (vartab info)
                                    }
                             in (M.insert m_x info' actv, inhbt)
                      _ ->
                        markFailedCoal (actv, inhbt) m_x

      -- (c) this stm is also a potential source for coalescing, so process it
      actv'' <- mkCoalsHelper3PatternMatch stm lutab td_env {inhibited = inhbt'} bu_env {activeCoals = actv'}
      pure $ bu_env {activeCoals = actv'', inhibit = inhbt'}
--
mkCoalsTabStm _ (Let pat _ (BasicOp Update {})) _ _ =
  error $ "In ArrayCoalescing.hs, fun mkCoalsTabStm, illegal pattern for in-place update: " ++ show pat
-- default handling
mkCoalsTabStm lutab stm@(Let pat aux (Op op)) td_env bu_env = do
  -- Process body
  on_op <- asks onOp
  bu_env' <- on_op lutab pat (stmAuxCerts aux) op td_env bu_env
  activeCoals' <- mkCoalsHelper3PatternMatch stm lutab td_env bu_env'
  pure $ bu_env' {activeCoals = activeCoals'}
mkCoalsTabStm lutab stm@(Let pat _ e) td_env bu_env = do
  --   i) Filter @activeCoals@ by the 3rd safety condition:
  --      this is now relaxed by use of LMAD eqs:
  --      the memory referenced in stm are added to memrefs::dstrefs
  --      in corresponding coal-tab entries.
  let (activeCoals', inhibit') = recordMemRefUses td_env bu_env stm

      --  ii) promote any of the entries in @activeCoals@ to @successCoals@ as long as
      --        - this statement defined a variable consumed in a coalesced statement
      --        - and safety conditions 2, 4, and 5 are satisfied.
      --      AND extend @activeCoals@ table for any definition of a variable that
      --      aliases a coalesced variable.
      safe_4 = createsNewArrOK e
      ((activeCoals'', inhibit''), successCoals') =
        foldl (foldfun safe_4) ((activeCoals', inhibit'), successCoals bu_env) (getArrMemAssoc pat)

  -- iii) record a potentially coalesced statement in @activeCoals@
  activeCoals''' <- mkCoalsHelper3PatternMatch stm lutab td_env bu_env {successCoals = successCoals', activeCoals = activeCoals''}
  pure bu_env {activeCoals = activeCoals''', inhibit = inhibit'', successCoals = successCoals'}
  where
    foldfun safe_4 ((a_acc, inhb), s_acc) (b, MemBlock tp shp mb _b_indfun) =
      case M.lookup mb a_acc of
        Nothing -> ((a_acc, inhb), s_acc)
        Just info@(CoalsEntry x_mem _ _ vtab _ _ certs) ->
          let failed = markFailedCoal (a_acc, inhb) mb
           in case M.lookup b vtab of
                Nothing ->
                  -- we hit the definition of some variable @b@ aliased with
                  --    the coalesced variable @x@, hence extend @activeCoals@, e.g.,
                  --       @let x = map f arr  @
                  --       @let b = alias x  @ <- current statement
                  --       @ ... use of b ...  @
                  --       @let c = alias b    @ <- currently fails
                  --       @let y[i] = x       @
                  -- where @alias@ can be @transpose@, @slice@, @reshape@.
                  -- We use getTransitiveAlias helper function to track the aliasing
                  --    through the td_env, and to find the updated ixfun of @b@:
                  case getDirAliasedIxfn td_env a_acc b of
                    Nothing -> (failed, s_acc)
                    Just (_, _, b_indfun') ->
                      case ( freeVarSubstitutions (scope td_env) (scals bu_env) b_indfun',
                             freeVarSubstitutions (scope td_env) (scals bu_env) certs
                           ) of
                        (Just fv_subst, Just fv_subst') ->
                          let mem_info = Coalesced TransitiveCoal (MemBlock tp shp x_mem b_indfun') (fv_subst <> fv_subst')
                              info' = info {vartab = M.insert b mem_info vtab}
                           in ((M.insert mb info' a_acc, inhb), s_acc)
                        _ -> (failed, s_acc)
                Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                  -- we are at the definition of the coalesced variable @b@
                  -- if 2,4,5 hold promote it to successful coalesced table,
                  -- or if e = transpose, etc. then postpone decision for later on
                  let safe_2 = isInScope td_env x_mem
                   in case ( freeVarSubstitutions (scope td_env) (scals bu_env) new_indfun,
                             freeVarSubstitutions (scope td_env) (scals bu_env) certs
                           ) of
                        (Just fv_subst, Just fv_subst')
                          | safe_2 ->
                              let mem_info = Coalesced k mblk (fv_subst <> fv_subst')
                                  info' = info {vartab = M.insert b mem_info vtab}
                               in if safe_4
                                    then -- array creation point, successful coalescing verified!

                                      let (a_acc', s_acc') = markSuccessCoal (a_acc, s_acc) mb info'
                                       in ((a_acc', inhb), s_acc')
                                    else -- this is an invertible alias case of the kind
                                    -- @ let b    = alias a @
                                    -- @ let x[i] = b @
                                    -- do not promote, but update the index function

                                      ((M.insert mb info' a_acc, inhb), s_acc)
                        _ -> (failed, s_acc) -- fail!

ixfunToAccessSummary :: LMAD.LMAD (TPrimExp Int64 VName) -> AccessSummary
ixfunToAccessSummary = Set . S.singleton

-- | Check safety conditions 2 and 5 and update new substitutions:
-- called on the pat-elements of loop and if-then-else expressions.
--
-- The safety conditions are: The allocation of merge target should dominate the
-- creation of the array we're trying to merge and the new index function of the
-- array can be translated at the definition site of b. The latter requires that
-- any variables used in the index function of the target array are available at
-- the definition site of b.
filterSafetyCond2and5 ::
  (HasMemBlock (Aliases rep)) =>
  CoalsTab ->
  InhibitTab ->
  ScalarTab ->
  TopdownEnv rep ->
  [PatElem (VarAliases, LetDecMem)] ->
  (CoalsTab, InhibitTab)
filterSafetyCond2and5 act_coal inhb_coal scals_env td_env pes =
  foldl helper (act_coal, inhb_coal) pes
  where
    helper (acc, inhb) patel = do
      -- For each pattern element in the input list
      case (patElemName patel, patElemDec patel) of
        (b, (_, MemArray tp0 shp0 _ (ArrayIn m_b _idxfn_b))) ->
          -- If it is an array in memory block m_b
          case M.lookup m_b acc of
            Nothing -> (acc, inhb)
            Just info@(CoalsEntry x_mem _ _ vtab _ _ certs) ->
              -- And m_b we're trying to coalesce m_b
              let failed = markFailedCoal (acc, inhb) m_b
               in -- It is not safe to short circuit if some other pattern
                  -- element is aliased to this one, as this indicates the
                  -- two pattern elements reference the same physical
                  -- value somehow.
                  if any ((`nameIn` aliasesOf patel) . patElemName) pes
                    then failed
                    else case M.lookup b vtab of
                      Nothing ->
                        case getDirAliasedIxfn td_env acc b of
                          Nothing -> failed
                          Just (_, _, b_indfun') ->
                            -- And we have the index function of b
                            case ( freeVarSubstitutions (scope td_env) scals_env b_indfun',
                                   freeVarSubstitutions (scope td_env) scals_env certs
                                 ) of
                              (Just fv_subst, Just fv_subst') ->
                                let mem_info = Coalesced TransitiveCoal (MemBlock tp0 shp0 x_mem b_indfun') (fv_subst <> fv_subst')
                                    info' = info {vartab = M.insert b mem_info vtab}
                                 in (M.insert m_b info' acc, inhb)
                              _ -> failed
                      Just (Coalesced k (MemBlock pt shp _ new_indfun) _) ->
                        let safe_2 = isInScope td_env x_mem
                         in case ( freeVarSubstitutions (scope td_env) scals_env new_indfun,
                                   freeVarSubstitutions (scope td_env) scals_env certs
                                 ) of
                              (Just fv_subst, Just fv_subst')
                                | safe_2 ->
                                    let mem_info = Coalesced k (MemBlock pt shp x_mem new_indfun) (fv_subst <> fv_subst')
                                        info' = info {vartab = M.insert b mem_info vtab}
                                     in (M.insert m_b info' acc, inhb)
                              _ -> failed
        _ -> (acc, inhb)

-- |   Pattern matches a potentially coalesced statement and
--     records a new association in @activeCoals@
mkCoalsHelper3PatternMatch ::
  (Coalesceable rep inner) =>
  Stm (Aliases rep) ->
  LUTabFun ->
  TopdownEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep CoalsTab
mkCoalsHelper3PatternMatch stm lutab td_env bu_env = do
  clst <- genCoalStmtInfo lutab td_env (scope td_env) stm
  case clst of
    Nothing -> pure activeCoals_tab
    Just clst' -> pure $ foldl processNewCoalesce activeCoals_tab clst'
  where
    successCoals_tab = successCoals bu_env
    activeCoals_tab = activeCoals bu_env
    processNewCoalesce acc (knd, alias_fn, x, m_x, ind_x, b, m_b, _, tp_b, shp_b, certs) =
      -- test whether we are in a transitive coalesced case, i.e.,
      --      @let b = scratch ...@
      --      @.....@
      --      @let x[j] = b@
      --      @let y[i] = x@
      -- and compose the index function of @x@ with that of @y@,
      -- and update aliasing of the @m_b@ entry to also contain @m_y@
      -- on top of @m_x@, i.e., transitively, any use of @m_y@ should
      -- be checked for the lifetime of @b@.
      let proper_coals_tab = case knd of
            InPlaceCoal -> activeCoals_tab
            _ -> successCoals_tab
          (m_yx, ind_yx, mem_yx_al, x_deps, certs') =
            case M.lookup m_x proper_coals_tab of
              Nothing ->
                (m_x, alias_fn ind_x, oneName m_x, M.empty, mempty)
              Just (CoalsEntry m_y ind_y y_al vtab x_deps0 _ certs'') ->
                let ind = case M.lookup x vtab of
                      Just (Coalesced _ (MemBlock _ _ _ ixf) _) ->
                        ixf
                      Nothing ->
                        ind_y
                 in (m_y, alias_fn ind, oneName m_x <> y_al, x_deps0, certs <> certs'')
          m_b_aliased_m_yx = areAnyAliased td_env m_b [m_yx] -- m_b \= m_yx
       in if not m_b_aliased_m_yx && isInScope td_env m_yx -- nameIn m_yx (alloc td_env)
      -- Finally update the @activeCoals@ table with a fresh
      --   binding for @m_b@; if such one exists then overwrite.
      -- Also, add all variables from the alias chain of @b@ to
      --   @vartab@, for example, in the case of a sequence:
      --   @ b0 = if cond then ... else ... @
      --   @ b1 = alias0 b0 @
      --   @ b  = alias1 b1 @
      --   @ x[j] = b @
      -- Then @b1@ and @b0@ should also be added to @vartab@ if
      --   @alias1@ and @alias0@ are invertible, otherwise fail early!
            then
              let mem_info = Coalesced knd (MemBlock tp_b shp_b m_yx ind_yx) M.empty
                  opts' =
                    if m_yx == m_x
                      then M.empty
                      else M.insert x m_x x_deps
                  vtab = M.singleton b mem_info
                  mvtab = addInvAliasesVarTab td_env vtab b

                  is_inhibited = case M.lookup m_b $ inhibited td_env of
                    Just nms -> m_yx `nameIn` nms
                    Nothing -> False
               in case (is_inhibited, mvtab) of
                    (True, _) -> acc -- fail due to inhibited
                    (_, Nothing) -> acc -- fail early due to non-invertible aliasing
                    (_, Just vtab') ->
                      -- successfully adding a new coalesced entry
                      let coal_etry =
                            CoalsEntry
                              m_yx
                              ind_yx
                              mem_yx_al
                              vtab'
                              opts'
                              mempty
                              (certs <> certs')
                       in M.insert m_b coal_etry acc
            else acc

-- | Information about a particular short-circuit point
type SSPointInfo =
  ( CoalescedKind,
    LMAD -> LMAD,
    VName,
    VName,
    LMAD,
    VName,
    VName,
    LMAD,
    PrimType,
    Shape,
    Certs
  )

-- | Given an op, return a list of potential short-circuit points
type GenSSPoint rep op =
  LUTabFun ->
  TopdownEnv rep ->
  ScopeTab rep ->
  Pat (VarAliases, LetDecMem) ->
  Certs ->
  op ->
  Maybe [SSPointInfo]

genSSPointInfoSeqMem ::
  GenSSPoint SeqMem (Op (Aliases SeqMem))
genSSPointInfoSeqMem _ _ _ _ _ _ =
  Nothing

-- | For 'SegOp', we currently only handle 'SegMap', and only under the following
-- circumstances:
--
--  1. The 'SegMap' has only one return/pattern value, which is a 'Returns'.
--
--  2. The 'KernelBody' contains an 'Index' statement that is indexing an array using
--  only the values from the 'SegSpace'.
--
--  3. The array being indexed is last-used in that statement, is free in the
--  'SegMap', is unique or has been recently allocated (specifically, it should
--  not be a non-unique argument to the enclosing function), has elements with
--  the same bit-size as the pattern elements, and has the exact same 'LMAD' as
--  the pattern of the 'SegMap' statement.
--
-- There can be multiple candidate arrays, but the current implementation will
-- always just try the first one.
--
-- The first restriction could be relaxed by trying to match up arrays in the
-- 'KernelBody' with patterns of the 'SegMap', but the current implementation
-- should be enough to handle many common cases.
--
-- The result of the 'SegMap' is treated as the destination, while the candidate
-- array from inside the body is treated as the source.
genSSPointInfoSegOp ::
  (Coalesceable rep inner) => GenSSPoint rep (SegOp lvl (Aliases rep))
genSSPointInfoSegOp
  lutab
  td_env
  scopetab
  (Pat [PatElem dst (_, MemArray dst_pt _ _ (ArrayIn dst_mem dst_ixf))])
  certs
  (SegMap _ space _ kernel_body@KernelBody {kernelBodyResult = [Returns {}]})
    | (src, MemBlock src_pt shp src_mem src_ixf) : _ <-
        mapMaybe getPotentialMapShortCircuit $
          stmsToList $
            kernelBodyStms kernel_body =
        Just [(MapCoal, id, dst, dst_mem, dst_ixf, src, src_mem, src_ixf, src_pt, shp, certs)]
    where
      iterators = map fst $ unSegSpace space
      frees = freeIn kernel_body

      getPotentialMapShortCircuit (Let (Pat [PatElem x _]) _ (BasicOp (Index src slc)))
        | Just inds <- sliceIndices slc,
          L.sort inds == L.sort (map Var iterators),
          Just last_uses <- M.lookup x lutab,
          src `nameIn` last_uses,
          Just memblock@(MemBlock src_pt _ src_mem src_ixf) <-
            getScopeMemInfo src scopetab,
          src_mem `nameIn` last_uses,
          -- The 'alloc' table contains allocated memory blocks, including
          -- unique memory blocks from the enclosing function. It does _not_
          -- include non-unique memory blocks from the enclosing function.
          src_mem `M.member` alloc td_env,
          src `nameIn` frees,
          src_ixf == dst_ixf,
          primBitSize src_pt == primBitSize dst_pt =
            Just (src, memblock)
      getPotentialMapShortCircuit _ = Nothing
genSSPointInfoSegOp _ _ _ _ _ _ =
  Nothing

genSSPointInfoMemOp ::
  GenSSPoint rep (inner (Aliases rep)) ->
  GenSSPoint rep (MemOp inner (Aliases rep))
genSSPointInfoMemOp onOp lutab td_end scopetab pat certs (Inner op) =
  onOp lutab td_end scopetab pat certs op
genSSPointInfoMemOp _ _ _ _ _ _ _ = Nothing

genSSPointInfoGPUMem ::
  GenSSPoint GPUMem (Op (Aliases GPUMem))
genSSPointInfoGPUMem = genSSPointInfoMemOp f
  where
    f lutab td_env scopetab pat certs (GPU.SegOp op) =
      genSSPointInfoSegOp lutab td_env scopetab pat certs op
    f _ _ _ _ _ _ = Nothing

genSSPointInfoMCMem ::
  GenSSPoint MCMem (Op (Aliases MCMem))
genSSPointInfoMCMem = genSSPointInfoMemOp f
  where
    f lutab td_env scopetab pat certs (MC.ParOp Nothing op) =
      genSSPointInfoSegOp lutab td_env scopetab pat certs op
    f _ _ _ _ _ _ = Nothing

genCoalStmtInfo ::
  (Coalesceable rep inner) =>
  LUTabFun ->
  TopdownEnv rep ->
  ScopeTab rep ->
  Stm (Aliases rep) ->
  ShortCircuitM rep (Maybe [SSPointInfo])
-- CASE a) @let x <- copy(b^{lu})@
genCoalStmtInfo lutab td_env scopetab (Let pat aux (BasicOp (Replicate (Shape []) (Var b))))
  | Pat [PatElem x (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat,
    Just last_uses <- M.lookup x lutab,
    Just (MemBlock tpb shpb m_b ind_b) <- getScopeMemInfo b scopetab,
    sameSpace td_env m_x m_b,
    b `nameIn` last_uses =
      pure $ Just [(CopyCoal, id, x, m_x, ind_x, b, m_b, ind_b, tpb, shpb, stmAuxCerts aux)]
-- CASE c) @let x[i] = b^{lu}@
genCoalStmtInfo lutab td_env scopetab (Let pat aux (BasicOp (Update _ x slice_x (Var b))))
  | Pat [PatElem x' (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat,
    Just last_uses <- M.lookup x' lutab,
    Just (MemBlock tpb shpb m_b ind_b) <- getScopeMemInfo b scopetab,
    sameSpace td_env m_x m_b,
    b `nameIn` last_uses =
      pure $ Just [(InPlaceCoal, (`updateIndFunSlice` slice_x), x, m_x, ind_x, b, m_b, ind_b, tpb, shpb, stmAuxCerts aux)]
  where
    updateIndFunSlice :: LMAD -> Slice SubExp -> LMAD
    updateIndFunSlice ind_fun slc_x =
      let slc_x' = map (fmap pe64) $ unSlice slc_x
       in LMAD.slice ind_fun $ Slice slc_x'
genCoalStmtInfo lutab td_env scopetab (Let pat aux (BasicOp (FlatUpdate x slice_x b)))
  | Pat [PatElem x' (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat,
    Just last_uses <- M.lookup x' lutab,
    Just (MemBlock tpb shpb m_b ind_b) <- getScopeMemInfo b scopetab,
    sameSpace td_env m_x m_b,
    b `nameIn` last_uses =
      pure $ Just [(InPlaceCoal, (`updateIndFunSlice` slice_x), x, m_x, ind_x, b, m_b, ind_b, tpb, shpb, stmAuxCerts aux)]
  where
    updateIndFunSlice :: LMAD -> FlatSlice SubExp -> LMAD
    updateIndFunSlice ind_fun (FlatSlice offset dims) =
      LMAD.flatSlice ind_fun $ FlatSlice (pe64 offset) $ map (fmap pe64) dims

-- CASE b) @let x = concat(a, b^{lu})@
genCoalStmtInfo lutab td_env scopetab (Let pat aux (BasicOp (Concat concat_dim (b0 :| bs) _)))
  | Pat [PatElem x (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat,
    Just last_uses <- M.lookup x lutab =
      pure $
        let (res, _, _) = foldl (markConcatParts last_uses x m_x ind_x) ([], zero, True) (b0 : bs)
         in if null res then Nothing else Just res
  where
    zero = pe64 $ intConst Int64 0
    markConcatParts _ _ _ _ acc@(_, _, False) _ = acc
    markConcatParts last_uses x m_x ind_x (acc, offs, True) b
      | Just (MemBlock tpb shpb@(Shape dims@(_ : _)) m_b ind_b) <- getScopeMemInfo b scopetab,
        Just d <- maybeNth concat_dim dims,
        offs' <- offs + pe64 d =
          if b `nameIn` last_uses && sameSpace td_env m_x m_b
            then
              let slc =
                    Slice $
                      map (unitSlice zero . pe64) (take concat_dim dims)
                        <> [unitSlice offs (pe64 d)]
                        <> map (unitSlice zero . pe64) (drop (concat_dim + 1) dims)
               in ( acc ++ [(ConcatCoal, (`LMAD.slice` slc), x, m_x, ind_x, b, m_b, ind_b, tpb, shpb, stmAuxCerts aux)],
                    offs',
                    True
                  )
            else (acc, offs', True)
      | otherwise = (acc, offs, False)
-- case d) short-circuit points from ops. For instance, the result of a segmap
-- can be considered a short-circuit point.
genCoalStmtInfo lutab td_env scopetab (Let pat aux (Op op)) = do
  ss_op <- asks ssPointFromOp
  pure $ ss_op lutab td_env scopetab pat (stmAuxCerts aux) op
-- CASE other than a), b), c), or d) not supported
genCoalStmtInfo _ _ _ _ = pure Nothing

sameSpace :: (Coalesceable rep inner) => TopdownEnv rep -> VName -> VName -> Bool
sameSpace td_env m_x m_b
  | MemMem pat_space <- runReader (lookupMemInfo m_x) $ removeScopeAliases $ scope td_env,
    MemMem return_space <- runReader (lookupMemInfo m_b) $ removeScopeAliases $ scope td_env =
      pat_space == return_space
  | otherwise = False

data MemBodyResult = MemBodyResult
  { patMem :: VName,
    _patName :: VName,
    bodyName :: VName,
    bodyMem :: VName
  }

-- | Results in pairs of pattern-blockresult pairs of (var name, mem block)
--   for those if-patterns that are candidates for coalescing.
findMemBodyResult ::
  (HasMemBlock (Aliases rep)) =>
  CoalsTab ->
  ScopeTab rep ->
  [PatElem (VarAliases, LetDecMem)] ->
  Body (Aliases rep) ->
  [MemBodyResult]
findMemBodyResult activeCoals_tab scope_env patelms bdy =
  mapMaybe
    findMemBodyResult'
    (zip patelms $ map resSubExp $ bodyResult bdy)
  where
    scope_env' = scope_env <> scopeOf (bodyStms bdy)
    findMemBodyResult' (patel, se_r) =
      case (patElemName patel, patElemDec patel, se_r) of
        (b, (_, MemArray _ _ _ (ArrayIn m_b _)), Var r) ->
          case getScopeMemInfo r scope_env' of
            Nothing -> Nothing
            Just (MemBlock _ _ m_r _) ->
              case M.lookup m_b activeCoals_tab of
                Nothing -> Nothing
                Just coal_etry ->
                  case M.lookup b (vartab coal_etry) of
                    Nothing -> Nothing
                    Just _ -> Just $ MemBodyResult m_b b r m_r
        _ -> Nothing

-- | transfers coalescing from if-pattern to then|else body result
--   in the active coalesced table. The transfer involves, among
--   others, inserting @(r,m_r)@ in the optimistically-dependency
--   set of @m_b@'s entry and inserting @(b,m_b)@ in the opt-deps
--   set of @m_r@'s entry. Meaning, ultimately, @m_b@ can be merged
--   if @m_r@ can be merged (and vice-versa). This is checked by a
--   fix point iteration at the function-definition level.
transferCoalsToBody ::
  M.Map VName (TPrimExp Int64 VName) -> -- (PrimExp VName)
  CoalsTab ->
  MemBodyResult ->
  CoalsTab
transferCoalsToBody exist_subs activeCoals_tab (MemBodyResult m_b b r m_r)
  | -- the @Nothing@ pattern for the two lookups cannot happen
    -- because they were already cheked in @findMemBodyResult@
    Just etry <- M.lookup m_b activeCoals_tab,
    Just (Coalesced knd (MemBlock btp shp _ ind_b) subst_b) <- M.lookup b $ vartab etry =
      -- by definition of if-stmt, r and b have the same basic type, shape and
      -- index function, hence, for example, do not need to rebase
      -- We will check whether it is translatable at the definition point of r.
      let ind_r = LMAD.substitute exist_subs ind_b
          subst_r = M.union exist_subs subst_b
          mem_info = Coalesced knd (MemBlock btp shp (dstmem etry) ind_r) subst_r
       in if m_r == m_b -- already unified, just add binding for @r@
            then
              let etry' =
                    etry
                      { optdeps = M.insert b m_b (optdeps etry),
                        vartab = M.insert r mem_info (vartab etry)
                      }
               in M.insert m_r etry' activeCoals_tab
            else -- make them both optimistically depend on each other

              let opts_x_new = M.insert r m_r (optdeps etry)
                  -- Here we should translate the @ind_b@ field of @mem_info@
                  -- across the existential introduced by the if-then-else
                  coal_etry =
                    etry
                      { vartab = M.singleton r mem_info,
                        optdeps = M.insert b m_b (optdeps etry)
                      }
               in M.insert m_b (etry {optdeps = opts_x_new}) $
                    M.insert m_r coal_etry activeCoals_tab
  | otherwise = error "Impossible"

mkSubsTab ::
  Pat (aliases, LetDecMem) ->
  [SubExp] ->
  M.Map VName (TPrimExp Int64 VName)
mkSubsTab pat res =
  let pat_elms = patElems pat
   in M.fromList $ mapMaybe mki64subst $ zip pat_elms res
  where
    mki64subst (a, Var v)
      | (_, MemPrim (IntType Int64)) <- patElemDec a = Just (patElemName a, le64 v)
    mki64subst (a, se@(Constant (IntValue (Int64Value _)))) = Just (patElemName a, pe64 se)
    mki64subst _ = Nothing

computeScalarTable ::
  (Coalesceable rep inner) =>
  ScopeTab rep ->
  Stm (Aliases rep) ->
  ScalarTableM rep (M.Map VName (PrimExp VName))
computeScalarTable scope_table (Let (Pat [pe]) _ e)
  | Just primexp <- primExpFromExp (vnameToPrimExp scope_table mempty) e =
      pure $ M.singleton (patElemName pe) primexp
computeScalarTable scope_table (Let _ _ (Loop loop_inits loop_form body)) =
  concatMapM
    ( computeScalarTable $
        scope_table
          <> scopeOfFParams (map fst loop_inits)
          <> scopeOfLoopForm loop_form
          <> scopeOf (bodyStms body)
    )
    (stmsToList $ bodyStms body)
computeScalarTable scope_table (Let _ _ (Match _ cases body _)) = do
  body_tab <- concatMapM (computeScalarTable $ scope_table <> scopeOf (bodyStms body)) (stmsToList $ bodyStms body)
  cases_tab <-
    concatMapM
      ( \(Case _ b) ->
          concatMapM
            (computeScalarTable $ scope_table <> scopeOf (bodyStms b))
            ( stmsToList $
                bodyStms body
            )
      )
      cases
  pure $ body_tab <> cases_tab
computeScalarTable scope_table (Let _ _ (Op op)) = do
  on_op <- asks scalarTableOnOp
  on_op scope_table op
computeScalarTable _ _ = pure mempty

computeScalarTableMemOp ::
  ComputeScalarTable rep (inner (Aliases rep)) -> ComputeScalarTable rep (MemOp inner (Aliases rep))
computeScalarTableMemOp _ _ (Alloc _ _) = pure mempty
computeScalarTableMemOp onInner scope_table (Inner op) = onInner scope_table op

computeScalarTableSegOp ::
  (Coalesceable rep inner) =>
  ComputeScalarTable rep (GPU.SegOp lvl (Aliases rep))
computeScalarTableSegOp scope_table segop = do
  concatMapM
    ( computeScalarTable $
        scope_table
          <> scopeOf (kernelBodyStms $ segBody segop)
          <> scopeOfSegSpace (segSpace segop)
    )
    (stmsToList $ kernelBodyStms $ segBody segop)

computeScalarTableGPUMem ::
  ComputeScalarTable GPUMem (GPU.HostOp NoOp (Aliases GPUMem))
computeScalarTableGPUMem scope_table (GPU.SegOp segop) =
  computeScalarTableSegOp scope_table segop
computeScalarTableGPUMem _ (GPU.SizeOp _) = pure mempty
computeScalarTableGPUMem _ (GPU.OtherOp NoOp) = pure mempty
computeScalarTableGPUMem scope_table (GPU.GPUBody _ body) =
  concatMapM
    (computeScalarTable $ scope_table <> scopeOf (bodyStms body))
    (stmsToList $ bodyStms body)

computeScalarTableMCMem ::
  ComputeScalarTable MCMem (MC.MCOp NoOp (Aliases MCMem))
computeScalarTableMCMem _ (MC.OtherOp NoOp) = pure mempty
computeScalarTableMCMem scope_table (MC.ParOp par_op segop) =
  (<>)
    <$> maybe (pure mempty) (computeScalarTableSegOp scope_table) par_op
    <*> computeScalarTableSegOp scope_table segop

filterMapM1 :: (Eq k, Monad m) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapM1 f m = fmap M.fromAscList $ filterM (f . snd) $ M.toAscList m
