{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.ArrayShortCircuiting.ArrayCoalescing (mkCoalsTab, CoalsTab, mkCoalsTabGPU) where

import qualified Control.Exception.Base as Exc
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Set as S
import Debug.Trace
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Optimise.ArrayShortCircuiting.LastUse
import Futhark.Optimise.ArrayShortCircuiting.MemRefAggreg
import Futhark.Optimise.ArrayShortCircuiting.TopDownAn
import Futhark.Util.Pretty (Pretty)

traceWith :: Pretty a => String -> a -> a
traceWith s a = trace (s <> ": " <> pretty a) a

traceWith' :: Show a => String -> a -> a
traceWith' s a = trace (s <> ": " <> show a) a

type Coalesceable rep inner =
  ( CreatesNewArrOp (OpWithAliases inner),
    ASTRep rep,
    CanBeAliased inner,
    Op rep ~ MemOp inner,
    HasMemBlock (Aliases rep),
    LetDec rep ~ LetDecMem,
    TopDownHelper (OpWithAliases inner)
  )

newtype ComputeScalarTableOnOp rep = ComputeScalarTableOnOp
  { scalarTableOnOp :: ScopeTab rep -> Op (Aliases rep) -> ScalarTableM rep (M.Map VName (PrimExp VName))
  }

type ScalarTableM rep a = Reader (ComputeScalarTableOnOp rep) a

newtype ShortCircuitReader rep = ShortCircuitReader
  { onOp :: LUTabFun -> Pat (Aliases rep) -> Op (Aliases rep) -> TopDnEnv rep -> BotUpEnv -> ShortCircuitM rep BotUpEnv
  }

newtype ShortCircuitM rep a = ShortCircuitM (ReaderT (ShortCircuitReader rep) (StateT VNameSource IO) a)
  deriving (Functor, Applicative, Monad, MonadReader (ShortCircuitReader rep), MonadState VNameSource, MonadIO)

instance MonadFreshNames (ShortCircuitM rep) where
  putNameSource = put
  getNameSource = get

emptyTopDnEnv :: TopDnEnv rep
emptyTopDnEnv =
  TopDnEnv
    { alloc = mempty,
      scope = mempty,
      inhibited = mempty,
      ranges = mempty,
      v_alias = mempty,
      m_alias = mempty,
      nonNegatives = mempty,
      scalarTable = mempty,
      knownLessThan = mempty
    }

emptyBotUpEnv :: BotUpEnv
emptyBotUpEnv =
  BotUpEnv
    { scals = mempty,
      activeCoals = mempty,
      successCoals = mempty,
      inhibit = mempty
    }

-- | basic conversion of a Var Expression to a PrimExp
basePMconv ::
  (CanBeAliased (Op rep), RepTypes rep) =>
  ScopeTab rep ->
  ScalarTab ->
  VName ->
  Maybe (PrimExp VName)
basePMconv scopetab scaltab v =
  case M.lookup v scaltab of
    Just _ ->
      error "Impossible"
    Nothing -> case M.lookup v scopetab of
      Just info ->
        case typeOf info of
          Prim tp -> Just $ LeafExp v tp
          _ -> Nothing
      _ -> Nothing

-- | promotion from active-to-successful coalescing tables
--   should be handled with this function (for clarity).
markSuccessCoal ::
  (CoalsTab, CoalsTab) ->
  VName ->
  CoalsEntry ->
  (CoalsTab, CoalsTab)
markSuccessCoal (actv, succc) m_b info_b =
  ( M.delete m_b actv,
    appendCoalsInfo m_b info_b succc
  )

--------------------------------------------------------------------------------
--- Main Coalescing Transformation computes a successful coalescing table    ---
--------------------------------------------------------------------------------

-- | Given a 'FunDef' in 'SegMem' representation, compute the coalescing table
-- by folding over each function.
mkCoalsTab :: (MonadIO m, MonadFreshNames m) => FunDef (Aliases SeqMem) -> m CoalsTab
mkCoalsTab =
  mkCoalsTabFun
    (snd . lastUseSeqMem)
    (ShortCircuitReader shortCircuitSeqMem)
    (ComputeScalarTableOnOp $ const $ const $ return mempty)

-- | Given a 'FunDef' in 'GPUMem' representation, compute the coalescing table
-- by folding over each function.
mkCoalsTabGPU :: (MonadIO m, MonadFreshNames m) => FunDef (Aliases GPUMem) -> m CoalsTab
mkCoalsTabGPU =
  mkCoalsTabFun
    (snd . lastUseGPUMem)
    (ShortCircuitReader shortCircuitGPUMem)
    (ComputeScalarTableOnOp computeScalarTableGPUMem)

-- | Given a function, compute the coalescing table
mkCoalsTabFun ::
  (MonadFreshNames m, MonadIO m, Coalesceable rep inner, FParamInfo rep ~ FParamMem) =>
  (FunDef (Aliases rep) -> LUTabFun) ->
  ShortCircuitReader rep ->
  ComputeScalarTableOnOp rep ->
  FunDef (Aliases rep) ->
  m CoalsTab
mkCoalsTabFun lufun r computeScalarOnOp fun@(FunDef _ _ _ _ fpars body) = do
  -- First compute last-use information
  let lutab = lufun fun
      unique_mems = getUniqueMemFParam fpars
      scalar_table =
        runReader
          ( mconcat
              <$> mapM
                (computeScalarTable $ scopeOf fun <> scopeOf (bodyStms body))
                (stmsToList $ bodyStms body)
          )
          computeScalarOnOp
      topenv =
        emptyTopDnEnv
          { scope = scopeOfFParams fpars,
            alloc = unique_mems,
            scalarTable = scalar_table,
            nonNegatives = foldMap paramSizes fpars
          }
      ShortCircuitM m = fixPointCoalesce lutab fpars body topenv
  modifyNameSourceIO $ runStateT (runReaderT m r)

paramSizes :: Param FParamMem -> Names
paramSizes (Param _ _ (MemArray _ shp _ _)) = freeIn shp
paramSizes _ = mempty

-- | Short-circuit handler for a 'SeqMem' 'Op'.
--
-- Because 'SeqMem' don't have any special operation, simply return the input
-- 'BotUpEnv'.
shortCircuitSeqMem :: LUTabFun -> Pat (Aliases SeqMem) -> Op (Aliases SeqMem) -> TopDnEnv SeqMem -> BotUpEnv -> ShortCircuitM SeqMem BotUpEnv
shortCircuitSeqMem _ _ _ _ = return

-- | Short-circuit handler for 'GPUMem' 'Op'.
--
-- When the 'Op' is a 'SegOp', we handle it accordingly, otherwise we do
-- nothing.
shortCircuitGPUMem :: LUTabFun -> Pat (Aliases GPUMem) -> Op (Aliases GPUMem) -> TopDnEnv GPUMem -> BotUpEnv -> ShortCircuitM GPUMem BotUpEnv
shortCircuitGPUMem _ _ (Alloc _ _) _ bu_env = return bu_env
shortCircuitGPUMem lutab pat (Inner (SegOp (SegMap lvl space _ kernel_body))) td_env bu_env =
  -- No special handling necessary for 'SegMap'. Just call the helper-function.
  shortCircuitGPUMemHelper 0 lvl lutab pat space kernel_body td_env bu_env
shortCircuitGPUMem lutab pat (Inner (SegOp (SegRed lvl space binops _ kernel_body))) td_env bu_env =
  -- When handling 'SegRed', we we first invalidate all active coalesce-entries
  -- where any of the variables in 'vartab' are also free in the list of
  -- 'SegBinOp'. In other words, anything that is used as part of the reduction
  -- step should probably not be coalesced.
  let to_fail = M.filter (\entry -> namesFromList (M.keys $ vartab entry) `namesIntersect` foldMap (freeIn . segBinOpLambda) binops) $ activeCoals bu_env
      (active, inh) = foldl markFailedCoal (activeCoals bu_env, inhibit bu_env) $ M.keys to_fail
      bu_env' = bu_env {activeCoals = active, inhibit = inh}
      num_reds = length red_ts
   in shortCircuitGPUMemHelper num_reds lvl lutab pat space kernel_body td_env bu_env'
  where
    segment_dims = init $ segSpaceDims space
    red_ts = do
      op <- binops
      let shp = Shape segment_dims <> segBinOpShape op
      map (`arrayOfShape` shp) (lambdaReturnType $ segBinOpLambda op)
shortCircuitGPUMem lutab pat (Inner (SegOp (SegScan lvl space binops _ kernel_body))) td_env bu_env =
  -- Like in the handling of 'SegRed', we do not want to coalesce anything that
  -- is used in the 'SegBinOp'
  let to_fail = M.filter (\entry -> namesFromList (M.keys $ vartab entry) `namesIntersect` foldMap (freeIn . segBinOpLambda) binops) $ activeCoals bu_env
      (active, inh) = foldl markFailedCoal (activeCoals bu_env, inhibit bu_env) $ M.keys to_fail
      bu_env' = bu_env {activeCoals = active, inhibit = inh}
   in shortCircuitGPUMemHelper 0 lvl lutab pat space kernel_body td_env bu_env'
shortCircuitGPUMem lutab pat (Inner (SegOp (SegHist lvl space histops _ kernel_body))) td_env bu_env = do
  -- Need to take zipped patterns and histDest (flattened) and insert transitive coalesces
  let to_fail = M.filter (\entry -> namesFromList (M.keys $ vartab entry) `namesIntersect` foldMap (freeIn . histOp) histops) $ activeCoals bu_env
      (active, inh) = foldl markFailedCoal (activeCoals bu_env, inhibit bu_env) $ M.keys to_fail
      bu_env' = bu_env {activeCoals = active, inhibit = inh}
  bu_env'' <- shortCircuitGPUMemHelper 0 lvl lutab pat space kernel_body td_env bu_env'
  return $
    foldl
      ( \acc (PatElem p _, hist_dest) ->
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
      )
      bu_env''
      $ zip (patElems pat) $ concatMap histDest histops
shortCircuitGPUMem _ _ (Inner (SizeOp _)) _ bu_env = return bu_env
shortCircuitGPUMem _ _ (Inner (OtherOp ())) _ bu_env = return bu_env

dropLastSegSpace :: SegSpace -> SegSpace
dropLastSegSpace space = space {unSegSpace = init $ unSegSpace space}

isSegThread :: SegLevel -> Bool
isSegThread SegThread {} = True
isSegThread _ = False

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
shortCircuitGPUMemHelper ::
  -- | The number of returns for which we should drop the last seg space
  Int ->
  SegLevel ->
  LUTabFun ->
  Pat (Aliases GPUMem) ->
  SegSpace ->
  KernelBody (Aliases GPUMem) ->
  TopDnEnv GPUMem ->
  BotUpEnv ->
  ShortCircuitM GPUMem BotUpEnv
shortCircuitGPUMemHelper num_reds lvl lutab pat@(Pat ps0) space0 kernel_body td_env bu_env = do
  -- We need to drop the last element of the 'SegSpace' for pattern elements
  -- that correspond to reductions.
  let ps_and_space = zip ps0 (replicate num_reds (dropLastSegSpace space0) <> repeat space0)
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
          else
            traceWith ("ps0: " <> pretty ps0 <> "\nspace0: " <> pretty space0 <> "\nps_and_space: " <> pretty ps_and_space <> "\nkernelBodyResult: " <> pretty (kernelBodyResult kernel_body) <> "\nsegmapcoals") $
              foldl (makeSegMapCoals lvl td_env kernel_body) (actv0, inhibit0) $ zip ps_and_space $ kernelBodyResult kernel_body

  -- Process kernel body statements
  bu_env' <- mkCoalsTabStms lutab (kernelBodyStms kernel_body) td_env $ bu_env {activeCoals = actv0 <> actv_return, inhibit = inhibit_return}

  -- Check partial overlap.
  --
  -- Okay, this does not currently work. The reason is that the source writes
  -- are not updated correctly. For a given entry point 'b = segmap (...)', the
  -- segmap constitutes a source write to the whole index function. But for
  -- checking the partial overlap (between different threads of the segmap), we
  -- also need to consider the partial access summary.
  --
  -- So, for each entry point, if any of the entries in the vartab correspond to
  -- any of the patterns in the statement, we need to compute a partial access
  -- summary for that statement and use that in addition to the original source
  -- writes. That temporary access summary is the used to determine if there's
  -- an overlap in between the threads. Oh, but also remember that the index
  -- function of the result needs to be projected into the index function of the
  -- destination. And what if there are more than one entry-point that matches
  -- in the vartab? Just add them all up? Yes
  --
  -- We also need to subtract the current index from the destination uses?
  bu_env'' <-
    foldM
      ( \bu_env_f (k, entry) -> do
          thread_writes <-
            mconcat
              <$> mapM
                ( \(p, space) -> case M.lookup (patElemName p) $ vartab entry of
                    Just (Coalesced _ (MemBlock _ _ _ ixf) _) ->
                      -- aggSummaryMapPartial (scalarTable td_env) (unSegSpace space) $
                      return $
                        ixfunToAccessSummary $
                          IxFun.slice ixf $
                            fullSlice (IxFun.shape ixf) $
                              Slice $
                                map (DimFix . TPrimExp . flip LeafExp (IntType Int64) . fst) $
                                  unSegSpace space
                    Nothing -> return mempty
                )
                ps_and_space
          let source_writes = srcwrts (memrefs entry) <> thread_writes
          destination_uses <-
            case dstrefs (memrefs entry)
              `accessSubtract` dstrefs (maybe mempty memrefs $ M.lookup k $ activeCoals bu_env) of
              Set s -> mconcat <$> mapM (\lm -> aggSummaryMapPartial (scalarTable td_env) (unSegSpace space0) (Set $ S.singleton lm)) (S.toList s)
              -- Set s -> mconcat <$> mapM undefined (S.toList s)
              Undeterminable -> return Undeterminable
          -- destination_uses <-
          --   aggSummaryMapPartial
          --     (traceWith "aggSummaryMapPartial scalars" $ scalarTable td_env)
          --     (traceWith "aggSummaryMapPartial segspace" $ unSegSpace space0)
          --     $ dstrefs (memrefs entry)
          --       `accessSubtract` dstrefs (maybe mempty memrefs $ M.lookup k $ activeCoals bu_env)
          res <- liftIO $ noMemOverlap td_env destination_uses source_writes
          if traceWith
            ( "blabla pat: " <> pretty (head $ patNames pat) <> "\nk: " <> pretty k
                <> "\ndstrefs: "
                <> pretty (dstrefs $ memrefs entry)
                <> "\nother dstrefs: "
                <> pretty (dstrefs (maybe mempty memrefs $ M.lookup k $ activeCoals bu_env))
                <> "\ndestination_uses: "
                <> pretty destination_uses
                <> "\nthread_writes: "
                <> pretty thread_writes
                <> "\nsource_writes: "
                <> pretty (srcwrts $ memrefs entry)
                <> "\nnoMemOverlap"
            )
            res
            then return bu_env_f
            else do
              let (ac, inh) = markFailedCoal (activeCoals bu_env_f, inhibit bu_env_f) $ traceWith "marking this as failed now" k
              return $ bu_env_f {activeCoals = ac, inhibit = inh}
      )
      bu_env'
      $ M.toList $ activeCoals bu_env'

  actv <-
    mapM
      ( \entry -> do
          wrts <- aggSummaryMapTotal (scalarTable td_env) (unSegSpace space0) $ srcwrts $ memrefs entry
          uses <- aggSummaryMapTotal (scalarTable td_env) (unSegSpace space0) $ dstrefs $ memrefs entry
          return $ entry {memrefs = MemRefs uses wrts}
      )
      $ activeCoals bu_env''
  let bu_env''' = bu_env'' {activeCoals = actv}

  -- Process pattern and return values
  let mergee_writes = mapMaybe (\(p, _) -> fmap (p,) $ getDirAliasedIxfn td_env (activeCoals bu_env''') $ patElemName p) ps_and_space
  -- Now, for each mergee write, we need to check that it doesn't overlap with any previous uses of the destination.
  bu_env'''' <-
    foldM
      ( \bu_env_f (p, (m_b, m_y, ixf)) ->
          let as = ixfunToAccessSummary ixf
           in -- Should be @bu_env@ here, because we need to check overlap
              -- against previous uses.
              case M.lookup m_b $ activeCoals bu_env of
                Just coal_entry -> do
                  let mrefs =
                        memrefs coal_entry
                  res <- liftIO $ noMemOverlap td_env as $ dstrefs mrefs
                  if traceWith
                    ( "pat: " <> pretty p
                        <> "\nm_b: "
                        <> pretty m_b
                        <> "\nm_y: "
                        <> pretty m_y
                        <> "\nixf: "
                        <> pretty ixf
                        <> "\nas: "
                        <> pretty as
                        <> "\ndstrefs mrefs: "
                        <> pretty (dstrefs mrefs)
                        <> "\nnoMemOverlap2"
                    )
                    res
                    then case M.lookup (patElemName p) $ vartab coal_entry of
                      Nothing -> return bu_env_f
                      Just (Coalesced knd mbd@(MemBlock _ _ _ ixfn) _) -> return $
                        case freeVarSubstitutions (scope td_env) (scalarTable td_env) ixfn of
                          Just fv_subst ->
                            if ixfunPermutation ixfn == ixfunPermutation (ixfun $ fromJust $ getScopeMemInfo (patElemName p) $ scope td_env)
                              then
                                let entry = coal_entry {vartab = M.insert (patElemName p) (Coalesced knd mbd fv_subst) (vartab coal_entry)}
                                    (ac, suc) =
                                      markSuccessCoal (activeCoals bu_env_f, successCoals bu_env_f) m_b entry
                                 in bu_env_f {activeCoals = ac, successCoals = suc}
                              else
                                let (ac, inh) = markFailedCoal (activeCoals bu_env_f, inhibit bu_env_f) m_b
                                 in bu_env_f {activeCoals = ac, inhibit = inh}
                          Nothing ->
                            let (ac, inh) = markFailedCoal (activeCoals bu_env_f, inhibit bu_env_f) m_b
                             in bu_env_f {activeCoals = ac, inhibit = inh}
                    else
                      let (ac, inh) = markFailedCoal (activeCoals bu_env_f, inhibit bu_env_f) m_b
                       in return $ bu_env_f {activeCoals = ac, inhibit = inh}
                _ -> return bu_env_f
      )
      bu_env'''
      mergee_writes
  return bu_env''''

ixfunPermutation :: IxFun -> [Int]
ixfunPermutation = map IxFun.ldPerm . IxFun.lmadDims . NE.head . IxFun.ixfunLMADs

-- | Given a pattern element and the corresponding kernel result, try to put the
-- kernel result directly in the memory block of pattern element
makeSegMapCoals :: SegLevel -> TopDnEnv GPUMem -> KernelBody (Aliases GPUMem) -> (CoalsTab, InhibitTab) -> ((PatElemT (VarAliases, LetDecMem), SegSpace), KernelResult) -> (CoalsTab, InhibitTab)
makeSegMapCoals lvl td_env kernel_body (active, inhb) ((PatElem pat_name (_, MemArray _ _ _ (ArrayIn pat_mem pat_ixf)), space), Returns _ _ (Var return_name))
  | Just mb@(MemBlock tp return_shp return_mem _) <-
      getScopeMemInfo return_name $ scope td_env <> scopeOf (kernelBodyStms kernel_body),
    isSegThread lvl,
    MemMem pat_space <- runReader (lookupMemInfo pat_mem) $ removeScopeAliases $ scope td_env,
    MemMem return_space <- runReader (lookupMemInfo return_mem) $ removeScopeAliases $ scope td_env <> scopeOf (kernelBodyStms kernel_body) <> scopeOfSegSpace space,
    pat_space == return_space =
    case M.lookup pat_mem active of
      Nothing ->
        -- We are not in a transitive case
        if IxFun.hasOneLmad pat_ixf
          then case ( maybe False (pat_mem `nameIn`) $ M.lookup return_mem inhb,
                      Coalesced InPlaceCoal mb mempty
                        & M.singleton return_name
                        & flip (addInvAliassesVarTab td_env) return_name
                        & fmap
                          ( M.adjust
                              ( \(Coalesced knd (MemBlock pt shp _ _) subst) ->
                                  Coalesced
                                    knd
                                    ( MemBlock pt shp pat_mem $
                                        IxFun.slice pat_ixf $
                                          fullSlice (IxFun.shape pat_ixf) $
                                            Slice $
                                              map (DimFix . TPrimExp . flip LeafExp (IntType Int64) . fst) $
                                                unSegSpace space
                                    )
                                    subst
                              )
                              return_name
                          )
                    ) of
            (False, Just vtab) ->
              (active <> M.singleton return_mem (CoalsEntry pat_mem pat_ixf (oneName pat_mem) vtab mempty mempty), inhb)
            _ -> (active, inhb)
          else (active, inhb)
      Just trans ->
        case ( maybe False (dstmem trans `nameIn`) $ M.lookup return_mem inhb,
               Coalesced InPlaceCoal (MemBlock tp return_shp (dstmem trans) (dstind trans)) mempty
                 & M.singleton return_name
                 & flip (addInvAliassesVarTab td_env) return_name
                 & fmap
                   ( M.adjust
                       ( \(Coalesced knd (MemBlock pt shp mem ixf@(IxFun.IxFun _ base_shape _)) subst) ->
                           Coalesced
                             knd
                             ( MemBlock pt shp mem $
                                 IxFun.slice ixf $
                                   fullSlice base_shape $
                                     Slice $
                                       map (DimFix . TPrimExp . flip LeafExp (IntType Int64) . fst) $
                                         unSegSpace space
                             )
                             subst
                       )
                       return_name
                   )
             ) of
          (False, Just vtab) ->
            let opts = if dstmem trans == pat_mem then mempty else M.insert pat_name pat_mem $ optdeps trans
             in ( M.insert
                    return_mem
                    ( CoalsEntry
                        (dstmem trans)
                        (dstind trans)
                        (oneName pat_mem <> alsmem trans)
                        vtab
                        opts
                        mempty
                    )
                    active,
                  inhb
                )
          _ -> (active, inhb)
makeSegMapCoals _ td_env _ x ((_, _), WriteReturns _ _ return_name _) =
  case getScopeMemInfo return_name $ scope td_env of
    Just (MemBlock _ _ return_mem _) -> markFailedCoal x return_mem
    Nothing -> error "Should not happen?"
makeSegMapCoals _ td_env _ x ((_, _), result) =
  freeIn result
    & namesToList
    & mapMaybe (flip getScopeMemInfo $ scope td_env)
    & foldr (\(MemBlock _ _ mem _) -> flip markFailedCoal mem) x

fullSlice :: [TPrimExp Int64 VName] -> Slice (TPrimExp Int64 VName) -> Slice (TPrimExp Int64 VName)
fullSlice shp (Slice slc) =
  Slice $ slc ++ map (\d -> DimSlice 0 d 1) (drop (length slc) shp)

fixPointCoalesce ::
  (Coalesceable rep inner) =>
  LUTabFun ->
  [Param FParamMem] ->
  Body (Aliases rep) ->
  TopDnEnv rep ->
  ShortCircuitM rep CoalsTab
fixPointCoalesce lutab fpar bdy topenv = do
  buenv <- mkCoalsTabStms lutab (bodyStms bdy) topenv (emptyBotUpEnv {inhibit = inhibited topenv})
  let (succ_tab, actv_tab, inhb_tab) = (successCoals buenv, activeCoals buenv, inhibit buenv)
      -- remove @fpar@ from @actv_tab@, as function's parameters cannot be merged
      mems = map ((\(MemBlock _ _ m _) -> m) . snd) $ getArrMemAssocFParam fpar
      (actv_tab', inhb_tab') =
        foldl markFailedCoal (actv_tab, inhb_tab) mems

      (succ_tab', failed_optdeps) = fixPointFilterDeps succ_tab M.empty
      inhb_tab'' = M.unionWith (<>) failed_optdeps inhb_tab'
   in --new_inhibited = M.unionWith (<>) inhb_tab'' (inhibited topenv)
      if not $ M.null actv_tab'
        then error ("COALESCING ROOT: BROKEN INV, active not empty: " ++ pretty (M.keys actv_tab'))
        else
          if M.null $ inhb_tab'' `M.difference` inhibited topenv
            then return succ_tab'
            else fixPointCoalesce lutab fpar bdy (topenv {inhibited = inhb_tab''}) --new_inhibited })
            -- helper to helper
  where
    fixPointFilterDeps :: CoalsTab -> InhibitTab -> (CoalsTab, InhibitTab)
    fixPointFilterDeps coaltab inhbtab =
      let (coaltab', inhbtab') = foldl filterDeps (coaltab, inhbtab) (M.keys coaltab)
       in if length (M.keys coaltab) == length (M.keys coaltab')
            then (coaltab', inhbtab')
            else fixPointFilterDeps coaltab' inhbtab'
    -- helper to helper to helper
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

mkCoalsTabStms ::
  (Coalesceable rep inner) =>
  LUTabFun ->
  Stms (Aliases rep) ->
  TopDnEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep BotUpEnv
mkCoalsTabStms lutab = traverseStms
  where
    traverseStms Empty _ bu_env = return bu_env
    traverseStms (stm :<| stms) td_env bu_env = do
      let td_env' = topdwnTravBinding td_env stm
      bu_env' <- traverseStms stms td_env' bu_env
      mkCoalsTabStm lutab stm td_env' bu_env'

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
  TopDnEnv rep ->
  BotUpEnv ->
  ShortCircuitM rep BotUpEnv
mkCoalsTabStm _ (Let (Pat [pe]) _ e) td_env bu_env
  | Just primexp <- primExpFromExp (basePMconv (scope td_env) (scals bu_env)) e =
    return $ bu_env {scals = M.insert (patElemName pe) primexp (scals bu_env)}
mkCoalsTabStm lutab (Let patt _ (If _ body_then body_else _)) td_env bu_env = do
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
      res_mem_then = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms body_then
      res_mem_else = findMemBodyResult activeCoals0 (scope td_env) pat_val_elms body_else

      subs_then = mkSubsTab patt $ map resSubExp $ bodyResult body_then
      subs_else = mkSubsTab patt $ map resSubExp $ bodyResult body_else

      actv_then_i = foldl (transferCoalsToBody subs_then) activeCoals0 res_mem_then
      actv_else_i = foldl (transferCoalsToBody subs_else) activeCoals0 res_mem_else

      -- eliminate the original pattern binding of the if statement,
      -- @let x = if y[0,0] > 0 then map (+y[0,0]) a else map (+1) b@
      -- @let y[0] = x@
      -- should succeed because @m_y@ is used before @x@ is created.
      (actv_then, actv_else) =
        foldl
          ( \(acth, acel) (m_b, _, _, m_r) ->
              if m_b == m_r
                then (acth, acel)
                else (M.delete m_b acth, M.delete m_b acel)
          )
          (actv_then_i, actv_else_i)
          res_mem_then

  --iii) process the then and else bodies
  res_then <- mkCoalsTabStms lutab (bodyStms body_then) td_env (bu_env {activeCoals = actv_then})
  res_else <- mkCoalsTabStms lutab (bodyStms body_else) td_env (bu_env {activeCoals = actv_else})
  let (actv_then0, succ_then0, inhb_then0) = (activeCoals res_then, successCoals res_then, inhibit res_then)
      (actv_else0, succ_else0, inhb_else0) = (activeCoals res_else, successCoals res_else, inhibit res_else)

      -- iv) optimistically mark the pattern succesful:
      ((activeCoals1, inhibit1), successCoals1) =
        --trace ("Success then/else: "++pretty res_mem_then++"\n"++prettyCoalTab actv_then_i++" \n "++prettyCoalTab actv_else_i) $
        foldl
          (foldfun ((actv_then0, succ_then0), (actv_else0, succ_else0)))
          ((activeCoals0, inhibit0), successCoals bu_env)
          (zip res_mem_then res_mem_else)

      --  v) unify coalescing results of all branches by taking the union
      --     of all entries in the current/then/else success tables.
      then_failed =
        M.difference actv_then0 $
          M.intersectionWith unionCoalsEntry actv_then0 activeCoals0
      (_, inhb_then1) =
        --trace ("DIFF COM: "++prettyCoalTab then_diff_com ++ " INHIBIT: "++prettyInhibitTab inhb_then0) $
        foldl markFailedCoal (then_failed, inhb_then0) (M.keys then_failed)

      else_failed =
        M.difference actv_else0 $
          M.intersectionWith unionCoalsEntry actv_else0 activeCoals0
      (_, inhb_else1) = foldl markFailedCoal (else_failed, inhb_else0) (M.keys else_failed)

      actv_res =
        M.intersectionWith unionCoalsEntry actv_then0 $
          M.intersectionWith unionCoalsEntry actv_else0 activeCoals1

      succ_res =
        M.unionWith unionCoalsEntry succ_then0 $
          M.unionWith unionCoalsEntry succ_else0 successCoals1
      --
      -- vi) The step of filtering by 3rd safety condition is not
      --       necessary, because we perform index analysis of the
      --       source/destination uses, and they should have been
      --       filtered during the analysis of the then/else bodies.
      --      body_free_vars = freeIn body_then <> freeIn body_else
      --      (actv_res, inhibit_res0) =
      --        trace ("COALESCING IF: active == "++prettyCoalTab actv_res0++" res_mem_then: "++pretty res_mem_then++" else: "++pretty res_mem_else) $
      --        mkCoalsHelper1FilterActive patt body_free_vars (scope td_env)
      --                                   (scals bu_env) actv_res0 inhibit1

      inhibit_res =
        --trace ("COALESCING IF inhibits: " ++ prettyInhibitTab inhibit_res0 ++ " " ++ prettyInhibitTab inhb_then1 ++ " " ++ prettyInhibitTab inhb_else1) $
        M.unionWith (<>) inhibit1 $ -- inhibit_res0 $
          M.unionWith (<>) inhb_then1 inhb_else1
  return
    bu_env
      { activeCoals =
          actv_res,
        successCoals = succ_res,
        inhibit = inhibit_res
      }
  where
    foldfun _ ((act, _), _) ((m_b, _, _, _), (_, _, _, _))
      | Nothing <- M.lookup m_b act =
        error "Imposible Case!!!"
    foldfun
      ((_, succ_then0), (_, succ_else0))
      ((act, inhb), succc)
      ((m_b, _, r1, mr1), (_, _, r2, mr2))
        | Just info <- M.lookup m_b act,
          Just _ <- M.lookup mr1 succ_then0,
          Just _ <- M.lookup mr2 succ_else0 =
          -- Optimistically promote to successful coalescing and append!
          let info' =
                info
                  { optdeps =
                      M.insert r2 mr2 $
                        M.insert r1 mr1 $ optdeps info
                  }
              (act', succc') = markSuccessCoal (act, succc) m_b info'
           in ((act', inhb), succc')
    foldfun
      ((actv_then0, _), (actv_else0, _))
      ((act, inhb), succc)
      ((m_b, _, _, mr1), (_, _, _, mr2))
        | Just info <- M.lookup m_b act,
          m_b == mr1 && m_b == mr2,
          Just info_then <- M.lookup mr1 actv_then0,
          Just info_else <- M.lookup mr2 actv_else0 =
          -- Treating special case resembling:
          -- @let x0 = map (+1) a                                  @
          -- @let x3 = if cond then let x1 = x0 with [0] <- 2 in x1@
          -- @                 else let x2 = x0 with [1] <- 3 in x2@
          -- @let z[1] = x3                                        @
          -- In this case the result active table should be the union
          -- of the @m_x@ entries of the then and else active tables.
          let info' =
                unionCoalsEntry info $
                  unionCoalsEntry info_then info_else
              act' = M.insert m_b info' act
           in ((act', inhb), succc)
    foldfun _ ((act, inhb), succc) ((m_b, _, _, _), (_, _, _, _)) =
      -- one of the branches has failed coalescing,
      -- hence remove the coalescing of the result.

      (markFailedCoal (act, inhb) m_b, succc)
--
--mkCoalsTabStm lutab (Let pat _ (Op (Inner (Kernel str cs ker_space tps ker_bdy)))) td_env bu_env =
--  bu_env
--

mkCoalsTabStm lutab lstm@(Let pat _ (DoLoop arginis lform body)) td_env bu_env = do
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
              if nameIn b coal_pat_names
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
      res_mem_bdy = zipWith (\(b, m_b) (r, m_r) -> (m_b, b, r, m_r)) patmems resmems
      res_mem_arg = zipWith (\(b, m_b) (r, m_r) -> (m_b, b, r, m_r)) patmems argmems
      res_mem_ini = zipWith (\(b, m_b) (r, m_r) -> (m_b, b, r, m_r)) patmems inimems

      -- ToDo: check that an optimistic dependency is placed on the ini.
      actv2 =
        let subs_res = mkSubsTab pat $ map resSubExp $ bodyResult body
            actv11 = foldl (transferCoalsToBody subs_res) actv1 res_mem_bdy
            subs_arg = mkSubsTab pat $ map (Var . paramName . fst) arginis
            actv12 = foldl (transferCoalsToBody subs_arg) actv11 res_mem_arg
            subs_ini = mkSubsTab pat $ map snd arginis
         in foldl (transferCoalsToBody subs_ini) actv12 res_mem_ini
      -- foldl (transferCoalsToBody M.empty) actv1 (res_mem_bdy++res_mem_arg++res_mem_ini)
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
      actv3 =
        foldl
          ( \tab ((_, _, _, m_r), (_, _, _, m_a)) ->
              if m_r == m_a
                then tab
                else case M.lookup m_r tab of
                  Nothing -> tab
                  Just etry ->
                    M.insert m_r (etry {alsmem = alsmem etry <> oneName m_a}) tab
          )
          actv2
          (zip res_mem_bdy res_mem_arg)
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
  res_actv1 <- filterMapM1 (loopSoundness1Entry (scope td_env' <> scopeOf (bodyStms body)) scals_loop mb_loop_idx) res_actv0

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
  res_actv3 <- liftIO $ filterMapWithKeyM1 (loopSoundness2Entry actv3) res_actv2

  let tmp_succ =
        M.filterWithKey (okLookup actv3) $
          M.difference res_succ0 (successCoals bu_env)
  ver_succ <- liftIO $ filterMapWithKeyM1 (loopSoundness2Entry actv3) tmp_succ
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
  return bu_env {activeCoals = fin_actv2, successCoals = fin_succ1, inhibit = fin_inhb2}
  where
    allocs_bdy = foldl getAllocs (alloc td_env') $ bodyStms body
    td_env_allocs = td_env' {alloc = allocs_bdy, scope = scope td_env' <> scopeOf (bodyStms body)}
    td_env' = topDownLoop td_env lstm
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
        Var a0 <- ini,
        Var r <- bdyres,
        Just coal_etry <- M.lookup m_b actv0,
        Just _ <- M.lookup b (vartab coal_etry),
        Just (MemBlock _ _ m_a _) <- getScopeMemInfo a (scope td_env_allocs),
        Just (MemBlock _ _ m_a0 _) <- getScopeMemInfo a0 (scope td_env_allocs),
        Just (MemBlock _ _ m_r _) <- getScopeMemInfo r (scope td_env_allocs),
        Just nms <- M.lookup a lutab,
        nameIn a0 nms,
        m_r `elem` M.keys (alloc td_env_allocs) =
        Just ((b, m_b), (a, m_a), (a0, m_a0), (r, m_r))
    mapmbFun _ (_patel, (_arg, _ini), _bdyres) = Nothing
    --trace ("COALESCING loop FAILED "++pretty patel++" "++pretty arg ++ pretty ini ++ pretty bdyres)
    --
    foldFunOptimPromotion ::
      ((CoalsTab, InhibitTab), CoalsTab) ->
      ((VName, VName), (VName, VName), (VName, VName), (VName, VName)) ->
      ((CoalsTab, InhibitTab), CoalsTab)
    foldFunOptimPromotion ((act, inhb), succc) ((b, m_b), (a, m_a), (_r, m_r), (b_i, m_i))
      | m_r == m_i,
        Just info <- M.lookup m_i act,
        Just vtab_i <- addInvAliassesVarTab td_env (vartab info) b_i =
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
        Just vtab_i <- addInvAliassesVarTab td_env (vartab info_i) b_i,
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
      LoopForm (Aliases rep) ->
      Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName))
    mbLoopIndexRange (WhileLoop _) = Nothing
    mbLoopIndexRange (ForLoop inm _inttp seN _) = Just (inm, (pe64 se0, pe64 seN))
    addBeforeLoop actv_bef m_b etry =
      case M.lookup m_b actv_bef of
        Nothing -> etry
        Just etry0 ->
          etry {memrefs = memrefs etry0 <> memrefs etry}
    -- aggAcrossLoopEntry ::
    --   (Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
    --   ScopeTab rep ->
    --   ScalarTab ->
    --   Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
    --   CoalsEntry ->
    --   CoalsEntry
    aggAcrossLoopEntry scope_loop scal_tab idx etry = do
      wrts <-
        aggSummaryLoopTotal (scope td_env) scope_loop scal_tab idx $
          (srcwrts . memrefs) etry
      uses <-
        aggSummaryLoopTotal (scope td_env) scope_loop scal_tab idx $
          (dstrefs . memrefs) etry
      return $ etry {memrefs = MemRefs uses wrts}
    loopSoundness1Entry scope_loop scal_tab idx etry = do
      let wrt_i = (srcwrts . memrefs) etry
      use_p <-
        aggSummaryLoopPartial (scope td_env) scope_loop (scal_tab <> scalarTable td_env) idx $
          dstrefs $ memrefs etry
      liftIO $ noMemOverlap td_env' wrt_i use_p
    loopSoundness2Entry :: CoalsTab -> VName -> CoalsEntry -> IO Bool
    loopSoundness2Entry old_actv m_b etry =
      case M.lookup m_b old_actv of
        Nothing -> return True
        Just etry0 ->
          let uses_before = (dstrefs . memrefs) etry0
              write_loop = (srcwrts . memrefs) etry
           in noMemOverlap td_env write_loop uses_before

-- The case of in-place update:
--   @let x' = x with slice <- elm@
mkCoalsTabStm lutab stm@(Let pat@(Pat [x']) _ e@(BasicOp (Update safety x _ _elm))) td_env bu_env
  | [(_, MemBlock _ _ m_x _)] <- getArrMemAssoc pat =
    do
      -- (a) filter by the 3rd safety for @elm@ and @x'@
      (actv, inhbt) <- liftIO $ recordMemRefUses td_env bu_env stm
      -- mkCoalsHelper1FilterActive pat (se2names elm) (scope td_env) (scals bu_env)
      --                                      (activeCoals bu_env) (inhibit bu_env)
      -- (b) if @x'@ is in active coalesced table, then add an entry for @x@ as well
      let (actv', inhbt') =
            case M.lookup m_x actv of
              Nothing -> (actv, inhbt)
              Just info ->
                case M.lookup (patElemName x') (vartab info) of
                  Nothing ->
                    -- error "In ArrayCoalescing.hs, fun mkCoalsTabStm, case in-place update!"
                    -- this case should not happen, but if it can that just fail conservatively
                    markFailedCoal (actv, inhbt) m_x
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
          actv'' = if safety == Unsafe then mkCoalsHelper3PatternMatch pat e lutab td_env (successCoals bu_env) actv' inhbt' else actv'
      return $
        bu_env {activeCoals = actv'', inhibit = inhbt'}
--
mkCoalsTabStm _ (Let pat _ (BasicOp Update {})) _ _ =
  error $ "In ArrayCoalescing.hs, fun mkCoalsTabStm, illegal pattern for in-place update: " ++ pretty pat
-- default handling
mkCoalsTabStm lutab (Let pat _ (Op op)) td_env bu_env = do
  -- Process body
  on_op <- asks onOp
  on_op lutab pat op td_env bu_env
mkCoalsTabStm lutab stm@(Let pat _ e) td_env bu_env = do
  --   i) Filter @activeCoals@ by the 3rd safety condition:
  --      this is now relaxed by use of LMAD eqs:
  --      the memory referenced in stm are added to memrefs::dstrefs
  --      in corresponding coal-tab entries.
  (activeCoals', inhibit') <- liftIO $ recordMemRefUses td_env bu_env stm
  -- mkCoalsHelper1FilterActive pat (freeIn e) (scope td_env) (scals bu_env)
  --                           (activeCoals bu_env) (inhibit bu_env)

  --  ii) promote any of the entries in @activeCoals@ to @successCoals@ as long as
  --        - this statement defined a variable consumed in a coalesced statement
  --        - and safety conditions 2, 4, and 5 are satisfied.
  --      AND extend @activeCoals@ table for any definition of a variable that
  --      aliases a coalesced variable.
  let safe_4 = createsNewArrOK e
      ((activeCoals'', inhibit''), successCoals') =
        foldl (foldfun safe_4) ((activeCoals', inhibit'), successCoals bu_env) (getArrMemAssoc pat)

      -- iii) record a potentially coalesced statement in @activeCoals@
      activeCoals''' = mkCoalsHelper3PatternMatch pat e lutab td_env successCoals' activeCoals'' (inhibited td_env)
  return bu_env {activeCoals = activeCoals''', inhibit = inhibit'', successCoals = successCoals'}
  where
    foldfun safe_4 ((a_acc, inhb), s_acc) (b, MemBlock tp shp mb _b_indfun) =
      case M.lookup mb a_acc of
        Nothing -> ((a_acc, inhb), s_acc)
        Just info@(CoalsEntry x_mem _ _ vtab _ _) ->
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
                  -- where @alias@ can be @transpose@, @slice@, @rotate@, @reshape@.
                  -- We use getTransitiveAlias helper function to track the aliasing
                  --    through the td_env, and to find the updated ixfun of @b@:
                  case getDirAliasedIxfn td_env a_acc b of
                    Nothing -> (failed, s_acc)
                    Just (_, _, b_indfun') ->
                      case freeVarSubstitutions (scope td_env) (scals bu_env) b_indfun' of
                        Nothing -> (failed, s_acc)
                        Just fv_subst ->
                          let mem_info = Coalesced TransitiveCoal (MemBlock tp shp x_mem b_indfun') fv_subst
                              info' = info {vartab = M.insert b mem_info vtab}
                           in ((M.insert mb info' a_acc, inhb), s_acc)
                Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                  -- we are at the definition of the coalesced variable @b@
                  -- if 2,4,5 hold promote it to successful coalesced table,
                  -- or if e = transpose, etc. then postpone decision for later on
                  let safe_2 = isInScope td_env x_mem
                   in case freeVarSubstitutions (scope td_env) (scals bu_env) new_indfun of
                        Just fv_subst
                          | safe_2 ->
                            let mem_info = Coalesced k mblk fv_subst
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

ixfunToAccessSummary :: IxFun.IxFun (TPrimExp Int64 VName) -> AccessSummary
ixfunToAccessSummary (IxFun.IxFun (lmad NE.:| []) _ _) = Set $ S.singleton lmad
ixfunToAccessSummary _ = Undeterminable

-- | Check safety conditions 2 and 5 and update new substitutions:
-- called on the pat-elements of loop and if-then-else expressions.
--
-- The safety conditions are: The allocation of merge target should dominate the
-- creation of the array we're trying to merge and the new index function of the
-- array can be translated at the definition site of b. The latter requires that
-- any variables used in the index function of the target array are available at
-- the definition site of b.
filterSafetyCond2and5 ::
  HasMemBlock (Aliases rep) =>
  CoalsTab ->
  InhibitTab ->
  ScalarTab ->
  TopDnEnv rep ->
  [PatElemT (VarAliases, LetDecMem)] ->
  (CoalsTab, InhibitTab)
filterSafetyCond2and5 act_coal inhb_coal scals_env td_env =
  foldl
    ( \(acc, inhb) patel ->
        -- For each pattern element in the input list
        case (patElemName patel, patElemDec patel) of
          (b, (_, MemArray tp0 shp0 _ (ArrayIn m_b _idxfn_b))) ->
            -- If it is an array in memory block m_b
            case M.lookup m_b acc of
              Nothing -> (acc, inhb)
              Just info@(CoalsEntry x_mem _ _ vtab _ _) ->
                -- And m_b we're trying to coalesce m_b
                let failed = markFailedCoal (acc, inhb) m_b
                 in case M.lookup b vtab of
                      Nothing ->
                        -- trace ("Too drastic case: "++pretty (b,m_b,x_mem)) failed
                        -- This is not too drastic, because it applies to the patelems of loop/if-then-else
                        --
                        -- If 'b', the name of the PatElem, is not already in the vartab, do something
                        -- Wait, getDirAliasedIxFn looks for 'b' in 'acc', but we already know that it's not there, right?
                        case getDirAliasedIxfn td_env acc b of
                          Nothing -> failed
                          Just (_, _, b_indfun') ->
                            -- And we have the index function of b
                            case freeVarSubstitutions (scope td_env) scals_env b_indfun' of
                              Nothing -> failed
                              Just fv_subst ->
                                let mem_info = Coalesced TransitiveCoal (MemBlock tp0 shp0 x_mem b_indfun') fv_subst
                                    info' = info {vartab = M.insert b mem_info vtab}
                                 in (M.insert m_b info' acc, inhb)
                      Just (Coalesced k (MemBlock pt shp _ new_indfun) _) ->
                        let safe_2 = isInScope td_env x_mem
                         in case freeVarSubstitutions (scope td_env) scals_env new_indfun of
                              Just fv_subst
                                | safe_2 ->
                                  let mem_info = Coalesced k (MemBlock pt shp x_mem new_indfun) fv_subst
                                      info' = info {vartab = M.insert b mem_info vtab}
                                   in (M.insert m_b info' acc, inhb)
                              _ -> failed
          _ -> (acc, inhb)
    )
    (act_coal, inhb_coal)

-- |   Pattern matches a potentially coalesced statement and
--     records a new association in @activeCoals@
mkCoalsHelper3PatternMatch ::
  HasMemBlock (Aliases rep) =>
  PatT (VarAliases, LetDecMem) ->
  Exp (Aliases rep) ->
  LUTabFun ->
  TopDnEnv rep ->
  CoalsTab ->
  CoalsTab ->
  InhibitTab ->
  CoalsTab
mkCoalsHelper3PatternMatch pat e lutab td_env _ activeCoals_tab _
  | Nothing <- genCoalStmtInfo lutab (scope td_env) pat e =
    activeCoals_tab
mkCoalsHelper3PatternMatch pat e lutab td_env successCoals_tab activeCoals_tab inhibit_tab
  | Just clst <- genCoalStmtInfo lutab (scope td_env) pat e =
    foldl processNewCoalesce activeCoals_tab clst
  where
    processNewCoalesce acc (knd, alias_fn, x, m_x, ind_x, b, m_b, _, tp_b, shp_b) =
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
          (m_yx, ind_yx, mem_yx_al, x_deps) =
            case M.lookup m_x proper_coals_tab of
              Nothing ->
                (m_x, alias_fn ind_x, oneName m_x, M.empty)
              Just (CoalsEntry m_y ind_y y_al vtab x_deps0 _) ->
                let ind = case M.lookup x vtab of
                      Just (Coalesced _ (MemBlock _ _ _ ixf) _) ->
                        ixf
                      Nothing ->
                        ind_y
                 in (m_y, alias_fn ind, oneName m_x <> y_al, x_deps0)
          success0 = IxFun.hasOneLmad ind_yx
          m_b_aliased_m_yx = areAnyAliased td_env m_b [m_yx] -- m_b \= m_yx
       in case (success0, not m_b_aliased_m_yx, isInScope td_env m_yx) of -- nameIn m_yx (alloc td_env)
            (True, True, True) ->
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
              let mem_info = Coalesced knd (MemBlock tp_b shp_b m_yx ind_yx) M.empty
                  opts' =
                    if m_yx == m_x
                      then M.empty
                      else M.insert x m_x x_deps
                  vtab = M.singleton b mem_info
                  mvtab = addInvAliassesVarTab td_env vtab b

                  is_inhibited = case M.lookup m_b inhibit_tab of
                    Just nms -> nameIn m_yx nms
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
                       in M.insert m_b coal_etry acc
            _ -> acc
mkCoalsHelper3PatternMatch _ _ _ _ _ _ _ =
  error "In ArrayCoalescing.hs, fun mkCoalsHelper3PatternMatch: Unreachable!!!"

genCoalStmtInfo ::
  HasMemBlock (Aliases rep) =>
  LUTabFun ->
  ScopeTab rep ->
  PatT (VarAliases, LetDecMem) ->
  Exp (Aliases rep) ->
  Maybe [(CoalescedKind, IxFun -> IxFun, VName, VName, IxFun, VName, VName, IxFun, PrimType, Shape)]
-- CASE a) @let x <- copy(b^{lu})@
genCoalStmtInfo lutab scopetab pat (BasicOp (Copy b))
  | Pat [PatElem x (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat =
    case (M.lookup x lutab, getScopeMemInfo b scopetab) of
      (Just last_uses, Just (MemBlock tpb shpb m_b ind_b)) ->
        if not (nameIn b last_uses)
          then Nothing
          else Just [(CopyCoal, id, x, m_x, ind_x, b, m_b, ind_b, tpb, shpb)]
      _ -> Nothing
-- CASE c) @let x[i] = b^{lu}@
genCoalStmtInfo lutab scopetab pat (BasicOp (Update _ x slice_x (Var b)))
  | Pat [PatElem x' (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat =
    case (M.lookup x' lutab, getScopeMemInfo b scopetab) of
      (Just last_uses, Just (MemBlock tpb shpb m_b ind_b)) ->
        if not (nameIn b last_uses)
          then Nothing
          else Just [(InPlaceCoal, (`updateIndFunSlice` slice_x), x, m_x, ind_x, b, m_b, ind_b, tpb, shpb)]
      _ -> Nothing
  where
    updateIndFunSlice :: IxFun -> Slice SubExp -> IxFun
    updateIndFunSlice ind_fun slc_x =
      let slc_x' = map (fmap pe64) $ unSlice slc_x
       in IxFun.slice ind_fun $ Slice slc_x'

-- CASE b) @let x = concat(a, b^{lu})@
genCoalStmtInfo lutab scopetab pat (BasicOp (Concat 0 b0 bs _))
  | Pat [PatElem x (_, MemArray _ _ _ (ArrayIn m_x ind_x))] <- pat =
    case M.lookup x lutab of
      Nothing -> Nothing
      Just last_uses ->
        let zero = pe64 $ intConst Int64 0
            (res, _, _) =
              foldl
                ( \(acc, offs, succ0) b ->
                    if not succ0
                      then (acc, offs, succ0)
                      else case getScopeMemInfo b scopetab of
                        Just (MemBlock tpb shpb@(Shape (fd : rdims)) m_b ind_b) ->
                          let offs' = offs + pe64 fd
                           in if nameIn b last_uses
                                then
                                  let slc = Slice $ unitSlice offs (pe64 fd) : map (unitSlice zero . pe64) rdims
                                   in -- ind_x_slice = IxFun.slice ind_x slc

                                      (acc ++ [(ConcatCoal, (`IxFun.slice` slc), x, m_x, ind_x, b, m_b, ind_b, tpb, shpb)], offs', True)
                                else (acc, offs', True)
                        _ -> (acc, offs, False)
                )
                ([], zero, True)
                (b0 : bs)
         in if null res then Nothing else Just res
-- CASE other than a), b), or c) not supported
genCoalStmtInfo _ _ _ _ = Nothing

-- | merges entries in the coalesced table.
appendCoalsInfo :: VName -> CoalsEntry -> CoalsTab -> CoalsTab
appendCoalsInfo mb info_new coalstab =
  case M.lookup mb coalstab of
    Nothing -> M.insert mb info_new coalstab
    Just info_old -> M.insert mb (unionCoalsEntry info_old info_new) coalstab

-- | Results in pairs of pattern-blockresult pairs of (var name, mem block)
--   for those if-patterns that are candidates for coalescing.
findMemBodyResult ::
  (HasMemBlock (Aliases rep)) =>
  CoalsTab ->
  ScopeTab rep ->
  [PatElemT (VarAliases, LetDecMem)] ->
  Body (Aliases rep) ->
  [(VName, VName, VName, VName)]
findMemBodyResult activeCoals_tab scope_env patelms bdy =
  let scope_env' = scope_env <> scopeOf (bodyStms bdy)
   in mapMaybe
        ( \(patel, se_r) ->
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
                          Just _ -> Just (m_b, b, r, m_r)
              _ -> Nothing
        )
        (zip patelms $ map resSubExp $ bodyResult bdy)

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
  (VName, VName, VName, VName) ->
  CoalsTab
transferCoalsToBody exist_subs activeCoals_tab (m_b, b, r, m_r)
  | -- the @Nothing@ pattern for the two lookups cannot happen
    -- because they were already cheked in @findMemBodyResult@
    Just etry <- M.lookup m_b activeCoals_tab,
    Just (Coalesced knd (MemBlock btp shp _ ind_b) subst_b) <- M.lookup b $ vartab etry =
    -- by definition of if-stmt, r and b have the same basic type, shape and
    -- index function, hence, for example, do not need to rebase
    -- We will check whether it is translatable at the definition point of r.
    let ind_r = IxFun.substituteInIxFun exist_subs ind_b
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
  PatT (aliases, LetDecMem) ->
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
  | Just primexp <- primExpFromExp (basePMconv scope_table mempty) e =
    return $ M.singleton (patElemName pe) primexp
computeScalarTable scope_table (Let _ _ (DoLoop loop_inits loop_form body)) =
  mconcat
    <$> mapM
      ( computeScalarTable $
          scope_table
            <> scopeOfFParams (map fst loop_inits)
            <> scopeOf loop_form
            <> scopeOf (bodyStms body)
      )
      (stmsToList $ bodyStms body)
computeScalarTable scope_table (Let _ _ (If _ then_body else_body _)) =
  (<>)
    <$> (mconcat <$> mapM (computeScalarTable $ scope_table <> scopeOf (bodyStms then_body)) (stmsToList $ bodyStms then_body))
    <*> (mconcat <$> mapM (computeScalarTable $ scope_table <> scopeOf (bodyStms else_body)) (stmsToList $ bodyStms else_body))
computeScalarTable scope_table (Let _ _ (Op op)) = do
  on_op <- asks scalarTableOnOp
  on_op scope_table op
computeScalarTable _ _ = return mempty

computeScalarTableGPUMem :: ScopeTab GPUMem -> Op (Aliases GPUMem) -> ScalarTableM GPUMem (M.Map VName (PrimExp VName))
computeScalarTableGPUMem _ (Alloc _ _) = return mempty
computeScalarTableGPUMem scope_table (Inner (SegOp segop)) = do
  mconcat
    <$> mapM
      (computeScalarTable $ scope_table <> scopeOf (kernelBodyStms $ segBody segop) <> scopeOfSegSpace (segSpace segop))
      (stmsToList $ kernelBodyStms $ segBody segop)
computeScalarTableGPUMem _ (Inner (SizeOp _)) = return mempty
computeScalarTableGPUMem _ (Inner (OtherOp ())) = return mempty

filterMapM1 :: (Eq k, Monad m) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapM1 f m = fmap M.fromAscList $ filterM (f . snd) $ M.toAscList m

filterMapWithKeyM1 :: (Eq k, Monad m) => (k -> v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapWithKeyM1 f m = fmap M.fromAscList $ filterM (uncurry f) $ M.toAscList m
