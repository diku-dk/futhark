{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.MemRefAggreg
  ( recordMemRefUses,
    freeVarSubstitutions,
    translateAccessSummary,
    aggSummaryLoopTotal,
    aggSummaryLoopPartial,
    aggSummaryMapPartial,
    aggSummaryMapTotal,
    noMemOverlap,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersect, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
--import Debug.Trace
import Futhark.Analysis.AlgSimplify2
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Optimise.ArrayShortCircuiting.TopDownAn
import Futhark.Util
import Futhark.Util.Pretty (Pretty)

trace _ x = x

traceWith :: Pretty a => String -> a -> a
traceWith s a = trace (s <> ": " <> pretty a) a

-----------------------------------------------------
-- Some translations of Accesses and Ixfuns        --
-----------------------------------------------------

-- | Checks whether the index function can be translated at the current program
-- point and also returns the substitutions.  It comes down to answering the
-- question: "can one perform enough substitutions (from the bottom-up scalar
-- table) until all vars appearing in the index function are defined in the
-- current scope?"
freeVarSubstitutions ::
  FreeIn a =>
  ScopeTab rep ->
  ScalarTab ->
  a ->
  Maybe FreeVarSubsts
freeVarSubstitutions scope0 scals0 indfun =
  freeVarSubstitutions' mempty $ namesToList $ freeIn indfun
  where
    freeVarSubstitutions' :: FreeVarSubsts -> [VName] -> Maybe FreeVarSubsts
    freeVarSubstitutions' subs [] = Just subs
    freeVarSubstitutions' subs0 fvs =
      let fvs_not_in_scope = filter (`M.notMember` scope0) fvs
       in case unzip <$> mapM getSubstitution fvs_not_in_scope of
            -- We require that all free variables can be substituted
            Just (subs, new_fvs) ->
              freeVarSubstitutions' (subs0 <> mconcat subs) $ concat new_fvs
            Nothing -> Nothing
    getSubstitution v
      | Just pe <- M.lookup v scals0,
        IntType _ <- primExpType pe =
        Just (M.singleton v $ TPrimExp pe, namesToList $ freeIn pe)
    getSubstitution _v = Nothing

-- | Translates free variables in an access summary
translateAccessSummary :: ScopeTab rep -> ScalarTab -> AccessSummary -> AccessSummary
translateAccessSummary _ _ Undeterminable = Undeterminable
translateAccessSummary scope0 scals0 (Set slmads)
  | Just subs <- freeVarSubstitutions scope0 scals0 slmads =
    slmads
      & S.map (IxFun.substituteInLMAD subs)
      & Set
translateAccessSummary _ _ _ = Undeterminable

-- | This function computes the written and read memory references for the current statement
getUseSumFromStm ::
  (Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
  TopDnEnv rep ->
  CoalsTab ->
  Stm (Aliases rep) ->
  -- | A pair of written and written+read memory locations, along with their
  -- associated array and the index function used
  Maybe ([(VName, VName, IxFun)], [(VName, VName, IxFun)])
getUseSumFromStm td_env coal_tab (Let p _ (BasicOp (Index arr (Slice slc))))
  | Just (MemBlock _ shp _ _) <- getScopeMemInfo arr (scope td_env),
    length slc == length (shapeDims shp) && all isFix slc = do
    (mem_b, mem_arr, ixfn_arr) <- getDirAliasedIxfn td_env coal_tab arr
    let new_ixfn = IxFun.slice ixfn_arr $ Slice $ map (fmap pe64) slc
    return ([], [(mem_b, mem_arr, new_ixfn)])
  where
    isFix DimFix {} = True
    isFix _ = False
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Index {})) = Just ([], []) -- incomplete slices
getUseSumFromStm td_env coal_tab (Let (Pat pes) _ (BasicOp (ArrayLit ses _))) =
  let rds = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ mapMaybe seName ses
      wrts = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) pes
   in Just (wrts, wrts ++ rds)
  where
    seName (Var a) = Just a
    seName (Constant _) = Nothing
-- In place update @x[slc] <- a@. In the "in-place update" case,
--   summaries should be added after the old variable @x@ has
--   been added in the active coalesced table.
getUseSumFromStm td_env coal_tab (Let (Pat [x']) _ (BasicOp (Update _ _x (Slice slc) a_se))) = do
  (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x')
  let x_ixfn_slc = IxFun.slice x_ixfn $ Slice $ map (fmap pe64) slc
      r1 = (m_b, m_x, x_ixfn_slc)
  case a_se of
    Constant _ -> Just ([r1], [r1])
    Var a -> case getDirAliasedIxfn td_env coal_tab a of
      Nothing -> Just ([r1], [r1])
      Just r2 -> Just ([r1], [r1, r2])
getUseSumFromStm td_env coal_tab (Let (Pat [y]) _ (BasicOp (Copy x))) = do
  -- y = copy x
  wrt <- getDirAliasedIxfn td_env coal_tab $ patElemName y
  rd <- getDirAliasedIxfn td_env coal_tab x
  return ([wrt], [wrt, rd])
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Copy {})) = error "Impossible"
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Concat _i a bs _ses))) =
  -- concat
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) (a : bs)
   in Just (ws, ws ++ rs)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Manifest _perm x))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) [x]
   in Just (ws, ws ++ rs)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Replicate _shp se))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
   in case se of
        Constant _ -> Just (ws, ws)
        Var x -> Just (ws, ws ++ mapMaybe (getDirAliasedIxfn td_env coal_tab) [x])
getUseSumFromStm td_env coal_tab (Let (Pat [x]) _ (BasicOp (FlatUpdate _ (FlatSlice offset slc) v)))
  | Just (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x) =
    let x_ixfn_slc = IxFun.flatSlice x_ixfn $ FlatSlice (pe64 offset) $ map (fmap pe64) slc
        r1 = (m_b, m_x, x_ixfn_slc)
     in case getDirAliasedIxfn td_env coal_tab v of
          Nothing -> Just ([r1], [r1])
          Just r2 -> Just ([r1], [r1, r2])
getUseSumFromStm _ _ (Let Pat {} _ BasicOp {}) = Just ([], [])
getUseSumFromStm _ _ (Let Pat {} _ (Op (Alloc _ _))) = Just ([], [])
getUseSumFromStm _ _ _ =
  -- if-then-else, loops are supposed to be treated separately,
  -- calls are not supported, and Ops are not yet supported
  -- error
  --   ("In MemRefAggreg.hs, getUseSumFromStm, unsuported case of stm being: " ++ pretty stm)
  Nothing

-- | This function:
--     1. computes the written and read memory references for the current statement
--          (by calling @getUseSumFromStm@)
--     2. fails the entries in active coalesced table for which the write set
--          overlaps the uses of the destination (to that point)
--   ToDo: a) the @noMemOverlap@ test probably requires the @scals@ field
--            from @BotUpEnv@ so that it translates the index functions.
--         b) it will also require a dictionary of ranges, e.g., for loop
--            indices, and positive constants, such as loop counts. This
--            should be computed on the top-down pass.
recordMemRefUses ::
  (CanBeAliased (Op rep), RepTypes rep, Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
  TopDnEnv rep ->
  BotUpEnv ->
  Stm (Aliases rep) ->
  IO (CoalsTab, InhibitTab)
recordMemRefUses td_env bu_env stm =
  let active_tab = activeCoals bu_env
      inhibit_tab = inhibit bu_env
      active_etries = M.toList active_tab
   in case getUseSumFromStm td_env active_tab stm of
        Nothing ->
          M.toList active_tab
            & foldl
              ( \state (m_b, entry) ->
                  if not $ null $ patNames (stmPat stm) `intersect` M.keys (vartab entry)
                    then markFailedCoal state m_b
                    else state
              )
              (active_tab, inhibit_tab)
            & return
        Just (stm_wrts, stm_uses) -> do
          (mb_wrts, prev_uses, mb_lmads) <-
            fmap unzip3 $
              liftIO $
                mapM
                  ( \(m_b, etry) -> do
                      let alias_m_b = getAliases mempty m_b
                          stm_uses' = filter (not . (`nameIn` alias_m_b) . tupFst) stm_uses
                          all_aliases = foldl getAliases mempty $ namesToList $ alsmem etry
                          ixfns = map tupThd $ filter ((`nameIn` all_aliases) . tupSnd) stm_uses'
                          lmads' = mapMaybe mbLmad ixfns
                          lmads'' =
                            if length lmads' == length ixfns
                              then Set $ S.fromList lmads'
                              else Undeterminable
                          wrt_ixfns = map tupThd $ filter ((`nameIn` alias_m_b) . tupFst) stm_wrts
                          wrt_tmps = mapMaybe mbLmad wrt_ixfns
                          prev_use =
                            translateAccessSummary (scope td_env) (scalarTable td_env) $
                              (dstrefs . memrefs) etry
                          wrt_lmads' =
                            if length wrt_tmps == length wrt_ixfns
                              then Set $ S.fromList wrt_tmps
                              else Undeterminable
                          original_mem_aliases =
                            fmap tupFst stm_uses
                              & uncons
                              & fmap fst
                              & (=<<) (`M.lookup` active_tab)
                              & maybe mempty alsmem
                          (wrt_lmads'', lmads) =
                            if m_b `nameIn` original_mem_aliases
                              then (wrt_lmads' <> lmads'', Set mempty)
                              else (wrt_lmads', lmads'')
                      no_overlap <- noMemOverlap td_env prev_use wrt_lmads''
                      let wrt_lmads =
                            if no_overlap
                              then Just wrt_lmads''
                              else Nothing
                      return (wrt_lmads, prev_use, lmads)
                  )
                  active_etries

          -- keep only the entries that do not overlap with the memory
          -- blocks defined in @pat@ or @inner_free_vars@.
          -- the others must be recorded in @inhibit_tab@ because
          -- they violate the 3rd safety condition.
          let active_tab1 =
                M.fromList $
                  map
                    ( \(wrts, (uses, prev_use, (k, etry))) ->
                        let mrefs' = (memrefs etry) {dstrefs = prev_use}
                            etry' = etry {memrefs = mrefs'}
                         in (k, addLmads wrts uses etry')
                    )
                    $ filter (isJust . fst) $
                      zip mb_wrts $ zip3 mb_lmads prev_uses active_etries
              failed_tab =
                M.fromList $
                  map snd $
                    filter (isNothing . fst) $
                      zip mb_wrts active_etries
              (_, inhibit_tab1) = foldl markFailedCoal (failed_tab, inhibit_tab) $ M.keys failed_tab
          return (active_tab1, inhibit_tab1)
  where
    tupFst (a, _, _) = a
    tupSnd (_, b, _) = b
    tupThd (_, _, c) = c
    getAliases acc m =
      oneName m
        <> acc
        <> fromMaybe mempty (M.lookup m (m_alias td_env))
    mbLmad indfun
      | Just subs <- freeVarSubstitutions (scope td_env) (scals bu_env) indfun,
        (IxFun.IxFun (lmad :| []) _ _) <- IxFun.substituteInIxFun subs indfun =
        Just lmad
    mbLmad _ = Nothing
    addLmads (Just wrts) uses etry =
      etry {memrefs = MemRefs uses wrts <> memrefs etry}
    addLmads _ _ _ =
      error "Impossible case reached because we have filtered Nothings before"

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (pure True) b

(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (pure False)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = foldr ((&&^) . p) (pure True)

-------------------------------------------------------------
-- Helper Functions for Partial and Total Loop Aggregation --
--  of memory references. Please implement as precise as   --
--  possible. Currently the implementations are dummy      --
--  aiming to be useful only when one of the sets is empty.--
-------------------------------------------------------------
noMemOverlap :: (CanBeAliased (Op rep), RepTypes rep) => TopDnEnv rep -> AccessSummary -> AccessSummary -> IO Bool
noMemOverlap _ _ (Set mr)
  | mr == mempty = return True
noMemOverlap _ (Set mr) _
  | mr == mempty = return True
noMemOverlap _ Undeterminable _ = return False
noMemOverlap _ _ Undeterminable = return False
noMemOverlap td_env (Set is0) (Set js0) = do
  -- TODO Expand this to be able to handle eg. nw
  bla1 <- mapM (\i -> allM (\j -> return (IxFun.disjoint less_thans (nonNegatives td_env) i j) ||^ return (IxFun.disjoint2 less_thans (nonNegatives td_env) i j) ||^ IxFun.disjoint3 (fmap typeOf $ scope td_env) less_thans (nonNegatives td_env) i j) js) is
  bla2 <- filterM (fmap not . \i -> allM (\j -> return (IxFun.disjoint less_thans (nonNegatives td_env) i j) ||^ return (IxFun.disjoint2 less_thans (nonNegatives td_env) i j) ||^ IxFun.disjoint3 (fmap typeOf $ scope td_env) less_thans (nonNegatives td_env) i j) js) is
  allM (\i -> allM (\j -> return (IxFun.disjoint less_thans (nonNegatives td_env) i j) ||^ return (IxFun.disjoint2 less_thans (nonNegatives td_env) i j) ||^ IxFun.disjoint3 (fmap typeOf $ scope td_env) less_thans (nonNegatives td_env) i j) js) is
    ||^ trace ("it failed\n" <> pretty bla1 <> "\nhalløj?: " <> pretty bla2 <> "\n og: " <> pretty js) (return False)
  where
    less_thans = map (fmap $ fixPoint $ substituteInPrimExp $ scalarTable td_env) $ knownLessThan td_env
    is = map (fixPoint (IxFun.substituteInLMAD $ TPrimExp <$> scalarTable td_env)) $ S.toList is0
    js = map (fixPoint (IxFun.substituteInLMAD $ TPrimExp <$> scalarTable td_env)) $ S.toList js0

-- | Suppossed to aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1
--   The current implementation is naive, in that it
--     treats the case when the summary is loop-invariant,
--     and returns Undeterminable in the other cases.
--   The current implementation is good for while, but needs
--     to be refined for @for i < n@ loops
aggSummaryLoopTotal ::
  MonadFreshNames m =>
  ScopeTab rep ->
  ScopeTab rep ->
  ScalarTab ->
  Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
  AccessSummary ->
  m AccessSummary
aggSummaryLoopTotal _ _ _ _ Undeterminable = return Undeterminable
aggSummaryLoopTotal _ _ _ _ (Set l)
  | l == mempty = return $ Set mempty
aggSummaryLoopTotal scope_bef scope_loop scals_loop _ access
  | Set ls <- translateAccessSummary scope_loop scals_loop access,
    nms <- foldl (<>) mempty $ map freeIn $ S.toList ls,
    all inBeforeScope $ namesToList nms = do
    return $ Set ls
  where
    inBeforeScope v =
      case M.lookup v scope_bef of
        Nothing -> False
        Just _ -> True
aggSummaryLoopTotal _ _ scalars_loop (Just (iterator_var, (lower_bound, upper_bound))) (Set lmads) =
  mconcat
    <$> mapM
      ( aggSummaryOne iterator_var lower_bound upper_bound
          . fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars_loop)
      )
      (trace "aggSummaryLoopTotal" $ S.toList lmads)
aggSummaryLoopTotal _ _ _ _ _ = return Undeterminable

-- | Partially aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1. This means that it
--     computes the summary: Union_{j=i+1..n} Access_j
aggSummaryLoopPartial ::
  MonadFreshNames m =>
  ScopeTab rep ->
  ScopeTab rep ->
  ScalarTab ->
  Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
  AccessSummary ->
  m AccessSummary
aggSummaryLoopPartial _ _ _ _ Undeterminable = return Undeterminable
aggSummaryLoopPartial _ _ _ Nothing _ = return Undeterminable
aggSummaryLoopPartial _ _ scalars_loop (Just (iterator_var, (_, upper_bound))) (Set lmads) = do
  -- map over each index function in the access summary
  --   Substitube a fresh variable k for the loop iterator
  --   if k is in stride or span of ixfun: fall back to total
  --   new_stride = old_offset - old_offset (where k+1 is substituted for k)
  --   new_offset = old_offset where k = lower bound of iteration
  --   new_span = upper bound of iteration
  mconcat
    <$> mapM
      ( aggSummaryOne
          iterator_var
          (isInt64 (LeafExp iterator_var $ IntType Int64) + 1)
          (upper_bound - typedLeafExp iterator_var - 1)
          . fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars_loop)
      )
      (trace "aggSummaryLoopTotal" $ S.toList lmads)

aggSummaryMapPartial :: MonadFreshNames m => ScalarTab -> [(VName, SubExp)] -> AccessSummary -> m AccessSummary
aggSummaryMapPartial _ [] _ = return mempty
aggSummaryMapPartial _ _ (Set lmads)
  | lmads == mempty = return mempty
aggSummaryMapPartial scalars ((gtid, size) : rest) as = do
  total_inner <-
    foldM
      ( \as' (gtid', size') -> case as' of
          Set lmads ->
            mconcat
              <$> mapM
                ( aggSummaryOne gtid' 0 $
                    TPrimExp $
                      primExpFromSubExp (IntType Int64) size'
                )
                (S.toList lmads)
          Undeterminable -> return Undeterminable
      )
      as
      $ trace "aggSummaryMapPartial" $ reverse rest
  this_dim <- aggSummaryMapPartialOne scalars (gtid, size) $ traceWith "total_inner" total_inner
  rest' <- aggSummaryMapPartial scalars rest as
  return $ this_dim <> rest'

aggSummaryMapPartialOne :: MonadFreshNames m => ScalarTab -> (VName, SubExp) -> AccessSummary -> m AccessSummary
aggSummaryMapPartialOne _ _ Undeterminable = return Undeterminable
aggSummaryMapPartialOne scalars (gtid, size) (Set lmads0) =
  mapM
    helper
    [ (0, isInt64 (LeafExp gtid $ IntType Int64)),
      ( isInt64 (LeafExp gtid $ IntType Int64) + 1,
        isInt64 (primExpFromSubExp (IntType Int64) size) - isInt64 (LeafExp gtid $ IntType Int64) - 1
      )
    ]
    <&> mconcat
    <&> traceWith ("aggSummaryMapPartialOne. (gtid, size): " <> pretty (gtid, size) <> "\nlmads: " <> pretty lmads <> "\nresult")
  where
    lmads = map (fixPoint (traceWith "fixpoint" . (IxFun.substituteInLMAD $ fmap TPrimExp scalars))) $ S.toList lmads0
    helper (x, y) = mconcat <$> mapM (aggSummaryOne gtid x y) lmads

aggSummaryMapTotal :: MonadFreshNames m => ScalarTab -> [(VName, SubExp)] -> AccessSummary -> m AccessSummary
aggSummaryMapTotal _ [] _ = return mempty
aggSummaryMapTotal _ _ (Set lmads)
  | lmads == mempty = return mempty
aggSummaryMapTotal _ _ Undeterminable = return Undeterminable
aggSummaryMapTotal scalars segspace (Set lmads0) =
  foldM
    ( \as' (gtid', size') -> case as' of
        Set lmads' ->
          mconcat
            <$> mapM
              ( aggSummaryOne gtid' 0 $
                  TPrimExp $
                    primExpFromSubExp (IntType Int64) size'
              )
              (S.toList lmads')
        Undeterminable -> return Undeterminable
    )
    (Set lmads)
    (reverse segspace)
  where
    lmads =
      trace "aggSummaryMapTotal" $
        S.fromList $
          map (fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars)) $
            S.toList lmads0

aggSummaryOne :: MonadFreshNames m => VName -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> LmadRef -> m AccessSummary
aggSummaryOne iterator_var lower_bound spn lmad@(IxFun.LMAD offset0 dims0)
  | iterator_var `nameIn` freeIn dims0 = return Undeterminable
  | not $ iterator_var `nameIn` freeIn offset0 = return $ Set $ S.singleton lmad
  | otherwise = do
    new_var <- newVName "k"
    let offset = replaceIteratorWith (typedLeafExp new_var) offset0
        offsetp1 = replaceIteratorWith (typedLeafExp new_var + 1) offset0
        new_stride = TPrimExp $ constFoldPrimExp $ simplify $ untyped $ offsetp1 - offset
        new_offset = replaceIteratorWith lower_bound offset0
        new_lmad =
          IxFun.LMAD new_offset $
            IxFun.LMADDim new_stride 0 spn 0 IxFun.Inc : map incPerm dims0
    if new_var `nameIn` freeIn new_lmad
      then
        return $
          trace
            ( "aggSummaryOne: " <> pretty lmad
                <> "\niterator_var: "
                <> pretty iterator_var
                <> "\nlower_bound: "
                <> pretty lower_bound
                <> "\nspn: "
                <> pretty spn
                <> "\nnew_var: "
                <> pretty new_var
                <> "\noffset: "
                <> pretty offset
                <> "\noffsetp1: "
                <> pretty offsetp1
                <> "\nnew_stride: "
                <> pretty new_stride
                <> "\nnew_offset: "
                <> pretty new_offset
                <> "\nnew_lmad: "
                <> pretty new_lmad
            )
            Undeterminable
      else return $ Set $ S.singleton new_lmad
  where
    incPerm dim = dim {IxFun.ldPerm = IxFun.ldPerm dim + 1}
    replaceIteratorWith se = TPrimExp . substituteInPrimExp (M.singleton iterator_var $ untyped se) . untyped

typedLeafExp :: VName -> TPrimExp Int64 VName
typedLeafExp vname = isInt64 $ LeafExp vname (IntType Int64)

vname s = VName (nameFromString s) (sum $ map ord s)

var s = TPrimExp $ LeafExp (vname s) $ IntType Int64

gtid_8472 = TPrimExp $ LeafExp (foo "gtid" 8472) $ IntType Int64

gtid_8473 = TPrimExp $ LeafExp (foo "gtid" 8473) $ IntType Int64

gtid_8474 = TPrimExp $ LeafExp (foo "gtid" 8474) $ IntType Int64

num_blocks_8284 = TPrimExp $ LeafExp (foo "num_blocks" 8284) $ IntType Int64

add_nw64 = (+)

mul_nw64 = (*)

sub64 = (-)

sub_nw64 = (-)

foo s i = VName (nameFromString s) i

nonnegs = freeIn [gtid_8472, gtid_8473, gtid_8474, num_blocks_8284]

-- lm1 :: IxFun.LMAD (TPrimExp Int64 VName)
-- lm1 =
--   LMAD
--     ( add_nw64
--         (mul_nw64 (256) (num_blocks_8284))
--         (256)
--     )
--     [ LMADDim (mul_nw64 (num_blocks_8284) (256)) 0 (sub_nw64 (gtid_8472) (1)) 0 Inc,
--       LMADDim 256 0 (sub64 (num_blocks_8284) (1)) 1 Inc,
--       LMADDim 16 0 16 2 Inc,
--       LMADDim 1 0 16 3 Inc
--     ]

-- lm2 :: IxFun.LMAD (TPrimExp Int64 VName)
-- lm2 =
--   LMAD
--     ( add_nw64
--         ( add_nw64
--             ( add_nw64
--                 ( add_nw64
--                     (mul_nw64 (256) (num_blocks_8284))
--                     (256)
--                 )
--                 ( mul_nw64
--                     (gtid_8472)
--                     (mul_nw64 (256) (num_blocks_8284))
--                 )
--             )
--             (mul_nw64 (gtid_8473) (256))
--         )
--         (mul_nw64 (gtid_8474) (16))
--     )
--     [LMADDim 1 0 16 0 Inc]

j_m_i_8287 :: TPrimExp Int64 VName
j_m_i_8287 = num_blocks_8284 - 1

lessthans :: [(VName, PrimExp VName)]
lessthans =
  [ (head $ namesToList $ freeIn gtid_8472, untyped j_m_i_8287),
    (head $ namesToList $ freeIn gtid_8473, untyped j_m_i_8287),
    (head $ namesToList $ freeIn gtid_8474, untyped (16 :: TPrimExp Int64 VName))
  ]

segspcs :: [(VName, SubExp)]
segspcs =
  [ (head $ namesToList $ freeIn gtid_8472, Var $ foo "j_m_i" 8287),
    (head $ namesToList $ freeIn gtid_8473, Var $ foo "j_m_i" 8287),
    (head $ namesToList $ freeIn gtid_8474, Constant $ IntValue $ Int64Value 16)
  ]

scalst :: ScalarTab
scalst = M.fromList [(foo "j_m_i" 8287, untyped $ (sub64 (num_blocks_8284) 1 :: TPrimExp Int64 VName))]

-- {offset: 256i64; strides: [256i64, 1i64, 16i64]; rotates: [0i64, 0i64, 0i64];
--   shape: [sub64 (num_blocks_8284) (1i64), 16i64, 16i64]; permutation: [0, 1, 2];
--   monotonicity: [Inc, Inc, Inc]

-- One of these
--
-- [{offset: add_nw64 (add_nw64 (mul_nw64 (add_nw64 (1i64) (gtid_8472)) (mul_nw64 (256i64) (num_blocks_8284))) (mul_nw64 (add_nw64 (1i64) (gtid_8473)) (256i64))) (mul_nw64 (gtid_8474) (16i64));
--   strides: [1i64]; rotates: [0i64]; shape: [16i64]; permutation: [0];
--   monotonicity: [Inc]},
--  {offset: add_nw64 (mul_nw64 (add_nw64 (1i64) (gtid_8472)) (mul_nw64 (256i64) (num_blocks_8284))) (mul_nw64 (gtid_8474) (16i64));
--   strides: [1i64]; rotates: [0i64]; shape: [16i64]; permutation: [0];
--   monotonicity: [Inc]},
--  {offset: mul_nw64 (add_nw64 (1i64) (gtid_8473)) (256i64);
--   strides: [1i64, 16i64]; rotates: [0i64, 0i64]; shape: [16i64, 16i64];
--   permutation: [0, 1]; monotonicity: [Inc, Inc]}]
--
-- turn into
--
-- {offset: add_nw64 (mul_nw64 (256i64) (num_blocks_8284)) (256i64);
--  strides: [mul_nw64 (num_blocks_8284) (256i64), 256i64, 16i64, 1i64];
--  rotates: [0i64, 0i64, 0i64, 0i64];
--  shape: [sub_nw64 (gtid_8472) (1i64), sub64 (num_blocks_8284) (1i64), 16i64, 16i64];
--  permutation: [0, 1, 2, 3]; monotonicity: [Inc, Inc, Inc, Inc]}

lm1 :: IxFun.LMAD (TPrimExp Int64 VName)
lm1 =
  IxFun.LMAD
    ( add_nw64
        ( add_nw64
            ( mul_nw64
                (add_nw64 (1) (gtid_8472))
                (mul_nw64 (256) (num_blocks_8284))
            )
            (mul_nw64 (add_nw64 (1) (gtid_8473)) (256))
        )
        (mul_nw64 (gtid_8474) (16))
    )
    [IxFun.LMADDim 1 0 16 0 IxFun.Inc]

lm2 :: IxFun.LMAD (TPrimExp Int64 VName)
lm2 =
  IxFun.LMAD
    ( add_nw64
        ( add_nw64
            ( add_nw64
                ( add_nw64
                    (mul_nw64 (256) (num_blocks_8284))
                    (256)
                )
                ( mul_nw64
                    (gtid_8472)
                    (mul_nw64 (256) (num_blocks_8284))
                )
            )
            (mul_nw64 (gtid_8473) (256))
        )
        (mul_nw64 (gtid_8474) (16))
    )
    [IxFun.LMADDim 1 0 16 0 IxFun.Inc]
