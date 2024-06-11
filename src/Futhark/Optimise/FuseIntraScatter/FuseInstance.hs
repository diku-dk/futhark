{-# LANGUAGE TypeFamilies #-}

-- | Tries to fuse an instance of a scatter-like kernel with an
--     intra-block kernel that has produced the indices and values
--     for the scatter.
module Futhark.Optimise.FuseIntraScatter.FuseInstance (fuseInstance) where

-- import Control.Monad
-- import Control.Monad.Reader
-- import Control.Monad.State
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence (Seq (..))
-- import Futhark.Builder
import Futhark.IR.GPU
-- import Futhark.Optimise.TileLoops.Shared
-- import Futhark.Pass
-- import Futhark.IR.Aliases
-- import Futhark.Analysis.Alias qualified as AnlAls
-- import Futhark.Analysis.LastUse
-- import Futhark.Tools
-- import Futhark.Transform.Rename
-- import Futhark.Pass (Pass (..))
-- import Futhark.Util
import Futhark.Optimise.FuseIntraScatter.DataStructs
import Debug.Trace

fuseInstance :: FISEnv -> Stms GPU -> Stm GPU -> FuseIScatM (Maybe (Stms GPU, Stms GPU, Stms GPU))
fuseInstance env body_stms scat_stm@(Let pat _aux (Op (SegOp scat_ker)))
  | SegMap SegThread {} _space _kertp kbody@(KernelBody _bdec _kstms kres) <- scat_ker,
    dst_idx_vals <- mapMaybe getScatterResult kres,
    length dst_idx_vals == length kres,
    -- ^ check that all results are WriteReturns
    fvs <- freeIn kbody,
    fst_pe : _rest_pes <- patElems pat,
    Just lastuses <- M.lookup (patElemName fst_pe) (lutab1 env),
    lus <- filter (`nameIn` lastuses) (namesToList fvs),
    -- ^ the names lastly used in the scatter statement
    not (null lus),
    Just (_stms_before0, intra_stm, stms_interm0, _stms_after0) <-
      splitStmsOnScatter lastuses scat_stm body_stms,
    dst_nms <- namesFromList $ map fst dst_idx_vals,
    (stms_dst, _stms_interm0') <- separateDestInits dst_nms stms_interm0,
    -- ^ the body of statements is split into the statements that:
    --   the ones that come before the to-be-fused-in intragroup statement,
    --   the ones that comes in between the intra-group and scatter and
    --       are further separated between the ones that serve as target
    --       to scatter's write and the rest
    --   and the ones that come after the scatter statement
    dst_nms' <- foldl (<>) mempty $ map patNmsFromStm $ stmsToList stms_dst,
    free_in_interm_intra <- freeIn (stms_interm0 <> oneStm intra_stm),
    not $ namesIntersect dst_nms' free_in_interm_intra,
    -- ^ check that statements such as (replicate n 0) that produce the
    --   target array of scatter can be moved before the intragroup kernel
    res_intra_nms <- patNmsFromStm intra_stm,
    not $ namesIntersect (namesSubtract fvs lastuses) res_intra_nms,
    -- ^ check that all free variables of the scatter that are produced
    --   by the intragroup kernel are lastly-used in scatter.
    fused_nms <- namesIntersection res_intra_nms (namesFromList lus),
    Just (_before, segop_stm, _after, fused_nms_in_segop) <-
        findPostDomSegOp (namesToList fused_nms) intra_stm
    = do
  -- here comes the implementation:
  -- ToDo: 1. check that the segop_stm has the same length as
  --          the inds-vals array of scatter, i.e., blockDim.x * num-blocks
  --       2. make a substitution Map: [ inds -> Scalar inds0, vals -> Array vals0 ]
  --       3. defined scatter's gtid = blockIdx.x * blockDim.x + threadIdx.x
  --       4. check that in scatter's body all accesses to inds and vals
  --          are of the form below and perform the substitutions below:
  --             inds[blockIdx.x, threadIdx.x] with inds0 &&
  --             vals[blockIdx.x, threadIdx.x] with vals0[threadIdx.x]
  --       5. also figure out that:
  --             gtid / blockDim.x === blockIdx.x   &&
  --             gtid % blockIdx.x === threadIdx.x
  trace ("\nstm_scatter:\n" ++ prettyString scat_stm  ++
         "\nsegop_stm:\n"   ++ prettyString segop_stm ++
         "\nfused-names-in-segop: " ++ show fused_nms_in_segop
        ) $ return Nothing
{--
  trace ("stms_after:\n"    ++ prettyString stms_after   ++ 
         "stm_scatter:\n"   ++ prettyString scat_stm     ++
         "\nstms_dst:\n"    ++ prettyString stms_dst     ++
         "\nstms_interm:\n" ++ prettyString stms_interm' ++
         "\nstm_intra:\n"   ++ prettyString intra_stm    ++
         "\nstm_before:\n"  ++ prettyString stms_before  ++
         "\ndst_nms: " ++ show dst_nms'                  ++
         "\nfree-in-intemr: " ++ show free_in_interm_intra ++
         "\nlast-used in scatter: " ++ show lus ++
         "\nfused-names: " ++ show fused_nms ++
         "\nsegop-fused: " ++ prettyString segop_stm
        )
--}
  where
    getScatterResult (WriteReturns _ dst_nm idx_vals) =
      Just (dst_nm, idx_vals)
    getScatterResult _ = Nothing

fuseInstance _ _ _ = return Nothing



findPostDomSegOp :: [VName] -> Stm GPU -> Maybe (Stms GPU, Stm GPU, Stms GPU, Names)
findPostDomSegOp fused_nms stm_intra@(Let _ _ (Op (SegOp segmap0) ))
  | SegMap segblk0 _ tps0 kbody0 <- segmap0,
    SegBlock _v (Just _grid) <- segblk0,
    KernelBody _ kstms0 kres0 <- kbody0,
    kprs0  <- zip3 (namesToList (patNmsFromStm stm_intra)) kres0 tps0,
    ckprs0 <- mapMaybe (getCorrespResult kprs0) fused_nms,
    length ckprs0 == length fused_nms,    
    -- ^ `ckrps` holds the (pattern-name, result-name) 
    --   corresponding to each of the `fused_nms`, each
    --   result being a "normal" return
    ckr0_nm_lst <- map (snd . fst) ckprs0,
    ckr0_nms <- namesFromList ckr0_nm_lst,
    stm0_lst <- L.reverse $ stmsToList kstms0,
    Just i  <- L.findIndex (fusedStmCand ckr0_nms) stm0_lst,
    -- ^ the index of the to-be-fused in segmap
    --   ToDo: this is very conservative: if such a statement
    --         is not found, one may still fuse by introducing
    --         a new segop at the end of the intra-group kernel.
    segmap_stm  <- stm0_lst !! i,
    Let _ _ (Op (SegOp segmap1) ) <- segmap_stm,
    SegMap (SegThreadInBlock {}) _ tps1 kbody1 <- segmap1,
    KernelBody _ _ krs1 <- kbody1
    =
    let patt_nms = patNmsFromStm segmap_stm
        after  = reverse $ take i stm0_lst
        before = reverse $ drop (i+1) stm0_lst
        before_nms = mconcat (patt_nms : map patNmsFromStm before)
        safety1 = mempty == namesSubtract ckr0_nms before_nms
    -- ^ checks that `segmap_stm` is a postdominator for all the
    --   other to-be-fused names, not produced by `segmap_stm`
        pat_res_nms = zip3 (namesToList patt_nms) krs1 tps1
        matching = mapMaybe (getCorrespResult pat_res_nms) ckr0_nm_lst
        fused_segop_res_nms = namesFromList $ map (snd . fst) matching
        safety2 = length matching == length (namesToList (namesIntersection ckr0_nms patt_nms))
        safety3 = all primType (map snd matching)
    -- ^ checks that all to-be-fused arrays produced by this segop
    --   have "normal" return AND each thread produces a PrimType value.
    in  if safety1 && safety2 && safety3
        then Just (stmsFromList before, segmap_stm, stmsFromList after, fused_segop_res_nms)
        else Nothing
        -- trace ("PostDOM: NNAYYY!!! fused_nms: "++show ckr0_nms ++ "before_nms: "++show before_nms) $ 
  where
    getCorrespResult krps nm =
      case L.find (\(pp,_,_) -> pp == nm) krps of
        Just (pp, Returns ResultMaySimplify _ (Var res_nm), tp) -> Just ((pp, res_nm), tp)
        _ -> Nothing
findPostDomSegOp _ _ = Nothing

------------------------
--- Helper functions ---
------------------------

patNmsFromStm :: Stm GPU -> Names
patNmsFromStm = namesFromList . map patElemName . patElems . stmPat

splitStmsOnScatter :: Names -> Stm GPU -> Stms GPU -> Maybe (Stms GPU, Stm GPU, Stms GPU, Stms GPU)
splitStmsOnScatter lu_scat scat_stm stms =
  let fv_scat = namesIntersection lu_scat $ freeIn scat_stm
      (bef, m_intra, interm, after, i) = 
        foldl (fop fv_scat) (Empty, Nothing, Empty, Empty, 0) $
              reverse $ stmsToList stms
  in  case (m_intra, i) of
        (Just intra, 2) -> Just (bef, intra, interm, after)
        _ -> Nothing
  where
    fop :: Names -> (Stms GPU, Maybe (Stm GPU), Stms GPU, Stms GPU, Integer) ->
           Stm GPU -> (Stms GPU, Maybe (Stm GPU), Stms GPU, Stms GPU, Integer)
    fop _ (bef, intra, interm, after, 0) stm
      | not (stm == scat_stm) = (bef, intra, interm, oneStm stm <> after, 0)
      |     (stm == scat_stm) = (bef, intra, interm, after, 1)
    fop lufvs (bef, _, interm, after, 1) stm
      | Let p _a (Op (SegOp segmap) ) <- stm,
        SegMap segblk _space _ts _kbody <- segmap,
        SegBlock _v (Just _grid) <- segblk,
        intra_res_nms <- namesFromList $ map patElemName (patElems p),
        namesIntersect intra_res_nms lufvs = do
      (bef, Just stm, interm, after, 2)
    fop _ (bef, intra, interm, after, 1) stm =
      (bef, intra, oneStm stm <> interm, after, 1)
    fop _ (bef, intra, interm, after, 2) stm =
      (oneStm stm <> bef, intra, interm, after, 2)
    fop _ acc _ = acc
        
separateDestInits :: Names -> Stms GPU -> (Stms GPU, Stms GPU)
separateDestInits nms stms =
  let (dst_stms, other_stms) = L.partition isDest $ stmsToList stms
  in  (stmsFromList dst_stms, stmsFromList other_stms)
  where
    isDest stm =
      namesIntersect nms $
      namesFromList $
      map patElemName $
      patElems $
      stmPat stm

fusedStmCand :: Names -> Stm GPU -> Bool
fusedStmCand target_nms (Let patt _ (Op (SegOp seg) ))
  | SegMap (SegThreadInBlock {}) _ _ _ <- seg =
    namesIntersect target_nms $
    namesFromList $
    map patElemName $
    patElems patt
fusedStmCand _ _ = False

