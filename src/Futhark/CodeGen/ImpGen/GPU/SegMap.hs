{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegMap' is quite straightforward.  The only
-- trick is virtualisation in case the physical number of threads is
-- not sufficient to cover the logical thread space.  This is handled
-- by having actual threadblocks run a loop to imitate multiple threadblocks.
module Futhark.CodeGen.ImpGen.GPU.SegMap (compileSegMap) where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.Block
import Futhark.IR.GPUMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Util.IntegralExp (ceilDiv)
import Prelude hiding (quot, rem)

-- | For each block result that is opted into global memory, map the
-- kernel-local memory block to the global memory block and byte offset
-- it should alias.
--
-- The block body writes the result using the tile's own layout, so the
-- alias offset is the address of the block's tile in the global result,
-- as given by the result's index function.  This is correct because
-- both the tile and the global result are directly laid out, which we
-- check; otherwise the result is staged in shared memory and copied out
-- as usual.
globalResultAliases ::
  SegSpace ->
  Stms GPUMem ->
  Pat LetDecMem ->
  [KernelResult] ->
  InKernelGen (M.Map VName (VName, Imp.TExp Int64))
globalResultAliases space stms pat res = do
  attrs <- askAttrs
  if not (hasIntrablockResultGlobal attrs)
    then pure mempty
    else do
      let returns = filter isReturns res
          aliases = mconcat $ zipWith onResult (patElems pat) res
      -- Warn if the attribute cannot do anything, so that the fallback
      -- to shared memory is not silent.
      when (M.null aliases && not (null returns)) noEffectWarning
      -- A loop-carried result is written through several memory blocks
      -- (the initial accumulator value and each iteration's result), all
      -- of which stand for the same global slice.  For a single result
      -- we can therefore map every alias-space memory in the kernel to
      -- that result's slice.
      pure $ case (returns, M.toList aliases) of
        ([_], [(_, slice)]) -> mapAllAliasMems slice
        _ -> aliases
  where
    isReturns Returns {} = True
    isReturns _ = False

    mapAllAliasMems slice =
      M.fromList
        [ (name, slice)
          | (name, LetName (MemMem mem_space)) <- M.toList (scopeOf stms),
            mem_space == intrablockResultSpace
        ]

    noEffectWarning = do
      Imp.Provenance locs loc <- askProvenance
      warn loc (reverse locs) $
        T.pack
          "The intrablock_result(global) attribute has no effect: the block's \
          \result cannot be placed in global memory, because it is not a \
          \directly laid out, freshly allocated array."

    body_scope = scopeOf stms
    gids = map (Imp.le64 . fst) $ unSegSpace space
    onResult pe (Returns _ _ (Var what)) =
      case (M.lookup what body_scope, patElemDec pe) of
        ( Just (LetName (MemArray pt _ _ (ArrayIn what_mem what_lmad))),
          MemArray _ _ _ (ArrayIn pe_mem pe_lmad)
          ) ->
            case M.lookup what_mem body_scope of
              -- The tile lives in the intra-block result space, which
              -- is bound to its slice of the global result.  This is
              -- only correct when both the tile and the global result
              -- are directly laid out, so that the tile's byte offset
              -- in the global result is a constant.
              Just (LetName (MemMem mem_space))
                | mem_space == intrablockResultSpace,
                  LMAD.isDirect what_lmad,
                  LMAD.isDirect pe_lmad ->
                    let offset = LMAD.index pe_lmad gids * primByteSize pt
                     in M.singleton what_mem (pe_mem, offset)
              _ -> mempty
        _ -> mempty
    onResult _ _ = mempty

-- | Compile 'SegMap' instance code.
compileSegMap ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegMap pat lvl space kbody = do
  attrs <- lvlKernelAttrs lvl

  let (is, dims) = unzip $ unSegSpace space
      dims' = map pe64 dims
      tblock_size' = pe64 <$> kAttrBlockSize attrs

  emit $ Imp.DebugPrint "\n# SegMap" Nothing
  case lvl of
    SegThread {} -> do
      virt_num_tblocks <- dPrimVE "virt_num_tblocks" $ sExt32 $ product dims' `ceilDiv` unCount tblock_size'
      sKernelThread "segmap" (segFlat space) attrs $
        virtualiseBlocks (segVirt lvl) virt_num_tblocks $ \tblock_id -> do
          local_tid <- kernelLocalThreadId . kernelConstants <$> askEnv

          global_tid <-
            dPrimVE "global_tid" $
              sExt64 tblock_id * sExt64 (unCount tblock_size')
                + sExt64 local_tid

          dIndexSpace (zip is dims') global_tid

          sWhen (isActive $ unSegSpace space) $
            compileStms mempty (bodyStms kbody) $
              zipWithM_ (compileThreadResult space) (patElems pat) $
                bodyResult kbody
    SegBlock {} -> do
      pc <- precomputeConstants tblock_size' $ bodyStms kbody
      virt_num_tblocks <- dPrimVE "virt_num_tblocks" $ sExt32 $ product dims'
      sKernelBlock "segmap_intrablock" (segFlat space) attrs $ do
        precomputedConstants pc $
          virtualiseBlocks (segVirt lvl) virt_num_tblocks $ \tblock_id -> do
            dIndexSpace (zip is dims') $ sExt64 tblock_id

            aliases <- globalResultAliases space (bodyStms kbody) pat $ bodyResult kbody
            localEnv (\env -> env {kernelGlobalResultAliases = aliases <> kernelGlobalResultAliases env}) $
              compileStms mempty (bodyStms kbody) $
                zipWithM_ (compileBlockResult space) (patElems pat) $
                  bodyResult kbody
    SegThreadInBlock {} ->
      error "compileSegMap: SegThreadInBlock"
  emit $ Imp.DebugPrint "" Nothing
