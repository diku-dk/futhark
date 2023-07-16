-- | General implementation of GPU copying, using LMAD representation.
-- That means the dynamic performance of this kernel depends crucially
-- on the LMAD.  In most cases we should use a more specialised kernel.
-- Written in ImpCode so we can compile it to both CUDA and OpenCL.
module Futhark.CodeGen.ImpGen.GPU.Copy (copyKernel) where

import Control.Monad
import Control.Monad.State
import Data.Foldable (toList)
import Futhark.CodeGen.ImpCode.GPU
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.Prop.Reshape
import Futhark.MonadFreshNames
import Futhark.Util (nubOrd)
import Futhark.Util.IntegralExp (divUp)
import Prelude hiding (quot, rem)

copyKernel ::
  PrimType ->
  (TExp Int64, GroupDim) ->
  (VName, LMAD.LMAD (TExp Int64)) ->
  (VName, LMAD.LMAD (TExp Int64)) ->
  Kernel
copyKernel pt (num_groups, group_dim) (dest_mem, dest_lmad) (src_mem, src_lmad) =
  Kernel
    { kernelBody = body,
      kernelUses =
        let frees =
              nubOrd
                ( foldMap toList dest_lmad
                    <> foldMap toList src_lmad
                    <> toList num_groups
                )
         in map (`ScalarUse` IntType Int64) frees
              ++ map MemoryUse [dest_mem, src_mem],
      kernelNumGroups = [untyped num_groups],
      kernelGroupSize = [group_dim],
      kernelName = nameFromString ("copy_" <> show rank <> "d_" <> prettyString pt),
      kernelFailureTolerant = True,
      kernelCheckLocalMemory = False
    }
  where
    shape = LMAD.shape dest_lmad
    rank = length shape

    body = flip evalState (newNameSource 1000) $ do
      group_id <- newVName "group_id"
      local_id <- newVName "local_id"
      local_size <- newVName "local_size"
      global_id <- newVName "global_id"
      group_iter <- newVName "group_iter"
      let global_id_e =
            ((le64 group_id + le64 group_iter * num_groups) * le64 local_size)
              + le64 local_id

      is <- replicateM rank $ newVName "i"
      let is_e = map untyped (unflattenIndex shape (le64 global_id))
          in_bounds = foldl1 (.&&.) (zipWith (.<.) (map le64 is) shape)

      element <- newVName "element"

      let dec v = DeclareScalar v Nonvolatile $ IntType Int64
          src_o = Count (LMAD.index src_lmad (map le64 is))
          dest_o = Count (LMAD.index dest_lmad (map le64 is))
          copy_elem =
            DeclareScalar element Nonvolatile pt
              <> Read element src_mem src_o pt (Space "device") Nonvolatile
              <> Write dest_mem dest_o pt (Space "device") Nonvolatile (LeafExp element pt)
      pure $
        foldMap dec [group_id, local_id, local_size, global_id]
          <> Op (GetLocalId local_id 0)
          <> Op (GetLocalSize local_size 0)
          <> Op (GetGroupId group_id 0)
          <> For
            group_iter
            (untyped (product shape `divUp` (le64 local_size * num_groups)))
            ( SetScalar global_id (untyped global_id_e)
                <> foldMap dec is
                <> mconcat (zipWith SetScalar is is_e)
                <> If in_bounds copy_elem mempty
            )
