module Futhark.CodeGen.ImpGen.Multicore.SegMap
  (compileSegMap
  )
  where

import Control.Monad

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.MCMem
import Futhark.CodeGen.ImpGen.Multicore.Base


compileSegMap :: Pattern MCMem
              -> SegSpace
              -> KernelBody MCMem
              -> MulticoreGen ()
compileSegMap pat space (KernelBody _ kstms kres) = do
  emit $ Imp.DebugPrint "SegMap " Nothing
  sUnpauseProfiling

  flat_idx <- dPrim "iter" int32

  tid <- dPrim "tid" int32
  tid' <- toExp $ Var tid

  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns
  num_tasks <- dPrim "ntask" $ IntType Int32

  body' <- collect $ do
   emit $ Imp.DebugPrint "SegMap fbody" Nothing
   dPrimV_ (segFlat space) tid'
   zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 flat_idx
   compileStms (freeIn kres) kstms $ do
     let writeResult pe (Returns _ se) =
           copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
         writeResult pe (WriteReturns dims _ idx_vals) = do
           let (iss, vs) = unzip idx_vals
           dims' <- mapM toExp dims
           forM_ (zip iss vs) $ \(idx, v) -> do
             is' <- mapM toExp idx
             let in_bounds = foldl1 (.&&.) ( zipWith (.<.) is' dims') .&&.
                             foldl1 (.&&.) ( map (0.<=.) is')
                 when_in_bounds = copyDWIMFix (patElemName pe) is' v []
             sWhen in_bounds when_in_bounds

         writeResult _ res =
           error $ "writeResult: cannot handle " ++ pretty res
     zipWithM_ writeResult (patternElements pat) kres
  let freeVariables = namesToList (freeIn body' `namesSubtract` namesFromList [tid, flat_idx])
  ts <- mapM lookupType freeVariables
  let freeParams = zipWith toParam freeVariables ts
      scheduling = decideScheduling body'

  let (body_allocs, body'') = extractAllocations body'

  emit $ Imp.Op $ Imp.ParLoop scheduling num_tasks flat_idx (product ns')
                             (Imp.MulticoreFunc freeParams body_allocs body'' tid)
