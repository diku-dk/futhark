module Futhark.CodeGen.ImpGen.Multicore.SegMap
  (compileSegMap
  )
  where

import Control.Monad

import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.ImpGen.Multicore.Base


compileSegMap :: Pattern ExplicitMemory
              -> SegSpace
              -> KernelBody ExplicitMemory
              -> MulticoreGen ()
compileSegMap pat space (KernelBody _ kstms kres) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  num_tasks <- dPrim "ntask" $ IntType Int32

  body' <- collect $ do
   zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
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
                 when_oob = return ()
             sIf in_bounds when_in_bounds when_oob

         writeResult _ res =
           error $ "writeResult: cannot handle " ++ pretty res
     zipWithM_ writeResult (patternElements pat) kres

  let paramsNames = namesToList (freeIn body' `namesSubtract` freeIn [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith toParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoop num_tasks (segFlat space) (product ns')
                              (Imp.MulticoreFunc params mempty body' num_tasks)
