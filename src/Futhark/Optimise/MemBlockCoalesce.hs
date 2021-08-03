{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce (memoryBlockMerging) where

import qualified Futhark.Analysis.Alias as AnlAls
import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as ExpMem
import Futhark.Optimise.MemBlockCoalesce.ArrayCoalescing
import Prelude

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

-- run it with:  `futhark dev --cpu --merge-mem test.fut`
memoryBlockMerging :: ExpMem.Prog ExpMem.SeqMem -> IO ()
memoryBlockMerging prg = do
  mapM_ lookAtFunction (progFuns prg)

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab) ++ "\n" ++ pretty coaltab

lookAtFunction :: ExpMem.FunDef ExpMem.SeqMem -> IO ()
lookAtFunction fdef = do
  putStrLn $ "Function:\n" ++ pretty fdef
