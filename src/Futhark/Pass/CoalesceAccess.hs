{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.CoalesceAccess (coalesceAccess) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Builder
import Futhark.IR.GPU
import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename

type CoalesceM = ReaderT (Scope GPU) (State VNameSource)

-- | The pass definition.
coalesceAccess :: Pass GPU GPU
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- \$ return
    $ \prog ->
      let prog' = analysis prog
       in intraproceduralTransformation onStms prog'
  where
    onStms scope stms =
      let a = undefined
       in modifyNameSource $
            runState $
              runReaderT (analyseStms (M.empty, M.empty) stms) scope

analysis :: Prog GPU -> Prog GPU
analysis prog = do
  let funaids = analyzeMemoryAccessPatterns prog
   in prog
