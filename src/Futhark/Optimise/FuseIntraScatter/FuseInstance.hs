{-# LANGUAGE TypeFamilies #-}

-- | Tries to fuse an instance of a scatter-like kernel with an
--     intra-block kernel that has produced the indices and values
--     for the scatter.
module Futhark.Optimise.FuseIntraScatter.FuseInstance (fuseInstance) where

-- import Control.Monad
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Data.List qualified as L
-- import Data.Map.Strict qualified as M
-- import Data.Maybe
-- import Data.Sequence (Seq (..))
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

fuseInstance :: Stms GPU -> Stm GPU -> FuseIScatM (Maybe (Stms GPU, Stm GPU, Stms GPU, Stm GPU, Stms GPU, Stm GPU))
fuseInstance stms smt = return Nothing

