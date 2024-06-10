{-# LANGUAGE TypeFamilies #-}

-- | Tries to fuse an instance of a scatter-like kernel with an
--     intra-block kernel that has produced the indices and values
--     for the scatter.
module Futhark.Optimise.FuseIntraScatter.DataStructs (FuseIScatM, TopDownEnv(..), BottomUpEnv(..)) where

-- import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
-- import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence (Seq (..))
import Futhark.Builder
import Futhark.IR.GPU
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
-- import Futhark.IR.Aliases
import Futhark.Analysis.Alias qualified as AnlAls
import Futhark.Analysis.LastUse
import Futhark.Tools
-- import Futhark.Transform.Rename
-- import Futhark.Pass (Pass (..))
import Futhark.Pass qualified as Pass
-- import Futhark.Util
import Debug.Trace

type FuseIScatM = ReaderT (Scope GPU) (State VNameSource)

data TopDownEnv = TopDownEnv
  { -- | expansions of scalar names to prim expressions
    scalarTable :: M.Map VName (PrimExp VName),
    -- | A list of known relations of the form 'VName' @<@ 'SubExp', typically
    -- gotten from 'LoopForm' and 'SegSpace'.
    knownLessThan :: [(VName, PrimExp VName)]
  }
  
data BottomUpEnv rep = BottomUpEnv
  { -- | the last-use table
    lutab :: LUTabFun, -- M.Map VName Names
    scatters :: Stms rep,
    inbetween :: Stms rep
  }
