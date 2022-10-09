-- | Sequential imperative code.
module Futhark.CodeGen.ImpCode.Sequential
  ( Program,
    Sequential,
    module Futhark.CodeGen.ImpCode,
  )
where

import Futhark.CodeGen.ImpCode
import Futhark.Util.Pretty

-- | An imperative program.
type Program = Definitions Sequential

-- | Phantom type for identifying sequential imperative code.
data Sequential

instance Pretty Sequential where
  pretty _ = mempty

instance FreeIn Sequential where
  freeIn' _ = mempty
