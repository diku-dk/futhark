-- | A convenient re-export of basic AST modules.
module Futhark.IR
  ( module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
  )
where

import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.Syntax
import Futhark.IR.Traversals
