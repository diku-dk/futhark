-- | A convenient re-export of basic AST modules.  Note that
-- "Futhark.IR.Lore" is not exported, as this would
-- cause name clashes.  You are advised to use a qualified import of
-- the lore module, if you need it.
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
