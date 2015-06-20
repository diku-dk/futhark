-- | A convenient re-export of basic AST modules.  Note that
-- "Futhark.Representation.AST.Lore" is not exported, as this would
-- cause name clashes.  You are advised to use a qualified import of
-- the lore module, if you need it.
module Futhark.Representation.AST
       ( module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       )
where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes hiding (Lore)
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
