-- | Re-export the external Futhark modules for convenience.
module Language.Futhark
  ( module Language.Futhark.Syntax,
    module Language.Futhark.Prop,
    module Language.Futhark.FreeVars,
    module Language.Futhark.Pretty,
  )
where

import Language.Futhark.FreeVars
import Language.Futhark.Pretty
import Language.Futhark.Prop
import Language.Futhark.Syntax
