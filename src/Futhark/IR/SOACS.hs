{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | A simple representation with SOACs and nested parallelism.
module Futhark.IR.SOACS
  ( SOACS,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
    module Futhark.IR.SOACS.SOAC,
  )
where

import Futhark.Builder
import Futhark.Construct
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import qualified Futhark.IR.TypeCheck as TC

-- | The rep for the basic representation.
data SOACS

instance RepTypes SOACS where
  type Op SOACS = SOAC SOACS

instance ASTRep SOACS where
  expTypesFromPat = return . expExtTypesFromPat

instance TC.CheckableOp SOACS where
  checkOp = typeCheckSOAC

instance TC.Checkable SOACS

instance Buildable SOACS where
  mkBody = Body ()
  mkExpPat merge _ = basicPat merge
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BuilderOps SOACS

instance PrettyRep SOACS
