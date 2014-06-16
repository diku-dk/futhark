module Futhark.Passes
  ( CompileError(..)
  , FutharkM
  , compileError
  , Pass(..)
  , canFail
  , fotransform
  , uttransform
  , eotransform
  , hotransform
  , inlinetransform
  , optimisePredicates
  , optimiseShapes
  )
where

import Futhark.Optimise.SimpleOpts
import Futhark.Optimise.Fusion
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Untrace
import Futhark.Pipeline
import qualified Futhark.Optimise.SuffCond
import qualified Futhark.Optimise.SplitShapes

fotransform :: Pass
fotransform = Pass { passName = "first-order transform"
                   , passOp = return . FOT.transformProg
                   }

uttransform :: Pass
uttransform = Pass { passName = "debugging annotation removal"
                   , passOp = return . untraceProg
                   }

eotransform :: Pass
eotransform = Pass { passName = "enabling optimations"
                   , passOp = liftPass simpleOpts
                   }

hotransform :: Pass
hotransform = Pass { passName = "higher-order optimisations"
                   , passOp = liftPass fuseProg
                   }

inlinetransform :: Pass
inlinetransform = Pass { passName = "inline functions"
                      , passOp = liftPass aggInlineDriver
                      }

optimisePredicates :: Pass
optimisePredicates = Pass { passName = "optimise predicates"
                          , passOp = return . Futhark.Optimise.SuffCond.optimiseProg
                          }

optimiseShapes :: Pass
optimiseShapes = Pass { passName = "optimise shape analysis"
                      , passOp = return . Futhark.Optimise.SplitShapes.splitShapes
                      }
