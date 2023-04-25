module Futhark.Internalise.FullNormalise (transformProg) where

import Futhark.MonadFreshNames
import Language.Futhark

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg = pure
