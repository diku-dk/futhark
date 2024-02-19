module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Control.Monad.RWS.Strict hiding (Sum)
import qualified Data.Map as M
import Debug.Trace (trace)
import Futhark.Util.Pretty (prettyString)

substituteViews :: View -> ViewM View
substituteViews view = do
  knownViews <- gets views
  pure $ idMap (m knownViews) view
  where
    m vs =
      ASTMapper
        { mapOnExp = onExp vs,
          mapOnView = astMap (m vs)
        }
    -- onView vs (Forall i dom e) = astMap m e
    onExp vs e@(Var x) = pure e
    onExp vs e@(Idx (Var xs) i) =
      case M.lookup xs vs of
        -- XXX check that domains are compatible
        -- XXX use index i (for starts, just support simple indexing only?)
        -- XXX merge cases (add cases first, lol)
        Just (Forall j d2 e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
          pure e2
        _ -> pure e
    onExp vs v = astMap (m vs) v
