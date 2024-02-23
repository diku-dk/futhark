module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Control.Monad.RWS.Strict hiding (Sum)
import qualified Data.Map as M
import Debug.Trace (trace)
import Futhark.Util.Pretty (prettyString)
-- import Control.Monad.Trans.State.Lazy qualified as S

substituteViews :: View -> ViewM View
substituteViews view = do
  knownViews <- gets views
  pure $ idMap (m knownViews) view
  where
    m vs =
      ASTMapper
        { mapOnExp = onExp vs }
    onExp _ e@(Var {}) = pure e
    onExp vs e@(Idx (Var xs) i) =
      case M.lookup xs vs of
        -- XXX check that domains are compatible
        -- XXX use index i (for starts, just support simple indexing only?)
        -- XXX merge cases (add cases first, lol)
        Just (View (Forall j d2) e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
          undefined
        _ -> pure e
    onExp vs v = astMap (m vs) v

-- Hoists case expressions to be the outermost contructor
-- in the view expression by merging cases.
-- 1. seems like a fold where the accumulator is the new Exp
--    that always maintains an outermost Case "invariant"
-- hoistCases :: View -> ViewM View
-- -- hoistCases = pure
-- hoistCases (Forall i dom e) = do
--   traceM ("🎭 hoisting ifs")
--   let cases = hoistCases' e
--   pure $ Forall i dom (Cases $ NE.fromList $ cases)
