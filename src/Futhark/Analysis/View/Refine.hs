module Futhark.Analysis.View.Refine where

import Futhark.SoP.Monad (AlgEnv, addRange)
import Language.Futhark qualified as E
-- The above can be removed if I don't write out the type signature
-- for onExp, I think.
import Futhark.SoP.FourierMotzkin
import Futhark.Analysis.View.Representation
import Control.Monad.RWS
import qualified Data.List.NonEmpty as NE
import Debug.Trace (trace)
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP


mkRange :: Exp -> Exp -> SoP.Range Exp
mkRange lb ub = SoP.Range (S.singleton . expToSoP $ lb) 1 (S.singleton . expToSoP $ ub)

int :: Int -> Exp
int n = SoP (SoP.int2SoP (toInteger n))

addIterator :: Iterator -> ViewM ()
addIterator (Forall i (Iota (Var n))) = do
  addRange (Var i) (mkRange (int 0) (Var n))
  addRange (Var n) (mkRange (int 1) (int maxBound))
addIterator _ = pure ()

refineView :: View -> ViewM View
refineView (View it (Cases cases)) = do
  -- maybe astMap over predicates in cases?
  -- recurse all the way down solving the simplest equations
  -- then solve their combinations as we traverse back up
  let preds = NE.toList $ NE.map fst cases
  addIterator it
  env <- gets algenv
  preds' <- NE.fromList <$> mapM (onExp env) preds
  let cases' = NE.filter (eliminateFalse . fst) $
                 NE.zipWith (\c (_,e) -> (c,e)) preds' cases
  case cases' of
    [] -> error "No true case; this should never happen."
    cs -> pure $ View it (Cases $ NE.fromList cs)
  -- ^ this is probably a janky way to do it, I just made it typecheck.
  where
    m env =
      ASTMapper
        { mapOnExp = onExp env }
    onExp :: AlgEnv Exp E.Exp -> Exp -> ViewM Exp
    onExp _ (Var vn) = pure $ Var vn
    onExp _ e@(x :== y) = do
      b <- expToSoP x $==$ expToSoP y
      pure $ if b then Bool True else e
    onExp _ e@(x :> y)  = do
      b <- expToSoP x $>$ expToSoP y
      trace ("O.O hi!! " <> show e) $ pure $ if b then Bool True else e
    onExp _ e@(x :< y)  = do
      b <- expToSoP x $<$ expToSoP y
      pure $ if b then Bool True else e
    onExp env v = astMap (m env) v
    -- XXX One problem is that Not is outside this SoP repr. I think we may
    -- want to bring it in here to leverage Not. (Hence special handling
    -- in eliminateFalse).
    -- XXX $==$ returns False also if the solution is unknown (i.e., we
    -- cannot disprove, hm). We could still prove Not x by proving
    -- x True and then returning False here.
    -- XXX Sequence onExp so that knowledge from previous cases can be
    -- used in later cases? In fact we want to incorporate negation of
    -- disjunctin of all previous cases!
    -- XXX Shouldn't these relations always be in SoP representation?
    eliminateFalse (Bool False) = False
    eliminateFalse (Not (Bool True)) = False
    eliminateFalse _ = True
refineView _ = error "unnormalised view (just hoistCases)"
-- XXX I should change the representation to ensure cases outermost, I guess.


refineCasePredicate :: Exp -> ViewM Exp
refineCasePredicate = undefined
