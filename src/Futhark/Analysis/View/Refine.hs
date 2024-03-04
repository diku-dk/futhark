module Futhark.Analysis.View.Refine where

import Futhark.SoP.Monad (AlgEnv, addRange, delFromEnv)
import Language.Futhark qualified as E
import Futhark.SoP.FourierMotzkin
import Futhark.Analysis.View.Representation
import Control.Monad.RWS
import qualified Data.List.NonEmpty as NE
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

delIterator :: Iterator -> ViewM ()
delIterator (Forall i (Iota (Var n))) = do
  delFromEnv (Var i)
  delFromEnv (Var n)
delIterator _ = pure ()

refineView :: View -> ViewM View
refineView (View it (Cases cases)) = do
  let preds = NE.toList $ NE.map fst cases
  addIterator it
  env <- gets algenv
  -- let env =  addIteratorLocal env it
  preds' <- NE.fromList <$> mapM (onExp env) preds
  let cases' = NE.filter (eliminateFalse . fst) $
                 NE.zipWith (\c (_,e) -> (c,e)) preds' cases
  delIterator it
  case cases' of
    [] -> error "No true case; this should never happen."
    cs -> pure $ View it (Cases $ NE.fromList cs)
  -- ^ this is probably a janky way to do it, I just made it typecheck.
  where
    m env =
      ASTMapper
        { mapOnExp = onExp env }

    -- NOTE the FME solver returns False if the expression is false
    -- _or_ if the result is unknown. Hence only True results may be used.
    -- XXX Not is outside the SoP repr. Should it be converted in expToSoP?
    onExp :: AlgEnv Exp E.Exp -> Exp -> ViewM Exp
    onExp _ (Var vn) = pure $ Var vn
    onExp _ e@(x :== y) = do
      b <- expToSoP x $==$ expToSoP y
      pure $ if b then Bool True else e
    onExp _ e@(x :> y)  = do
      b <- expToSoP x $>$ expToSoP y
      pure $ if b then Bool True else e
    onExp _ e@(x :< y)  = do
      b <- expToSoP x $<$ expToSoP y
      pure $ if b then Bool True else e
    onExp env v = astMap (m env) v

    eliminateFalse (Bool False) = False
    eliminateFalse (Not (Bool True)) = False
    eliminateFalse _ = True
refineView _ = error "unnormalised view (just apply hoistCases)"
-- XXX I should change the representation to ensure cases outermost, I guess.


refineCasePredicate :: Exp -> ViewM Exp
refineCasePredicate = undefined
