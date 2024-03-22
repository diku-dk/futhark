module Futhark.Analysis.View.Refine where

import Futhark.SoP.Monad (AlgEnv, addRange, delFromEnv, substEquivs, addEquiv)
import Language.Futhark qualified as E
import Futhark.SoP.FourierMotzkin
import Futhark.Analysis.View.Representation
import Control.Monad.RWS hiding (Sum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.SoP.SoP (Rel (..))
import Futhark.SoP.Refine (addRel)
import Futhark.Util.Pretty
import Debug.Trace (traceM)


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

-- I assume exp is already in NNF.
toRel :: Exp -> Maybe (Rel Exp)
toRel (x :<= y) = Just $ expToSoP x :<=: expToSoP y
toRel (x :< y)  = Just $ expToSoP x :<: expToSoP y
toRel (x :> y)  = Just $ expToSoP x :>: expToSoP y
toRel (x :>= y) = Just $ expToSoP x :>=: expToSoP y
toRel (x :== y) = Just $ expToSoP x :==: expToSoP y
toRel (x :&& y) = (:&&:) <$> toRel x <*> toRel y
toRel (_ :|| _) = undefined -- there is :||: but unsure if we need DNF/CNF first
toRel _ = Nothing

-- Do `computation` in a separate scope for AlgEnv.
rollbackAlgEnv :: ViewM a -> ViewM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env { algenv = alg })
  pure res

refineView :: View -> ViewM View
refineView (View it (Cases cases)) = do
  let preds = NE.toList $ NE.map fst cases
  let vals = NE.toList $ NE.map snd cases
  (preds'', vals'') <- rollbackAlgEnv (
    do
      addIterator it
      env <- gets algenv
      traceM $ "Alg env: " <> prettyString env
      preds' <- mapM refineExp preds
      vals' <- mapM refineCase (zip preds' vals)
      pure (preds', vals'))
  pure $ View it (Cases . NE.fromList $ zip preds'' vals'')
  where
    m =
      ASTMapper
        { mapOnExp = refineExp }

    refineCase :: (Exp, Exp) -> ViewM Exp
    refineCase (p, v)
      | Just rel <- toRel p =
        rollbackAlgEnv (
          do
            addRel rel
            env' <- gets algenv
            traceM $ "refine " <> prettyString (p,v) <> " Alg env: " <> prettyString env'
            refineExp v)
    refineCase (_, v) =
      refineExp v

    -- NOTE the FME solver returns False if the expression is false
    -- _or_ if the result is unknown. Hence only True results may be used.
    -- XXX Not is outside the SoP repr. Should it be converted in expToSoP?
    -- refineExp :: AlgEnv Exp E.Exp -> Exp -> ViewM Exp
    refineExp :: Exp -> ViewM Exp
    refineExp (Var vn) = do
      -- TODO case statement is untested.
      -- If the substitution is simply a variable---or if there's no
      -- substitution---then unpack the SoP representation.
      -- Is this even a good idea? Maybe everything should just be
      -- SoP at some point.
      sop <- substEquivs $ expToSoP $ Var vn
      case getSoP sop of
        [([Var x], 1)] -> pure $ Var x
        _ -> pure $ SoP sop
      -- pure (Var vn)
    refineExp e@(x :== y) = do
      b <- expToSoP x $==$ expToSoP y
      pure $ if b then Bool True else e
    refineExp e@(x :> y)  = do
      b <- expToSoP x $>$ expToSoP y
      pure $ if b then Bool True else e
    refineExp e@(x :< y)  = do
      b <- expToSoP x $<$ expToSoP y
      pure $ if b then Bool True else e
    refineExp (Sum j lb ub e) = do
      start <- refineExp lb
      end <- refineExp ub
      case (start, end) of
        (SoP a, SoP b) | SoP.padWithZero a == SoP.padWithZero b -> do
            -- If the sum has only one term (one iteration), eliminate it.
            addEquiv j b -- j is unique to the sum.
            refineExp e
        _ -> do
          addRange j (mkRange start end)
          Sum j start end <$> refineExp e
    refineExp v = astMap m v

refineCasePredicate :: Exp -> ViewM Exp
refineCasePredicate = undefined
