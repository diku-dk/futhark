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


mkRange :: SoP.SoP Term -> SoP.SoP Term -> SoP.Range Term
mkRange lb ub = SoP.Range (S.singleton lb) 1 (S.singleton ub)

int :: Int -> SoP.SoP Term
int n = SoP.int2SoP (toInteger n)

addIterator :: Iterator -> IndexFnM ()
addIterator (Forall i (Iota (Var n))) = do
  addRange (Var i) (mkRange (int 0) (termToSoP $ Var n))
  addRange (Var n) (mkRange (int 1) (int maxBound))
addIterator _ = pure ()

delIterator :: Iterator -> IndexFnM ()
delIterator (Forall i (Iota (Var n))) = do
  delFromEnv (Var i)
  delFromEnv (Var n)
delIterator _ = pure ()

-- I assume exp is already in NNF.
toRel :: Term -> Maybe (Rel Term)
toRel (x :<= y) = Just $ termToSoP x :<=: termToSoP y
toRel (x :< y)  = Just $ termToSoP x :<: termToSoP y
toRel (x :> y)  = Just $ termToSoP x :>: termToSoP y
toRel (x :>= y) = Just $ termToSoP x :>=: termToSoP y
toRel (x :== y) = Just $ termToSoP x :==: termToSoP y
toRel (Not (x :<= y)) = Just $ termToSoP x :>: termToSoP y
toRel (Not (x :< y))  = Just $ termToSoP x :>=: termToSoP y
toRel (Not (x :> y))  = Just $ termToSoP x :<=: termToSoP y
toRel (Not (x :>= y)) = Just $ termToSoP x :<: termToSoP y
-- toRel (Not (x :== y)) = Just $ termToSoP x :/=: termToSoP y
-- TODO the above is checkable as 'x > y || x < y',
-- which appears to be doable if we run each check separately?
toRel (x :&& y) = (:&&:) <$> toRel x <*> toRel y
toRel (_ :|| _) = undefined -- there is :||: but unsure if we need DNF/CNF first
toRel _ = Nothing

-- Do `computation` in a separate scope for AlgEnv.
rollbackAlgEnv :: IndexFnM a -> IndexFnM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env { algenv = alg })
  pure res

refineIndexFn :: IndexFn -> IndexFnM IndexFn
refineIndexFn (IndexFn it (Cases cases)) = do
  let preds = NE.toList $ NE.map fst cases
  let vals = NE.toList $ NE.map snd cases
  (preds'', vals'') <- rollbackAlgEnv (
    do
      addIterator it
      preds' <- mapM refineTerm preds
      vals' <- mapM refineCase (zip preds' vals)
      pure (preds', vals'))
  pure $ IndexFn it (Cases . NE.fromList $ zip preds'' vals'')
  where
    m =
      ASTMapper
        { mapOnTerm = refineTerm,
          mapOnVName = pure
        }

    refineCase :: (Term, Term) -> IndexFnM Term
    refineCase (p, v)
      | Just rel <- toRel p =
        rollbackAlgEnv (
          do
            addRel rel
            env' <- gets algenv
            debugM $ "refine " <> prettyString (p,v) <> " Alg env: " <> prettyString env'
            refineTerm v)
    refineCase (_, v) =
      refineTerm v

    -- NOTE the FME solver returns False if the expression is false
    -- _or_ if the result is unknown. Hence only True results may be used.
    -- XXX Not is outside the SoP repr. Should it be converted in termToSoP?
    -- refineTerm :: AlgEnv Exp E.Exp -> Exp -> IndexFnM Term
    refineTerm :: Term -> IndexFnM Term
    refineTerm (Var vn) = do
      -- TODO case statement is untested.
      -- If the substitution is simply a variable---or if there's no
      -- substitution---then unpack the SoP representation.
      -- Is this even a good idea? Maybe everything should just be
      -- SoP at some point.
      sop <- substEquivs $ termToSoP $ Var vn
      case getSoP sop of
        [([Var x], 1)] -> pure $ Var x
        _ -> pure $ SoP2 sop
      -- pure (Var vn)
    refineTerm e@(x :== y) = do
      b <- termToSoP x $==$ termToSoP y
      pure $ if b then Bool True else e
    refineTerm e@(x :> y)  = do
      b <- termToSoP x $>$ termToSoP y
      pure $ if b then Bool True else e
    refineTerm e@(x :>= y)  = do
      b <- termToSoP x $>=$ termToSoP y
      debugM ("QQ " <> show x)
      debugM ("QQ " <> show y)
      debugM ("QQ " <> show b)
      pure $ if b then Bool True else e
    refineTerm e@(x :< y)  = do
      b <- termToSoP x $<$ termToSoP y
      pure $ if b then Bool True else e
    refineTerm e@(x :<= y)  = do
      b <- termToSoP x $<=$ termToSoP y
      pure $ if b then Bool True else e
    refineTerm (Sum j lb ub e) = do
      -- XXX test this after changing Sum.
      start <- astMap m lb
      end <- astMap m ub
      case (start, end) of
        (a, b) | SoP.padWithZero a == SoP.padWithZero b -> do
            -- If the sum has only one term (one iteration), eliminate it.
            addEquiv (Var j) b -- j is unique to the sum.
            refineTerm (SoP2 e)
        _ -> do
          addRange (Var j) (mkRange start end)
          Sum j start end <$> (termToSoP <$> refineTerm (SoP2 e))
    -- refineTerm (SumSlice vn lb ub) = do
    --   -- XXX test this after changing Sum.
    --   start <- astMap m lb
    --   end <- astMap m ub
    --   case (start, end) of
    --     (a, b) | SoP.padWithZero a == SoP.padWithZero b -> do
    --         -- If the slice is just a single element, eliminate the sum over it.
    --         pure $ Idx (Var vn) (SoP a)
    --     _ -> do
    --       -- addRange j (mkRange start end)
    --       -- Sum j start end <$> refineTerm e
    --       pure $ SumSlice vn start end
    refineTerm v = astMap m v

refineCasePredicate :: Term -> IndexFnM Term
refineCasePredicate = undefined
