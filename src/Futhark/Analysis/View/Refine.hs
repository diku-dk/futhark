module Futhark.Analysis.View.Refine where

import Futhark.SoP.Monad (AlgEnv (ranges), addRange, delFromEnv, substEquivs, addEquiv, lookupRange)
import Futhark.SoP.FourierMotzkin
import Futhark.Analysis.View.Representation hiding (debugM)
import Control.Monad.RWS hiding (Sum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.SoP.SoP (Rel (..))
import Futhark.SoP.Refine (addRel)
import Futhark.Util.Pretty
import Debug.Trace (traceM)
import qualified Data.Map as M


debugM :: Applicative f => String -> f ()
debugM x = traceM $ "🐞 " <> x

mkRange :: SoP.SoP Term -> SoP.SoP Term -> SoP.Range Term
mkRange lb ub = SoP.Range (S.singleton lb) 1 (S.singleton ub)

mkRangeLB :: SoP.SoP Term -> SoP.Range Term
mkRangeLB lb = SoP.Range (S.singleton lb) 1 mempty

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
-- toRel (x :/= y) = Just $ termToSoP x :/=: termToSoP y
-- toRel (Not (x :<= y)) = Just $ termToSoP x :>: termToSoP y
-- toRel (Not (x :< y))  = Just $ termToSoP x :>=: termToSoP y
-- toRel (Not (x :> y))  = Just $ termToSoP x :<=: termToSoP y
-- toRel (Not (x :>= y)) = Just $ termToSoP x :<: termToSoP y
-- toRel (Not (x :== y)) = Just $ termToSoP x :/=: termToSoP y
-- TODO the above is checkable as 'x > y || x < y',
-- which appears to be doable if we run each check separately?
toRel (x :&& y) = (:&&:) <$> toRel x <*> toRel y
toRel (x :|| y) = (:||:) <$> toRel x <*> toRel y
-- toRel (_ :|| _) = error "toRel on :||" -- there is :||: but unsure if we need DNF/CNF first
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
  (preds', vals') <- rollbackAlgEnv (
    do
      addIterator it
      ps <- mapM refineTerm preds
      -- Eliminate cases for which the predicate is always False. (The solver
      -- may return false when the query is undecidable, so we instead check
      -- if the negated predicate is True.)
      -- TODO can we return Nothing when undecidable instead?
      neg_preds <- mapM (refineTerm . toNNF . Not) ps
      let (_, ps', vs) = unzip3 $
                           filter (\(negp, _, _) -> negp /= Bool True) $
                             zip3 neg_preds ps vals
      vs' <- mapM refineCase (zip ps' vs)
      pure (ps', vs'))
  pure $ IndexFn it (Cases . NE.fromList $ zip preds' vals')
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
            debugM $ "refine CASE " <> prettyString (p,v)
            p' <- refineTerm (toNNF (Not p))
            traceM $ "refined predicate " <> prettyString p'
            addRel rel
            env' <- gets algenv
            traceM $ "Alg env: " <> prettyString env'
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
    refineTerm (x :== y) = refineRelation (:==) x y
    refineTerm (x :/= y) = refineRelation (:/=) x y
    refineTerm (x :> y)  = refineRelation (:>) x y
    refineTerm (x :>= y) = refineRelation (:>=) x y
    refineTerm (x :< y) = refineRelation (:<) x y
    refineTerm (x :<= y) = refineRelation (:<=) x y
    refineTerm (x :&& y) = refineRelation (:&&) x y
    refineTerm (x :|| y) = refineRelation (:||) x y
    -- refineTerm (Sum j lb ub e) = do
    --   -- XXX test this after changing Sum.
    --   start <- astMap m lb
    --   end <- astMap m ub
    --   case (start, end) of
    --     (a, b) | SoP.padWithZero a == SoP.padWithZero b ->
    --       rollbackAlgEnv (do
    --         -- If the sum has only one term (one iteration), eliminate it.
    --         addEquiv (Var j) b
    --         refineTerm (SoP2 e)
    --       )
    --     _ ->
    --       rollbackAlgEnv (do
    --         addRange (Var j) (mkRange start end)
    --         Sum j start end <$> astMap m e
    --       )
    refineTerm (SumSlice x lb ub) = do
      debugM $ "SumSlice " <> prettyString x <> " " <> prettyString lb <> " " <> prettyString ub
      start <- astMap m lb
      end <- astMap m ub
      -- If the slice is empty or just a single element, eliminate the sum.
      single <- start $==$ end
      empty <- start $>$ end
      case () of
        _ | single -> pure $ Idx (Var x) start
        _ | empty -> pure . SoP2 $ SoP.int2SoP 0
        _ -> pure $ SumSlice x start end
    refineTerm v = astMap m v

    refineRelation rel x y = do
      x' <- refineTerm x
      y' <- refineTerm y
      env' <- gets algenv
      debugM ("refine " <> prettyString (x `rel` y) <> "\nWith ranges:\n" <> prettyRanges (ranges env'))
      traceM ("x' = " <> prettyString x')
      traceM ("y' = " <> prettyString y')
      -- TODO don't do this twice lol. First is to get the sum in the env.
      b <- solve (x' `rel` y')
      unless b refineSumsInEnv
      b' <- if b then pure b else solve (x' `rel` y')
      env'' <- gets algenv
      traceM ("Ranges after refinement:\n" <> prettyRanges (ranges env''))
      traceM ("Result: " <> prettyString b')
      pure $ if b' then Bool True else x' `rel` y'
      where
        -- Use Fourier-Motzkin elimination to determine the truth value
        -- of an expresion, if it can be determined in the given environment.
        -- If the truth value cannot be determined, False is also returned.
        solve (Bool True :&& Bool True) = pure True
        solve (Bool True :|| _) = pure True
        solve (_ :|| Bool True) = pure True
        solve (a :== b) = termToSoP a $==$ termToSoP b
        solve (a :> b)  = termToSoP a $>$ termToSoP b
        solve (a :>= b) = termToSoP a $>=$ termToSoP b
        solve (a :< b)  = termToSoP a $<$ termToSoP b
        solve (a :<= b) = termToSoP a $<=$ termToSoP b
        solve _ = pure False

-- Want to add sum terms as _symbols_ to the env denoting the test.
-- (Use addRel like for done the predicates above.)
-- For example, see how in the following, the ranges include that
-- shape[i] is non-negative!
-- We also have that 0 <= i <= m.
-- Not sure if the machinery will be able to chain the fact that
-- the term shape[j-1] in Sum_j=1^i shape[j-1] is also positive.
-- (At least we need to add j to the environment.)
--
-- 🪲 refine (¬((shape₆₀₇₁)[i₆₁₇₂] < 0), Σj₆₁₆₄∈[1, ..., i₆₁₇₂] ((shape₆₀₇₁)[-1 + j₆₁₆₄])) Alg env: Untranslatable environment:
-- dir:
-- []
-- inv:
-- []
-- Equivalence environment:
-- []
-- Ranges:
-- [max{1} <= m₆₀₆₉ <= min{9223372036854775807}, max{0} <= i₆₁₇₂ <= min{m₆₀₆₉}, max{0} <= (shape₆₀₇₁)[i₆₁₇₂] <= min{}]

-- If the sum ranges are constant after refinement,
-- we could unroll it!
-- refineSums :: Term -> IndexFnM Term
-- refineSums (SoP2 x :>= SoP2 y) =
--   -- Treat as x - y >= 0.
--   -- So check that t
--   let rewrite = x .-. y :>=
--   undefined
-- refineSums _ = undefined

-- [max{1} <= m₆₀₆₉ <= min{9223372036854775807}
--  max{1} <= j₆₁₆₄ <= min{i₆₁₇₂},
--  max{0} <= i₆₁₇₂ <= min{m₆₀₆₉}, 
--  max{} <= Σj₆₁₆₄∈[1, ..., i₆₁₇₂] ((shape₆₀₇₁)[-1 + j₆₁₆₄]) <= min{}, 
--  max{0} <= (shape₆₀₇₁)[i₆₁₇₂] <= min{}]

-- Takes any Sum in the alg env ranges and adds min/max values
-- using monotonicity logic.
-- The reason I don't do this in the case for Sum is that I think
-- the sums only end up in the env when there's a relation on them?
refineSumsInEnv :: IndexFnM ()
refineSumsInEnv = do
  ranges <- ranges <$> gets algenv
  -- debugM $ "refineSumRangesInEnv " <> prettyString ranges
  mapM_ (refineSumRange . fst) (M.toList ranges)
  where
    -- zero = SoP.int2SoP 0
    refineSumRange :: Term -> IndexFnM ()
    -- refineSumRange t@(Sum _ _ _ term) = do
    --   -- if term is non-negative for all j in [lb,ub],
    --   -- then the sum itself is non-negative
    --   b <- term $>=$ zero
    --   debugM $ "refineSumRangesInEnv "
    --            <> prettyString (SoP2 term :>= SoP2 zero)
    --            <> " is " <> prettyString b
    --   when b $ addRange t (mkRangeLB zero)
    refineSumRange _ = pure ()
