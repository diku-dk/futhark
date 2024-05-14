module Futhark.Analysis.View.Refine where

import Futhark.SoP.Monad (addRange, delFromEnv, substEquivs)
import Futhark.SoP.FourierMotzkin
import Futhark.Analysis.View.Representation hiding (debugM)
import Futhark.Analysis.View.Monad
import Control.Monad.RWS hiding (Sum)
import Data.List.NonEmpty(NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.SoP.SoP (Rel (..))
import Futhark.SoP.Refine (addRel)
import Futhark.Util.Pretty
import Debug.Trace (traceM)
import Futhark.MonadFreshNames (newNameFromString)


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
  addRange (Var i) (mkRange (int 0) (termToSoP (Var n) SoP..-. int 1))
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
      let (_neg_ps', ps', vs) = unzip3 $
                           filter (\(negp, _, _) -> negp /= Bool True) $
                             zip3 neg_preds ps vals
      -- Cases are considered sequentially, so negation of previous cases
      -- are part of current predicate.
      -- let psInformed = zipWith (:&&) (init $ scanl (:&&) (Bool True) neg_ps') ps'
      -- let neg_preds = map (toNNF . Not) preds
      -- let psInformed = zipWith (:&&)
      --                          (init $ scanl (:&&) (Bool True) neg_preds)
      --                          preds
      vs' <- mapM refineCase (zip ps' vs)
      pure (ps', vs'))
  pure $ IndexFn it (Cases . NE.fromList $ zip preds' vals')
  where
    refineCase :: (Term, Term) -> IndexFnM Term
    refineCase (p, v)
      | Just rel <- toRel p =
        rollbackAlgEnv (
          do
            -- debugM $ "refine CASE " <> prettyString (p,v)
            addRel rel
            refineTerm v)
    refineCase (_, v) =
      refineTerm v

-- NOTE the FME solver returns False if the expression is false
-- _or_ if the result is unknown. Hence only True results may be used.
-- XXX Not is outside the SoP repr. Should it be converted in termToSoP?
refineTerm :: Term -> IndexFnM Term
refineTerm t =
  refineT t
  where
    m =
      ASTMapper
        { mapOnTerm = refineT,
          mapOnVName = pure
        }
    refineT (Var vn) = do
      -- TODO case statement is untested.
      -- If the substitution is simply a variable---or if there's no
      -- substitution---then unpack the SoP representation.
      -- Is this even a good idea? Maybe everything should just be
      -- SoP at some point.
      sop <- substEquivs $ termToSoP $ Var vn
      case getSoP sop of
        [([Var x], 1)] -> pure $ Var x
        _ -> pure $ SoP2 sop
    refineT (x :== y) = refineRelation (:==) x y
    refineT (x :/= y) = refineRelation (:/=) x y
    refineT (x :> y)  = refineRelation (:>) x y
    refineT (x :>= y) = refineRelation (:>=) x y
    refineT (x :< y) = refineRelation (:<) x y
    refineT (x :<= y) = refineRelation (:<=) x y
    refineT (x :&& y) = refineRelation (:&&) x y
    refineT (x :|| y) = refineRelation (:||) x y
    refineT (SumSlice x lb ub) = do
      start <- astMap m lb
      end <- astMap m ub
      -- If the slice is empty or just a single element, eliminate the sum.
      single <- start $==$ end
      empty <- start $>$ end
      case () of
        _ | single -> pure $ Idx (Var x) start
        _ | empty -> pure . SoP2 $ SoP.int2SoP 0
        _ -> pure $ SumSlice x start end
    refineT x@(SoP2 _) = do
      astMap m x >>= mergeSums
      where
        -- Takes a sum of products which may have Sum terms and merges other
        -- compatible terms into those Sums. Time complexity is quadratic in
        -- the number of terms in the SoP.
        mergeSums :: Term -> IndexFnM Term
        mergeSums (SoP2 s) = do
          SoP2 . SoP.sopFromList <$> foldM absorbTerm [] (getSoP s)
        mergeSums t = pure t

        absorbTerm [] t2 = pure [t2]
        absorbTerm (t1:ts) t2 = do
          res <- merge t1 t2
          case res of
            Just t' -> pure (t':ts)
            Nothing -> do
              ts' <- absorbTerm ts t2
              pure (t1 : ts')

        -- Rewrite sum y[lb:ub] - sum y[lb:mid] ==> sum y[mid+1:ub]
        -- with mid <= ub.
        -- Relies on sorting of SoP and Term to match.
        merge :: ([Term], Integer) -> ([Term], Integer) -> IndexFnM (Maybe ([Term], Integer))
        merge ([SumSlice y lb ub], 1) ([SumSlice y' lb' mid], -1)
          | y == y',
            lb == lb' = do
              b <- mid $<=$ ub
              pure $
                if b
                then Just ([SumSlice y (mid SoP..+. SoP.int2SoP 1) ub], 1)
                else Nothing
        merge ([SumSliceIndicator y lb ub], 1) ([SumSliceIndicator y' lb' mid], -1)
          | y == y',
            lb == lb' = do
              b <- mid $<=$ ub
              pure $
                if b
                then Just ([SumSliceIndicator y (mid SoP..+. SoP.int2SoP 1) ub], 1)
                else Nothing
        merge _ _ = pure Nothing
    refineT v = astMap m v

refineRelation :: (Term -> Term -> Term) -> Term -> Term -> IndexFnM Term
refineRelation rel x y = do
  x' <- refineTerm x
  y' <- refineTerm y
  b <- solve (x' `rel` y')
  pure $ if b then Bool True else x' `rel` y'
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

-- Takes any Sum in the alg env ranges and adds min/max values
-- using monotonicity logic.
-- refineSumsInEnv :: IndexFnM ()
-- refineSumsInEnv = do
--   ranges <- ranges <$> gets algenv
--   mapM_ (refineSumRange . fst) (M.toList ranges)
--   where
--     zero = SoP.int2SoP 0
--     refineSumRange :: Term -> IndexFnM ()
--     refineSumRange t@(SumSlice vn _ _) = do
--       -- If the array being summed over is non-negative at all locations,
--       -- then the sum itself is non-negative.
--       b <- SoP.sym2SoP (Var vn) $>=$ zero
--       when b $ addRange t (mkRangeLB zero)
--     refineSumRange t@(SumSliceIndicator {}) = do
--       -- A sum over indicators is always non-negative.
--       addRange t (mkRangeLB zero)
--     refineSumRange _ = pure ()

checkMonotonic :: Iterator -> (Term, Term) -> IndexFnM Bool
checkMonotonic iter@(Forall _ _) (cond, SumSlice xs _ _) = do
  -- For a SumSlice it's sufficient to check that each term is non-negative.
  let test = (cond, Var xs :>= SoP2 (SoP.int2SoP 0))
  let test' = IndexFn iter (Cases . NE.singleton $ test)
  IndexFn _ (Cases res) <- refineIndexFn test'
  case res of
    (_, Bool True) :| [] -> pure True
    _ -> pure False
checkMonotonic iter@(Forall i dom) (cond, x) = do
-- TODO This case is not doing anything right now.
  -- i is name in iter
  -- new name q
  -- new name r
  -- add range q in [min iter,r]
  -- add range r in [min iter,max iter]
  -- test that x{q/i} <= x{r/i}
  q <- newNameFromString "q"
  r <- newNameFromString "r"
  let (min_iter, max_iter) = (termToSoP $ domainStart dom, termToSoP $ domainEnd dom)
  addRange (Var r) (mkRange min_iter max_iter)
  addRange (Var q) (mkRange min_iter (SoP.sym2SoP $ Var r))
  let x_q = substituteName i (Var q) x
  let x_r = substituteName i (Var r) x
  let test = (cond, x_q :>= x_r)
  -- Have:
  --   max{} <= ∑shape₆₀₈₁[0 : -1 + q₆₁₈₈] <= min{}
  --   max{0} <= ∑shape₆₀₈₁[0 : -1 + r₆₁₈₉] <= min{}
  -- Need that:
  --   max{} <= ∑shape₆₀₈₁[0 : -1 + q₆₁₈₈] <= min{∑shape₆₀₈₁[0 : -1 + r₆₁₈₉]}
  let test' = IndexFn iter (Cases . NE.singleton $ test)
  IndexFn _ (Cases res) <- refineIndexFn test'
  case res of
    (_, Bool True) :| [] -> pure True
    _ -> pure False
checkMonotonic _ _ = pure False
