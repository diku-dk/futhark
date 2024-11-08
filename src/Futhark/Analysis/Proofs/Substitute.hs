-- Index function substitution.
module Futhark.Analysis.Proofs.Substitute (($$)) where

import Control.Monad (unless)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.SymbolPlus (toSumOfSums)
import Futhark.Analysis.Proofs.Unify (Renameable (..), Replaceable (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..))
import Futhark.Analysis.Proofs.Util (prettyBinding')
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), newName)
import Futhark.SoP.SoP (SoP, mapSymSoP, sym2SoP)
import Futhark.Util.Pretty (prettyString)
import Language.Futhark (VName)
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.IndexFnPlus (repIndexFn, repCase, domainEnd, domainStart)
import Futhark.Analysis.Proofs.Rewrite (rewrite)

-- We use an operator so as not to confuse it with substitution from Unify.
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
($$) :: IndexFn -> (VName, IndexFn) -> IndexFnM IndexFn
f@(IndexFn (Forall j _) _) $$ (vn, g@(IndexFn (Forall i _) _)) = do -- subst x for@(IndexFn (Forall i _) _) into@(IndexFn (Forall j _) _) = do
  whenDebug $ traceM $
      "🎭 substitute\n    "
        <> prettyBinding' vn g
        <> prettyBinding' ("\n    into _" :: String) f
  i' <- sym2SoP . Var <$> newName i
  vns <- getNameSource
  for' <- rename vns g
  into' <- rename vns f
  substitute vn (repIndexFn (mkRep i i') for') (repIndexFn (mkRep j i') into')
f $$ (vn, g@(IndexFn (Forall {}) _)) = do -- subst x for@(IndexFn (Forall _ _) _) into = do
  whenDebug $ traceM $
      "🎭 substitute\n    "
        <> prettyBinding' vn g
        <> prettyBinding' ("\n    into _" :: String) f
  substitute vn g f
f $$ (vn, g) = substitute vn g f

sameRange dom_x dom_y = do
  start_x <- rewrite (domainStart dom_x)
  start_y <- rewrite (domainStart dom_y)
  end_x <- rewrite (domainEnd dom_x)
  end_y <- rewrite (domainEnd dom_y)
  pure $ start_x == start_y && end_x == end_y

-- Assumes that Forall-variables (i) of non-Empty iterators are equal.
substitute :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
substitute x (IndexFn Empty xs) (IndexFn iter_y ys) =
  -- Substitute scalar `x` into index function `y`.
  pure $
    IndexFn
      iter_y
      ( cases $ do
          (x_cond, x_val) <- casesToList xs
          (y_cond, y_val) <- casesToList ys
          pure $ repCase (mkRep x x_val) (y_cond :&& x_cond, y_val)
      )
substitute x_fn (IndexFn (Forall i (Iota _)) xs) (IndexFn Empty ys) =
  -- Substitute array `x` into scalar `y` (type-checker ensures that this is valid,
  -- e.g., y is a sum).
  pure $
    IndexFn
      Empty
      ( cases $ do
          (x_cond, x_val) <- casesToList xs
          (y_cond, y_val) <- casesToList ys
          let rip_x = rip x_fn i x_val
          pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
      )
substitute x_fn (IndexFn (Forall i (Iota n)) xs) (IndexFn (Forall _ (Iota n')) ys)
  | n == n' =
      pure $
        IndexFn
          (Forall i (Iota n))
          ( cases $ do
              (x_cond, x_val) <- casesToList xs
              (y_cond, y_val) <- casesToList ys
              let rip_x = rip x_fn i x_val
              pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
          )
substitute x_fn (IndexFn (Forall i dom_x@(Iota _)) xs) (IndexFn (Forall _ dom_y@(Cat _ m _)) ys) = do
  eqEnd1 :: Maybe (Substitution Symbol) <- unify (domainEnd dom_x) (domainEnd dom_y)
  eqEnd2 :: Maybe (Substitution Symbol) <- unify (domainEnd dom_x) (domainEnd (Iota m))
  unless (isJust eqEnd1 || isJust eqEnd2) $ error "substitute iota cat: Incompatible domains."
  pure $
    IndexFn
      (Forall i dom_y)
      ( cases $ do
          (x_cond, x_val) <- casesToList xs
          (y_cond, y_val) <- casesToList ys
          let rip_x = rip x_fn i x_val
          pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
      )
substitute x_fn (IndexFn (Forall i dom_x@(Cat {})) xs) (IndexFn (Forall _ dom_y@(Iota _)) ys) = do
  -- | domainStart dom_x == domainStart dom_y,
  --   domainEnd dom_x == domainEnd dom_y = do
  debugPrettyM "starts" (domainStart dom_x, domainStart dom_y)
  debugPrettyM "ends" (domainEnd dom_x, domainEnd dom_y)
  error "lol"
  pure $
    IndexFn
      (Forall i dom_x)
      ( cases $ do
          (x_cond, x_val) <- casesToList xs
          (y_cond, y_val) <- casesToList ys
          let rip_x = rip x_fn i x_val
          pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
      )
substitute x_fn (IndexFn (Forall i dom_x@(Cat {})) xs) (IndexFn (Forall _ dom_y@(Cat {})) ys)
  | dom_x == dom_y =
      pure $
        IndexFn
          (Forall i dom_y)
          ( cases $ do
              (x_cond, x_val) <- casesToList xs
              (y_cond, y_val) <- casesToList ys
              let rip_x = rip x_fn i x_val
              pure (sop2Symbol . rip_x $ y_cond :&& x_cond, mapSymSoP rip_x y_val)
          )
substitute _ x y = error $ "substitute: not implemented for " <> prettyString x <> prettyString y

-- TODO Sad that we basically have to copy rep here;
--      everything but the actual substitutions could be delegated to
--      a helper function that takes a replacement as argument?
rip :: VName -> VName -> SoP Symbol -> Symbol -> SoP Symbol
rip fnName fnArg fnVal = apply mempty
  where
    applySoP = mapSymSoP . apply

    apply :: Replacement Symbol -> Symbol -> SoP Symbol
    apply s (Apply (Var f) [idx])
      | f == fnName =
          rep (M.insert fnArg idx s) fnVal
    apply s (Idx (Var f) idx)
      | f == fnName =
          rep (M.insert fnArg idx s) fnVal
    apply s (Var f)
      | f == fnName =
          rep s fnVal
    apply _ x@(Var _) = sym2SoP x
    apply _ x@(Hole _) = sym2SoP x
    apply s (Idx x idx) =
      sym2SoP $ Idx (sop2Symbol $ apply s x) (applySoP s idx)
    apply s (Sum j lb ub x) =
      let s' = addRep j (Var j) s
       in toSumOfSums j (applySoP s' lb) (applySoP s' ub) (apply s' x)
    apply s (Apply f xs) =
      sym2SoP $ Apply (sop2Symbol $ apply s f) (map (applySoP s) xs)
    apply s (Tuple xs) =
      sym2SoP $ Tuple (map (applySoP s) xs)
    apply _ x@(Bool _) = sym2SoP x
    apply _ Recurrence = sym2SoP Recurrence
    apply s sym = case sym of
      Not x -> sym2SoP . neg . sop2Symbol $ apply s x
      x :< y -> binop (:<) x y
      x :<= y -> binop (:<=) x y
      x :> y -> binop (:>) x y
      x :>= y -> binop (:>=) x y
      x :== y -> binop (:==) x y
      x :/= y -> binop (:/=) x y
      x :&& y -> binopS (:&&) x y
      x :|| y -> binopS (:||) x y
      where
        binop op x y = sym2SoP $ applySoP s x `op` applySoP s y
        binopS op x y = sym2SoP $ sop2Symbol (apply s x) `op` sop2Symbol (apply s y)
