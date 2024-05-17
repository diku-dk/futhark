module Futhark.Analysis.View.Substitution (sub) where
import Language.Futhark (VName)
import Futhark.Util.Pretty
import Futhark.Analysis.View.Representation
import qualified Data.List.NonEmpty as NE
import Futhark.Analysis.View.Monad (IndexFnM, VEnv (algenv))
import qualified Text.LaTeX.Packages.AMSMath as Math
import Control.Monad.RWS
import Futhark.Analysis.View.Latex (toLaTeX)
import Data.Functor.Identity
import Debug.Trace (trace)
import Futhark.Analysis.View.Refine (refineTerm)
import Futhark.SoP.SoP (scaleSoP, (.-.), (.+.), int2SoP, padWithZero, sopToLists, SoP)

debug :: String -> a -> a
debug msg = trace ("🌪️🎭 " <> msg)

-- Substitution rules.
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
sub :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
sub x (IndexFn Empty xs) (IndexFn iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  pure $
    IndexFn
      iter_y
      (listToCases $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q@(IndexFn (Forall i (Iota {})) xs) r@(IndexFn (Forall j yD) ys) = do
  -- This is solely to handle substiution of parameters for top-level defs.
  let yD' = case casesToList xs of
              [(Bool True, xval)] -> substituteIdx (i, x, xval) (j, yD)
              _ -> yD
  -- Substitution Rules 1 and 2.
  let res =
        IndexFn
          (Forall j yD')
          (listToCases $ do
            (xcond, xval) <- casesToList xs
            (ycond, yval) <- casesToList ys
            pure (substituteIdxT (i, x, xval) (j, ycond) :&& xcond,
                  substituteIdxT (i, x, xval) (j, yval)))
  tell ["Substitute (Rule 1) " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q <> " to get " <> toLaTeX res]
  pure $
    debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r) $
    res
sub x q@(IndexFn (Forall i (Cat k m b)) xs) r@(IndexFn iter_y@(Forall j (Cat k' m' b')) ys)
  | k == k',
    m == m',
    b == b' = do
      -- Substitution Rule 4. (NOTE Indexing may depend on i/j and k.)
      tell ["Substitute (Rule 4) " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
      pure $
        debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r) $
        IndexFn
          iter_y
          (listToCases $ do
            (xcond, xval) <- casesToList xs
            (ycond, yval) <- casesToList ys
            pure (substituteIdxT (i, x, xval) (j, ycond) :&& xcond,
                  substituteIdxT (i, x, xval) (j, yval)))
sub x q@(IndexFn iter_x@(Forall i xD@(Cat {})) xs) r@(IndexFn (Forall j yD@(Iota {})) ys) = do
      -- eqDomains <- refineTerm $ domainStart xD :== domainStart yD :&& domainEnd xD :== domainEnd yD
      -- case eqDomains of
      --   Bool True -> do
      --     tell ["Substitute(?) " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
      --     pure $
      --       debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r) $
      --       IndexFn
      --         iter_x -- XXX iter_x
      --         (listToCases $ do
      --           (xcond, xval) <- casesToList xs
      --           (ycond, yval) <- casesToList $ substituteName j (Var i) ys
      --           pure (substituteIdxT (i, x, xval) (i, ycond) :&& xcond,
      --                 substituteIdx (i, x, xval) (i, yval)))
      --   _ -> do
      --     algenv <- gets algenv
      --     pure . error $ "💀 sub "
      --             <> prettyString x <> " for "
      --             <> prettyString q <> "\n   in "
      --             <> prettyString r
      --             <> prettyString (xD == yD)
      --             <> prettyString algenv
      xStart <- refineTerm $ domainStart xD
      yStart <- refineTerm $ domainStart yD
      xEnd <- refineTerm $ domainEnd xD
      yEnd <- refineTerm $ domainEnd yD
      if xStart == yStart && xEnd == yEnd
      then do
        tell ["Substitute(?) " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
        pure $
          debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r) $
          IndexFn
            iter_x -- XXX iter_x
            (listToCases $ do
              (xcond, xval) <- casesToList xs
              (ycond, yval) <- casesToList $ substituteName j (Var i) ys
              pure (substituteIdxT (i, x, xval) (i, ycond) :&& xcond,
                    substituteIdx (i, x, xval) (i, yval)))
      else do
        algenv <- gets algenv
        pure . error $ "💀 sub "
                <> prettyString x <> " for "
                <> prettyString q <> "\n   in "
                <> prettyString r
                <> prettyString (xD == yD)
                <> prettyString algenv
sub x q r = do
  algenv <- gets algenv
  pure . error $ "💀 sub "
          <> prettyString x <> " for "
          <> prettyString q <> "\n   in "
          <> prettyString r
          <> prettyString algenv

-- Incredibly stupid hack to make onTerm apply to y (in contrast to
-- astMap m being applied to y) without restricting the type of substiuteIdx.
substituteIdxT :: (VName, VName, Term) -> (VName, Term) -> Term
substituteIdxT a (j, y) =
  fst $ substituteIdx a (j, (y, Recurrence))

-- substituteIdx :: (VName, VName, Term) -> (VName, Term) -> Term
substituteIdx :: ASTMappable a => (VName, VName, Term) -> (VName, a) -> a
substituteIdx (i, x, xval) (j, y) =
  runIdentity $ astMap m y
  where
    m =
      ASTMapper
        { mapOnTerm = onTerm,
          mapOnVName = pure
        }
    -- onTerm e | debug ("onTerm " <> prettyString e) False = undefined
    onTerm (Var vn) | vn == x =
      -- debug ("(1) vn, xval " <> prettyString vn <> ", " <> prettyString xval)
      pure (substituteName i (Var j) xval)
    onTerm e@(Var _) =
      pure e
    onTerm (Idx (Var vn) idx) | vn == x =
      -- debug ("(2) vn, xval " <> prettyString vn <> ", " <> prettyString xval)
      pure (flatten $ substituteName i (SoP2 idx) xval)
    onTerm (SumSlice e lb ub) | e == Var x || e == Indicator (Var x) =
      case justAffine (termToSoP xval) >>= mapM (mkSum e) of
        Just sums ->
          pure $ SoP2 (foldl1 (.+.) sums)
        Nothing -> error "gg, need sums over non-affine terms"
      where
        justAffine :: SoP Term -> Maybe [([Term], Integer)]
        justAffine e1 | trace ("justAffine: " <> prettyString e1) False = undefined
        justAffine sop
          | termsWithFactors <- sopToLists (padWithZero sop),
            isAffineSoP sop =
              Just termsWithFactors
        justAffine _ = Nothing

        mkSum e1 e2 | trace ("mkSum: " <> prettyString e1 <> " " <> prettyString e2) False = undefined
        mkSum _ ([], c) =
          -- This would be ∑(c)[lb : ub] c so rewrite it to (ub - lb + 1) * c.
          pure $ scaleSoP c (ub .-. lb .+. int2SoP 1)
        mkSum (Var _) ([Idx (Var vnx) idx], c) = do
          pure . scaleSoP c . termToSoP $
            SumSlice (Var vnx) (substituteName i (SoP2 lb) idx) (substituteName i (SoP2 ub) idx)
        mkSum (Var _) ([Indicator (Idx (Var vnx) idx)], c) = do
          pure . scaleSoP c . termToSoP $
            SumSlice (Indicator (Var vnx)) (substituteName i (SoP2 lb) idx) (substituteName i (SoP2 ub) idx)
        mkSum (Indicator (Var _)) ([Idx (Var vnx) idx], c) = do
          pure . scaleSoP c . termToSoP $
            SumSlice (Indicator (Var vnx)) (substituteName i (SoP2 lb) idx) (substituteName i (SoP2 ub) idx)
        mkSum _ _ = Nothing
    onTerm e = astMap m e
-- (Case 1)
-- sub xs for
--   forall i in iota n . | True => inputs[i-1]
-- in
--   forall i in iota n . | True => ∑(xs₆₁₅₉)[1, ..., i₆₂₃₅]
--
-- (Case 2)
-- sub xs for
--   forall i in iota n . | True => 1
-- in
--   forall i in iota n . | True => ∑(xs₆₁₅₉)[1, ..., i₆₂₃₅]
--
-- (Case 3)
-- sub xs for
--   forall i in iota n . | True => True
-- in
--   forall i in iota n . | True => ∑(⟦xs₆₁₅₉⟧)[1, ..., i₆₂₃₅]
--
-- (Case 4)
-- sub xs for
--   forall i in iota n . | True => ⟦True⟧
-- in
--   forall i in iota n . | True => ∑(xs₆₁₅₉)[1, ..., i₆₂₃₅]
--
-- In case 1,
-- onTerm (SumSlice e lb ub) | e == (Var x) || e == (Indicator (Var x)) =
--   case justAffine xval >>= mapM (mkSum e) of
--     Just sums ->
--       SoP2 (foldl1 (SoP..+.) sums)
--     Nothing -> error "gg, need sums over non-affine terms"
--   where
--     mkSum i (Var vn) ([], c) =
--       -- This would be ∑(c)[lb : ub] c so rewrite it to (ub - lb + 1) * c.
--       pure $ SoP.scaleSoP c (ub SoP..-. lb SoP..+. SoP.int2SoP 1)
--     mkSum i (Var vn) ([Idx (Var vnx) idx], c) = do
--       let lb = substituteName i (SoP2 $ termToSoP b SoP..+. SoP.int2SoP 1) idx
--       pure . SoP.scaleSoP c . termToSoP $
--         SumSlice (Var vnx) (substituteName i lb idx) (substituteName i ub idx)
--     mkSum i (Var vn) ([Indicator (Idx (Var vnx) idx)] idx], c) = do
--       let lb = substituteName i (SoP2 $ termToSoP b SoP..+. SoP.int2SoP 1) idx
--       pure . SoP.scaleSoP c . termToSoP $
--         SumSlice (Indicator (Var vnx)) (substituteName i lb idx) (substituteName i ub idx)
--     mkSum i (Indicator (Var vn)) ([Idx (Var vnx) idx], c) = do
--       let lb = substituteName i (SoP2 $ termToSoP b SoP..+. SoP.int2SoP 1) idx
--       pure . SoP.scaleSoP c . termToSoP $
--         SumSlice (Indicator (Var vnx)) (substituteName i lb idx) (substituteName i ub idx)
--     mkSum _ _ _ = Nothing
