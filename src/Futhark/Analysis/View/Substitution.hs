module Futhark.Analysis.View.Substitution (sub) where
import Language.Futhark (VName)
import Futhark.Util.Pretty
import Futhark.Analysis.View.Representation
import qualified Data.List.NonEmpty as NE

-- Substitution rules.
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
sub :: VName -> IndexFn -> IndexFn -> IndexFn
sub x q@(IndexFn Empty xs) r@(IndexFn iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n   in " <> prettyString r) $
    IndexFn
      iter_y
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q@(IndexFn (Forall i (Iota {})) xs) r@(IndexFn (Forall j yD) ys) =
  -- Substitution Rules 1 and 2.
    debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n   in " <> prettyString r) $ IndexFn
      (Forall j yD)
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList $ substituteName i (Var j) xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q r =
  error $ "💀 sub "
          <> prettyString x <> " for "
          <> prettyString q <> "\n   in "
          <> prettyString r
