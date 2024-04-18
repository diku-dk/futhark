module Futhark.Analysis.View.Substitution (sub) where
import Language.Futhark (VName)
import Futhark.Util.Pretty
import Futhark.Analysis.View.Representation
import qualified Data.List.NonEmpty as NE

-- Substitution rules.
sub :: VName -> View -> View -> View
sub x q@(View (Forall i xD) xs) r@(View (Forall j yD) ys)
  | xD == yD =
  -- Substitution Rule 1 where y indexes x.
    debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n   in " <> prettyString r) $ View
      (Forall j yD)
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList $ substituteName i (Var j) xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q@(View Empty xs) r@(View iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n   in " <> prettyString r) $
    View
      iter_y
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q r =
  error $ "💀 sub "
          <> prettyString x <> " for "
          <> prettyString q <> "\n   in "
          <> prettyString r
