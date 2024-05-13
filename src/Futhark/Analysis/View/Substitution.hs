module Futhark.Analysis.View.Substitution (sub) where
import Language.Futhark (VName)
import Futhark.Util.Pretty
import Futhark.Analysis.View.Representation
import qualified Data.List.NonEmpty as NE
import Futhark.Analysis.View.Monad (IndexFnM)
import qualified Text.LaTeX.Packages.AMSMath as Math
import Control.Monad.RWS
import Futhark.Analysis.View.Latex (toLaTeX)
import Data.Functor.Identity
import Debug.Trace (trace)

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
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q@(IndexFn (Forall i (Iota {})) xs) r@(IndexFn (Forall j yD) ys) = do
  -- Substitution Rules 1 and 2.
  tell ["Substitute " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
  pure $
    debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r) $
      IndexFn
        (Forall j yD)
        (Cases . NE.fromList $ do
          (xcond, xval) <- casesToList xs
          (ycond, yval) <- casesToList ys
          pure (substituteIdx (i, x, xval) (j, ycond) :&& xcond,
                substituteIdx (i, x, xval) (j, yval)))
sub x q@(IndexFn (Forall i (Cat k m b)) xs) r@(IndexFn iter_y@(Forall j (Cat k' m' b')) ys)
  | k == k',
    m == m',
    b == b' = do
      tell ["Substitute " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
      pure $
        debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r) $
        IndexFn
          iter_y
          (Cases . NE.fromList $ do
            (xcond, xval) <- casesToList $ substituteName i (Var j) xs
            (ycond, yval) <- casesToList ys
            pure (substituteIdx (i, x, xval) (j, ycond) :&& xcond,
                  substituteIdx (i, x, xval) (j, yval)))
sub x q r =
  pure . error $ "💀 sub "
          <> prettyString x <> " for "
          <> prettyString q <> "\n   in "
          <> prettyString r

substituteIdx :: ASTMappable a => (VName, VName, Term) -> (VName, a) -> a
substituteIdx (i, x, xval) (j, y) = do
  runIdentity $ astMap substituter y
  where
    substituter =
      ASTMapper
        { mapOnTerm = onTerm,
          mapOnVName = pure
        }
    onTerm (Var vn) | vn == x =
      pure (substituteName i (Var j) xval)
    onTerm e@(Var _) =
      pure e
    onTerm (Idx (Var vn) idx) | vn == x =
      pure (flatten $ substituteName i (SoP2 idx) xval)
    onTerm e = astMap substituter e
