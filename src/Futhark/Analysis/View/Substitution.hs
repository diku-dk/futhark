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
    onTerm e = astMap m e
