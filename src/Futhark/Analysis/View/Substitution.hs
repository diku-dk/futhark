module Futhark.Analysis.View.Substitution (sub) where
import Language.Futhark (VName)
import Futhark.Util.Pretty
import Futhark.Analysis.View.Representation
import Futhark.Analysis.View.Monad (IndexFnM, VEnv (algenv))
import qualified Text.LaTeX.Packages.AMSMath as Math
import Control.Monad.RWS
import Futhark.Analysis.View.Latex (toLaTeX)
import Data.Functor.Identity
import Debug.Trace (trace)
import Futhark.Analysis.View.Refine (equivD)
import Futhark.SoP.SoP (scaleSoP, (.-.), (.+.), int2SoP, padWithZero, sopToLists, SoP)
import Futhark.MonadFreshNames (newNameFromString)

debug :: String -> a -> a
debug msg = trace ("🌪️🎭 " <> msg)

-- Substitution rules.
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
sub :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
sub x for@(IndexFn (Forall i _) _) into@(IndexFn (Forall j _) _) = do
  i' <- newNameFromString "i"
  debugM ("fresh name " <> prettyString i')
  sub' x (rename i i' for) (rename j i' into)
sub x q r = sub' x q r

sub' :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
sub' x (IndexFn Empty xs) (IndexFn iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  pure $
    IndexFn
      iter_y
      (listToCases $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure $ substituteName x xval (ycond :&& xcond, yval))
sub' x q@(IndexFn (Forall i (Iota {})) xs) r@(IndexFn iter_y@(Forall i' _) ys)
  | i == i' = do
    -- This is solely to handle substitution of parameters for top-level defs.
    let iter = case casesToList xs of
                 [(Bool True, xval)] -> substituteIdx i (x, xval) iter_y
                 _ -> iter_y
    -- Substitution Rules 1 and 2.
    let res =
          IndexFn
            iter
            (listToCases $ do
              (xcond, xval) <- casesToList xs
              (ycond, yval) <- casesToList ys
              pure $ substituteIdx i (x, xval) (ycond :&& xcond, yval))
    tell ["Substitute (Rule 1) " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q <> " to get " <> toLaTeX res]
    pure $
      debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r)
      res
sub' x q@(IndexFn (Forall i (Cat k m b)) xs) r@(IndexFn iter_y@(Forall i' (Cat k' m' b')) ys)
  | i == i',
    k == k',
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
            pure $ substituteIdx i (x, xval) (ycond :&& xcond, yval))
sub' x q@(IndexFn iter_x@(Forall i xD@(Cat {})) xs) r@(IndexFn (Forall i' yD@(Iota {})) ys)
  | i == i' = do
      equivalent_domains <- equivD xD yD
      if equivalent_domains
      then do
        let res =
              IndexFn
                iter_x -- XXX iter_x
                (listToCases $ do
                  (xcond, xval) <- casesToList xs
                  (ycond, yval) <- casesToList ys
                  pure $ substituteIdx i (x, xval) (ycond :&& xcond, yval))
        tell ["Substitute(?) " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
        pure $
          debug ("sub(?) " <> prettyString x <> " for " <> prettyString q <> "\n  in " <> prettyString r
                 <> "\n  to get " <> prettyString res) res
      else do
        algenv <- gets algenv
        pure . error $ "💀 sub "
                <> prettyString x <> " for "
                <> prettyString q <> "\n   in "
                <> prettyString r
                <> prettyString (xD == yD)
                <> prettyString algenv
sub' x q r = do
  algenv <- gets algenv
  pure . error $ "💀 sub "
          <> prettyString x <> " for "
          <> prettyString q <> "\n   in "
          <> prettyString r
          <> prettyString algenv

substituteIdx :: ASTMappable a => VName -> (VName, Term) -> a -> a
substituteIdx i (x, xterm) yterm =
  runIdentity $ astMap m yterm
  where
    m =
      ASTMapper
        { mapOnTerm = onTerm,
          mapOnVName = pure
        }
    onTerm (Var vn) =
      pure $ if vn == x then xterm else Var vn
    onTerm (Idx (Var vn) idx) | vn == x =
      pure (flatten $ substituteName i (SoP2 idx) xterm)
    onTerm (SumSlice e lb ub) | e == Var x || e == Indicator (Var x) = do
      e' <- onTerm e
      case justAffine e' of
        Just terms ->
          pure . SoP2 $ mkSumOfSums i lb ub terms
        Nothing -> error "gg, need sums over non-affine terms"
    onTerm e = astMap m e
