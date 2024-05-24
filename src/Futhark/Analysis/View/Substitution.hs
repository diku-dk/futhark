module Futhark.Analysis.View.Substitution (sub) where
import Language.Futhark (VName)
import Futhark.Util.Pretty hiding (cat)
import Futhark.Analysis.View.Representation
import Futhark.Analysis.View.Monad (IndexFnM, VEnv (algenv))
import qualified Text.LaTeX.Packages.AMSMath as Math
import Control.Monad.RWS
import Futhark.Analysis.View.Latex (toLaTeX)
import Data.Functor.Identity
import Debug.Trace (trace)
import Futhark.SoP.SoP (scaleSoP, (.-.), (.+.), int2SoP, padWithZero, sopToLists, SoP)
import Futhark.MonadFreshNames (newNameFromString)
import Futhark.Analysis.View.Refine (refineTerm)

whenM :: Monad m => m Bool -> a -> m (Maybe a)
whenM condition value = do
  c <- condition
  pure $ if c then Just value else Nothing

eqDomains :: Domain -> Domain -> IndexFnM Bool
eqDomains a b | trace ("eqDomains " <> unwords (map prettyString [a, b])) False = undefined
eqDomains a b = do
  match <- refineTerm $
    domainStart a :== domainStart b :&& domainEnd a :== domainEnd b
  case match of
    Bool True -> pure True
    _ -> pure False

leqDomains :: Domain -> Domain -> IndexFnM Bool
leqDomains a b | trace ("leqDomains " <> unwords (map prettyString [a, b])) False = undefined
leqDomains a b = do
  match <- refineTerm $
    domainStart a :>= domainStart b :&& domainEnd a :<= domainEnd b
  case match of
    Bool True -> pure True
    _ -> pure False

getCompatibleDomain :: Domain -> Domain -> IndexFnM (Maybe Domain)
getCompatibleDomain iota1@(Iota {}) iota2@(Iota {}) = do
  whenM (iota2 `leqDomains` iota1) iota2
getCompatibleDomain (Iota m) cat@(Cat _ m' _)
  | m == m' =
      pure (Just cat)
getCompatibleDomain iota@(Iota {}) cat@(Cat {}) = do
  whenM (iota `eqDomains` cat) cat
getCompatibleDomain cat@(Cat {}) iota@(Iota {}) = do
  whenM (cat `eqDomains` iota) cat
getCompatibleDomain (Cat k m b) cat2@(Cat k' m' b')
  | k == k',
    m == m',
    b == b' =
      pure (Just cat2)
getCompatibleDomain _ _ = pure Nothing

-- Substitution rules.
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
sub :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
sub x for@(IndexFn (Forall i _) _) into@(IndexFn (Forall j _) _) = do
  i' <- newNameFromString "i"
  debugM ("🌪️🎭 sub " <> prettyString x <> " for " <> prettyString for <> "\n  in " <> prettyString into)
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
sub' x q@(IndexFn (Forall i dx) xs) r@(IndexFn (Forall i' dy) ys)
  | i == i' = do
  d' <- getCompatibleDomain dx dy
  case d' of
    Just d'' -> do
      -- This is solely to handle substitution of parameters for top-level defs.
      let d = case casesToList xs of
                 [(Bool True, xval)] -> substituteIdx i (x, xval) d''
                 _ -> d''
      -- Substitution rule.
      tell ["Substitute " <> Math.math (toLaTeX x) <> " for " <> toLaTeX q]
      pure $
        IndexFn
          (Forall i d)
          (listToCases $ do
            (xcond, xval) <- casesToList xs
            (ycond, yval) <- casesToList ys
            pure $ substituteIdx i (x, xval) (ycond :&& xcond, yval))
    Nothing -> do
      algenv <- gets algenv
      pure . error $ "💀 sub "
              <> prettyString x <> " for "
              <> prettyString q <> "\n   in "
              <> prettyString r
              <> prettyString (dx == dy)
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
