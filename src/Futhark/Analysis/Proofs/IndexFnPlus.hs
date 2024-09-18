{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.IndexFnPlus
where

import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.SymbolPlus ()
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Unify (Renameable (..), Replaceable (..), Substitution (..), Unify (..), unifies_, SubstitutionBuilder (..))
import Futhark.SoP.SoP (SoP, sym2SoP, int2SoP, (.+.), (.-.), mapSymSoP)
import Futhark.Util.Pretty (Pretty (pretty), (<+>), parens, commasep, prettyString, line, indent, stack)
import Language.Futhark (VName)
import Futhark.MonadFreshNames (MonadFreshNames, newNameFromString, newVName)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Futhark.Analysis.Proofs.Util (prettyName, prettyHole)
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.Traversals (ASTMapper(..), ASTMappable (astMap))
import Data.Functor.Identity (runIdentity)

instance Eq Domain where
  -- Since the whole domain must be covered by an index function,
  -- it is sufficient to check that starts and ends are equal.
  u == v =
    start u == start v && end u == end v
    where
      start :: Domain -> SoP Symbol
      start (Iota _) = int2SoP 0
      start (Cat k _ b) = rep (mkSub k (int2SoP 0 :: SoP Symbol)) b

      end (Iota n) = n .-. int2SoP 1
      end (Cat k m b) = rep (mkSub k m) b .-. int2SoP 1

instance Eq Iterator where
  (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
  (Forall _ u) == (Forall _ v) = u == v
  Empty == Empty = True
  _ == _ = False


deriving instance Eq IndexFn

-- TODO Eww don't do that here.
deriving instance Ord u => Eq (Substitution u)

-------------------------------------------------------------------------------
-- Pretty.
-------------------------------------------------------------------------------
instance (Pretty a, Pretty b) => Pretty (Cases a b) where
  pretty (Cases cs) =
    line <> indent 4 (stack (map prettyCase (NE.toList cs)))
    where
      prettyCase (p, e) = "|" <+> pretty p <+> "⇒ " <+> pretty e

instance Pretty Domain where
  pretty (Iota n) = "iota" <+> parens (pretty n)
  pretty (Cat k m b) =
    "⊎"
      <> prettyName k
      <> "="
      <> "iota" <+> pretty m
      <+> "[" <> commasep [
            pretty b,
            "...",
            pretty intervalEnd
          ] <> ")"
    where
      intervalEnd :: SoP Symbol
      intervalEnd = rep (mkSub k (sym2SoP (Var k) .+. int2SoP 1)) b

instance Pretty Iterator where
  pretty (Forall i dom) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom
  pretty Empty = ""

instance Pretty IndexFn where
  pretty (IndexFn iter e) = pretty iter <+> "." <+> pretty e

-------------------------------------------------------------------------------
-- Unification.
-------------------------------------------------------------------------------
repVName :: Substitution Symbol -> VName -> VName
repVName s vn
  | Var i <- sop2Symbol $ rep s (Var vn) =
    i
repVName _ _ = error "repVName substitutes for non-VName."

repCase :: (Ord u, Replaceable v1 u, Replaceable v2 u) => Substitution u -> (v1, v2) -> (u, SoP u)
repCase s (a, b) = (sop2Symbol (rep s a), rep s b)

repCases :: (Ord a, Replaceable v1 a, Replaceable v2 a) => Substitution a -> Cases v1 v2 -> Cases a (SoP a)
repCases s (Cases cs) = Cases $ NE.map (repCase s) cs

repDomain :: Substitution Symbol -> Domain -> Domain
repDomain s (Iota n) = Iota (rep s n)
repDomain s (Cat k m b) = Cat k (rep s m) (rep s b)

repIndexFn :: Substitution Symbol -> IndexFn -> IndexFn
repIndexFn s = rip
  where
    rip (IndexFn Empty body) = IndexFn Empty (repCases s body)
    rip (IndexFn (Forall i dom) body) =
      IndexFn (Forall (repVName s i) (repDomain s dom)) (repCases s body)

subIndexFn :: Substitution Symbol -> IndexFn -> IndexFnM IndexFn
subIndexFn s indexfn = repIndexFn s <$> rename indexfn

instance (Renameable a, Renameable b) => Renameable (Cases a b) where
  rename_ tau (Cases cs) = Cases <$> mapM re cs
    where
      re (p,q) = (,) <$> rename_ tau p <*> rename_ tau q

instance Renameable Domain where
  rename_ tau (Cat k m b) = do
    k' <- newNameFromString "k"
    let tau' = M.insert k k' tau
    Cat k' <$> rename_ tau' m <*> rename_ tau' b
  rename_ tau (Iota n) = Iota <$> rename_ tau n

instance Renameable IndexFn where
  rename_ tau indexfn = case indexfn of
    IndexFn Empty body -> IndexFn Empty <$> rename_ tau body
    IndexFn (Forall i dom) body -> do
      -- NOTE that i is not renamed.
      dom' <- rename_ tau dom
      IndexFn (Forall i dom') <$> rename_ tau body

instance SubstitutionBuilder (Cases Symbol (SoP Symbol)) Symbol where
  addSub vn x s = s { subCases = M.insert vn x $ subCases s }

instance SubstitutionBuilder Domain Symbol where
  addSub vn x s = s { subDomain = M.insert vn x $ subDomain s }

instance MonadFreshNames m => Unify Domain Symbol m where
  unify_ k (Iota n) (Iota m) = unify_ k n m
  unify_ k (Cat _ m1 b1) (Cat _ m2 b2) = do
    s <- unify_ k m1 m2
    (s <>) <$> unify_ k (rep s b1) (rep s b2)
  unify_ _ _ _ = fail "Incompatible domains"

instance MonadFreshNames m => Unify (Cases Symbol (SoP Symbol)) Symbol m where
  unify_ k (Cases cs1) (Cases cs2) = do
    s <- unifies_ k (zip (map fst xs) (map fst ys))
    s2 <- unifies_ k (zip (map (rep s . snd) xs) (map (rep s . snd) ys))
    pure $ s <> s2
    where
      xs = NE.toList cs1
      ys = NE.toList cs2

-- XXX we require that index function quantifiers (indexing variables) are unique!
instance MonadFreshNames m => Unify IndexFn Symbol m where
  unify_ k (IndexFn Empty body1) (IndexFn Empty body2) =
    unify_ k body1 body2
  unify_ k (IndexFn (Forall i dom1) body1) (IndexFn (Forall j dom2) body2) = do
    s <- unify_ k (Hole i) (Var j)
    s' <- (s <>) <$> unify_ k (repDomain s dom1) (repDomain s dom2)
    (s' <>) <$> unify_ k (repCases s' body1) (repCases s' body2)
  unify_ _ _ _ = fail "Incompatible iterators"

-------------------------------------------------------------------------------
-- Index function substitution.
-------------------------------------------------------------------------------
-- 'sub vn x y' substitutes name 'vn' for indexfn 'x' in indexfn 'y'.
subst :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
subst x for@(IndexFn (Forall i _) _) into@(IndexFn (Forall j _) _) = do
  traceM ("🎭 sub " <> prettyString x <> " for " <> prettyString for <> "\n  in " <> prettyString into)
  i' <- sym2SoP . Var <$> newNameFromString "i"
  traceM ("fresh name " <> prettyString i')
  for' <- rename for
  into' <- rename into
  subst' x (repIndexFn (mkSub i i') for') (repIndexFn (mkSub j i') into')
subst x q r = subst' x q r

subst' :: VName -> IndexFn -> IndexFn -> IndexFnM IndexFn
subst' x (IndexFn Empty xs) (IndexFn iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  pure $
    IndexFn
      iter_y
      (cases $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure $ repCase (mkSub x xval) (ycond :&& xcond, yval))
subst' f (IndexFn (Forall _ (Iota n)) xs) (IndexFn (Forall i (Iota n')) ys)
  | n == n' =
    pure $
      IndexFn
        (Forall i (Iota n))
        (cases $ do
          (xcond, xval) <- casesToList xs
          (ycond, yval) <- casesToList ys
          let app = apply i (mkSub f xval)
          pure (sop2Symbol (app ycond) :&& sop2Symbol (app xcond), mapSymSoP app yval))
subst' _ _ _ = undefined

apply :: VName -> Substitution Symbol -> Symbol -> SoP Symbol
apply i s symbol = case symbol of
  Hole _ -> error "Apply on Hole."
  Idx (Var vn) j -> rep (mkSub i j) $ rep s (Var vn)
  _ -> rep s symbol

-- rimelig sikker på refactor var en fejl;
-- 1. ikke lav index fn sub som unification
-- 2. fjern subCases subDomain fra Substitution
-- 3. rul refactor tilbage som nødvendig (tror ikke nogen ændringer nødvendige)
-- 4. definer compatible domains? eller lav cases i subst'
