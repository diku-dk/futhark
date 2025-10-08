{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Properties.IndexFnPlus
  ( domainStart,
    domainEnd,
    repIndexFn,
    subIndexFn,
    repCases,
    intervalEnd,
    repDomain,
    unifyIndexFnWith,
    intervalStart,
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.SymbolPlus (repVName)
import Futhark.Analysis.Properties.Unify (FreeVariables (..), Renameable (..), Rep (..), Replacement, ReplacementBuilder (..), Substitution (..), Unify (..), freshNameFromString, unifies_)
import Futhark.Analysis.Properties.Util (prettyName)
import Futhark.SoP.SoP (SoP (SoP), int2SoP, isConstTerm, sym2SoP, (.+.), (.-.))
import Futhark.Util.Pretty (Pretty (pretty), align, comma, commastack, hang, indent, line, parens, punctuate, sep, softline, stack, (<+>))
import Language.Futhark (VName)

domainStart :: Domain -> SoP Symbol
domainStart (Iota _) = int2SoP 0
domainStart (Cat k _ b) = rep (mkRep k (int2SoP 0 :: SoP Symbol)) b

domainEnd :: Domain -> SoP Symbol
domainEnd (Iota n) = n .-. int2SoP 1
domainEnd (Cat k m b) = rep (mkRep k m) b .-. int2SoP 1

intervalStart :: Domain -> SoP Symbol
intervalStart (Cat _ _ b) = b
intervalStart (Iota _) = error "intervalEnd on iota"

intervalEnd :: Domain -> SoP Symbol
intervalEnd (Cat k _ b) = rep (mkRep k (sym2SoP (Var k) .+. int2SoP 1)) b .-. int2SoP 1
intervalEnd (Iota _) = error "intervalEnd on iota"

-------------------------------------------------------------------------------
-- Pretty.
-------------------------------------------------------------------------------
instance (Pretty a, Pretty b) => Pretty (Cases a (SoP b)) where
  pretty (Cases cs) =
    stack (map prettyCase (NE.toList cs))
    where
      prettyCase (p, e) = "|" <+> pretty p <+> "⇒ " <> softline <> indent 2 (hang 2 $ prettySoP e)

      -- Like pretty instance for SoP, but inserts soft linebreaks between top-level +.
      prettySoP (SoP ts)
        | M.null ts = "0"
        | otherwise =
            mconcat $
              punctuate (softline <> "+ ") $
                map (uncurry pTerm) $
                  M.toList ts

      pTerm term n
        | isConstTerm term = pretty n
        | n == 1 = pretty term
        | otherwise = pretty n <> "*" <> pretty term

instance Pretty Domain where
  pretty (Iota n) = parens $ "0 .." <+> parens (pretty n)
  pretty dom@(Cat k m b) =
    "⊎"
      <> prettyName k
      <> "="
      <> parens ("0 .." <+> pretty (m .-. int2SoP 1))
        <+> "["
      <> align (sep $ punctuate comma [pretty b, "...", pretty (intervalEnd dom)])
      <> "]"

instance Pretty Iterator where
  pretty (Forall i (Iota n)) =
    "for " <> prettyName i <+> "<" <+> pretty n <+> "."
  pretty (Forall i dom@(Cat k m seg)) =
    "for "
      <> prettyName i
        <+> "∈"
        <+> "⊎"
      <> parens (prettyName k <+> "<" <+> pretty m)
        <+> "["
      <> commastack [pretty seg, "...", pretty (intervalEnd dom)]
      <> "]"
        <+> "."
      <> line

instance Pretty IndexFn where
  pretty (IndexFn [] e) = "•" <+> pretty e
  pretty (IndexFn iter e) = stack (map pretty iter) <+> pretty e

instance FreeVariables Domain where
  fv (Iota n) = fv n
  fv (Cat _ m b) = fv m <> fv b

instance FreeVariables Iterator where
  fv (Forall _ d) = fv d -- FIXME kept existing behaviour, but shouldn't it be as below?
  -- fv (Forall i d) = fv d S.\\ S.singleton i

instance FreeVariables (Cases Symbol (SoP Symbol)) where
  fv cs = mconcat $ map (\(c, v) -> fv c <> fv v) $ casesToList cs

instance FreeVariables IndexFn where
  fv (IndexFn dims cs) = mconcat (map fv dims) <> fv cs

-------------------------------------------------------------------------------
-- Unification.
-------------------------------------------------------------------------------
repCases :: Replacement Symbol -> Cases Symbol (SoP Symbol) -> Cases Symbol (SoP Symbol)
repCases s (Cases cs) = Cases $ NE.map repCase cs
  where
    repCase (a, b) = (sop2Symbol (rep s a), rep s b)

repDomain :: Replacement Symbol -> Domain -> Domain
repDomain s (Iota n) = Iota (rep s n)
repDomain s (Cat k m b) = Cat k (rep s m) (rep s b)

repIterator :: Replacement Symbol -> Quantified Domain -> Quantified Domain
repIterator s (Forall i dom) = Forall (repVName s i) (repDomain s dom)

repIndexFn :: Replacement Symbol -> IndexFn -> IndexFn
repIndexFn s (IndexFn dims body) = IndexFn (map (repIterator s) dims) (repCases s body)

subIndexFn :: Substitution Symbol -> IndexFn -> IndexFnM IndexFn
subIndexFn s indexfn = repIndexFn (mapping s) <$> rename (vns s) indexfn

instance (Renameable a, Renameable b) => Renameable (Cases a b) where
  rename_ vns tau (Cases cs) = Cases <$> mapM re cs
    where
      re (p, q) = (,) <$> rename_ vns tau p <*> rename_ vns tau q

instance Renameable Domain where
  rename_ vns tau (Cat xn m b) = do
    (xm, vns') <- freshNameFromString vns "k"
    let tau' = M.insert xn xm tau
    Cat xm <$> rename_ vns' tau' m <*> rename_ vns' tau' b
  rename_ vns tau (Iota n) = Iota <$> rename_ vns tau n

instance Renameable Iterator where
  -- NOTE that i is not renamed.
  rename_ vns tau (Forall i dom) = Forall i <$> rename_ vns tau dom

instance Renameable IndexFn where
  rename_ vns tau (IndexFn dims body) = do
    (vns', tau', xs) <- foldM (\(v, t, ds) d -> append3rd ds <$> renameIter v t d) (vns, tau, []) dims
    let dims' = reverse xs
    IndexFn dims' <$> rename_ vns' tau' body
    where
      append3rd cs (a, b, c) = (a, b, c : cs)
      -- Wraps rename_ on Iterator to also return new state for renaming k in body.
      renameIter v t (Forall i (Cat xn m b)) = do
        (xm, v') <- freshNameFromString v "k"
        let t' = M.insert xn xm t
        dom <- Cat xm <$> rename_ v' t' m <*> rename_ v' t' b
        pure (v', t', Forall i dom)
      renameIter v t it =
        (v,t,) <$> rename_ v t it

instance Unify Domain Symbol where
  unify_ k (Iota n) (Iota m) = unify_ k n m
  unify_ k (Cat _ m1 b1) (Cat _ m2 b2) = do
    s <- unify_ k m1 m2
    (s <>) <$> unify_ k (rep s b1) (rep s b2)
  unify_ _ _ _ = fail "Incompatible domains"

instance Unify (Cases Symbol (SoP Symbol)) Symbol where
  unify_ k (Cases cs1) (Cases cs2) = do
    s <- unifies_ k (map fst xs) (map fst ys)
    s2 <- unifies_ k (map (rep s . snd) xs) (map (rep s . snd) ys)
    pure $ s <> s2
    where
      xs = NE.toList cs1
      ys = NE.toList cs2

instance Unify Iterator Symbol where
  unify_ k (Forall i d1) (Forall j d2) = do
    s <- if i == j then pure mempty else unify_ k (Hole i) (Var j)
    (s <>) <$> unify_ k (repDomain s d1) (repDomain s d2)
  unify_ _ _ _ = fail "Incompatible iterators"

instance Unify IndexFn Symbol where
  unify_ = unifyIndexFnWith unify_

unifyIndexFnWith ::
  (VName -> Cases Symbol (SoP Symbol) -> Cases Symbol (SoP Symbol) -> MaybeT IndexFnM (Replacement Symbol)) ->
  VName ->
  IndexFn ->
  IndexFn ->
  MaybeT IndexFnM (Replacement Symbol)
unifyIndexFnWith unifyBody k (IndexFn dims1 body1) (IndexFn dims2 body2) = do
  s <- unifiesIter_ k dims1 dims2
  (s <>) <$> unifyBody k (repCases s body1) (repCases s body2)

unifiesIter_ :: VName -> [Iterator] -> [Iterator] -> MaybeT IndexFnM (Replacement Symbol)
unifiesIter_ k xs ys
  | length xs == length ys = do
      go (zip xs ys)
  | otherwise = fail "different lengths"
  where
    go [] = pure mempty
    go (u : us) = do
      s0 <- uncurry (unify_ k) u
      foldM (\s (a, b) -> (s <>) <$> unify_ k (repIterator s a) (repIterator s b)) s0 us
