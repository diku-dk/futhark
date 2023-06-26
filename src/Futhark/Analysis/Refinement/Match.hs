{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Futhark.Analysis.Refinement.Match where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.SoP (SoP)
import Futhark.SoP.SoP qualified as SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark (VName)
import Language.Futhark qualified as E

class Unify a b where
  unify :: VName -> a -> b -> [Subst]

instance (AddSubst b, Holes b, Free b, Context b) => Unify VName b where
  unify k x b
    | x >= k = mempty
    | x `occursIn` b = mempty
    | not $ S.null (S.filter (>= k) $ fv b) = mempty
    | hasHoles b = mempty
    | otherwise =
        let s = M.singleton x b
         in pure $ addSubst x b mempty

instance Unify Term Term where
  unify k e1 e2 = unifies [(e1, e2)]
    where
      unifies :: [(Term, Term)] -> [Subst]
      unifies [] = pure mempty
      unifies ((t1, t2) : es) = do
        (s, new_es) <- unifyOne (flatten t1) (flatten t2)
        let es' = replace s es
        fmap (s <>) $ unifies $ new_es ++ es'

      unifyHole ::
        Hole -> Term -> [Subst]
      unifyHole Unnamed _ = mempty
      unifyHole h@(CHole x a@(SoP sop)) b@(SoP sop') =
        choice $
          ( do
              (ctx, arg) <- sop_contexts
              s <- unify k a arg
              pure [addSubst x ctx s]
          )
            ++ ( do
                   (ctx, arg) <- contexts b
                   s <- unifyHole h arg
                   pure [addSubst x ctx s]
               )
        where
          sop_contexts = do
            perm <- L.permutations $ SoP.sopToLists sop'
            let (cand, ctx) = L.splitAt (SoP.numTerms sop) perm
            pure
              ((SoP (SoP.sopFromList ctx) ~+~), SoP $ SoP.sopFromList cand)
      unifyHole h@(CHole x a) b =
        choice $
          unify k a b : do
            (ctx, arg) <- contexts b
            s <- unifyHole h arg
            pure [addSubst x ctx s]
      unifyHole (Hole x) b =
        pure $ addSubst x b mempty

      unifyOneSoPTerm ::
        (SoP.Term Term, Integer) ->
        (SoP.Term Term, Integer) ->
        [Subst]
      unifyOneSoPTerm (xs, a) (ys, b) =
        case (xs', ys') of
          ([THole h], _) -> unifyHole h $ SoP $ SoP.term2SoP ys b
          (_, [THole h]) -> unifyHole h $ SoP $ SoP.term2SoP xs a
          _
            | length xs' == length ys',
              a == b ->
                do
                  xs'' <- L.permutations xs'
                  unifies $ zip xs'' ys'
          _ -> mempty
        where
          xs' = SoP.termToList xs
          ys' = SoP.termToList ys
          isHole THole {} = True
          isHole _ = False

      unifyOneSoP :: SoP Term -> SoP Term -> [(Subst, [(Term, Term)])]
      unifyOneSoP x y
        | length xs == length ys = do
            xs' <- L.permutations xs
            noSubProblems $
              unifySoPTerms $
                zip xs' ys
        | otherwise = mempty
        where
          xs = SoP.sopToList x
          ys = SoP.sopToList y
          unifySoPTerms :: [((SoP.Term Term, Integer), (SoP.Term Term, Integer))] -> [Subst]
          unifySoPTerms [] = pure mempty
          unifySoPTerms ((t1, t2) : es) = do
            s <- unifyOneSoPTerm t1 t2
            let es' =
                  map
                    ( \((t1', a), (t2', b)) ->
                        ((replace s t1', a), (replace s t2', b))
                    )
                    es
            fmap (s <>) $ unifySoPTerms es'

      unifyOne :: Term -> Term -> [(Subst, [(Term, Term)])]
      unifyOne t1 t2
        | t1 == t2 = pure mempty
      unifyOne (THole h) t2 =
        noSubProblems $ unifyHole h t2
      unifyOne t1 (THole h) =
        noSubProblems $ unifyHole h t1
      unifyOne (Var x) t2 = noSubProblems $ unify k x t2
      unifyOne e1 (Var y) = pure (mempty, [(Var y, e1)])
      unifyOne (SoP x) (SoP y) = unifyOneSoP x y
      unifyOne (Len x) (Len y) = pure (mempty, [(x, y)])
      unifyOne (Elems x) (Elems y) = pure (mempty, [(x, y)])
      unifyOne (Set xs) (Set ys) =
        pure (mempty, zip (S.toList xs) (S.toList ys))
      unifyOne (Array xs) (Array ys) =
        pure (mempty, zip xs ys)
      unifyOne (Range from step to) (Range from' step' to') =
        pure (mempty, [(from, from'), (step, step'), (to, to')])
      unifyOne (Idx arr i) (Idx arr' i') =
        pure (mempty, [(arr, arr'), (i, i')])
      unifyOne (Union x y) (Union x' y') =
        pure (mempty, [(x, x'), (y, y')])
      unifyOne (Unions i s c xs) (Unions i' s' c' xs') =
        pure (mempty, [(i, i'), (s, s'), (c, c'), (xs, xs')])
      unifyOne (Sigma i s e) (Sigma i' s' e') =
        pure (mempty, [(i, i'), (s, s'), (e, e')])
      unifyOne (If c t f) (If c' t' f') =
        pure (mempty, [(c, c'), (t, t'), (f, f')])
      unifyOne (BoolToInt x) (BoolToInt x') =
        pure (mempty, [(x, x')])
      unifyOne (e1 :== e2) (e1' :== e2') = pure (mempty, [(e1, e1'), (e2, e2')])
      unifyOne (e1 :> e2) (e1' :> e2') = pure (mempty, [(e1, e1'), (e2, e2')])
      unifyOne (PermutationOf e1 e2) (PermutationOf e1' e2') = pure (mempty, [(e1, e1'), (e2, e2')])
      unifyOne (Forall x p1 p2) (Forall y p1' p2') = pure (mempty, [(p1, p1'), (p2, p2')])
      unifyOne (Not p) (Not p') =
        pure (mempty, [(p, p')])
      unifyOne _ _ = mempty

      noSubProblems = fmap (,mempty)

unifyM ::
  (Show a, Show b, MonadFreshNames m, Rename a, Rename b, Holes a, Holes b, Unify a b) =>
  a ->
  b ->
  m ([Subst], (a, b))
unifyM a b = do
  a' <- instHole a
  b' <- instHole b
  k <- newVName "k"
  vns <- getNameSource
  a'' <- rename mempty a'
  putNameSource vns
  b'' <- rename mempty b'
  pure (unify k a'' b'', (a'', b''))

class Match a b where
  match :: (MonadFreshNames m) => a -> b -> m [Subst]
  testmatch :: (MonadFreshNames m) => a -> b -> m [(b, Subst)]
  varMatch :: (MonadFreshNames m) => a -> b -> m [Subst]

instance
  (Show a, Show b, Rename a, Rename b, Holes a, Holes b, Replace a, Replace b, Unify a b) =>
  Match a b
  where
  varMatch x y = fst <$> unifyM x y
  match x y =
    filter
      ( \(Subst s_term s_ctx) ->
          S.fromList (M.keys s_term <> M.keys s_ctx)
            `S.isSubsetOf` holes x
      )
      <$> varMatch x y
  testmatch x y = do
    (mm, (x', y')) <- unifyM x y
    pure $ do
      s@(Subst s_term s_ectx) <- mm
      guard
        ( S.fromList (M.keys s_term <> M.keys s_ectx)
            `S.isSubsetOf` holes x
        )
      pure s
      pure $ (replace s y', s)

class (Unify a b) => Instantiate a b where
  instantiateWith :: (MonadFreshNames m) => a -> b -> m [a]

instance Instantiate Term Term where
  instantiateWith e p = do
    ctx <- mkCHole
    ss <- varMatch (ctx e :: Term) inner_p
    pure $ map (flip replace $ ctx e) ss
    where
      (inner_p, reqs) = popBinders p
      popBinders (Forall x p1 p2) =
        let (p, ps) = popBinders p2
         in (p, p1 : ps)
      popBinders p = (p, mempty)
