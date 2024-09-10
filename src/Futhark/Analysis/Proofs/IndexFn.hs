{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Proofs.IndexFn
where

import qualified Data.Map as M
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.List.NonEmpty as NE
import Futhark.Analysis.Proofs.Symbol
import Futhark.SoP.SoP (SoP, int2SoP, (.-.), (.+.), sym2SoP)
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))
import Futhark.MonadFreshNames
import Control.Monad.RWS.Strict
import Futhark.Analysis.Proofs.Unify (Renameable (..), Replaceable (..), Substitution, Unify (..), unifies)
import Debug.Trace (trace, traceM)
import Futhark.Util.Pretty (Pretty (pretty), (<+>), commasep, parens, stack, indent, line, prettyString)
import Futhark.Analysis.Proofs.Util (prettyName)

data Domain = Iota (SoP Symbol) -- [0, ..., n-1]
            | Cat            -- Catenate_{k=1}^{m-1} [b_{k-1}, ..., b_k)
                VName        -- k
                (SoP Symbol) -- m
                (SoP Symbol) -- b
  deriving Show

data Iterator = Forall VName Domain
              | Empty
  deriving Show

newtype Cases a b = Cases (NE.NonEmpty (a, b))
  deriving (Show, Eq, Ord)

cases :: [(a,b)] -> Cases a b
cases = Cases . NE.fromList

casesToList ::  Cases a b -> [(a, b)]
casesToList (Cases cs) = NE.toList cs

data IndexFn = IndexFn
  { iterator :: Iterator,
    body :: Cases Symbol (SoP Symbol)
  }
  deriving (Show, Eq)

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Symbol E.Exp,
    indexfns :: M.Map VName IndexFn
    -- toplevel :: M.Map E.VName ([E.Pat], IndexFn)
  }


-- The IndexFn monad keeps a source of fresh names and writes indexfns.
newtype IndexFnM a = IndexFnM (RWS () () VEnv a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      MonadState VEnv
    )

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

-- This is required by MonadSoP.
instance Nameable Symbol where
  mkName (VNameSource i) = (Var $ E.VName "x" i, VNameSource $ i + 1)

instance MonadSoP Symbol E.Exp IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

runIndexFnM :: IndexFnM a -> VNameSource -> (a, M.Map VName IndexFn)
runIndexFnM (IndexFnM m) vns = getRes $ runRWS m () s
  where
    getRes (x, env, _) = (x, indexfns env)
    s = VEnv vns mempty mempty

insertIndexFn :: E.VName -> IndexFn -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}

-- insertTopLevel :: E.VName -> ([E.Pat], IndexFn) -> IndexFnM ()
-- insertTopLevel vn (args, ixfn) =
--   modify $
--     \env -> env {toplevel = M.insert vn (args, ixfn) $ toplevel env}

-- clearAlgEnv :: IndexFnM ()
-- clearAlgEnv =
--   modify $ \env -> env {algenv = mempty}

tracer :: Pretty a => a -> a
tracer x = trace (prettyString x) x

repCases :: Substitution (SoP Symbol) -> Cases Symbol (SoP Symbol) -> Cases Symbol (SoP Symbol)
repCases s (Cases cs) | trace ("repCases " <> prettyString s <> " " <> prettyString cs) False = undefined
repCases s (Cases cs) =
  tracer $ Cases $ NE.map (\(p,q) -> (sop2Symbol (rep s p), rep s q)) cs

repDomain :: Substitution (SoP Symbol) -> Domain -> Domain
repDomain s (Iota n) = Iota (rep s n)
repDomain s (Cat k m b) = Cat k (rep s m) (rep s b)

repVName :: Substitution (SoP Symbol) -> VName -> VName
repVName s vn
  | Var i <- sop2Symbol $ rep s (Var vn) =
    i
repVName _ _ = error "repVName substitutes for non-VName."


subIndexFn :: Substitution (SoP Symbol) -> IndexFn -> IndexFnM IndexFn
subIndexFn s indexfn = rip <$> rename indexfn
  where
  rip (IndexFn Empty body) = IndexFn Empty (repCases s body)
  rip (IndexFn (Forall i dom) body) =
    IndexFn (Forall (repVName s i) (repDomain s dom)) (repCases s body)

repIteratorInBody :: SoP Symbol -> IndexFn -> IndexFn
repIteratorInBody x (IndexFn it@(Forall i _) body) =
  IndexFn it (repCases (M.singleton i x) body)
repIteratorInBody _ indexfn = indexfn


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


instance MonadFreshNames m => Unify Domain (SoP Symbol) m where
  unify_ k (Iota n) (Iota m) = unify_ k n m
  unify_ k (Cat _ m1 b1) (Cat _ m2 b2) = do
    s <- unify_ k m1 m2
    (s <>) <$> unify_ k (rep s b1) (rep s b2)
  unify_ _ _ _ = fail "Incompatible domains"

instance MonadFreshNames m => Unify (Cases Symbol (SoP Symbol)) (SoP Symbol) m where
  unify_ k (Cases cs1) (Cases cs2) = do
    s <- unifies k (zip (map fst xs) (map fst ys))
    traceM ("unify " <> prettyString (map fst xs) <> prettyString (map fst ys))
    s2 <- unifies k (zip (map (rep s . snd) xs) (map (rep s . snd) ys))
    traceM ("unify " <> prettyString (map snd xs) <> prettyString (map snd ys))
    traceM ("unify " <> prettyString (map (rep s . snd) xs) <> prettyString (map (rep s . snd) ys))
    pure $ s <> s2
    where
      xs = NE.toList cs1
      ys = NE.toList cs2

-- XXX we require that index function quantifiers (indexing variables) are unique!
instance MonadFreshNames m => Unify IndexFn (SoP Symbol) m where
  unify_ _ fn1 fn2 | trace ("\nunify\n  " <> prettyString fn1 <> "\n  " <> prettyString fn2) False = undefined
  unify_ k (IndexFn Empty body1) (IndexFn Empty body2) =
    unify_ k body1 body2
  unify_ k (IndexFn (Forall i dom1) body1) (IndexFn (Forall j dom2) body2) = do
    s <- unify_ k (Hole i) (Var j)
    s' <- (s <>) <$> unify_ k (repDomain s dom1) (repDomain s dom2)
    (s' <>) <$> unify_ k (repCases s' body1) (repCases s' body2)
  unify_ _ _ _ = fail "Incompatible iterators"


-- NOTE don't think this should be re-implemented. Instead use
-- unification on index functions.
--
instance Eq Domain where
  -- Since the whole domain must be covered by an index function,
  -- it is sufficient to check that starts and ends are equal.
  u == v =
    start u == start v && end u == end v
    where
      start :: Domain -> SoP Symbol
      start (Iota _) = int2SoP 0
      start (Cat k _ b) = rep (M.singleton k $ int2SoP 0) b

      end (Iota n) = n .-. int2SoP 1
      end (Cat k m b) = rep (M.singleton k m) b .-. int2SoP 1

instance Eq Iterator where
  (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
  (Forall _ u) == (Forall _ v) = u == v
  Empty == Empty = True
  _ == _ = False

instance (Pretty a, Pretty b) => Pretty (Cases a b) where
  pretty (Cases cs) = -- stack (map prettyCase (NE.toList cases))
    line <> indent 4 (stack (map prettyCase (NE.toList cs)))
    where
      prettyCase (p, e) = "|" <+> pretty p <+> "⇒ " <+> pretty e

instance Pretty Domain where
  pretty (Iota n) = "iota" <+> parens (pretty n)
  pretty dom@(Cat k m _) =
    "⊎"
      <> prettyName k
      <> "="
      <> "iota" <+> pretty m
      <+> "[" <> commasep [
            pretty (intervalStart dom),
            "...",
            pretty (intervalEnd dom)
          ] <> ")"
    where
      intervalStart (Cat _ _ b) = b
      intervalStart (Iota _) = error "intervalStart on iota"

      intervalEnd (Cat k _ b) = rep (M.singleton k (sym2SoP (Var k) .+. int2SoP 1)) b
      intervalEnd (Iota _) = error "intervalEnd on iota"


instance Pretty Iterator where
  pretty (Forall i dom) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom
  pretty Empty = ""

instance Pretty IndexFn where
  pretty (IndexFn iter e) = pretty iter <+> "." <+> pretty e
