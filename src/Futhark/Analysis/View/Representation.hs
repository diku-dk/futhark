-- Task: Create automated test suite.
-- Task: refactor forward to decouple it from substitution logic.
-- Task: Change representation to more restricted one
--   where Exp is a leaf/term in an SoP and cases are
--   of type (Exp, SoP Exp). Possibly rename Exp lol.
--   Use SoP.Rel for relations?
-- Task: mkFlagArray
--   [ ] Extend representation to allow Union iterator
--   [ ] Support necessary scatter rule
-- Task: partition2L
--   Empty segments may be problematic in lstL; maybe
--   add outer condition to index funciton representation?
-- Task: prove that partition2indices index function
--   is a permutation of 0...n-1.
--
-- Task: TODO change Iota to SoP.Range? Then Empty just becomes mempty.
-- Task: Rebase this on top of master (don't need Refinement type machinery rn).

module Futhark.Analysis.View.Representation where

import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)
import Futhark.SoP.SoP (SoP)
import Futhark.SoP.SoP qualified as SoP
import Futhark.MonadFreshNames
import Language.Futhark (VName (VName))
import Language.Futhark qualified as E
import Futhark.Util.Pretty
import Data.Functor.Identity
import Control.Monad.RWS.Strict hiding (Sum)
import Data.List.NonEmpty qualified as NE
import Futhark.SoP.Monad
import Futhark.SoP.Convert (ToSoP (toSoPNum))
import Debug.Trace (traceM, trace)

data Exp =
    Var VName
  -- | SumSlice
  --     VName       -- array
  --     (SoP Exp)   -- lower bound
  --     (SoP Exp)   -- upper bound
  | Sum
      VName       -- index
      (SoP Exp)   -- lower bound
      (SoP Exp)   -- upper bound
      Exp         -- indexed expression
  | Idx
      Exp         -- array -- XXX VName
      (SoP Exp)   -- index
  | SoP2 (SoP Exp) -- XXX Remove!
  | Indicator Exp -- predicate (the corresponding value of 0 or 1 is implicit)
  | -- Predicate expressions follow here for simplicity.
    -- I'm assuming it's safe because the source program was typechecked.
    -- TODO CNF
    Bool Bool
    -- TODO change this to SoP.Rel?
  | Not Exp
  | (:==) Exp Exp
  | (:<) Exp Exp
  | (:>) Exp Exp
  | (:/=) Exp Exp
  | (:>=) Exp Exp
  | (:<=) Exp Exp
  | (:&&) Exp Exp
  | (:||) Exp Exp
  | -- Keep Recurrence last for ordering in Ord; we depend
    -- on this for Rule matching.
    Recurrence -- self-reference y[i-1]
  deriving (Show, Eq, Ord)

data Domain = Iota Exp      -- [0, ..., n-1]
            | Range Exp Exp -- [from, ..., to]
            | Union         -- Union_{k=1}^{m-1} [b_{k-1}, ..., b_k)
                VName       -- k
                Exp         -- m
                Domain      -- D
  deriving (Show, Eq, Ord)

data Iterator = Forall VName Domain
              | Empty    -- Promoted variable.
  deriving (Show, Ord)

iteratorName :: Iterator -> Maybe VName
iteratorName (Forall vn _) = Just vn
iteratorName _ = Nothing

instance Eq Iterator where
  (Forall _ dom_i) == (Forall _ dom_j) = dom_i == dom_j
  _ == _ = False -- TODO

newtype Cases a = Cases (NE.NonEmpty (a, a))
  deriving (Show, Eq)

-- TODO add "bottom" for failure?
data View = View
  { iterator :: Iterator,
    value :: Cases Exp
  }
  deriving (Show, Eq)

type Views = M.Map VName View

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Exp E.Exp,
    views :: Views
  }

-- The View monad keeps a source of fresh names and writes views.
newtype ViewM a = ViewM (RWS () () VEnv a)
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
instance Nameable Exp where
  mkName (VNameSource i) = (Var $ VName "x" i, VNameSource $ i + 1)

instance MonadSoP Exp E.Exp ViewM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

execViewM :: ViewM a -> VNameSource -> Views
execViewM (ViewM m) vns = views . fst $ execRWS m () s
  where
    s = VEnv vns mempty mempty

insertView :: VName -> View -> ViewM ()
insertView x v =
  modify $ \env -> env {views = M.insert x v $ views env}

data ASTMapper m = ASTMapper
  { mapOnExp :: Exp -> m Exp,
    mapOnVName :: VName -> m VName
  }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

-- Mapping over AST for substitutions.
instance ASTMappable View where
  astMap m (View (Forall i dom) e) = View (Forall i dom) <$> astMap m e
  astMap m (View Empty e) = View Empty <$> astMap m e

instance ASTMappable (Cases Exp) where
  astMap m (Cases cases) = Cases <$> traverse (astMap m) cases

instance ASTMappable (Exp, Exp) where
  astMap m (p, e) = (,) <$> mapOnExp m p <*> mapOnExp m e

-- TODO test and think about this...
instance ASTMappable (SoP Exp) where
  astMap m sop = do
    foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    where
      g (ts, n) = do
        ts' <- traverse (mapOnExp m) ts
        pure $ foldl (SoP..*.) (SoP.int2SoP 1) (SoP.int2SoP n : map expToSoP ts')

instance ASTMappable Exp where
  astMap _ Recurrence = pure Recurrence
  -- astMap m (Var x) = Var <$> mapOnVName m x
  astMap m (Var x) = do
    vn <- mapOnVName m x
    mapOnExp m $ Var vn
  -- astMap m (SumSlice vn lb ub) =
  --   SumSlice <$> mapOnVName m vn <*> astMap m lb <*> astMap m ub
  astMap m (Sum i lb ub e) =
    Sum <$> mapOnVName m i <*> astMap m lb <*> astMap m ub <*> mapOnExp m e
  astMap m (Idx xs i) = Idx <$> mapOnExp m xs <*> astMap m i
  astMap m (SoP2 sop) = do
    sop' <- foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    case SoP.justSym sop' of
      Just x -> pure x
      Nothing -> pure $ SoP2 sop'
    where
      g (ts, n) = do
        ts' <- traverse (mapOnExp m) ts
        pure $ foldl (SoP..*.) (SoP.int2SoP 1) (SoP.int2SoP n : map expToSoP ts')
  astMap m (Indicator p) = Indicator <$> mapOnExp m p
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = Not <$> mapOnExp m x
  astMap m (x :== y) = (:==) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :< y) = (:<) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :> y) = (:>) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :/= y) = (:/=) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :>= y) = (:>=) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :<= y) = (:<=) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :&& y) = (:&&) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :|| y) = (:||) <$> mapOnExp m x <*> mapOnExp m y

idMap :: (ASTMappable a) => ASTMapper Identity -> a -> a
idMap m = runIdentity . astMap m

flatten :: (ASTMappable a) => a -> a
flatten = idMap m
  where
    m =
      ASTMapper
        { mapOnExp =
            \e ->
              case e of
                Var x -> pure $ Var x
                _ -> astMap m e,
          mapOnVName = pure
        }

instance ToSoP Exp E.Exp where
  toSoPNum e = do
    x <- lookupUntransPE e
    pure (1, SoP.sym2SoP x)

expToSoP :: Exp -> SoP Exp
expToSoP e =
  case flatten e of
    SoP2 sop -> sop
    e' -> SoP.sym2SoP e'

(~-~) :: Exp -> Exp -> Exp
x ~-~ y = flatten $ SoP2 $ expToSoP x SoP..-. expToSoP y

(~+~) :: Exp -> Exp -> Exp
x ~+~ y = flatten $ SoP2 $ expToSoP x SoP..+. expToSoP y

(~*~) :: Exp -> Exp -> Exp
x ~*~ y = flatten $ SoP2 $ expToSoP x SoP..*. expToSoP y

-- (~<~) :: Exp -> Exp -> Exp
-- x ~<~ y = flatten $ SoP (expToSoP x) :< SoP (expToSoP y)

-- (~>~) :: Exp -> Exp -> Exp
-- x ~>~ y = flatten $ SoP (expToSoP x) :> SoP (expToSoP y)

-- (~==~) :: Exp -> Exp -> Exp
-- x ~==~ y = flatten $ SoP (expToSoP x) :== SoP (expToSoP y)

prettyName :: VName -> Doc a
prettyName (VName vn i) = pretty vn <> pretty (mapMaybe subscript (show i))
  where
    subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

instance Pretty Exp where
  pretty Recurrence = "%₍₋₁₎"
  pretty (Var x) = prettyName x
  pretty (Idx arr i) = parens (pretty arr) <> brackets (pretty i)
  -- pretty (SumSlice vn lb ub) =
  --   "Σ"
  --     <> parens (prettyName vn)
  --     <> brackets (pretty lb <+> ":" <+> pretty ub)
  pretty (Sum i lb ub e) =
    "Σ"
      <> prettyName i
      <> "∈"
      <> brackets (commasep [pretty lb, "...", pretty ub])
      <+> parens (pretty e)
  pretty (SoP2 sop) = pretty sop
  pretty (Indicator p) = iversonbrackets (pretty p)
    where
      iversonbrackets = enclose "⟦" "⟧"
  pretty (Bool x) = pretty x
  pretty (Not x) = "¬" <> parens (pretty x)
  pretty (x :== y) = pretty x <+> "==" <+> pretty y
  pretty (x :< y) = pretty x <+> "<" <+> pretty y
  pretty (x :> y) = pretty x <+> ">" <+> pretty y
  pretty (x :/= y) = pretty x <+> "/=" <+> pretty y
  pretty (x :>= y) = pretty x <+> ">=" <+> pretty y
  pretty (x :<= y) = pretty x <+> "<=" <+> pretty y
  pretty (x :&& y) = pretty x <+> "&&" <+> pretty y
  pretty (x :|| y) = pretty x <+> "||" <+> pretty y

instance Pretty a => Pretty (Cases a) where
  pretty (Cases cases) = -- stack (map prettyCase (NE.toList cases))
    line <> indent 4 (stack (map prettyCase (NE.toList cases)))
    where
      prettyCase (p, e) = "|" <+> pretty p <+> "=>" <+> pretty e

instance Pretty Domain where
  pretty (Iota e) = "iota" <+> pretty e
  pretty (Range start end) =
      brackets (commasep [pretty start, "...", pretty end])
  pretty (Union k e dom) =
    "⋃"
      <> pretty k
      <> "="
      <> commasep ["1", "...", pretty e]
      <+> parens (pretty dom)

instance Pretty View where
  pretty (View (Forall i dom) e) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom <+> "." <+> pretty e
  pretty (View Empty e) = "." <+> pretty e

instance Pretty Views where
  pretty env =
    stack $ map (\(a, b) -> pretty a <+> "=" <+> pretty b) $ M.toList env

substituteNames :: ASTMappable x => M.Map VName Exp -> x -> x
substituteNames substitutions x = do
  runIdentity $ astMap (substituter substitutions) x
  where
    substituter subst =
      -- ASTMapper
      --   { mapOnExp = onExp subst }
      ASTMapper
        { mapOnExp = onExp subst,
          mapOnVName = pure
        }
    onExp subst e@(Var x') =
      case M.lookup x' subst of
        Just x'' -> pure x''
        Nothing -> pure e
    onExp subst e = astMap (substituter subst) e

substituteName :: ASTMappable x => VName -> Exp -> x -> x
substituteName vn x = substituteNames (M.singleton vn x)

-- Convert expression to Negation Normal Form.
toNNF :: Exp -> Exp
toNNF (Not (Not x)) = x
toNNF (Not (Bool True)) = Bool False
toNNF (Not (Bool False)) = Bool True
toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
toNNF x = x

-- A kind of map that only admits type-preserving functions.
cmap :: ((a, a) -> (a, a)) -> Cases a -> Cases a
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (a -> a) -> Cases a -> Cases a
cmapValues f = cmap (second f)

getSoP :: SoP.SoP Exp -> [([Exp], Integer)]
getSoP = SoP.sopToLists . SoP.normalize

debugM :: Applicative f => String -> f ()
debugM x = traceM $ "🪲 " <> x

debug :: String -> a -> a
debug msg = trace ("🪲 " <> msg)

toCases :: Exp -> Cases Exp
toCases e = Cases (NE.singleton (Bool True, e))
