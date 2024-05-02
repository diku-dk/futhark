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
import Data.Text (unpack)

data Term =
    Var VName
  | SumSlice
      VName       -- array
      (SoP Term)   -- lower bound
      (SoP Term)   -- upper bound
  | SumSliceIndicator
      VName       -- array
      (SoP Term)   -- lower bound
      (SoP Term)   -- upper bound
  | Sum
      VName        -- index
      (SoP Term)   -- lower bound
      (SoP Term)   -- upper bound
      (SoP Term)   -- indexed expression
  | Idx
      Term         -- array
      (SoP Term)   -- index
  | SoP2 (SoP Term)
  | Indicator Term -- predicate (the corresponding value of 0 or 1 is implicit)
  | -- Predicate expressions follow here for simplicity.
    -- I'm assuming it's safe because the source program was typechecked.
    -- TODO CNF
    Bool Bool
    -- TODO change this to SoP.Rel?
  | Not Term
  | (:==) Term Term
  | (:<) Term Term
  | (:>) Term Term
  | (:/=) Term Term
  | (:>=) Term Term
  | (:<=) Term Term
  | (:&&) Term Term
  | (:||) Term Term
  | -- Keep Recurrence last for ordering in Ord; we depend
    -- on this for Rule matching.
    Recurrence -- self-reference y[i-1]
  deriving (Show, Eq, Ord)

infixr 3 :&&
infixr 2 :||

data Domain = Iota Term       -- [0, ..., n-1]
            | Cat             -- Catenate_{k=1}^{m-1} [b_{k-1}, ..., b_k)
                VName         -- k
                Term          -- m
                Term          -- b
  deriving (Show, Eq, Ord)

data Iterator = Forall VName Domain
              | Empty    -- Promoted variable.
  deriving (Show, Ord)

iteratorName :: Iterator -> Maybe VName
iteratorName (Forall vn _) = Just vn
iteratorName _ = Nothing

-- The final value that the iterator takes on.
-- (Given monotonicity in the Domain, this is also the maximum value.)
iteratorEnd :: Iterator -> Maybe Term
iteratorEnd (Forall _ dom) = Just (domainEnd dom)
iteratorEnd _ = Nothing

-- The final value in the domain (which is ordered).
domainEnd :: Domain -> Term
domainEnd (Iota n) = n
domainEnd (Cat _k _m _b) =
  undefined -- don't want to implement this before I can test it.
  -- domainEnd (substituteName k m dom) -- substitution should actually be m-1
-- instance ASTMappable Domain where
--   astMap m (Iota n) = Iota <$> astMap m n
--   astMap m (Union k n b) = Union <$> mapOnVName m k <*> astMap m n <*> astMap m b

domainStart :: Domain -> Term
domainStart (Iota _) = SoP2 (SoP.int2SoP 0)
domainStart (Cat _k _m _b) = undefined


instance Eq Iterator where
  (Forall _ dom_i) == (Forall _ dom_j) = dom_i == dom_j
  Empty == Empty = True
  _ == _ = False -- TODO

newtype Cases a = Cases (NE.NonEmpty (a, a))
  deriving (Show, Eq)

-- TODO add "bottom" for failure?
data IndexFn = IndexFn
  { iterator :: Iterator,
    value :: Cases Term
  }
  deriving (Show, Eq)

type IndexFns = M.Map VName IndexFn

data VEnv = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Term E.Exp,
    indexfns :: IndexFns
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
instance Nameable Term where
  mkName (VNameSource i) = (Var $ VName "x" i, VNameSource $ i + 1)

instance MonadSoP Term E.Exp IndexFnM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

execIndexFnM :: IndexFnM a -> VNameSource -> IndexFns
execIndexFnM (IndexFnM m) vns = indexfns . fst $ execRWS m () s
  where
    s = VEnv vns mempty mempty

insertIndexFn :: VName -> IndexFn -> IndexFnM ()
insertIndexFn x v =
  modify $ \env -> env {indexfns = M.insert x v $ indexfns env}

data ASTMapper m = ASTMapper
  { mapOnTerm :: Term -> m Term,
    mapOnVName :: VName -> m VName
  }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

-- Mapping over AST for substitutions.
instance ASTMappable IndexFn where
  astMap m (IndexFn (Forall i dom) e) = IndexFn (Forall i dom) <$> astMap m e
  astMap m (IndexFn Empty e) = IndexFn Empty <$> astMap m e

instance ASTMappable (Cases Term) where
  astMap m (Cases cases) = Cases <$> traverse (astMap m) cases

instance ASTMappable (Term, Term) where
  astMap m (p, e) = (,) <$> mapOnTerm m p <*> mapOnTerm m e

-- TODO test and think about this...
instance ASTMappable (SoP Term) where
  astMap m sop = do
    foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    where
      g (ts, n) = do
        ts' <- traverse (mapOnTerm m) ts
        pure $ foldl (SoP..*.) (SoP.int2SoP 1) (SoP.int2SoP n : map termToSoP ts')

instance ASTMappable Term where
  astMap _ Recurrence = pure Recurrence
  -- astMap m (Var x) = mapOnTerm m (Var x)
  astMap m (Var x) = do
    vn <- mapOnVName m x
    mapOnTerm m $ Var vn
  astMap m (SumSlice vn lb ub) =
    SumSlice <$> mapOnVName m vn <*> astMap m lb <*> astMap m ub
  astMap m (SumSliceIndicator vn lb ub) =
    SumSliceIndicator <$> mapOnVName m vn <*> astMap m lb <*> astMap m ub
  astMap m (Sum i lb ub e) =
    Sum <$> mapOnVName m i <*> astMap m lb <*> astMap m ub <*> astMap m e
  astMap m (Idx xs i) = Idx <$> mapOnTerm m xs <*> astMap m i
  astMap m (SoP2 sop) = do
    sop' <- foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    case SoP.justSym sop' of
      Just x -> pure x
      Nothing -> pure $ SoP2 sop'
    where
      g (ts, n) = do
        ts' <- traverse (mapOnTerm m) ts
        pure $ foldl (SoP..*.) (SoP.int2SoP 1) (SoP.int2SoP n : map termToSoP ts')
  astMap m (Indicator p) = Indicator <$> mapOnTerm m p
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = Not <$> mapOnTerm m x
  astMap m (x :== y) = (:==) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :< y) = (:<) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :> y) = (:>) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :/= y) = (:/=) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :>= y) = (:>=) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :<= y) = (:<=) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :&& y) = (:&&) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :|| y) = (:||) <$> mapOnTerm m x <*> mapOnTerm m y

idMap :: (ASTMappable a) => ASTMapper Identity -> a -> a
idMap m = runIdentity . astMap m

flatten :: (ASTMappable a) => a -> a
flatten = idMap m
  where
    m =
      ASTMapper
        { mapOnTerm =
            \e ->
              case e of
                Var x -> pure $ Var x
                _ -> astMap m e,
          mapOnVName = pure
        }

instance ToSoP Term E.Exp where
  toSoPNum e = do
    x <- lookupUntransPE e
    pure (1, SoP.sym2SoP x)

termToSoP :: Term -> SoP Term
termToSoP e =
  case flatten e of
    SoP2 sop -> sop
    e' -> SoP.sym2SoP e'

(~-~) :: Term -> Term -> Term
x ~-~ y = flatten $ SoP2 $ termToSoP x SoP..-. termToSoP y

(~+~) :: Term -> Term -> Term
x ~+~ y = flatten $ SoP2 $ termToSoP x SoP..+. termToSoP y

(~*~) :: Term -> Term -> Term
x ~*~ y = flatten $ SoP2 $ termToSoP x SoP..*. termToSoP y

-- (~<~) :: Term -> Term -> Term
-- x ~<~ y = flatten $ SoP (termToSoP x) :< SoP (termToSoP y)

-- (~>~) :: Term -> Term -> Term
-- x ~>~ y = flatten $ SoP (termToSoP x) :> SoP (termToSoP y)

-- (~==~) :: Term -> Term -> Term
-- x ~==~ y = flatten $ SoP (termToSoP x) :== SoP (termToSoP y)

prettyName :: VName -> Doc a
prettyName (VName vn i) = pretty vn <> pretty (mapMaybe subscript (show i))
  where
    subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

instance Pretty Term where
  pretty Recurrence = "%₍₋₁₎"
  pretty (Var x) = prettyName x
  pretty (Idx (Var x) i) = prettyName x <> brackets (pretty i)
  pretty (Idx arr i) = parens (pretty arr) <> brackets (pretty i)
  pretty (SumSlice vn lb ub) =
    "∑"
      <> prettyName vn
      <> brackets (pretty lb <+> ":" <+> pretty ub)
  pretty (SumSliceIndicator vn lb ub) =
    "∑"
      <> pretty (Indicator (Var vn))
      <> brackets (pretty lb <+> ":" <+> pretty ub)
  pretty (Sum i lb ub e) =
    "∑"
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
      prettyCase (p, e) = "|" <+> pretty p <+> "⇒ " <+> pretty e

instance Pretty Domain where
  pretty (Iota e) = "iota" <+> pretty e
  pretty (Cat k e dom) =
    "⊎"
      <> prettyName k
      <> "="
      <> commasep ["1", "...", pretty e]
      -- <> "iota" <+> pretty e
      <+> parens (commasep [
            pretty (SoP.padWithZero . termToSoP $ substituteName k (Var k ~-~ SoP2 (SoP.int2SoP 1)) dom),
            "...",
            pretty (SoP.padWithZero (termToSoP dom))
          ])

instance Pretty IndexFn where
  pretty (IndexFn (Forall i dom) e) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom <+> "." <+> pretty e
  pretty (IndexFn Empty e) = "." <+> pretty e

instance Pretty IndexFns where
  pretty env =
    stack $ map (\(a, b) -> pretty a <+> "=" <+> pretty b) $ M.toList env

substituteNames :: ASTMappable x => M.Map VName Term -> x -> x
substituteNames substitutions x = do
  runIdentity $ astMap (substituter substitutions) x
  where
    substituter subst =
      ASTMapper
        { mapOnTerm = onTerm subst,
          mapOnVName = pure
        }
    onTerm subst e@(Var x') =
      case M.lookup x' subst of
        Just x'' -> pure x''
        Nothing -> pure e
    onTerm subst e = astMap (substituter subst) e

substituteName :: ASTMappable x => VName -> Term -> x -> x
substituteName vn x = substituteNames (M.singleton vn x)

-- Convert expression to Negation Normal Form.
toNNF :: Term -> Term
toNNF (Not (Not x)) = x
toNNF (Not (Bool True)) = Bool False
toNNF (Not (Bool False)) = Bool True
toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
toNNF (Not (x :== y)) = x :/= y
toNNF (Not (x :< y)) = x :>= y
toNNF (Not (x :> y)) = x :<= y
toNNF (Not (x :/= y)) = x :== y
toNNF (Not (x :>= y)) = x :< y
toNNF (Not (x :<= y)) = x :> y
toNNF x = x

-- A kind of map that only admits type-preserving functions.
cmap :: ((a, a) -> (a, a)) -> Cases a -> Cases a
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (a -> a) -> Cases a -> Cases a
cmapValues f = cmap (second f)

getSoP :: SoP.SoP Term -> [([Term], Integer)]
getSoP = SoP.sopToLists . SoP.normalize

debugM :: Applicative f => String -> f ()
debugM x = traceM $ "🪲 " <> x

debug :: String -> a -> a
debug msg = trace ("🪲 " <> msg)

toCases :: Term -> Cases Term
toCases e = Cases (NE.singleton (Bool True, e))

casesToList :: Cases a -> [(a, a)]
casesToList (Cases xs) = NE.toList xs

toScalarIndexFn :: Term -> IndexFn
toScalarIndexFn e = IndexFn Empty (toCases e)

prettyRanges :: Pretty u => M.Map u (SoP.Range u) -> String
prettyRanges = toString . indent 4 . stack . map pretty . M.toList
  where
    toString = unpack . docText
