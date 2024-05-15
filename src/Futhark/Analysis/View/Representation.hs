-- Task: mkFlagArray
--   [x] Extend representation to allow Union iterator
--   [x] Support necessary scatter rule
-- Task: Make Fourier-Motzkin elimination return Nothing when undecidable?
--   This would be useful in refineIndexFn (see TODO there).
-- Task: partition2L
--   Empty segments may be problematic in lstL; maybe
--   add outer condition to index funciton representation?
-- Task: prove that partition2indices index function
--   is a permutation of 0...n-1.
-- Task: Use SoP.Rel for relations?
--
-- Task: TODO change Iota to SoP.Range? Then Empty just becomes mempty.
-- Task: Rebase this on top of master (don't need Refinement type machinery rn).

module Futhark.Analysis.View.Representation where

import Data.List qualified as L
import Futhark.MonadFreshNames
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Futhark.SoP.SoP (SoP)
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark (VName (VName))
import Language.Futhark qualified as E
import Futhark.Util.Pretty
import Data.Functor.Identity
import Data.List.NonEmpty qualified as NE
import Futhark.SoP.Monad
import Futhark.SoP.Convert (ToSoP (toSoPNum))
import Debug.Trace (traceM, trace)
import Data.Text (unpack)
import qualified Data.Set as S

data Term =
    Var VName
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
  | Tuple [Term]
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

-- This is required by MonadSoP.
instance Nameable Term where
  mkName (VNameSource i) = (Var $ VName "x" i, VNameSource $ i + 1)

data Domain = Iota Term       -- [0, ..., n-1]
            | Cat             -- Catenate_{k=1}^{m-1} [b_{k-1}, ..., b_k)
                VName         -- k
                Term          -- m
                Term          -- b
  deriving (Show, Ord)

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
domainEnd (Cat k m b) = substituteName k m b

domainStart :: Domain -> Term
domainStart (Iota _) = SoP2 (SoP.int2SoP 0)
domainStart (Cat k _ b) = substituteName k (SoP2 $ SoP.int2SoP 0) b


instance Eq Domain where
  u == v | trace ("Domain equality check:\n  " <> prettyString u <> " == " <> prettyString v) False = undefined
  u == v =
    trace ("Domain start u: " <> prettyString (domainStart u)) $
    trace ("Domain start v: " <> prettyString (domainStart v)) $
    trace ("Domain end u: " <> prettyString (domainEnd u)) $
    trace ("Domain end v: " <> prettyString (domainEnd v)) $
    -- Since the whole domain must be covered by an index function,
    -- it is sufficient to check that starts and ends are equal.
    domainStart u == domainStart v && domainEnd u == domainEnd v

instance Eq Iterator where
  (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
  (Forall _ u) == (Forall _ v) = u == v
  Empty == Empty = True
  _ == _ = False

newtype Cases a = Cases (NE.NonEmpty (a, a))
  deriving (Show, Eq, Ord)

-- TODO add "bottom" for failure?
data IndexFn = IndexFn
  { iterator :: Iterator,
    value :: Cases Term
  }
  deriving (Show, Eq)

type IndexFns = M.Map VName IndexFn

data ASTMapper m = ASTMapper
  { mapOnTerm :: Term -> m Term,
    mapOnVName :: VName -> m VName
  }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

-- Mapping over AST for substitutions.
instance ASTMappable IndexFn where
  astMap m (IndexFn (Forall i dom) e) = do
    dom' <- astMap m dom
    e' <- astMap m e
    pure $ IndexFn (Forall i dom') e'
  astMap m (IndexFn Empty e) = IndexFn Empty <$> astMap m e

instance ASTMappable Domain where
  astMap m (Iota n) = Iota <$> mapOnTerm m n
  astMap m (Cat k n b) = Cat k <$> mapOnTerm m n <*> mapOnTerm m b

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
  astMap m (Tuple ts) = Tuple <$> mapM (mapOnTerm m) ts
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
prettyName (VName vn i) = pretty vn <> pretty (map (fromJust . subscript) (show i))
  where
    subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

instance Pretty Term where
  pretty Recurrence = "↺ "
  pretty (Var x) = prettyName x
  pretty (Idx (Var x) i) = prettyName x <> brackets (pretty i)
  pretty (Idx arr i) = parens (pretty arr) <> brackets (pretty i)
  pretty (Sum i lb ub e)
    | [([Idx (Var vn) idx], 1)] <- getSoP e =
    "∑"
      <> prettyName vn
      <> brackets (pretty (substituteName i (SoP2 lb) idx) <+> ":" <+> pretty (substituteName i (SoP2 ub) idx))
  pretty (Sum i lb ub e)
    | [([Indicator (Idx (Var vn) idx)], 1)] <- getSoP e =
    "∑"
      <> pretty (Indicator (Var vn))
      <> brackets (pretty (substituteName i (SoP2 lb) idx) <+> ":" <+> pretty (substituteName i (SoP2 ub) idx))
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
  pretty (Tuple ts) = parens . commasep . map pretty $ ts
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
  pretty (Iota (Var vn)) = "iota" <+> pretty (Var vn)
  pretty (Iota e) = "iota" <+> parens (pretty e)
  pretty (Cat k m b) =
    "⊎"
      <> prettyName k
      <> "="
      -- <> commasep ["0", "...", pretty (termToSoP $ e ~-~ SoP2 (SoP.int2SoP 1))]
      <> "iota" <+> pretty m
      <+> "[" <> commasep [
            pretty (termToSoP b),
            "...",
            pretty (termToSoP $ substituteName k (Var k ~+~ SoP2 (SoP.int2SoP 1)) b)
          ] <> ")"

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

unzipT :: IndexFn -> [IndexFn]
unzipT (IndexFn iter (Cases cases))
  | Just vss <- mapM (getTuple . snd) (NE.toList cases),
    n <- length (head vss),
    all ((==) n . length) vss = -- All branches are n-tuples.
    let cs = map fst (NE.toList cases)
    in map (\vs ->
              IndexFn iter (Cases . NE.fromList . dedup $ zip cs vs))
           (L.transpose vss)
    where
      getTuple (Tuple vs) = Just vs
      getTuple _ = Nothing
      -- Unzipping may duplicate values, e.g., | b => (x, x) | b => (x, y)
      -- yields | b => x | not b => x for the first projection.
      dedup ((c,v):cs) | all ((==) v . snd) cs =
          [(foldl (:||) c (map fst cs), v)]
      dedup cs = cs
unzipT index_fn = [index_fn]
-- unzipT index_fn = error $ "unzipT not implemented for " <> prettyString index_fn

getIterator :: IndexFn -> Iterator
getIterator (IndexFn iter _) = iter

class Ord a => FreeIn a where
  freeIn :: a -> S.Set VName

instance FreeIn (SoP Term) where
  freeIn sop =
    let frees :: S.Set Term = SoP.free sop
    in  foldMap freeIn frees

instance FreeIn Term where
  freeIn (Var vn) = S.singleton vn
  freeIn (Sum _ lb ub e) = freeIn lb <> freeIn ub <> freeIn e
  freeIn (Idx xs i) = freeIn xs <> freeIn i
  freeIn (SoP2 sop) = freeIn sop
  freeIn (Indicator t) = freeIn t
  freeIn (Tuple ts) = foldMap freeIn ts
  freeIn (Bool _) = mempty
  freeIn (Not t) = freeIn t
  freeIn (a :== b) = freeIn a <> freeIn b
  freeIn (a :< b) = freeIn a <> freeIn b
  freeIn (a :> b) = freeIn a <> freeIn b
  freeIn (a :/= b) = freeIn a <> freeIn b
  freeIn (a :>= b) = freeIn a <> freeIn b
  freeIn (a :<= b) = freeIn a <> freeIn b
  freeIn (a :&& b) = freeIn a <> freeIn b
  freeIn (a :|| b) = freeIn a <> freeIn b
  freeIn Recurrence = mempty
-- instance FreeIn (Cases Term) where
--   freeIn (Cases cases) = foldMap (\(c,v) -> freeIn c <> freeIn v) cases
