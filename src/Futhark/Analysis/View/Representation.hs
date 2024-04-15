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
import Futhark.SoP.SoP (SoP, Rel (..))
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
import Debug.Trace (traceM)

data Term =
    Var VName
  | Idx VName (SoP Term)  -- array index
  | Sum
      VName       -- sum index
      (SoP Term)  -- lower bound
      (SoP Term)  -- upper bound
      (SoP Term)  -- indexed expression
  | Indicator BoolExp  -- Encode truth as 1 and falsehood as 0.
  | -- NOTE Keep Recurrence last for ordering in Ord; we depend
    -- on this for Rule matching.
    Recurrence -- self-reference y[i-1]
  deriving (Show, Eq, Ord)

data BoolExp =
    Bool Bool
  | BoolVar VName
  | Not BoolExp
  | (:&&) BoolExp BoolExp
  | (:||) BoolExp BoolExp
  | (:>) (SoP Term) (SoP Term)
  | (:>=) (SoP Term) (SoP Term)
  | (:<) (SoP Term) (SoP Term)
  | (:<=) (SoP Term) (SoP Term)
  | (:==) (SoP Term) (SoP Term)
  | (:/=) (SoP Term) (SoP Term)
  -- TODO Use SoP.Rel directly?
  -- | Rel (SoP.Rel Term)
    -- TODO There's also (&&) and (||) in SoP.Rel, but no Bool or Not.
    -- Bool could be encoded as a relation, though.
  deriving (Show, Eq, Ord)

type Exp = Either (SoP Term) BoolExp

data Domain = Iota (SoP Term) -- [0, ..., n-1]
            | Range (SoP Term) (SoP Term) -- [from, ..., to]
            | Union        -- Union_{k=1}^{m-1} [b_{k-1}, ..., b_k)
                VName      -- k
                (SoP Term) -- m
                Domain     -- D
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

newtype Cases = Cases (NE.NonEmpty (BoolExp, Exp))
  deriving (Show, Eq)

-- TODO add "bottom" for failure?
data View = View
  { iterator :: Iterator,
    value :: Cases
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
-- instance Nameable Exp where
--   mkName (VNameSource i) = (Var $ VName "x" i, VNameSource $ i + 1)

-- instance MonadSoP Exp E.Exp ViewM where
--   getUntrans = gets (untrans . algenv)
--   getRanges = gets (ranges . algenv)
--   getEquivs = gets (equivs . algenv)
--   modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

execViewM :: ViewM a -> VNameSource -> Views
execViewM (ViewM m) vns = views . fst $ execRWS m () s
  where
    s = VEnv vns mempty mempty

insertView :: VName -> View -> ViewM ()
insertView x v =
  modify $ \env -> env {views = M.insert x v $ views env}

data ASTMapper m = ASTMapper
  { mapOnExp :: Exp -> m Exp,
    mapOnBoolExp :: BoolExp -> m BoolExp,
    mapOnTerm :: Term -> m Term,
    mapOnVName :: VName -> m VName
  }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

-- Mapping over AST for substitutions.
instance ASTMappable View where
  astMap m (View (Forall i dom) e) = View (Forall i dom) <$> astMap m e
  astMap m (View Empty e) = View Empty <$> astMap m e

instance ASTMappable Cases where
  astMap m (Cases cases) = Cases <$> traverse (astMap m) cases

instance ASTMappable (BoolExp, Exp) where
  astMap m (p, e) = (,) <$> mapOnBoolExp m p <*> mapOnExp m e

-- TODO test and think about this...
instance ASTMappable (SoP Term) where
  astMap m sop = do
    foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    where
      g (ts, n) = do
        ts' <- traverse (mapOnTerm m) ts
        pure $ foldl (SoP..*.) (SoP.int2SoP 1) (SoP.int2SoP n : map SoP.sym2SoP ts')
        -- XXX restricted to sym2SoP here now; ie cant change
        -- 1+[[Not c]] to 1+1-[[c]] because that would require
        -- mapping a Term to a SoP Term.

instance ASTMappable Term where
  astMap m (Var vn) = do
    vn' <- mapOnVName m vn
    mapOnTerm m $ Var vn'
  astMap m (Idx xs i) = Idx <$> mapOnVName m xs <*> astMap m i
  -- astMap m (SumSlice vn lb ub) =
  --   SumSlice <$> mapOnVName m vn <*> astMap m lb <*> astMap m ub
  astMap m (Sum i lb ub e) =
    Sum <$> mapOnVName m i <*> astMap m lb <*> astMap m ub <*> astMap m e
  astMap m (Indicator p) = Indicator <$> astMap m p
  astMap _ Recurrence = pure Recurrence

instance ASTMappable BoolExp where
  astMap _ x@(Bool {}) = pure x
  astMap m (BoolVar vn) = do
    vn' <- mapOnVName m vn
    mapOnBoolExp m $ BoolVar vn'
  astMap m (Not x) = Not <$> mapOnBoolExp m x
  astMap m (x :&& y) = (:&&) <$> mapOnBoolExp m x <*> mapOnBoolExp m y
  astMap m (x :|| y) = (:||) <$> mapOnBoolExp m x <*> mapOnBoolExp m y
  astMap m (x :> y) = (:>) <$> astMap m x <*> astMap m y
  astMap m (x :>= y) = (:>=) <$> astMap m x <*> astMap m y
  astMap m (x :< y) = (:<) <$> astMap m x <*> astMap m y
  astMap m (x :<= y) = (:<=) <$> astMap m x <*> astMap m y
  astMap m (x :== y) = (:==) <$> astMap m x <*> astMap m y
  astMap m (x :/= y) = (:/=) <$> astMap m x <*> astMap m y
  -- astMap m (Rel (x :<: y)) = let lol = (:<:) <$> astMap m x <*> astMap m y
  --                            in  Rel <$> lol

instance ASTMappable Exp where
  astMap m (Left sop) = Left <$> astMap m sop
  astMap m (Right bool) = Right <$> astMap m bool

-- idMap :: (ASTMappable a) => ASTMapper Identity -> a -> a
-- idMap m = runIdentity . astMap m

-- flatten :: (ASTMappable a) => a -> a
-- flatten = idMap m
--   where
--     m =
--       ASTMapper
--         { mapOnExp =
--             \e ->
--               case e of
--                 Var x -> pure $ Var x
--                 _ -> astMap m e,
--           mapOnVName = pure
--         }

-- instance ToSoP Exp E.Exp where
--   toSoPNum e = do
--     x <- lookupUntransPE e
--     pure (1, SoP.sym2SoP x)

-- expToSoP :: Exp -> SoP Exp
-- expToSoP e =
--   case flatten e of
--     SoP sop -> sop
--     e' -> SoP.sym2SoP e'

-- (~-~) :: Exp -> Exp -> Exp
-- x ~-~ y = flatten $ SoP $ expToSoP x SoP..-. expToSoP y

-- (~+~) :: Exp -> Exp -> Exp
-- x ~+~ y = flatten $ SoP $ expToSoP x SoP..+. expToSoP y

-- (~*~) :: Exp -> Exp -> Exp
-- x ~*~ y = flatten $ SoP $ expToSoP x SoP..*. expToSoP y

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

instance Pretty Term where
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
  pretty (Indicator p) = iversonbrackets (pretty p)
    where
      iversonbrackets = enclose "⟦" "⟧"
  pretty Recurrence = "%₍₋₁₎"

instance Pretty BoolExp where
  pretty (Bool x) = pretty x
  pretty (BoolVar x) = pretty x
  pretty (Not x) = "¬" <> parens (pretty x)
  pretty (x :&& y) = pretty x <+> "&&" <+> pretty y
  pretty (x :|| y) = pretty x <+> "||" <+> pretty y
  pretty (x :== y) = pretty x <+> "==" <+> pretty y
  pretty (x :< y) = pretty x <+> "<" <+> pretty y
  pretty (x :> y) = pretty x <+> ">" <+> pretty y
  pretty (x :/= y) = pretty x <+> "/=" <+> pretty y
  pretty (x :>= y) = pretty x <+> ">=" <+> pretty y
  pretty (x :<= y) = pretty x <+> "<=" <+> pretty y

instance Pretty Exp where
  pretty (Left sop) = pretty sop
  pretty (Right bool) = pretty bool

instance Pretty Cases where
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

-- substituteNames :: ASTMappable x => M.Map VName Term -> x -> ViewM x
-- substituteNames substitutions x = do
--   pure $ runIdentity $ astMap (substituter substitutions) x
--   where
--     substituter subst =
--       ASTMapper
--         { mapOnExp = astMap (substituter subst),
--           mapOnTerm = onTerm subst,
--           mapOnBoolExp = pure,
--           mapOnVName = pure
--         }
--     onTerm subst e@(Var x') =
--       case M.lookup x' subst of
--         Just x'' -> pure x''
--         Nothing -> pure e
--     onTerm subst e = astMap (substituter subst) e
--     -- XXX what about BoolVar?

-- substituteName :: ASTMappable x => VName -> Term -> x -> ViewM x
-- substituteName vn x = substituteNames (M.singleton vn x)

-- TODO keep this or the monadic one...
substituteNames' :: ASTMappable x => M.Map VName Term -> x -> x
substituteNames' substitutions x = do
  runIdentity $ astMap (substituter substitutions) x
  where
    substituter subst =
      ASTMapper
        { mapOnExp = astMap (substituter subst),
          mapOnTerm = onTerm subst,
          mapOnBoolExp = pure,
          mapOnVName = pure
        }
    onTerm subst e@(Var x') =
      case M.lookup x' subst of
        Just x'' -> pure x''
        Nothing -> pure e
    onTerm subst e = astMap (substituter subst) e
    -- XXX what about BoolVar?

substituteName' :: ASTMappable x => VName -> Term -> x -> x
substituteName' vn x = substituteNames' (M.singleton vn x)

-- Convert expression to Negation Normal Form.
toNNF :: BoolExp -> BoolExp
toNNF (Not (Not x)) = x
toNNF (Not (Bool True)) = Bool False
toNNF (Not (Bool False)) = Bool True
toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
toNNF x = x

-- A kind of map that only admits type-preserving functions.
cmap :: ((BoolExp, Exp) -> (BoolExp, Exp)) -> Cases -> Cases
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (Exp -> Exp) -> Cases -> Cases
cmapValues f = cmap (second f)

getSoP :: SoP.SoP Exp -> [([Exp], Integer)]
getSoP = SoP.sopToLists . SoP.normalize

debugM :: Applicative f => String -> f ()
debugM x = traceM $ "🪲 " <> x

toCases :: Exp -> Cases
toCases e = Cases (NE.singleton (Bool True, e))
