-- XXX use case predicates for simplifying ranges in case values
-- e.g. n = 0 => sum (0, n-1) can be simplified
-- XXX add negation of disjunction of previous cases
-- XXX Next: Make it robust: play with part2indices; rewrite it in other ways such as
--       [x] fflgs using negation on conds
--       [x] introduce let bindings inside maps
--       [ ] Make substition of view into conditions of other view
--           work. See tests/refinement/nikolaj/view_cond.fut and hoistCases'.
--       [ ] use exclusive scan and dont subtract one from indices later
--           (also changes definition of lst)
--       [ ] use parts from partition2L as inspiration fro changes, like:
--           let begs   = scan (+) 0 shp --- prepend 0 here; prepend 0 to shp
--       - etc
-- Next: mkFlagArray
-- Next: partition2L
--       - empty segments may be problematic in lstL;
--         maybe add outer condition to index funciton representation?
-- Next: prove that partition2indices result is actually a permutation of 0...n-1
--
-- XXX use case perdicates for simplifying ranges in case values
--     TODO change Iota to SoP.Range? Then Empty just becomes mempty.
-- XXX make tests/refinement/iota0 reduce to just 0.
-- XXX Also recurse into conditions in `hoistIf` (see TODOs).
-- XXX Rebase this on top of master (don't need Refinement type machinery rn).
-- XXX Check that iterators/domains are compatible.
-- XXX Make iterators over same domains unify.
-- XXX handle let bindings inside expressions
-- [1, let x = 2 in x]
-- XXX handle subexpressions that ought to be views
-- map id [[1, 2], map (+1) [1,2]]
-- A simple first implementation could simply create a name
-- for the map subexp, create a view for iti and then reference
-- that here. Later on it would get substituted.

module Futhark.Analysis.View.Representation where

import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
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

data Exp =
    Var VName
  | Array [Exp]
  | Sum
      Exp         -- index
      Exp         -- lower bound
      Exp         -- upper bound
      Exp         -- indexed expression
  | Idx
      Exp         -- array
      Exp         -- index
  | SoP (SoP Exp)
  | Cases (NE.NonEmpty (Exp, Exp))
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

-- Match (ExpBase f vn) (NE.NonEmpty (CaseBase f vn)) SrcLoc
-- | A case in a match expression.
-- data CaseBase f vn = CasePat (PatBase f vn StructType) (ExpBase f vn) SrcLoc

newtype Domain = Iota Exp -- [0, ..., n-1]
            -- | Union ...
  deriving (Show, Eq, Ord)

data Iterator = Forall VName Domain
              | Empty    -- Promoted variable. -- XXX Is this nicer than Iota 0?
              -- | Union VName Iterator
  deriving (Show, Ord)

instance Eq Iterator where
  _i == _j = True --- TODO

-- Case statement evaluated from left to right to ensure no two
-- cases overlap. Must additionally partition the domain, so the
-- last element should be a tautology. The plan is to enforce this
-- by construction. Alternatively, we could ensure this by defining:
--
--   data Case p e = Case
--                     [(p, e)] -- [predicate => value]
--                     e        -- otherwise
--     deriving (Show, Eq, Ord)
--
-- but this makes it more convoluted to merge the "otherwise" cases
-- when substituting views into views.
-- newtype Cases a = Cases (NE.NonEmpty (a, a)) -- [predicate => value]
--   deriving (Show, Eq, Ord)

-- TODO add "bottom" for failure?
data View = View
  { iterator :: Iterator,
    value :: Exp
    -- shape :: Maybe Shape -- Might make sense to use this.
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

-- Copy code from Futhark.Traversals
-- (Copying Robert's code)
newtype ASTMapper m = ASTMapper
  { mapOnExp :: Exp -> m Exp }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

-- Mapping over AST for substitutions.
instance ASTMappable View where
  astMap m (View (Forall i dom) e) = View (Forall i dom) <$> astMap m e
  astMap m (View Empty e) = View Empty <$> astMap m e

-- instance ASTMappable (Cases Exp) where
--   astMap m (Cases cases) = Cases <$> traverse (astMap m) cases

-- instance ASTMappable [Exp] where
--   astMap m = map (mapOnExp m)

instance ASTMappable (Exp, Exp) where
  astMap m (p, e) = (,) <$> mapOnExp m p <*> mapOnExp m e

-- TODO shouldn't many of these astMaps should be mapOnExp? if I want
-- to be able to target them using mapOnExp (usually define
-- mapOnExp catch all to be `astMap m`)
instance ASTMappable Exp where
  astMap _ Recurrence = pure Recurrence
  astMap m (Var x) = mapOnExp m $ Var x
  astMap m (Array ts) = Array <$> traverse (mapOnExp m) ts
  -- astMap m (If c t f) = If <$> mapOnExp m c <*> mapOnExp m t <*> mapOnExp m f
  astMap m (Cases cases) = Cases <$> traverse (astMap m) cases
  astMap m (Sum i lb ub e) = Sum <$> mapOnExp m i <*> mapOnExp m lb <*> mapOnExp m ub <*> mapOnExp m e
  astMap m (Idx xs i) = Idx <$> mapOnExp m xs <*> mapOnExp m i
  astMap m (SoP sop) = do
    sop' <- foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    case SoP.justSym sop' of
      Just x -> pure x
      Nothing -> pure $ SoP sop'
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
                _ -> astMap m e
        }

instance ToSoP Exp E.Exp where
  toSoPNum e = do
    x <- lookupUntransPE e
    pure (1, SoP.sym2SoP x)

expToSoP :: Exp -> SoP Exp
expToSoP e =
  case flatten e of
    SoP sop -> sop
    e' -> SoP.sym2SoP e'

(~-~) :: Exp -> Exp -> Exp
x ~-~ y = flatten $ SoP $ expToSoP x SoP..-. expToSoP y

(~+~) :: Exp -> Exp -> Exp
x ~+~ y = flatten $ SoP $ expToSoP x SoP..+. expToSoP y

(~*~) :: Exp -> Exp -> Exp
x ~*~ y = flatten $ SoP $ expToSoP x SoP..*. expToSoP y

-- (~<~) :: Exp -> Exp -> Exp
-- x ~<~ y = flatten $ SoP (expToSoP x) :< SoP (expToSoP y)

-- (~>~) :: Exp -> Exp -> Exp
-- x ~>~ y = flatten $ SoP (expToSoP x) :> SoP (expToSoP y)

-- (~==~) :: Exp -> Exp -> Exp
-- x ~==~ y = flatten $ SoP (expToSoP x) :== SoP (expToSoP y)

-- SoP foreshadowing:

-- instance (ASTMappable a) => Substitute VName Exp a where
--   substitute subst = idMap m
--     where
--       m =
--         ASTMapper
--           { mapOnExp =
--               \e ->
--                 case e of
--                   (Var x)
--                     | Just x' <- M.lookup x subst -> pure x'
--                     | otherwise -> pure $ Var x
--                   _ -> astMap m e
--           }

prettyName :: VName -> Doc a
prettyName (VName vn i) = pretty vn <> pretty (mapMaybe subscript (show i))
  where
    subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

instance Pretty Exp where
  pretty Recurrence = "%₍₋₁₎"
  pretty (Var x) = prettyName x
  pretty (Array ts) = pretty ts
  pretty (Idx arr i) = parens (pretty arr) <> brackets (pretty i)
  pretty (Sum i lb ub e) =
    "Σ"
      <> pretty i
      <> "∈"
      <> brackets (commasep [pretty lb, "...", pretty ub])
      <+> parens (pretty e)
  -- pretty (If c t f) =
  --   "If"
  --     <+> parens (pretty c)
  --     <+> "then"
  --     <+> parens (pretty t)
  --     <+> "else"
  --     <+> parens (pretty f)
  pretty (SoP sop) = pretty sop
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
-- instance Pretty a => Pretty (Cases a) where
  pretty (Cases cases) =
    line <> indent 4 (stack (map prettyCase (NE.toList cases)))
    where
      prettyCase (p, e) = "|" <+> pretty p <+> "=>" <+> pretty e

instance Pretty Domain where
  pretty (Iota e) = "iota" <+> pretty e

-- instance Pretty a => Pretty (Cases a) where
--   pretty (Cases cases) = -- stack (map prettyCase (NE.toList cases))
--     line <> indent 4 (stack (map prettyCase (NE.toList cases)))
--     where
--       prettyCase (p, e) = "|" <+> pretty p <+> "=>" <+> pretty e

instance Pretty View where
  pretty (View (Forall i dom) e) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom <+> "." <+> pretty e
  pretty (View Empty e) = "." <+> pretty e

instance Pretty Views where
  pretty env =
    stack $ map (\(a, b) -> pretty a <+> "=" <+> pretty b) $ M.toList env

substituteNames :: ASTMappable x => M.Map VName Exp -> x -> ViewM x
substituteNames substitutions x = do
  pure $ runIdentity $ astMap (substituter substitutions) x
  where
    substituter subst =
      ASTMapper
        { mapOnExp = onExp subst }
    onExp subst e@(Var x') =
      case M.lookup x' subst of
        -- Just x'' -> trace ("hihi substituting " <> prettyString x' <> " for " <> prettyString x'') $ pure x''
        Just x'' -> pure x''
        Nothing -> pure e
    onExp subst e = astMap (substituter subst) e

substituteName :: ASTMappable x => VName -> Exp -> x -> ViewM x
substituteName vn x = substituteNames (M.singleton vn x)

-- Convert expression to Negation Normal Form.
toNNF :: Exp -> Exp
toNNF (Not (Not x)) = x
toNNF (Not (Bool True)) = Bool False
toNNF (Not (Bool False)) = Bool True
toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
toNNF x = x
