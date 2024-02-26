-- XXX Create domains from sizes.
-- Make iterators over same domains unify.
--
-- XXX Finish substituteViews (see Rules).

module Futhark.Analysis.View.Representation where

import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Futhark.SoP.SoP (SoP)
import Futhark.SoP.SoP qualified as SoP
import Futhark.MonadFreshNames
import Language.Futhark (VName (VName))
import Futhark.Util.Pretty
import Data.Functor.Identity
import Control.Monad.RWS.Strict hiding (Sum)
import Data.List.NonEmpty qualified as NE

-- data PredExp =
--   Bool Bool
--   | Not PredExp
--   | (:==) Exp Exp
--   | (:<) Exp Exp
--   | (:>) Exp Exp
--   deriving (Show, Eq, Ord)

data Exp =
    Var VName
  | Array [Exp]
  -- | If
  --     Exp    -- predicate
  --     Exp    -- true branch
  --     Exp    -- false branch
  | Sum
      Exp    -- index
      Exp    -- lower bound
      Exp    -- upper bound
      Exp    -- indexed expression
  | Idx
      Exp    -- array
      Exp    -- index
  | SoP (SoP Exp)
  | Recurrence -- self-reference y[i-1]
  | -- Have Predicate expressions here to for a simpler `toExp`.
    -- I'm assuming it's safe because the source program was typechecked.
    -- TODO CNF
    Bool Bool
  | Not Exp
  | (:==) Exp Exp
  | (:<) Exp Exp
  | (:>) Exp Exp
  | (:&&) Exp Exp
  | Cases (NE.NonEmpty (Exp, Exp))
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
    -- algenv :: AlgEnv Exp E.Exp,
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

-- instance Semigroup View where
--   (View i (Cases xs)) <> (View Empty (Cases ys)) = View i (Cases $ xs <> ys)
--   (View Empty (Cases xs)) <> (View i (Cases ys)) = View i (Cases $ xs <> ys)
--   (View _i (Cases _xs)) <> (View _j (Cases _ys)) = error "view semigroup"

instance (Monoid w) => MonadFreshNames (RWS r w VEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

execViewM :: ViewM a -> VNameSource -> Views
execViewM (ViewM m) vns = views . fst $ execRWS m () s
  where
    s = VEnv vns mempty

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
  astMap m (Var x) = mapOnExp m $ Var x
  astMap m (Array ts) = Array <$> traverse (mapOnExp m) ts
  -- astMap m (If c t f) = If <$> mapOnExp m c <*> mapOnExp m t <*> mapOnExp m f
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
  astMap _ Recurrence = pure Recurrence
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = Not <$> mapOnExp m x
  astMap m (x :== y) = (:==) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :< y) = (:<) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :> y) = (:>) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (x :&& y) = (:&&) <$> mapOnExp m x <*> mapOnExp m y
  astMap m (Cases cases) = Cases <$> traverse (astMap m) cases

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

-- SoP foreshadowing:

-- instance (Monad m) => MonadSoP Exp E.Exp ViewM where
--   getUntrans = gets (untrans . algenv)
--   getRanges = gets (ranges . algenv)
--   getEquivs = gets (equivs . algenv)
--   modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

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
  pretty (Var x) = prettyName x
  pretty (Array ts) = pretty ts
  pretty (Idx arr i) = parens (pretty arr) <> "[" <> pretty i <> "]"
  pretty (Sum i lb ub e) =
    "Σ_"
      <> pretty i
      <> "="
      <+> pretty lb
      <> "^"
      <+> pretty ub
      <+> parens (pretty e)
  -- pretty (If c t f) =
  --   "If"
  --     <+> parens (pretty c)
  --     <+> "then"
  --     <+> parens (pretty t)
  --     <+> "else"
  --     <+> parens (pretty f)
  pretty (SoP sop) = pretty sop
  pretty Recurrence = "%₍₋₁₎"
  pretty (Bool x) = pretty x
  pretty (Not x) = "¬" <> parens (pretty x)
  pretty (x :== y) = pretty x <+> "==" <+> pretty y
  pretty (x :< y) = pretty x <+> "<" <+> pretty y
  pretty (x :> y) = pretty x <+> ">" <+> pretty y
  pretty (x :&& y) = pretty x <+> "&&" <+> pretty y
  pretty (Cases cases) = -- stack (map prettyCase (NE.toList cases))
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
