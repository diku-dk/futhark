module Futhark.Analysis.View.Representation where

import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Futhark.SoP.SoP (SoP)
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty
import Futhark.MonadFreshNames
import Language.Futhark (VName (VName))
import Data.Functor.Identity
import Control.Monad.RWS.Strict hiding (Sum)

--------------------------------------------------------------
prettyName :: VName -> Doc ann
prettyName (VName vn i) = pretty vn <> pretty (mapMaybe subscript (show i))
  where
    subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"
--------------------------------------------------------------

data BoolExp =
  Bool Bool
  | Not Exp
  | (:==) Exp Exp
  | (:<) Exp Exp
  | (:>) Exp Exp
  deriving (Show, Eq, Ord)

instance Pretty BoolExp where
  pretty (Bool x) = pretty x
  pretty (Not x) = "¬" <> parens (pretty x)
  pretty (x :< y) = pretty x <+> "<" <+> pretty y
  pretty (x :> y) = pretty x <+> ">" <+> pretty y
  pretty (x :== y) = pretty x <+> "==" <+> pretty y

data Exp =
    Var VName
  | Array [Exp]
  | If Exp Exp Exp
  | Sum Exp Exp Exp Exp  -- index lower_bound upper_bound indexed_expression
  | Idx Exp Exp          -- array index
  | SoP (SoP Exp)
  | Recurrence           -- self-reference y[i-1]
  | BoolExp BoolExp      -- TODO CNF
  deriving (Show, Eq, Ord)

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
  pretty (If c t f) =
    "If"
      <+> parens (pretty c)
      <+> "then"
      <+> parens (pretty t)
      <+> "else"
      <+> parens (pretty f)
  pretty (SoP sop) = pretty sop
  pretty Recurrence = "%₍₋₁₎"
  pretty (BoolExp x) = pretty x

newtype Domain = Iota Exp -- [0, ..., n-1]
            -- | Union ...
  deriving (Show, Eq, Ord)

instance Pretty Domain where
  pretty (Iota e) = "iota" <+> pretty e

-- TODO add "bottom" for failure
data View = Forall
  { iterator :: VName,
    domain :: Domain,
    -- shape :: Maybe Shape, -- Might make sense to use this.
    value :: Exp
  }
  | Empty Exp -- XXX Promoted variable; no iteration.
  deriving (Show, Eq)

instance Pretty View where
  pretty (Forall i dom e) =
    "∀" <> prettyName i <+> "∈" <+> pretty dom <+> "." <+> pretty e
  pretty (Empty e) = pretty e

type Views = M.Map VName View

instance Pretty Views where
  pretty env =
    stack $ map (\(a, b) -> pretty a <+> "=" <+> pretty b) $ M.toList env

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
newtype ASTMapper m = ASTMapper { mapOnExp :: Exp -> m Exp }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

-- Mapping over AST for substitutions.
instance ASTMappable Exp where
  astMap m (Var x) = mapOnExp m $ Var x
  astMap m (Array ts) = Array <$> traverse (mapOnExp m) ts
  astMap m (If c t f) = If <$> astMap m c <*> astMap m t <*> astMap m f
  astMap m (Sum i lb ub e) = Sum <$> astMap m i <*> astMap m lb <*> astMap m ub <*> astMap m e
  astMap m (Idx arr i) = Idx <$> astMap m arr <*> astMap m i
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
  astMap m (BoolExp x) = BoolExp <$> astMap m x

instance ASTMappable BoolExp where
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = Not <$> astMap m x
  astMap m (x :== y) = (:==) <$> astMap m x <*> astMap m y
  astMap m (x :< y) = (:<) <$> astMap m x <*> astMap m y
  astMap m (x :> y) = (:>) <$> astMap m x <*> astMap m y

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
