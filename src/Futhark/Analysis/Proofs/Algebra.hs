module Futhark.Analysis.Proofs.Algebra where

import Control.Monad.RWS.Strict
import Data.Map (Map)
import Data.Map qualified as M
import Futhark.Analysis.Proofs.Rule
import Futhark.Analysis.Proofs.Util (prettyHole, prettyName)
import Futhark.MonadFreshNames
import Futhark.SoP.Expression
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))
import Futhark.SoP.SoP (SoP)
import Futhark.Util.Pretty (Pretty, brackets, enclose, parens, pretty, (<+>))
import Language.Futhark (VName)
import Language.Futhark qualified as E

data Symbol
  = Var VName
  | Hole VName
  | Idx Symbol (SoP Symbol)
  | LinComb VName (SoP Symbol) (SoP Symbol) Symbol
  | Pow2 (SoP Symbol)
  deriving (Show, Eq, Ord)

instance Pretty Symbol where
  pretty symbol = case symbol of
    (Var x) -> prettyName x
    (Hole x) -> prettyHole x
    (Idx x i) -> autoParens x <> brackets (pretty i)
    (LinComb i lb ub e) ->
      "∑"
        <> prettyName i
        <> "∈"
        <> parens (pretty lb <+> ".." <+> pretty ub)
        <> " "
        <> autoParens e
    (Pow2 x) -> parens (pretty x) <> "²"
    where
      autoParens x@(Var _) = pretty x
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      iversonbrackets = enclose "⟦" "⟧"
      prettyOp s x y = pretty x <+> s <+> pretty y

data Property
  = Monotonic
  deriving (Show, Eq, Ord)

data VEnv e = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Symbol e Property
  }

newtype AlgM e a = AlgM (RWS () () (VEnv e) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      MonadState (VEnv e)
    )

instance (Monoid w) => MonadFreshNames (RWS r w (VEnv e)) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

-- This is required by MonadSoP.
instance Nameable Symbol where
  mkName (VNameSource i) = (Var $ E.VName "x" i, VNameSource $ i + 1)

instance (Expression e, Ord e) => MonadSoP Symbol e Property (AlgM e) where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  getProperties = gets (properties . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

runAlgM :: (Ord e) => AlgM e a -> VNameSource -> (a, VEnv e)
runAlgM (AlgM m) vns = getRes $ runRWS m () s
  where
    getRes (x, env, _) = (x, env)
    s = VEnv vns mempty

rules :: RuleBook (SoP Symbol) Symbol (AlgM e)
rules = []
