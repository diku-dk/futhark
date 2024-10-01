module Futhark.Analysis.Proofs.AlgebraPC.Algebra
  ( Symbol(..),
    MonDir(..),
    Property(..),
    AlgM(..),
    runAlgM,
  ) 
where

import Futhark.SoP.Util
import Control.Monad.RWS.Strict hiding (Sum)
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
import qualified Futhark.SoP.FourierMotzkin as FM

data Symbol
  = Var VName
  | Hole VName
  | Idx VName (SoP Symbol)
  | Mdf MonDir VName (SoP Symbol) (SoP Symbol)
  -- ^ `Mdf dir A i1 i2` means `A[i1] - A[i2]` where
  -- `A` is known to be monotonic with direction `dir`
  | Sum VName (SoP Symbol) (SoP Symbol)
  | Pow (Integer, SoP Symbol)
  deriving (Show, Eq, Ord)

instance Pretty Symbol where
  pretty symbol = case symbol of
    (Var x) -> prettyName x
    (Hole x) -> prettyHole x
    (Idx x i) -> (prettyName x) <> brackets (pretty i)
    (Mdf _ x i1 i2) ->
      parens $
        ((prettyName x) <> (brackets (pretty i1)))
        <+> "-" <+> 
        ((prettyName x) <> (brackets (pretty i2)))
    (Sum x lb ub) ->
      "∑"
        <> prettyName x
        <> brackets (pretty lb <+> ":" <+> pretty ub)
    (Pow (b,s)) -> parens $ (pretty b) <+> "^" <+> parens (pretty s)
    where
      autoParens x@(Var _) = pretty x
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
      iversonbrackets = enclose "⟦" "⟧"
      prettyOp s x y = pretty x <+> s <+> pretty y

data MonDir = Inc | IncS | Dec | DecS
  deriving (Show, Eq, Ord)

data Property
  = Monotonic MonDir
  | Injective
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

runAlgM :: (Ord e) => AlgM e a -> AlgEnv Symbol e Property -> VNameSource -> (a, VEnv e)
runAlgM (AlgM m) env vns = getRes $ runRWS m () s
  where
    getRes (x, env1, _) = (x, env1)
    s = VEnv vns env

-- f :: (SoP Symbol >= 0) -> AlgM e Bool
-- f sop = do
--   modifyEnv $ undefined
--   undefined

-- runF :: (SoP Symbol >= 0) -> AlgEnv Symbol e Property -> VNameSource -> (Bool, VEnv e)
-- runF sop env vns= runAlgM (f sop) env vns

rules :: RuleBook (SoP Symbol) Symbol (AlgM e)
rules = []

(&<) :: (Expression e, Ord e) => SoP Symbol -> SoP Symbol ->  AlgM e Bool
sop1 &< sop2 = do
 prop  <- getProperties
 sop1 FM.$<$ sop2 -- fourier mo
