module Futhark.Analysis.Proofs.AlgebraPC.Algebra
  ( module Futhark.Analysis.Proofs.AlgebraPC.Monad,
    module Futhark.Analysis.Proofs.AlgebraPC.Symbol,
  )
where

import Control.Monad.RWS.Strict hiding (Sum)
import Futhark.Analysis.Proofs.AlgebraPC.Monad
import Futhark.Analysis.Proofs.AlgebraPC.Solve (findSymbolLEq0)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.SoP.Expression
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))
import Futhark.SoP.SoP (SoP)

{--
      autoParens x@(Var _) = pretty x
      autoParens x@(Hole _) = pretty x
      autoParens x = parens (pretty x)
--}

instance (Expression e, Ord e) => MonadSoP Symbol e Property (AlgM e) where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  getProperties = gets (properties . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}
  findSymLEq0 = findSymbolLEq0

{--
f :: (SoP Symbol >= 0) -> AlgM e Bool
f sop = do
  modifyEnv $ undefined
  undefined

runF :: (SoP Symbol >= 0) -> AlgEnv Symbol e Property -> VNameSource -> (Bool, VEnv e)
runF sop env vns= runAlgM (f sop) env vns

rules :: RuleBook (SoP Symbol) Symbol (AlgM e)
rules = []

(&<) :: (Expression e, Ord e) => SoP Symbol -> SoP Symbol ->  AlgM e Bool
sop1 &< sop2 = do
 prop  <- getProperties
 sop1 FM.$<$ sop2 -- fourier mo
 --}
