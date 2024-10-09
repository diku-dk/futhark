module Futhark.Analysis.Proofs.AlgebraPC.Monad
  ( AlgM (..),
    VEnv (..),
    runAlgM,
  )
where

import Control.Monad.RWS.Strict hiding (Sum)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.Analysis.Proofs.Util (prettyName)
import Futhark.MonadFreshNames
import Futhark.SoP.Expression
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))
import Futhark.SoP.SoP (SoP)
import Futhark.Util.Pretty (Pretty, brackets, enclose, parens, pretty, (<+>))
import Language.Futhark (VName)
import Language.Futhark qualified as E

data VEnv e = VEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Symbol e Property
  }

instance (Monoid w) => MonadFreshNames (RWS r w (VEnv e)) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

newtype AlgM e a = AlgM (RWS () () (VEnv e) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      MonadState (VEnv e)
    )

runAlgM :: AlgM e a -> AlgEnv Symbol e Property -> VNameSource -> (a, VEnv e)
runAlgM (AlgM m) env vns = getRes $ runRWS m () s
  where
    getRes (x, env1, _) = (x, env1)
    s = VEnv vns env
