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
import Futhark.Analysis.Proofs.IndexFn (IndexFnM (..), VEnv (vnamesource, VEnv))
import Futhark.Analysis.Proofs.Symbol qualified as IxFn (Symbol)

-- data VEnv e = VEnv
--   { vnamesource :: VNameSource,
--     algenv :: AlgEnv Symbol e Property
--   }

type AlgM a = IndexFnM

-- runAlgM :: AlgM e a -> AlgEnv Symbol e Property -> VNameSource -> (a, VEnv e)
runAlgM :: IndexFnM a -> AlgEnv Symbol IxFn.Symbol Property -> VNameSource -> (a, VEnv)
runAlgM (IndexFnM m) env vns = getRes $ runRWS m () s
  where
    getRes (x, env1, _) = (x, env1)
    s = VEnv vns env mempty mempty False
