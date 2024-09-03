module Futhark.Analysis.Proofs.RefineTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.Unify
import Futhark.Analysis.Proofs.Rules
import Control.Monad.RWS.Strict
import Futhark.SoP.SoP (sym2SoP, (.+.), int2SoP, (.-.))
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (..), addRange)
import qualified Language.Futhark as E
import qualified Data.Set as S
import qualified Futhark.SoP.SoP as SoP
import Futhark.Analysis.Proofs.IndexFn

runTest :: IndexFnM a -> a
runTest test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests = testGroup "Proofs.Rules"
  []
  where
    -- int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

    varsM =
      (,,,,,,,) <$> newVName "x" <*> newVName "y" <*> newVName "z" <*> newVName "w"
                <*> newVName "a" <*> newVName "b" <*> newVName "c" <*> newVName "d"
    (x,y,z,w,a,b,c,_) = runTest varsM

    run f = runTest (varsM >>= f)

    -- Less fragile renaming.
    e @??= e' = renamed e @?= renamed e'
    renamed x = runTest $ do
          putNameSource (newNameSource (-10000))
          rename x

