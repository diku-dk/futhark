module Futhark.EnablingOpts.Simplifier.DataDependencies
  ( dataDependencies
  )
  where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Futhark.InternalRep

dataDependencies :: Body -> HM.HashMap VName (HS.HashSet VName)
dataDependencies (Body bnds _) =
  foldl grow HM.empty bnds
  where grow deps (Let pat e) =
          let free = freeNamesInExp e
          in  HM.fromList [ (identName v, free) | v <- pat ] `HM.union` deps
