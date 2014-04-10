module Futhark.EnablingOpts.Simplifier.DataDependencies
  ( dataDependencies
  )
  where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Monoid

import Futhark.InternalRep

dataDependencies :: Body -> HM.HashMap VName (HS.HashSet VName)
dataDependencies (Body bnds _) =
  foldl grow HM.empty bnds
  where grow deps (Let pat e) =
          let free = freeNamesInExp e
          in  HM.fromList [ (identName v, free) | v <- pat ] `HM.union` deps
        grow deps (DoLoop merge i bound body) =
          let realFreeInBody = freeNamesInBody body `HS.difference`
                               HS.fromList (identName i : map (identName . fst) merge)
              free = mconcat (map (freeNamesInExp . subExp) (bound : map snd merge))
                     `HS.union` realFreeInBody
          in  HM.fromList [ (identName v, free) | v <- map fst merge ] `HM.union` deps
