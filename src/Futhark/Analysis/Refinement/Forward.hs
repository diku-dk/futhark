module Futhark.Analysis.Refinement.Forward where

import Control.Monad.RWS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Debug.Trace
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Prop
import Futhark.Analysis.Refinement.Relations
import Futhark.Analysis.Refinement.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.Refine
import Futhark.SoP.SoP qualified as SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Prop qualified as E

forwards :: E.Exp -> RefineM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
      forward x e
      forwards body
forwards _ = pure ()

forward :: E.VName -> E.Exp -> RefineM ()
forward x e = pure ()

-- ifM
--  (isNonNeg e)
--  ( do
--      i <- newVName "i"
--      let info = ForAll (Var i) (Var x) (Var i :>= intToExp 0)
--      modify $ \senv ->
--        senv
--          { known = known senv ++ [info],
--            known_map =
--              M.insertWith (<>) x [info] $ known_map senv
--          }
--  )
--  (pure ())
