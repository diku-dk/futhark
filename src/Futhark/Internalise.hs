{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- |
--
-- This module implements a transformation from source to core
-- Futhark.
module Futhark.Internalise (internaliseProg) where

import qualified Data.Text as T
import Futhark.Compiler.Config
import Futhark.IR.SOACS as I hiding (stmPat)
import Futhark.Internalise.Defunctionalise as Defunctionalise
import Futhark.Internalise.Defunctorise as Defunctorise
import qualified Futhark.Internalise.Exps as Exps
import Futhark.Internalise.LiftLambdas as LiftLambdas
import Futhark.Internalise.Monad as I
import Futhark.Internalise.Monomorphise as Monomorphise
import Futhark.Util.Log
import Language.Futhark.Semantic (Imports)

-- | Convert a program in source Futhark to a program in the Futhark
-- core language.
internaliseProg ::
  (MonadFreshNames m, MonadLogger m) =>
  FutharkConfig ->
  Imports ->
  m (I.Prog SOACS)
internaliseProg config prog = do
  maybeLog "Defunctorising"
  prog_decs <- Defunctorise.transformProg prog
  maybeLog "Monomorphising"
  prog_decs' <- Monomorphise.transformProg prog_decs
  maybeLog "Lifting lambdas"
  prog_decs'' <- LiftLambdas.transformProg prog_decs'
  maybeLog "Defunctionalising"
  prog_decs''' <- Defunctionalise.transformProg prog_decs''
  maybeLog "Converting to core IR"
  Exps.transformProg (futharkSafe config) prog_decs'''
  where
    verbose = fst (futharkVerbose config) > NotVerbose
    maybeLog s
      | verbose = logMsg (s :: T.Text)
      | otherwise = pure ()
