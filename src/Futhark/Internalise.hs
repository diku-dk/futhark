-- |
--
-- This module implements a transformation from source to core
-- Futhark.
--
-- The source and core language is similar in spirit, but the core
-- language is much more regular (and mostly much simpler) in order to
-- make it easier to write program transformations.
--
-- * "Language.Futhark.Syntax" contains the source language definition.
--
-- * "Futhark.IR.Syntax" contains the core IR definition.
--
-- Specifically, internalisation generates the SOACS dialect of the
-- core IR ("Futhark.IR.SOACS").  This is then initially used by the
-- compiler middle-end.  The main differences between the source and
-- core IR are as follows:
--
-- * The core IR has no modules.  These are removed in
--   "Futhark.Internalise.Defunctorise".
--
-- * The core IR has no type abbreviations.  These are removed in
--   "Futhark.Internalise.ApplyTypeAbbrs".
--
-- * The core IR has little syntactic niceties. A lot of syntactic
--   sugar is removed in "Futhark.Internalise.FullNormalise".
--
-- * Lambda lifting is performed by "Futhark.Internalise.LiftLambdas",
-- * mostly to make the job of later passes simpler.
--
-- * The core IR is monomorphic.  Polymorphic functions are monomorphised in
--   "Futhark.Internalise.Monomorphise"
--
-- * The core IR is first-order. "Futhark.Internalise.Defunctionalise"
--   removes higher-order functions.
--
-- * The core IR is in [ANF](https://en.wikipedia.org/wiki/A-normal_form).
--
-- * The core IR does not have arrays of tuples (or tuples or records
--   at all, really).  Arrays of tuples are turned into multiple
--   arrays.  For example, a source language transposition of an array
--   of pairs becomes a core IR that contains two transpositions of
--   two distinct arrays.  The guts of this transformation is in
--   "Futhark.Internalise.Exps".
--
-- * For the above reason, SOACs also accept multiple input arrays.
--   The available primitive operations are also somewhat different
--   than in the source language.  See 'Futhark.IR.SOACS.SOAC.SOAC'.
module Futhark.Internalise (internaliseProg) where

import Data.Text qualified as T
import Futhark.Compiler.Config
import Futhark.IR.SOACS as I hiding (stmPat)
import Futhark.Internalise.ApplyTypeAbbrs as ApplyTypeAbbrs
import Futhark.Internalise.Defunctionalise as Defunctionalise
import Futhark.Internalise.Defunctorise as Defunctorise
import Futhark.Internalise.Entry (visibleTypes)
import Futhark.Internalise.Exps qualified as Exps
import Futhark.Internalise.FullNormalise qualified as FullNormalise
import Futhark.Internalise.LiftLambdas as LiftLambdas
import Futhark.Internalise.Monad as I
import Futhark.Internalise.Monomorphise as Monomorphise
import Futhark.Internalise.ReplaceRecords as ReplaceRecords
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
  prog_decs0 <- ApplyTypeAbbrs.transformProg =<< Defunctorise.transformProg prog
  maybeLog "Full Normalising"
  prog_decs1 <- FullNormalise.transformProg prog_decs0
  maybeLog "Replacing records"
  prog_decs2 <- ReplaceRecords.transformProg prog_decs1
  maybeLog "Lifting lambdas"
  prog_decs3 <- LiftLambdas.transformProg prog_decs2
  maybeLog "Monomorphising"
  prog_decs4 <- Monomorphise.transformProg prog_decs3
  maybeLog "Defunctionalising"
  prog_decs5 <- Defunctionalise.transformProg prog_decs4
  maybeLog "Converting to core IR"
  Exps.transformProg (futharkSafe config) (visibleTypes prog) prog_decs5
  where
    verbose = fst (futharkVerbose config) > NotVerbose
    maybeLog s
      | verbose = logMsg (s :: T.Text)
      | otherwise = pure ()
