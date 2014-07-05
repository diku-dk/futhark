-- | This module defines the concept of a simplification rule for
-- bindings.  The intent is that you pass some context (such as symbol
-- table) and a binding, and is given back a sequence of bindings that
-- compute the same result, but are "better" in some sense.
--
-- These rewrite rules are "local", in that they do not maintain any
-- state or look at the program as a whole.  Compare this to the
-- fusion algorithm in @Futhark.Optimise.Fusion.Fusion@, which must be implemented
-- as its own pass.
module Futhark.Optimise.Simplifier.Rule
       ( -- * Rule definition
         TopDownRule
       , TopDownRules
       , BottomUpRule
       , BottomUpRules
       , RuleBook
         -- * Applying rules
       , topDownSimplifyBinding
       , bottomUpSimplifyBinding
       ) where

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Optimise.Simplifier.Simplify
import Futhark.InternalRep
import Futhark.MonadFreshNames

type SimplificationRule a = a -> Binding -> Simplify [Binding]

-- | A rule applied during top-down traversal of the program.  Takes a
-- symbol table as argument.
type TopDownRule u = SimplificationRule (ST.SymbolTable u)

-- | A collection of top-down rules.
type TopDownRules u = [TopDownRule u]

-- | A rule applied during bottom-up traversal of the program.  Takes
-- a symbol table and usage table as arguments.
type BottomUpRule u = SimplificationRule (ST.SymbolTable u, UT.UsageTable)

-- | A collection of bottom-up rules.
type BottomUpRules u = [BottomUpRule u]

-- | A collection of both top-down and bottom-up rules.
type RuleBook u = (TopDownRules u, BottomUpRules u)

-- | @simplifyBinding lookup bnd@ performs simplification of the
-- binding @bnd@.  If simplification is possible, a replacement list
-- of bindings is returned, that bind at least the same banes as the
-- original binding (and possibly more, for intermediate results).
topDownSimplifyBinding :: MonadFreshNames m =>
                          RuleBook u
                       -> ST.SymbolTable u
                       -> Binding
                       -> m (Maybe [Binding])
topDownSimplifyBinding = applyRules . fst

-- | @simplifyBinding uses bnd@ performs simplification of the binding
-- @bnd@.  If simplification is possible, a replacement list of
-- bindings is returned, that bind at least the same banes as the
-- original binding (and possibly more, for intermediate results).
-- The first argument is the set of names used after this binding.
bottomUpSimplifyBinding :: MonadFreshNames m =>
                           RuleBook u
                        -> (ST.SymbolTable u, UT.UsageTable)
                        -> Binding
                        -> m (Maybe [Binding])
bottomUpSimplifyBinding = applyRules . snd

applyRules :: MonadFreshNames m =>
              [SimplificationRule a] -> a -> Binding -> m (Maybe [Binding])
applyRules []           _    _      = return Nothing
applyRules (rule:rules) context bnd = do
  res <- simplify $ rule context bnd
  case res of Just bnds -> return $ Just bnds
              Nothing   -> applyRules rules context bnd
