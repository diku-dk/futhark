-- | Go through the program and use algebraic simplification and range
-- analysis to try to figure out which assertions are statically true.
--
-- Currently implemented by running the simplifier with a special rule
-- that is too expensive to run all the time.

module Futhark.Pass.ResolveAssertions
  ( resolveAssertions
  )
  where

import Data.Maybe

import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplify.Rule
import qualified Futhark.Analysis.AlgSimplify as AS
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.AST.Syntax
import Futhark.Construct
import Futhark.Pass
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.SOACS.Simplify as Simplify
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rules

-- | The assertion-resolver pass.
resolveAssertions :: Pass SOACS SOACS
resolveAssertions = Pass
  "resolve assertions"
  "Try to statically resolve bounds checks and similar." $
  Simplify.simplifyProg Simplify.simpleSOACS rulebook Simplify.noExtraHoistBlockers
  where rulebook = standardRules <> ruleBook [ RuleBasicOp simplifyScalExp ] []

simplifyScalExp :: BinderOps lore => TopDownRuleBasicOp lore
simplifyScalExp vtable pat _ e = do
  res <- SE.toScalExp (`ST.lookupScalExp` vtable) $ BasicOp e
  case res of
    -- If the sufficient condition is 'True', then it statically succeeds.
    Just se
      | SE.scalExpType se == Bool,
        isNothing $ valOrVar se,
        SE.scalExpSize se < size_bound,
        Just se' <- valOrVar $ AS.simplify se ranges ->
        letBind_ pat $ BasicOp $ SubExp se'
    _ -> cannotSimplify
  where ranges = ST.rangesRep vtable
        size_bound = 10 -- don't touch scalexps bigger than this.

        valOrVar (SE.Val v)  = Just $ Constant v
        valOrVar (SE.Id v _) = Just $ Var v
        valOrVar _           = Nothing
