{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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
       ( -- * The rule monad
         RuleM
       , cannotSimplify
       , liftMaybe

       -- * Rule definition
       , SimplificationRule(..)
       , RuleGeneric
       , RuleBasicOp
       , RuleIf
       , RuleDoLoop

       -- * Top-down rules
       , TopDown
       , TopDownRule
       , TopDownRuleGeneric
       , TopDownRuleBasicOp
       , TopDownRuleIf
       , TopDownRuleDoLoop
       , TopDownRuleOp

       -- * Bottom-up rules
       , BottomUp
       , BottomUpRule
       , BottomUpRuleGeneric
       , BottomUpRuleBasicOp
       , BottomUpRuleIf
       , BottomUpRuleDoLoop
       , BottomUpRuleOp

       -- * Assembling rules
       , RuleBook
       , ruleBook

         -- * Applying rules
       , topDownSimplifyStm
       , bottomUpSimplifyStm
       ) where

import Data.Monoid
import Control.Monad.State

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Representation.AST
import Futhark.Binder

-- | The monad in which simplification rules are evaluated.
newtype RuleM lore a = RuleM (BinderT lore (StateT VNameSource Maybe) a)
  deriving (Functor, Applicative, Monad,
            MonadBinder, MonadFreshNames, HasScope lore, LocalScope lore)

-- | Execute a 'RuleM' action.  If succesful, returns the result and a
-- list of new bindings.  Even if the action fail, there may still be
-- a monadic effect - particularly, the name source may have been
-- modified.
simplify :: (MonadFreshNames m, HasScope lore m, BinderOps lore) =>
            RuleM lore a
         -> m (Maybe (a, Stms lore))
simplify (RuleM m) = do
  scope <- askScope
  modifyNameSource $ \src ->
    case runStateT (runBinderT m scope) src of
      Nothing        -> (Nothing, src)
      Just (x, src') -> (Just x, src')

cannotSimplify :: RuleM lore a
cannotSimplify = fail "Cannot simplify"

liftMaybe :: Maybe a -> RuleM lore a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x

type RuleGeneric lore a = a -> Stm lore -> RuleM lore ()
type RuleBasicOp lore a = (a -> Pattern lore -> StmAux (ExpAttr lore) ->
                           BasicOp lore -> RuleM lore ())
type RuleIf lore a = a -> Pattern lore -> StmAux (ExpAttr lore) ->
                     (SubExp, BodyT lore, BodyT lore,
                      IfAttr (BranchType lore)) ->
                     RuleM lore ()
type RuleDoLoop lore a = a -> Pattern lore -> StmAux (ExpAttr lore) ->
                         ([(FParam lore, SubExp)], [(FParam lore, SubExp)],
                          LoopForm lore, BodyT lore) ->
                         RuleM lore ()
type RuleOp lore a = a -> Pattern lore -> StmAux (ExpAttr lore) ->
                     Op lore -> RuleM lore ()

-- | A simplification rule takes some argument and a statement, and
-- tries to simplify the statement.
data SimplificationRule lore a = RuleGeneric (RuleGeneric lore a)
                               | RuleBasicOp (RuleBasicOp lore a)
                               | RuleIf (RuleIf lore a)
                               | RuleDoLoop (RuleDoLoop lore a)
                               | RuleOp (RuleOp lore a)

-- | A collection of rules grouped by which forms of statements they
-- may apply to.
data Rules lore a = Rules { rulesAny :: [SimplificationRule lore a]
                       , rulesBasicOp :: [SimplificationRule lore a]
                       , rulesIf :: [SimplificationRule lore a]
                       , rulesDoLoop :: [SimplificationRule lore a]
                       , rulesOp :: [SimplificationRule lore a]
                       }

instance Monoid (Rules lore a) where
  mempty = Rules mempty mempty mempty mempty mempty
  Rules as1 bs1 cs1 ds1 es1 `mappend` Rules as2 bs2 cs2 ds2 es2 =
    Rules (as1<>as2) (bs1<>bs2) (cs1<>cs2) (ds1<>ds2) (es1<>es2)

-- | Context for a rule applied during top-down traversal of the
-- program.  Takes a symbol table as argument.
type TopDown lore = ST.SymbolTable lore

type TopDownRuleGeneric lore = RuleGeneric lore (TopDown lore)
type TopDownRuleBasicOp lore = RuleBasicOp lore (TopDown lore)
type TopDownRuleIf lore = RuleIf lore (TopDown lore)
type TopDownRuleDoLoop lore = RuleDoLoop lore (TopDown lore)
type TopDownRuleOp lore = RuleOp lore (TopDown lore)
type TopDownRule lore = SimplificationRule lore (TopDown lore)

-- | Context for a rule applied during bottom-up traversal of the
-- program.  Takes a symbol table and usage table as arguments.
type BottomUp lore = (ST.SymbolTable lore, UT.UsageTable)

type BottomUpRuleGeneric lore = RuleGeneric lore (BottomUp lore)
type BottomUpRuleBasicOp lore = RuleBasicOp lore (BottomUp lore)
type BottomUpRuleIf lore = RuleIf lore (BottomUp lore)
type BottomUpRuleDoLoop lore = RuleDoLoop lore (BottomUp lore)
type BottomUpRuleOp lore = RuleOp lore (BottomUp lore)
type BottomUpRule lore = SimplificationRule lore (BottomUp lore)

-- | A collection of top-down rules.
type TopDownRules lore = Rules lore (TopDown lore)

-- | A collection of bottom-up rules.
type BottomUpRules lore = Rules lore (BottomUp lore)

-- | A collection of both top-down and bottom-up rules.
data RuleBook lore = RuleBook { bookTopDownRules :: TopDownRules lore
                              , bookBottomUpRules :: BottomUpRules lore
                              }

instance Monoid (RuleBook lore) where
  mempty = RuleBook mempty mempty
  RuleBook ts1 bs1 `mappend` RuleBook ts2 bs2 = RuleBook (ts1<>ts2) (bs1<>bs2)

-- | Construct a rule book from a collection of rules.
ruleBook :: [TopDownRule m]
         -> [BottomUpRule m]
         -> RuleBook m
ruleBook topdowns bottomups =
  RuleBook (groupRules topdowns) (groupRules bottomups)
  where groupRules :: [SimplificationRule m a] -> Rules m a
        groupRules rs = Rules rs
                              (filter forBasicOp rs)
                              (filter forIf rs)
                              (filter forDoLoop rs)
                              (filter forOp rs)

        forBasicOp RuleBasicOp{} = True
        forBasicOp RuleGeneric{} = True
        forBasicOp _ = False

        forIf RuleIf{} = True
        forIf RuleGeneric{} = True
        forIf _ = False

        forDoLoop RuleDoLoop{} = True
        forDoLoop RuleGeneric{} = True
        forDoLoop _ = False

        forOp RuleOp{} = True
        forOp RuleGeneric{} = True
        forOp _ = False

-- | @simplifyStm lookup bnd@ performs simplification of the
-- binding @bnd@.  If simplification is possible, a replacement list
-- of bindings is returned, that bind at least the same names as the
-- original binding (and possibly more, for intermediate results).
topDownSimplifyStm :: (MonadFreshNames m, HasScope lore m, BinderOps lore) =>
                      RuleBook lore
                   -> ST.SymbolTable lore
                   -> Stm lore
                   -> m (Maybe (Stms lore))
topDownSimplifyStm = applyRules . bookTopDownRules

-- | @simplifyStm uses bnd@ performs simplification of the binding
-- @bnd@.  If simplification is possible, a replacement list of
-- bindings is returned, that bind at least the same names as the
-- original binding (and possibly more, for intermediate results).
-- The first argument is the set of names used after this binding.
bottomUpSimplifyStm :: (MonadFreshNames m, HasScope lore m, BinderOps lore) =>
                       RuleBook lore
                    -> (ST.SymbolTable lore, UT.UsageTable)
                    -> Stm lore
                    -> m (Maybe (Stms lore))
bottomUpSimplifyStm = applyRules . bookBottomUpRules

rulesForStm :: Stm lore -> Rules lore a -> [SimplificationRule lore a]
rulesForStm stm = case stmExp stm of BasicOp{} -> rulesBasicOp
                                     DoLoop{} -> rulesDoLoop
                                     Op{} -> rulesOp
                                     If{} -> rulesIf
                                     _ -> rulesAny

applyRule :: SimplificationRule lore a -> a -> Stm lore -> RuleM lore ()
applyRule (RuleGeneric f) a stm = f a stm
applyRule (RuleBasicOp f) a (Let pat aux (BasicOp e)) = f a pat aux e
applyRule (RuleDoLoop f) a (Let pat aux (DoLoop ctx val form body)) =
  f a pat aux (ctx, val, form, body)
applyRule (RuleIf f) a (Let pat aux (If cond tbody fbody ifsort)) =
  f a pat aux (cond, tbody, fbody, ifsort)
applyRule (RuleOp f) a (Let pat aux (Op op)) =
  f a pat aux op
applyRule _ _ _ =
  cannotSimplify

applyRules :: (MonadFreshNames m, HasScope lore m, BinderOps lore) =>
              Rules lore a -> a -> Stm lore
           -> m (Maybe (Stms lore))
applyRules rules context stm = applyRules' (rulesForStm stm rules) context stm

applyRules' :: (MonadFreshNames m, HasScope lore m, BinderOps lore) =>
               [SimplificationRule lore a] -> a -> Stm lore
            -> m (Maybe (Stms lore))
applyRules' []           _       _   = return Nothing
applyRules' (rule:rules) context bnd = do
  res <- simplify $ applyRule rule context bnd
  case res of Just ((), bnds) -> return $ Just bnds
              Nothing         -> applyRules' rules context bnd
