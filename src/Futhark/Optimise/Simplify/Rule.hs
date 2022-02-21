{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
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
module Futhark.Optimise.Simplify.Rule
  ( -- * The rule monad
    RuleM,
    cannotSimplify,
    liftMaybe,

    -- * Rule definition
    Rule (..),
    SimplificationRule (..),
    RuleGeneric,
    RuleBasicOp,
    RuleIf,
    RuleDoLoop,

    -- * Top-down rules
    TopDown,
    TopDownRule,
    TopDownRuleGeneric,
    TopDownRuleBasicOp,
    TopDownRuleIf,
    TopDownRuleDoLoop,
    TopDownRuleOp,

    -- * Bottom-up rules
    BottomUp,
    BottomUpRule,
    BottomUpRuleGeneric,
    BottomUpRuleBasicOp,
    BottomUpRuleIf,
    BottomUpRuleDoLoop,
    BottomUpRuleOp,

    -- * Assembling rules
    RuleBook,
    ruleBook,

    -- * Applying rules
    topDownSimplifyStm,
    bottomUpSimplifyStm,
  )
where

import Control.Monad.State
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Builder
import Futhark.IR

-- | The monad in which simplification rules are evaluated.
newtype RuleM rep a = RuleM (BuilderT rep (StateT VNameSource Maybe) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFreshNames,
      HasScope rep,
      LocalScope rep
    )

instance (ASTRep rep, BuilderOps rep) => MonadBuilder (RuleM rep) where
  type Rep (RuleM rep) = rep
  mkExpDecM pat e = RuleM $ mkExpDecM pat e
  mkBodyM stms res = RuleM $ mkBodyM stms res
  mkLetNamesM pat e = RuleM $ mkLetNamesM pat e

  addStms = RuleM . addStms
  collectStms (RuleM m) = RuleM $ collectStms m

-- | Execute a 'RuleM' action.  If succesful, returns the result and a
-- list of new bindings.
simplify ::
  Scope rep ->
  VNameSource ->
  Rule rep ->
  Maybe (Stms rep, VNameSource)
simplify _ _ Skip = Nothing
simplify scope src (Simplify (RuleM m)) =
  runStateT (runBuilderT_ m scope) src

cannotSimplify :: RuleM rep a
cannotSimplify = RuleM $ lift $ lift Nothing

liftMaybe :: Maybe a -> RuleM rep a
liftMaybe Nothing = cannotSimplify
liftMaybe (Just x) = return x

-- | An efficient way of encoding whether a simplification rule should even be attempted.
data Rule rep
  = -- | Give it a shot.
    Simplify (RuleM rep ())
  | -- | Don't bother.
    Skip

type RuleGeneric rep a = a -> Stm rep -> Rule rep

type RuleBasicOp rep a =
  ( a ->
    Pat (LetDec rep) ->
    StmAux (ExpDec rep) ->
    BasicOp ->
    Rule rep
  )

type RuleIf rep a =
  a ->
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  ( SubExp,
    Body rep,
    Body rep,
    IfDec (BranchType rep)
  ) ->
  Rule rep

type RuleDoLoop rep a =
  a ->
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  ( [(FParam rep, SubExp)],
    LoopForm rep,
    Body rep
  ) ->
  Rule rep

type RuleOp rep a =
  a ->
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  Op rep ->
  Rule rep

-- | A simplification rule takes some argument and a statement, and
-- tries to simplify the statement.
data SimplificationRule rep a
  = RuleGeneric (RuleGeneric rep a)
  | RuleBasicOp (RuleBasicOp rep a)
  | RuleIf (RuleIf rep a)
  | RuleDoLoop (RuleDoLoop rep a)
  | RuleOp (RuleOp rep a)

-- | A collection of rules grouped by which forms of statements they
-- may apply to.
data Rules rep a = Rules
  { rulesAny :: [SimplificationRule rep a],
    rulesBasicOp :: [SimplificationRule rep a],
    rulesIf :: [SimplificationRule rep a],
    rulesDoLoop :: [SimplificationRule rep a],
    rulesOp :: [SimplificationRule rep a]
  }

instance Semigroup (Rules rep a) where
  Rules as1 bs1 cs1 ds1 es1 <> Rules as2 bs2 cs2 ds2 es2 =
    Rules (as1 <> as2) (bs1 <> bs2) (cs1 <> cs2) (ds1 <> ds2) (es1 <> es2)

instance Monoid (Rules rep a) where
  mempty = Rules mempty mempty mempty mempty mempty

-- | Context for a rule applied during top-down traversal of the
-- program.  Takes a symbol table as argument.
type TopDown rep = ST.SymbolTable rep

type TopDownRuleGeneric rep = RuleGeneric rep (TopDown rep)

type TopDownRuleBasicOp rep = RuleBasicOp rep (TopDown rep)

type TopDownRuleIf rep = RuleIf rep (TopDown rep)

type TopDownRuleDoLoop rep = RuleDoLoop rep (TopDown rep)

type TopDownRuleOp rep = RuleOp rep (TopDown rep)

type TopDownRule rep = SimplificationRule rep (TopDown rep)

-- | Context for a rule applied during bottom-up traversal of the
-- program.  Takes a symbol table and usage table as arguments.
type BottomUp rep = (ST.SymbolTable rep, UT.UsageTable)

type BottomUpRuleGeneric rep = RuleGeneric rep (BottomUp rep)

type BottomUpRuleBasicOp rep = RuleBasicOp rep (BottomUp rep)

type BottomUpRuleIf rep = RuleIf rep (BottomUp rep)

type BottomUpRuleDoLoop rep = RuleDoLoop rep (BottomUp rep)

type BottomUpRuleOp rep = RuleOp rep (BottomUp rep)

type BottomUpRule rep = SimplificationRule rep (BottomUp rep)

-- | A collection of top-down rules.
type TopDownRules rep = Rules rep (TopDown rep)

-- | A collection of bottom-up rules.
type BottomUpRules rep = Rules rep (BottomUp rep)

-- | A collection of both top-down and bottom-up rules.
data RuleBook rep = RuleBook
  { bookTopDownRules :: TopDownRules rep,
    bookBottomUpRules :: BottomUpRules rep
  }

instance Semigroup (RuleBook rep) where
  RuleBook ts1 bs1 <> RuleBook ts2 bs2 = RuleBook (ts1 <> ts2) (bs1 <> bs2)

instance Monoid (RuleBook rep) where
  mempty = RuleBook mempty mempty

-- | Construct a rule book from a collection of rules.
ruleBook ::
  [TopDownRule m] ->
  [BottomUpRule m] ->
  RuleBook m
ruleBook topdowns bottomups =
  RuleBook (groupRules topdowns) (groupRules bottomups)
  where
    groupRules :: [SimplificationRule m a] -> Rules m a
    groupRules rs =
      Rules
        rs
        (filter forBasicOp rs)
        (filter forIf rs)
        (filter forDoLoop rs)
        (filter forOp rs)

    forBasicOp RuleBasicOp {} = True
    forBasicOp RuleGeneric {} = True
    forBasicOp _ = False

    forIf RuleIf {} = True
    forIf RuleGeneric {} = True
    forIf _ = False

    forDoLoop RuleDoLoop {} = True
    forDoLoop RuleGeneric {} = True
    forDoLoop _ = False

    forOp RuleOp {} = True
    forOp RuleGeneric {} = True
    forOp _ = False

-- | @simplifyStm lookup stm@ performs simplification of the
-- binding @stm@.  If simplification is possible, a replacement list
-- of bindings is returned, that bind at least the same names as the
-- original binding (and possibly more, for intermediate results).
topDownSimplifyStm ::
  (MonadFreshNames m, HasScope rep m) =>
  RuleBook rep ->
  ST.SymbolTable rep ->
  Stm rep ->
  m (Maybe (Stms rep))
topDownSimplifyStm = applyRules . bookTopDownRules

-- | @simplifyStm uses stm@ performs simplification of the binding
-- @stm@.  If simplification is possible, a replacement list of
-- bindings is returned, that bind at least the same names as the
-- original binding (and possibly more, for intermediate results).
-- The first argument is the set of names used after this binding.
bottomUpSimplifyStm ::
  (MonadFreshNames m, HasScope rep m) =>
  RuleBook rep ->
  (ST.SymbolTable rep, UT.UsageTable) ->
  Stm rep ->
  m (Maybe (Stms rep))
bottomUpSimplifyStm = applyRules . bookBottomUpRules

rulesForStm :: Stm rep -> Rules rep a -> [SimplificationRule rep a]
rulesForStm stm = case stmExp stm of
  BasicOp {} -> rulesBasicOp
  DoLoop {} -> rulesDoLoop
  Op {} -> rulesOp
  If {} -> rulesIf
  _ -> rulesAny

applyRule :: SimplificationRule rep a -> a -> Stm rep -> Rule rep
applyRule (RuleGeneric f) a stm = f a stm
applyRule (RuleBasicOp f) a (Let pat aux (BasicOp e)) = f a pat aux e
applyRule (RuleDoLoop f) a (Let pat aux (DoLoop merge form body)) =
  f a pat aux (merge, form, body)
applyRule (RuleIf f) a (Let pat aux (If cond tbody fbody ifsort)) =
  f a pat aux (cond, tbody, fbody, ifsort)
applyRule (RuleOp f) a (Let pat aux (Op op)) =
  f a pat aux op
applyRule _ _ _ =
  Skip

applyRules ::
  (MonadFreshNames m, HasScope rep m) =>
  Rules rep a ->
  a ->
  Stm rep ->
  m (Maybe (Stms rep))
applyRules all_rules context stm = do
  scope <- askScope

  modifyNameSource $ \src ->
    let applyRules' [] = Nothing
        applyRules' (rule : rules) =
          case simplify scope src (applyRule rule context stm) of
            Just x -> Just x
            Nothing -> applyRules' rules
     in case applyRules' $ rulesForStm stm all_rules of
          Just (stms, src') -> (Just stms, src')
          Nothing -> (Nothing, src)
