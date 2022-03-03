{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- Perform general rule-based simplification based on data dependency
-- information.  This module will:
--
--    * Perform common-subexpression elimination (CSE).
--
--    * Hoist expressions out of loops (including lambdas) and
--    branches.  This is done as aggressively as possible.
--
--    * Apply simplification rules (see
--    "Futhark.Optimise.Simplification.Rules").
--
-- If you just want to run the simplifier as simply as possible, you
-- may prefer to use the "Futhark.Optimise.Simplify" module.
module Futhark.Optimise.Simplify.Engine
  ( -- * Monadic interface
    SimpleM,
    runSimpleM,
    SimpleOps (..),
    SimplifyOp,
    bindableSimpleOps,
    Env (envHoistBlockers, envRules),
    emptyEnv,
    HoistBlockers (..),
    neverBlocks,
    noExtraHoistBlockers,
    neverHoist,
    BlockPred,
    orIf,
    hasFree,
    isConsumed,
    isFalse,
    isOp,
    isNotSafe,
    asksEngineEnv,
    askVtable,
    localVtable,

    -- * Building blocks
    SimplifiableRep,
    Simplifiable (..),
    simplifyFun,
    simplifyStms,
    simplifyStmsWithUsage,
    simplifyLambda,
    simplifyLambdaNoHoisting,
    bindLParams,
    simplifyBody,
    ST.SymbolTable,
    hoistStms,
    blockIf,
    enterLoop,
    module Futhark.Optimise.Simplify.Rep,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Either
import Data.List (find, foldl', mapAccumL)
import qualified Data.Map as M
import Data.Maybe
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Util (nubOrd)

data HoistBlockers rep = HoistBlockers
  { -- | Blocker for hoisting out of parallel loops.
    blockHoistPar :: BlockPred (Wise rep),
    -- | Blocker for hoisting out of sequential loops.
    blockHoistSeq :: BlockPred (Wise rep),
    -- | Blocker for hoisting out of branches.
    blockHoistBranch :: BlockPred (Wise rep),
    isAllocation :: Stm (Wise rep) -> Bool
  }

noExtraHoistBlockers :: HoistBlockers rep
noExtraHoistBlockers =
  HoistBlockers neverBlocks neverBlocks neverBlocks (const False)

neverHoist :: HoistBlockers rep
neverHoist =
  HoistBlockers alwaysBlocks alwaysBlocks alwaysBlocks (const False)

data Env rep = Env
  { envRules :: RuleBook (Wise rep),
    envHoistBlockers :: HoistBlockers rep,
    envVtable :: ST.SymbolTable (Wise rep)
  }

emptyEnv :: RuleBook (Wise rep) -> HoistBlockers rep -> Env rep
emptyEnv rules blockers =
  Env
    { envRules = rules,
      envHoistBlockers = blockers,
      envVtable = mempty
    }

-- | A function that protects a hoisted operation (if possible).  The
-- first operand is the condition of the 'If' we have hoisted out of
-- (or equivalently, a boolean indicating whether a loop has nonzero
-- trip count).
type Protect m = SubExp -> Pat (LetDec (Rep m)) -> Op (Rep m) -> Maybe (m ())

type SimplifyOp rep op = op -> SimpleM rep (op, Stms (Wise rep))

data SimpleOps rep = SimpleOps
  { mkExpDecS ::
      ST.SymbolTable (Wise rep) ->
      Pat (LetDec (Wise rep)) ->
      Exp (Wise rep) ->
      SimpleM rep (ExpDec (Wise rep)),
    mkBodyS ::
      ST.SymbolTable (Wise rep) ->
      Stms (Wise rep) ->
      Result ->
      SimpleM rep (Body (Wise rep)),
    -- | Make a hoisted Op safe.  The SubExp is a boolean
    -- that is true when the value of the statement will
    -- actually be used.
    protectHoistedOpS :: Protect (Builder (Wise rep)),
    opUsageS :: Op (Wise rep) -> UT.UsageTable,
    simplifyOpS :: SimplifyOp rep (Op (Wise rep))
  }

bindableSimpleOps ::
  (SimplifiableRep rep, Buildable rep) =>
  SimplifyOp rep (Op (Wise rep)) ->
  SimpleOps rep
bindableSimpleOps =
  SimpleOps mkExpDecS' mkBodyS' protectHoistedOpS' (const mempty)
  where
    mkExpDecS' _ pat e = return $ mkExpDec pat e
    mkBodyS' _ stms res = return $ mkBody stms res
    protectHoistedOpS' _ _ _ = Nothing

newtype SimpleM rep a
  = SimpleM
      ( ReaderT
          (SimpleOps rep, Env rep)
          (State (VNameSource, Bool, Certs))
          a
      )
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadReader (SimpleOps rep, Env rep),
      MonadState (VNameSource, Bool, Certs)
    )

instance MonadFreshNames (SimpleM rep) where
  putNameSource src = modify $ \(_, b, c) -> (src, b, c)
  getNameSource = gets $ \(a, _, _) -> a

instance SimplifiableRep rep => HasScope (Wise rep) (SimpleM rep) where
  askScope = ST.toScope <$> askVtable
  lookupType name = do
    vtable <- askVtable
    case ST.lookupType name vtable of
      Just t -> return t
      Nothing ->
        error $
          "SimpleM.lookupType: cannot find variable "
            ++ pretty name
            ++ " in symbol table."

instance
  SimplifiableRep rep =>
  LocalScope (Wise rep) (SimpleM rep)
  where
  localScope types = localVtable (<> ST.fromScope types)

runSimpleM ::
  SimpleM rep a ->
  SimpleOps rep ->
  Env rep ->
  VNameSource ->
  ((a, Bool), VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (src', b, _)) = runState (runReaderT m (simpl, env)) (src, False, mempty)
   in ((x, b), src')

askEngineEnv :: SimpleM rep (Env rep)
askEngineEnv = asks snd

asksEngineEnv :: (Env rep -> a) -> SimpleM rep a
asksEngineEnv f = f <$> askEngineEnv

askVtable :: SimpleM rep (ST.SymbolTable (Wise rep))
askVtable = asksEngineEnv envVtable

localVtable ::
  (ST.SymbolTable (Wise rep) -> ST.SymbolTable (Wise rep)) ->
  SimpleM rep a ->
  SimpleM rep a
localVtable f = local $ \(ops, env) -> (ops, env {envVtable = f $ envVtable env})

collectCerts :: SimpleM rep a -> SimpleM rep (a, Certs)
collectCerts m = do
  x <- m
  (a, b, cs) <- get
  put (a, b, mempty)
  return (x, cs)

-- | Mark that we have changed something and it would be a good idea
-- to re-run the simplifier.
changed :: SimpleM rep ()
changed = modify $ \(src, _, cs) -> (src, True, cs)

usedCerts :: Certs -> SimpleM rep ()
usedCerts cs = modify $ \(a, b, c) -> (a, b, cs <> c)

-- | Indicate in the symbol table that we have descended into a loop.
enterLoop :: SimpleM rep a -> SimpleM rep a
enterLoop = localVtable ST.deepen

bindFParams :: SimplifiableRep rep => [FParam (Wise rep)] -> SimpleM rep a -> SimpleM rep a
bindFParams params =
  localVtable $ ST.insertFParams params

bindLParams :: SimplifiableRep rep => [LParam (Wise rep)] -> SimpleM rep a -> SimpleM rep a
bindLParams params =
  localVtable $ \vtable -> foldr ST.insertLParam vtable params

bindArrayLParams ::
  SimplifiableRep rep =>
  [LParam (Wise rep)] ->
  SimpleM rep a ->
  SimpleM rep a
bindArrayLParams params =
  localVtable $ \vtable -> foldl' (flip ST.insertLParam) vtable params

bindMerge ::
  SimplifiableRep rep =>
  [(FParam (Wise rep), SubExp, SubExpRes)] ->
  SimpleM rep a ->
  SimpleM rep a
bindMerge = localVtable . ST.insertLoopMerge

bindLoopVar :: SimplifiableRep rep => VName -> IntType -> SubExp -> SimpleM rep a -> SimpleM rep a
bindLoopVar var it bound =
  localVtable $ ST.insertLoopVar var it bound

-- | We are willing to hoist potentially unsafe statements out of
-- branches, but they most be protected by adding a branch on top of
-- them.  (This means such hoisting is not worth it unless they are in
-- turn hoisted out of a loop somewhere.)
protectIfHoisted ::
  SimplifiableRep rep =>
  -- | Branch condition.
  SubExp ->
  -- | Which side of the branch are we
  -- protecting here?
  Bool ->
  SimpleM rep (Stms (Wise rep), a) ->
  SimpleM rep (Stms (Wise rep), a)
protectIfHoisted cond side m = do
  (hoisted, x) <- m
  ops <- asks $ protectHoistedOpS . fst
  hoisted' <- runBuilder_ $ do
    if not $ all (safeExp . stmExp) hoisted
      then do
        cond' <-
          if side
            then return cond
            else letSubExp "cond_neg" $ BasicOp $ UnOp Not cond
        mapM_ (protectIf ops unsafeOrCostly cond') hoisted
      else addStms hoisted
  pure (hoisted', x)
  where
    unsafeOrCostly e = not (safeExp e) || not (cheapExp e)

-- | We are willing to hoist potentially unsafe statements out of
-- loops, but they most be protected by adding a branch on top of
-- them.
protectLoopHoisted ::
  SimplifiableRep rep =>
  [(FParam (Wise rep), SubExp)] ->
  LoopForm (Wise rep) ->
  SimpleM rep (a, b, Stms (Wise rep)) ->
  SimpleM rep (a, b, Stms (Wise rep))
protectLoopHoisted merge form m = do
  (x, y, stms) <- m
  ops <- asks $ protectHoistedOpS . fst
  stms' <- runBuilder_ $ do
    if not $ all (safeExp . stmExp) stms
      then do
        is_nonempty <- checkIfNonEmpty
        mapM_ (protectIf ops (not . safeExp) is_nonempty) stms
      else addStms stms
  pure (x, y, stms')
  where
    checkIfNonEmpty =
      case form of
        WhileLoop cond
          | Just (_, cond_init) <-
              find ((== cond) . paramName . fst) merge ->
            return cond_init
          | otherwise -> return $ constant True -- infinite loop
        ForLoop _ it bound _ ->
          letSubExp "loop_nonempty" $
            BasicOp $ CmpOp (CmpSlt it) (intConst it 0) bound

protectIf ::
  MonadBuilder m =>
  Protect m ->
  (Exp (Rep m) -> Bool) ->
  SubExp ->
  Stm (Rep m) ->
  m ()
protectIf _ _ taken (Let pat aux (If cond taken_body untaken_body (IfDec if_ts IfFallback))) = do
  cond' <- letSubExp "protect_cond_conj" $ BasicOp $ BinOp LogAnd taken cond
  auxing aux . letBind pat $
    If cond' taken_body untaken_body $ IfDec if_ts IfFallback
protectIf _ _ taken (Let pat aux (BasicOp (Assert cond msg loc))) = do
  not_taken <- letSubExp "loop_not_taken" $ BasicOp $ UnOp Not taken
  cond' <- letSubExp "protect_assert_disj" $ BasicOp $ BinOp LogOr not_taken cond
  auxing aux $ letBind pat $ BasicOp $ Assert cond' msg loc
protectIf protect _ taken (Let pat aux (Op op))
  | Just m <- protect taken pat op =
    auxing aux m
protectIf _ f taken (Let pat aux e)
  | f e =
    case makeSafe e of
      Just e' ->
        auxing aux $ letBind pat e'
      Nothing -> do
        taken_body <- eBody [pure e]
        untaken_body <-
          eBody $ map (emptyOfType $ patNames pat) (patTypes pat)
        if_ts <- expTypesFromPat pat
        auxing aux . letBind pat $
          If taken taken_body untaken_body $ IfDec if_ts IfFallback
protectIf _ _ _ stm =
  addStm stm

makeSafe :: Exp rep -> Maybe (Exp rep)
makeSafe (BasicOp (BinOp (SDiv t _) x y)) =
  Just $ BasicOp (BinOp (SDiv t Safe) x y)
makeSafe (BasicOp (BinOp (SDivUp t _) x y)) =
  Just $ BasicOp (BinOp (SDivUp t Safe) x y)
makeSafe (BasicOp (BinOp (SQuot t _) x y)) =
  Just $ BasicOp (BinOp (SQuot t Safe) x y)
makeSafe (BasicOp (BinOp (UDiv t _) x y)) =
  Just $ BasicOp (BinOp (UDiv t Safe) x y)
makeSafe (BasicOp (BinOp (UDivUp t _) x y)) =
  Just $ BasicOp (BinOp (UDivUp t Safe) x y)
makeSafe (BasicOp (BinOp (SMod t _) x y)) =
  Just $ BasicOp (BinOp (SMod t Safe) x y)
makeSafe (BasicOp (BinOp (SRem t _) x y)) =
  Just $ BasicOp (BinOp (SRem t Safe) x y)
makeSafe (BasicOp (BinOp (UMod t _) x y)) =
  Just $ BasicOp (BinOp (UMod t Safe) x y)
makeSafe _ =
  Nothing

emptyOfType :: MonadBuilder m => [VName] -> Type -> m (Exp (Rep m))
emptyOfType _ Mem {} =
  error "emptyOfType: Cannot hoist non-existential memory."
emptyOfType _ Acc {} =
  error "emptyOfType: Cannot hoist accumulator."
emptyOfType _ (Prim pt) =
  return $ BasicOp $ SubExp $ Constant $ blankPrimValue pt
emptyOfType ctx_names (Array et shape _) = do
  let dims = map zeroIfContext $ shapeDims shape
  return $ BasicOp $ Scratch et dims
  where
    zeroIfContext (Var v) | v `elem` ctx_names = intConst Int32 0
    zeroIfContext se = se

-- | Statements that are not worth hoisting out of loops, because they
-- are unsafe, and added safety (by 'protectLoopHoisted') may inhibit
-- further optimisation..
notWorthHoisting :: ASTRep rep => BlockPred rep
notWorthHoisting _ _ (Let pat _ e) =
  not (safeExp e) && any ((> 0) . arrayRank) (patTypes pat)

-- Top-down simplify a statement (including copy propagation into the
-- pattern and such).  Does not recurse into any sub-Bodies or Ops.
nonrecSimplifyStm ::
  SimplifiableRep rep =>
  Stm (Wise rep) ->
  SimpleM rep (Stm (Wise rep))
nonrecSimplifyStm (Let pat (StmAux cs attrs (_, dec)) e) = do
  cs' <- simplify cs
  (pat', pat_cs) <- collectCerts $ simplifyPat $ removePatWisdom pat
  let aux' = StmAux (cs' <> pat_cs) attrs dec
  mkWiseLetStm pat' aux' <$> simplifyExpBase e

-- Bottom-up simplify a statement.  Recurses into sub-Bodies and Ops.
-- Does not copy-propagate into the pattern and similar, as it is
-- assumed 'nonrecSimplifyStm' has already touched it (and worst case,
-- it'll get it on the next round of the overall fixpoint iteration.)
recSimplifyStm ::
  SimplifiableRep rep =>
  Stm (Wise rep) ->
  UT.UsageTable ->
  SimpleM rep (Stms (Wise rep), Stm (Wise rep))
recSimplifyStm (Let pat (StmAux cs attrs (_, dec)) e) usage = do
  ((e', e_hoisted), e_cs) <- collectCerts $ simplifyExp usage pat e
  let aux' = StmAux (cs <> e_cs) attrs dec
  pure (e_hoisted, mkWiseLetStm (removePatWisdom pat) aux' e')

hoistStms ::
  SimplifiableRep rep =>
  RuleBook (Wise rep) ->
  BlockPred (Wise rep) ->
  Stms (Wise rep) ->
  SimpleM rep (a, UT.UsageTable) ->
  SimpleM rep (a, Stms (Wise rep), Stms (Wise rep))
hoistStms rules block orig_stms final = do
  (a, blocked, hoisted) <- simplifyStmsBottomUp orig_stms
  unless (null hoisted) changed
  pure (a, stmsFromList blocked, stmsFromList hoisted)
  where
    simplifyStmsBottomUp stms = do
      opUsage <- asks $ opUsageS . fst
      let usageInStm stm =
            UT.usageInStm stm
              <> case stmExp stm of
                Op op -> opUsage op
                _ -> mempty
      (x, _, stms') <- hoistableStms usageInStm stms
      -- We need to do a final pass to ensure that nothing is
      -- hoisted past something that it depends on.
      let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps stms'
      pure (x, blocked, hoisted)

    descend usageInStm stms m =
      case stmsHead stms of
        Nothing -> m
        Just (stms_h, stms_t) -> localVtable (ST.insertStm stms_h) $ do
          (x, usage, stms_t') <- descend usageInStm stms_t m
          process usageInStm stms_h stms_t' usage x

    process usageInStm stm stms usage x = do
      vtable <- askVtable
      res <- bottomUpSimplifyStm rules (vtable, usage) stm
      case res of
        Nothing -- Nothing to optimise - see if hoistable.
          | block vtable usage stm ->
            -- No, not hoistable.
            pure
              ( x,
                expandUsage usageInStm vtable usage stm
                  `UT.without` provides stm,
                Left stm : stms
              )
          | otherwise ->
            -- Yes, hoistable.
            pure
              ( x,
                expandUsage usageInStm vtable usage stm,
                Right stm : stms
              )
        Just optimstms -> do
          changed
          descend usageInStm optimstms $ pure (x, usage, stms)

    hoistableStms usageInStm stms =
      case stmsHead stms of
        Nothing -> do
          (x, usage) <- final
          pure (x, usage, mempty)
        Just (stms_h, stms_t) -> do
          stms_h' <- nonrecSimplifyStm stms_h

          vtable <- askVtable
          simplified <- topDownSimplifyStm rules vtable stms_h'

          case simplified of
            Just newstms -> do
              changed
              hoistableStms usageInStm (newstms <> stms_t)
            Nothing -> do
              (x, usage, stms_t') <-
                localVtable (ST.insertStm stms_h') $
                  hoistableStms usageInStm stms_t
              if not $ any (`UT.isUsedDirectly` usage) $ provides stms_h'
                then -- Dead statement.
                  pure (x, usage, stms_t')
                else do
                  (stms_h_stms, stms_h'') <- recSimplifyStm stms_h' usage
                  descend usageInStm stms_h_stms $
                    process usageInStm stms_h'' stms_t' usage x

blockUnhoistedDeps ::
  ASTRep rep =>
  [Either (Stm rep) (Stm rep)] ->
  [Either (Stm rep) (Stm rep)]
blockUnhoistedDeps = snd . mapAccumL block mempty
  where
    block blocked (Left need) =
      (blocked <> namesFromList (provides need), Left need)
    block blocked (Right need)
      | blocked `namesIntersect` freeIn need =
        (blocked <> namesFromList (provides need), Left need)
      | otherwise =
        (blocked, Right need)

provides :: Stm rep -> [VName]
provides = patNames . stmPat

expandUsage ::
  (ASTRep rep, Aliased rep) =>
  (Stm rep -> UT.UsageTable) ->
  ST.SymbolTable rep ->
  UT.UsageTable ->
  Stm rep ->
  UT.UsageTable
expandUsage usageInStm vtable utable stm@(Let pat _ e) =
  stmUsages <> utable
  where
    stmUsages =
      UT.expand (`ST.lookupAliases` vtable) (usageInStm stm <> usageThroughAliases)
        <> ( if any (`UT.isSize` utable) (patNames pat)
               then UT.sizeUsages (freeIn e)
               else mempty
           )
    usageThroughAliases =
      mconcat . mapMaybe usageThroughBindeeAliases $
        zip (patNames pat) (patAliases pat)
    usageThroughBindeeAliases (name, aliases) = do
      uses <- UT.lookup name utable
      pure . mconcat $
        map (`UT.usage` (uses `UT.withoutU` UT.presentU)) $ namesToList aliases

type BlockPred rep = ST.SymbolTable rep -> UT.UsageTable -> Stm rep -> Bool

neverBlocks :: BlockPred rep
neverBlocks _ _ _ = False

alwaysBlocks :: BlockPred rep
alwaysBlocks _ _ _ = True

isFalse :: Bool -> BlockPred rep
isFalse b _ _ _ = not b

orIf :: BlockPred rep -> BlockPred rep -> BlockPred rep
orIf p1 p2 body vtable need = p1 body vtable need || p2 body vtable need

andAlso :: BlockPred rep -> BlockPred rep -> BlockPred rep
andAlso p1 p2 body vtable need = p1 body vtable need && p2 body vtable need

isConsumed :: BlockPred rep
isConsumed _ utable = any (`UT.isConsumed` utable) . patNames . stmPat

isOp :: BlockPred rep
isOp _ _ (Let _ _ Op {}) = True
isOp _ _ _ = False

constructBody ::
  SimplifiableRep rep =>
  Stms (Wise rep) ->
  Result ->
  SimpleM rep (Body (Wise rep))
constructBody stms res =
  fmap fst . runBuilder . buildBody_ $ do
    addStms stms
    pure res

blockIf ::
  SimplifiableRep rep =>
  BlockPred (Wise rep) ->
  Stms (Wise rep) ->
  SimpleM rep (a, UT.UsageTable) ->
  SimpleM rep (a, Stms (Wise rep), Stms (Wise rep))
blockIf block stms m = do
  rules <- asksEngineEnv envRules
  hoistStms rules block stms m

hasFree :: ASTRep rep => Names -> BlockPred rep
hasFree ks _ _ need = ks `namesIntersect` freeIn need

isNotSafe :: ASTRep rep => BlockPred rep
isNotSafe _ _ = not . safeExp . stmExp

isConsuming :: Aliased rep => BlockPred rep
isConsuming _ _ = isUpdate . stmExp
  where
    isUpdate e = consumedInExp e /= mempty

isNotCheap :: ASTRep rep => BlockPred rep
isNotCheap _ _ = not . cheapStm

cheapStm :: ASTRep rep => Stm rep -> Bool
cheapStm = cheapExp . stmExp

cheapExp :: ASTRep rep => Exp rep -> Bool
cheapExp (BasicOp BinOp {}) = True
cheapExp (BasicOp SubExp {}) = True
cheapExp (BasicOp UnOp {}) = True
cheapExp (BasicOp CmpOp {}) = True
cheapExp (BasicOp ConvOp {}) = True
cheapExp (BasicOp Copy {}) = False
cheapExp (BasicOp Replicate {}) = False
cheapExp (BasicOp Manifest {}) = False
cheapExp DoLoop {} = False
cheapExp (If _ tbranch fbranch _) =
  all cheapStm (bodyStms tbranch)
    && all cheapStm (bodyStms fbranch)
cheapExp (Op op) = cheapOp op
cheapExp _ = True -- Used to be False, but
-- let's try it out.

stmIs :: (Stm rep -> Bool) -> BlockPred rep
stmIs f _ _ = f

loopInvariantStm :: ASTRep rep => ST.SymbolTable rep -> Stm rep -> Bool
loopInvariantStm vtable =
  all (`nameIn` ST.availableAtClosestLoop vtable) . namesToList . freeIn

hoistCommon ::
  SimplifiableRep rep =>
  UT.UsageTable ->
  [UT.Usages] ->
  SubExp ->
  IfSort ->
  Body (Wise rep) ->
  Body (Wise rep) ->
  SimpleM
    rep
    ( Body (Wise rep),
      Body (Wise rep),
      Stms (Wise rep)
    )
hoistCommon res_usage res_usages cond ifsort body1 body2 = do
  is_alloc_fun <- asksEngineEnv $ isAllocation . envHoistBlockers
  branch_blocker <- asksEngineEnv $ blockHoistBranch . envHoistBlockers
  vtable <- askVtable
  let -- We are unwilling to hoist things that are unsafe or costly,
      -- except if they are invariant to the most enclosing loop,
      -- because in that case they will also be hoisted past that
      -- loop.
      --
      -- We also try very hard to hoist allocations or anything that
      -- contributes to memory or array size, because that will allow
      -- allocations to be hoisted.
      cond_loop_invariant =
        all (`nameIn` ST.availableAtClosestLoop vtable) $ namesToList $ freeIn cond

      desirableToHoist stm =
        is_alloc_fun stm
          || ( ST.loopDepth vtable > 0
                 && cond_loop_invariant
                 && ifsort /= IfFallback
                 && loopInvariantStm vtable stm
             )

      -- No matter what, we always want to hoist constants as much as
      -- possible.
      isNotHoistableBnd _ _ (Let _ _ (BasicOp ArrayLit {})) = False
      isNotHoistableBnd _ _ (Let _ _ (BasicOp SubExp {})) = False
      isNotHoistableBnd _ usage (Let pat _ _)
        | any (`UT.isSize` usage) $ patNames pat =
          False
      isNotHoistableBnd _ _ stm
        | is_alloc_fun stm = False
      isNotHoistableBnd _ _ _ =
        -- Hoist aggressively out of versioning branches.
        ifsort /= IfEquiv

      block =
        branch_blocker
          `orIf` ((isNotSafe `orIf` isNotCheap) `andAlso` stmIs (not . desirableToHoist))
          `orIf` isConsuming
          `orIf` isNotHoistableBnd

  (hoisted1, body1') <-
    protectIfHoisted cond True $
      simplifyBody block res_usage res_usages body1
  (hoisted2, body2') <-
    protectIfHoisted cond False $
      simplifyBody block res_usage res_usages body2
  return (body1', body2', hoisted1 <> hoisted2)

-- | Simplify a single body.
simplifyBody ::
  SimplifiableRep rep =>
  BlockPred (Wise rep) ->
  UT.UsageTable ->
  [UT.Usages] ->
  Body (Wise rep) ->
  SimpleM rep (Stms (Wise rep), Body (Wise rep))
simplifyBody blocker usage res_usages (Body _ stms res) = do
  (res', stms', hoisted) <-
    blockIf blocker stms $ do
      (res', res_usage) <- simplifyResult res_usages res
      pure (res', res_usage <> usage)
  body' <- constructBody stms' res'
  pure (hoisted, body')

-- | Simplify a single body.
simplifyBodyNoHoisting ::
  SimplifiableRep rep =>
  UT.UsageTable ->
  [UT.Usages] ->
  Body (Wise rep) ->
  SimpleM rep (Body (Wise rep))
simplifyBodyNoHoisting usage res_usages body =
  snd <$> simplifyBody (isFalse False) usage res_usages body

usageFromDiet :: Diet -> UT.Usages
usageFromDiet Consume = UT.consumedU
usageFromDiet _ = mempty

-- | Simplify a single 'Result'.
simplifyResult ::
  SimplifiableRep rep => [UT.Usages] -> Result -> SimpleM rep (Result, UT.UsageTable)
simplifyResult usages res = do
  res' <- mapM simplify res
  vtable <- askVtable
  let more_usages = mconcat $ do
        (u, Var v) <- zip usages $ map resSubExp res
        let als_usages =
              map
                (`UT.usage` (u `UT.withoutU` UT.presentU))
                (namesToList (ST.lookupAliases v vtable))
        UT.usage v u : als_usages
  return (res', UT.usages (freeIn res') <> more_usages)

isDoLoopResult :: Result -> UT.UsageTable
isDoLoopResult = mconcat . map checkForVar
  where
    checkForVar (SubExpRes _ (Var ident)) = UT.inResultUsage ident
    checkForVar _ = mempty

simplifyStms ::
  SimplifiableRep rep =>
  Stms (Wise rep) ->
  SimpleM rep (Stms (Wise rep))
simplifyStms stms = do
  simplifyStmsWithUsage all_used stms
  where
    all_used =
      UT.usages (namesFromList (M.keys (scopeOf stms)))

simplifyStmsWithUsage ::
  SimplifiableRep rep =>
  UT.UsageTable ->
  Stms (Wise rep) ->
  SimpleM rep (Stms (Wise rep))
simplifyStmsWithUsage usage stms = do
  ((), stms', _) <- blockIf (isFalse False) stms $ pure ((), usage)
  pure stms'

simplifyOp :: Op (Wise rep) -> SimpleM rep (Op (Wise rep), Stms (Wise rep))
simplifyOp op = do
  f <- asks $ simplifyOpS . fst
  f op

simplifyExp ::
  SimplifiableRep rep =>
  UT.UsageTable ->
  Pat (LetDec (Wise rep)) ->
  Exp (Wise rep) ->
  SimpleM rep (Exp (Wise rep), Stms (Wise rep))
simplifyExp usage (Pat pes) (If cond tbranch fbranch (IfDec ts ifsort)) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We should also try to do CSE
  -- across branches.
  let pes_usages = map (fromMaybe mempty . (`UT.lookup` usage) . patElemName) pes
  cond' <- simplify cond
  ts' <- mapM simplify ts
  (tbranch', fbranch', hoisted) <-
    hoistCommon usage pes_usages cond' ifsort tbranch fbranch
  return (If cond' tbranch' fbranch' $ IfDec ts' ifsort, hoisted)
simplifyExp _ _ (DoLoop merge form loopbody) = do
  let (params, args) = unzip merge
  params' <- mapM (traverse simplify) params
  args' <- mapM simplify args
  let merge' = zip params' args'
  (form', boundnames, wrapbody) <- case form of
    ForLoop loopvar it boundexp loopvars -> do
      boundexp' <- simplify boundexp
      let (loop_params, loop_arrs) = unzip loopvars
      loop_params' <- mapM (traverse simplify) loop_params
      loop_arrs' <- mapM simplify loop_arrs
      let form' = ForLoop loopvar it boundexp' (zip loop_params' loop_arrs')
      return
        ( form',
          namesFromList (loopvar : map paramName loop_params') <> fparamnames,
          bindLoopVar loopvar it boundexp'
            . protectLoopHoisted merge' form'
            . bindArrayLParams loop_params'
        )
    WhileLoop cond -> do
      cond' <- simplify cond
      return
        ( WhileLoop cond',
          fparamnames,
          protectLoopHoisted merge' (WhileLoop cond')
        )
  seq_blocker <- asksEngineEnv $ blockHoistSeq . envHoistBlockers
  (loopres, loopstms, hoisted) <-
    enterLoop . consumeMerge $
      bindMerge (zipWith withRes merge' (bodyResult loopbody)) . wrapbody $
        blockIf
          ( hasFree boundnames `orIf` isConsumed
              `orIf` seq_blocker
              `orIf` notWorthHoisting
          )
          (bodyStms loopbody)
          $ do
            let params_usages =
                  map
                    (\p -> if unique (paramDeclType p) then UT.consumedU else mempty)
                    params'
            (res, uses) <- simplifyResult params_usages $ bodyResult loopbody
            pure (res, uses <> isDoLoopResult res)
  loopbody' <- constructBody loopstms loopres
  return (DoLoop merge' form' loopbody', hoisted)
  where
    fparamnames =
      namesFromList (map (paramName . fst) merge)
    consumeMerge =
      localVtable $ flip (foldl' (flip ST.consume)) $ namesToList consumed_by_merge
    consumed_by_merge =
      freeIn $ map snd $ filter (unique . paramDeclType . fst) merge
    withRes (p, x) y = (p, x, y)
simplifyExp _ _ (Op op) = do
  (op', stms) <- simplifyOp op
  return (Op op', stms)
simplifyExp usage _ (WithAcc inputs lam) = do
  (inputs', inputs_stms) <- fmap unzip . forM inputs $ \(shape, arrs, op) -> do
    (op', op_stms) <- case op of
      Nothing ->
        pure (Nothing, mempty)
      Just (op_lam, nes) -> do
        (op_lam', op_lam_stms) <- simplifyLambda op_lam
        nes' <- simplify nes
        return (Just (op_lam', nes'), op_lam_stms)
    (,op_stms) <$> ((,,op') <$> simplify shape <*> simplify arrs)
  let noteAcc = ST.noteAccTokens (zip (map paramName (lambdaParams lam)) inputs')
  (lam', lam_stms) <- simplifyLambdaWith noteAcc (isFalse True) usage lam
  pure (WithAcc inputs' lam', mconcat inputs_stms <> lam_stms)
simplifyExp _ _ e = do
  e' <- simplifyExpBase e
  pure (e', mempty)

-- The simple nonrecursive case that we can perform without bottom-up
-- information.
simplifyExpBase :: SimplifiableRep rep => Exp (Wise rep) -> SimpleM rep (Exp (Wise rep))
-- Special case for simplification of commutative BinOps where we
-- arrange the operands in sorted order.  This can make expressions
-- more identical, which helps CSE.
simplifyExpBase (BasicOp (BinOp op x y))
  | commutativeBinOp op = do
    x' <- simplify x
    y' <- simplify y
    pure $ BasicOp $ BinOp op (min x' y') (max x' y')
simplifyExpBase e = mapExpM hoist e
  where
    hoist =
      identityMapper
        { mapOnSubExp = simplify,
          mapOnVName = simplify,
          mapOnRetType = simplify,
          mapOnBranchType = simplify
        }

type SimplifiableRep rep =
  ( ASTRep rep,
    Simplifiable (LetDec rep),
    Simplifiable (FParamInfo rep),
    Simplifiable (LParamInfo rep),
    Simplifiable (RetType rep),
    Simplifiable (BranchType rep),
    TraverseOpStms (Wise rep),
    CanBeWise (Op rep),
    ST.IndexOp (OpWithWisdom (Op rep)),
    BuilderOps (Wise rep),
    IsOp (Op rep)
  )

class Simplifiable e where
  simplify :: SimplifiableRep rep => e -> SimpleM rep e

instance (Simplifiable a, Simplifiable b) => Simplifiable (a, b) where
  simplify (x, y) = (,) <$> simplify x <*> simplify y

instance
  (Simplifiable a, Simplifiable b, Simplifiable c) =>
  Simplifiable (a, b, c)
  where
  simplify (x, y, z) = (,,) <$> simplify x <*> simplify y <*> simplify z

-- Convenient for Scatter.
instance Simplifiable Int where
  simplify = pure

instance Simplifiable a => Simplifiable (Maybe a) where
  simplify Nothing = return Nothing
  simplify (Just x) = Just <$> simplify x

instance Simplifiable a => Simplifiable [a] where
  simplify = mapM simplify

instance Simplifiable SubExp where
  simplify (Var name) = do
    stm <- ST.lookupSubExp name <$> askVtable
    case stm of
      Just (Constant v, cs) -> do
        changed
        usedCerts cs
        return $ Constant v
      Just (Var id', cs) -> do
        changed
        usedCerts cs
        return $ Var id'
      _ -> return $ Var name
  simplify (Constant v) =
    return $ Constant v

instance Simplifiable SubExpRes where
  simplify (SubExpRes cs se) = do
    cs' <- simplify cs
    (se', se_cs) <- collectCerts $ simplify se
    pure $ SubExpRes (se_cs <> cs') se'

simplifyPat ::
  (SimplifiableRep rep, Simplifiable dec) =>
  Pat dec ->
  SimpleM rep (Pat dec)
simplifyPat (Pat xs) =
  Pat <$> mapM inspect xs
  where
    inspect (PatElem name rep) = PatElem name <$> simplify rep

instance Simplifiable () where
  simplify = pure

instance Simplifiable VName where
  simplify v = do
    se <- ST.lookupSubExp v <$> askVtable
    case se of
      Just (Var v', cs) -> do
        changed
        usedCerts cs
        return v'
      _ -> return v

instance Simplifiable d => Simplifiable (ShapeBase d) where
  simplify = fmap Shape . simplify . shapeDims

instance Simplifiable ExtSize where
  simplify (Free se) = Free <$> simplify se
  simplify (Ext x) = return $ Ext x

instance Simplifiable Space where
  simplify (ScalarSpace ds t) = ScalarSpace <$> simplify ds <*> pure t
  simplify s = pure s

instance Simplifiable PrimType where
  simplify = pure

instance Simplifiable shape => Simplifiable (TypeBase shape u) where
  simplify (Array et shape u) =
    Array <$> simplify et <*> simplify shape <*> pure u
  simplify (Acc acc ispace ts u) =
    Acc <$> simplify acc <*> simplify ispace <*> simplify ts <*> pure u
  simplify (Mem space) =
    Mem <$> simplify space
  simplify (Prim bt) =
    return $ Prim bt

instance Simplifiable d => Simplifiable (DimIndex d) where
  simplify (DimFix i) = DimFix <$> simplify i
  simplify (DimSlice i n s) = DimSlice <$> simplify i <*> simplify n <*> simplify s

instance Simplifiable d => Simplifiable (Slice d) where
  simplify = traverse simplify

simplifyLambda ::
  SimplifiableRep rep =>
  Lambda (Wise rep) ->
  SimpleM rep (Lambda (Wise rep), Stms (Wise rep))
simplifyLambda lam = do
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  simplifyLambdaMaybeHoist par_blocker mempty lam

simplifyLambdaNoHoisting ::
  SimplifiableRep rep =>
  Lambda (Wise rep) ->
  SimpleM rep (Lambda (Wise rep))
simplifyLambdaNoHoisting lam =
  fst <$> simplifyLambdaMaybeHoist (isFalse False) mempty lam

simplifyLambdaMaybeHoist ::
  SimplifiableRep rep =>
  BlockPred (Wise rep) ->
  UT.UsageTable ->
  Lambda (Wise rep) ->
  SimpleM rep (Lambda (Wise rep), Stms (Wise rep))
simplifyLambdaMaybeHoist = simplifyLambdaWith id

simplifyLambdaWith ::
  SimplifiableRep rep =>
  (ST.SymbolTable (Wise rep) -> ST.SymbolTable (Wise rep)) ->
  BlockPred (Wise rep) ->
  UT.UsageTable ->
  Lambda (Wise rep) ->
  SimpleM rep (Lambda (Wise rep), Stms (Wise rep))
simplifyLambdaWith f blocked usage lam@(Lambda params body rettype) = do
  params' <- mapM (traverse simplify) params
  let paramnames = namesFromList $ boundByLambda lam
  (hoisted, body') <-
    bindLParams params' . localVtable f $
      simplifyBody
        (blocked `orIf` hasFree paramnames `orIf` isConsumed)
        usage
        (map (const mempty) rettype)
        body
  rettype' <- simplify rettype
  return (Lambda params' body' rettype', hoisted)

instance Simplifiable Certs where
  simplify (Certs ocs) = Certs . nubOrd . concat <$> mapM check ocs
    where
      check idd = do
        vv <- ST.lookupSubExp idd <$> askVtable
        case vv of
          Just (Constant _, Certs cs) -> return cs
          Just (Var idd', _) -> return [idd']
          _ -> return [idd]

simplifyFun ::
  SimplifiableRep rep =>
  FunDef (Wise rep) ->
  SimpleM rep (FunDef (Wise rep))
simplifyFun (FunDef entry attrs fname rettype params body) = do
  rettype' <- simplify rettype
  params' <- mapM (traverse simplify) params
  let usages = map (usageFromDiet . diet . declExtTypeOf) rettype'
  body' <- bindFParams params $ simplifyBodyNoHoisting mempty usages body
  pure $ FunDef entry attrs fname rettype' params' body'
