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
    simplifyStms,
    simplifyFun,
    simplifyLambda,
    simplifyLambdaNoHoisting,
    bindLParams,
    simplifyBody,
    SimplifiedBody,
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

type Protect m = SubExp -> Pat (Rep m) -> Op (Rep m) -> Maybe (m ())

data SimpleOps rep = SimpleOps
  { mkExpDecS ::
      ST.SymbolTable (Wise rep) ->
      Pat (Wise rep) ->
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
    simplifyOpS :: SimplifyOp rep (Op rep)
  }

type SimplifyOp rep op = op -> SimpleM rep (OpWithWisdom op, Stms (Wise rep))

bindableSimpleOps ::
  (SimplifiableRep rep, Buildable rep) =>
  SimplifyOp rep (Op rep) ->
  SimpleOps rep
bindableSimpleOps =
  SimpleOps mkExpDecS' mkBodyS' protectHoistedOpS' (const mempty)
  where
    mkExpDecS' _ pat e = return $ mkExpDec pat e
    mkBodyS' _ bnds res = return $ mkBody bnds res
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
  SimpleM rep (a, Stms (Wise rep)) ->
  SimpleM rep (a, Stms (Wise rep))
protectIfHoisted cond side m = do
  (x, stms) <- m
  ops <- asks $ protectHoistedOpS . fst
  runBuilder $ do
    if not $ all (safeExp . stmExp) stms
      then do
        cond' <-
          if side
            then return cond
            else letSubExp "cond_neg" $ BasicOp $ UnOp Not cond
        mapM_ (protectIf ops unsafeOrCostly cond') stms
      else addStms stms
    return x
  where
    unsafeOrCostly e = not (safeExp e) || not (cheapExp e)

-- | We are willing to hoist potentially unsafe statements out of
-- loops, but they most be protected by adding a branch on top of
-- them.
protectLoopHoisted ::
  SimplifiableRep rep =>
  [(FParam (Wise rep), SubExp)] ->
  LoopForm (Wise rep) ->
  SimpleM rep (a, Stms (Wise rep)) ->
  SimpleM rep (a, Stms (Wise rep))
protectLoopHoisted merge form m = do
  (x, stms) <- m
  ops <- asks $ protectHoistedOpS . fst
  runBuilder $ do
    if not $ all (safeExp . stmExp) stms
      then do
        is_nonempty <- checkIfNonEmpty
        mapM_ (protectIf ops (not . safeExp) is_nonempty) stms
      else addStms stms
    return x
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

hoistStms ::
  SimplifiableRep rep =>
  RuleBook (Wise rep) ->
  BlockPred (Wise rep) ->
  ST.SymbolTable (Wise rep) ->
  UT.UsageTable ->
  Stms (Wise rep) ->
  SimpleM
    rep
    ( Stms (Wise rep),
      Stms (Wise rep)
    )
hoistStms rules block vtable uses orig_stms = do
  (blocked, hoisted) <- simplifyStmsBottomUp vtable uses orig_stms
  unless (null hoisted) changed
  return (stmsFromList blocked, stmsFromList hoisted)
  where
    simplifyStmsBottomUp vtable' uses' stms = do
      (_, stms') <- simplifyStmsBottomUp' vtable' uses' stms
      -- We need to do a final pass to ensure that nothing is
      -- hoisted past something that it depends on.
      let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps stms'
      return (blocked, hoisted)

    simplifyStmsBottomUp' vtable' uses' stms = do
      opUsage <- asks $ opUsageS . fst
      let usageInStm stm =
            UT.usageInStm stm
              <> case stmExp stm of
                Op op -> opUsage op
                _ -> mempty
      foldM (hoistable usageInStm) (uses', []) $ reverse $ zip (stmsToList stms) vtables
      where
        vtables = scanl (flip ST.insertStm) vtable' $ stmsToList stms

    hoistable usageInStm (uses', stms) (stm, vtable')
      | not $ any (`UT.isUsedDirectly` uses') $ provides stm -- Dead statement.
        =
        return (uses', stms)
      | otherwise = do
        res <-
          localVtable (const vtable') $
            bottomUpSimplifyStm rules (vtable', uses') stm
        case res of
          Nothing -- Nothing to optimise - see if hoistable.
            | block vtable' uses' stm ->
              return
                ( expandUsage usageInStm vtable' uses' stm
                    `UT.without` provides stm,
                  Left stm : stms
                )
            | otherwise ->
              return
                ( expandUsage usageInStm vtable' uses' stm,
                  Right stm : stms
                )
          Just optimstms -> do
            changed
            (uses'', stms') <- simplifyStmsBottomUp' vtable' uses' optimstms
            return (uses'', stms' ++ stms)

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
  UT.expand (`ST.lookupAliases` vtable) (usageInStm stm <> usageThroughAliases)
    <> ( if any (`UT.isSize` utable) (patNames pat)
           then UT.sizeUsages (freeIn e)
           else mempty
       )
    <> utable
  where
    usageThroughAliases =
      mconcat $
        mapMaybe usageThroughBindeeAliases $
          zip (patNames pat) (patAliases pat)
    usageThroughBindeeAliases (name, aliases) = do
      uses <- UT.lookup name utable
      return $ mconcat $ map (`UT.usage` uses) $ namesToList aliases

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

type SimplifiedBody rep a = ((a, UT.UsageTable), Stms (Wise rep))

blockIf ::
  SimplifiableRep rep =>
  BlockPred (Wise rep) ->
  SimpleM rep (SimplifiedBody rep a) ->
  SimpleM rep ((Stms (Wise rep), a), Stms (Wise rep))
blockIf block m = do
  ((x, usages), stms) <- m
  vtable <- askVtable
  rules <- asksEngineEnv envRules
  (blocked, hoisted) <- hoistStms rules block vtable usages stms
  return ((blocked, x), hoisted)

hasFree :: ASTRep rep => Names -> BlockPred rep
hasFree ks _ _ need = ks `namesIntersect` freeIn need

isNotSafe :: ASTRep rep => BlockPred rep
isNotSafe _ _ = not . safeExp . stmExp

isInPlaceBound :: BlockPred m
isInPlaceBound _ _ = isUpdate . stmExp
  where
    isUpdate (BasicOp Update {}) = True
    isUpdate _ = False

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
  SubExp ->
  IfSort ->
  SimplifiedBody rep Result ->
  SimplifiedBody rep Result ->
  SimpleM
    rep
    ( Body (Wise rep),
      Body (Wise rep),
      Stms (Wise rep)
    )
hoistCommon cond ifsort ((res1, usages1), stms1) ((res2, usages2), stms2) = do
  is_alloc_fun <- asksEngineEnv $ isAllocation . envHoistBlockers
  branch_blocker <- asksEngineEnv $ blockHoistBranch . envHoistBlockers
  vtable <- askVtable
  let -- We are unwilling to hoist things that are unsafe or costly,

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
      isNotHoistableBnd _ usages (Let pat _ _)
        | any (`UT.isSize` usages) $ patNames pat =
          False
      isNotHoistableBnd _ _ stm
        | is_alloc_fun stm = False
      isNotHoistableBnd _ _ _ =
        -- Hoist aggressively out of versioning branches.
        ifsort /= IfEquiv

      block =
        branch_blocker
          `orIf` ((isNotSafe `orIf` isNotCheap) `andAlso` stmIs (not . desirableToHoist))
          `orIf` isInPlaceBound
          `orIf` isNotHoistableBnd

  rules <- asksEngineEnv envRules
  (body1_bnds', safe1) <-
    protectIfHoisted cond True $
      hoistStms rules block vtable usages1 stms1
  (body2_bnds', safe2) <-
    protectIfHoisted cond False $
      hoistStms rules block vtable usages2 stms2
  let hoistable = safe1 <> safe2
  body1' <- constructBody body1_bnds' res1
  body2' <- constructBody body2_bnds' res2
  return (body1', body2', hoistable)

-- | Simplify a single body.  The @[Diet]@ only covers the value
-- elements, because the context cannot be consumed.
simplifyBody ::
  SimplifiableRep rep =>
  [Diet] ->
  Body rep ->
  SimpleM rep (SimplifiedBody rep Result)
simplifyBody ds (Body _ bnds res) =
  simplifyStms bnds $ do
    res' <- simplifyResult ds res
    return (res', mempty)

-- | Simplify a single 'Result'.  The @[Diet]@ only covers the value
-- elements, because the context cannot be consumed.
simplifyResult ::
  SimplifiableRep rep => [Diet] -> Result -> SimpleM rep (Result, UT.UsageTable)
simplifyResult ds res = do
  res' <- mapM simplify res
  vtable <- askVtable
  let consumption = consumeResult vtable $ zip ds res'
  return (res', UT.usages (freeIn res') <> consumption)

isDoLoopResult :: Result -> UT.UsageTable
isDoLoopResult = mconcat . map checkForVar
  where
    checkForVar (SubExpRes _ (Var ident)) = UT.inResultUsage ident
    checkForVar _ = mempty

simplifyStms ::
  SimplifiableRep rep =>
  Stms rep ->
  SimpleM rep (a, Stms (Wise rep)) ->
  SimpleM rep (a, Stms (Wise rep))
simplifyStms stms m =
  case stmsHead stms of
    Nothing -> inspectStms mempty m
    Just (Let pat (StmAux stm_cs attrs dec) e, stms') -> do
      stm_cs' <- simplify stm_cs
      ((e', e_stms), e_cs) <- collectCerts $ simplifyExp e
      (pat', pat_cs) <- collectCerts $ simplifyPat pat
      let cs = stm_cs' <> e_cs <> pat_cs
      inspectStms e_stms $
        inspectStm (mkWiseLetStm pat' (StmAux cs attrs dec) e') $
          simplifyStms stms' m

inspectStm ::
  SimplifiableRep rep =>
  Stm (Wise rep) ->
  SimpleM rep (a, Stms (Wise rep)) ->
  SimpleM rep (a, Stms (Wise rep))
inspectStm = inspectStms . oneStm

inspectStms ::
  SimplifiableRep rep =>
  Stms (Wise rep) ->
  SimpleM rep (a, Stms (Wise rep)) ->
  SimpleM rep (a, Stms (Wise rep))
inspectStms stms m =
  case stmsHead stms of
    Nothing -> m
    Just (stm, stms') -> do
      vtable <- askVtable
      rules <- asksEngineEnv envRules
      simplified <- topDownSimplifyStm rules vtable stm
      case simplified of
        Just newbnds -> changed >> inspectStms (newbnds <> stms') m
        Nothing -> do
          (x, stms'') <- localVtable (ST.insertStm stm) $ inspectStms stms' m
          return (x, oneStm stm <> stms'')

simplifyOp :: Op rep -> SimpleM rep (Op (Wise rep), Stms (Wise rep))
simplifyOp op = do
  f <- asks $ simplifyOpS . fst
  f op

simplifyExp ::
  SimplifiableRep rep =>
  Exp rep ->
  SimpleM rep (Exp (Wise rep), Stms (Wise rep))
simplifyExp (If cond tbranch fbranch (IfDec ts ifsort)) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We should also try to do CSE
  -- across branches.
  cond' <- simplify cond
  ts' <- mapM simplify ts
  -- FIXME: we have to be conservative about the diet here, because we
  -- lack proper ifnormation.  Something is wrong with the order in
  -- which the simplifier does things - it should be purely bottom-up
  -- (or else, If expressions should indicate explicitly the diet of
  -- their return types).
  let ds = map (const Consume) ts
  tbranch' <- simplifyBody ds tbranch
  fbranch' <- simplifyBody ds fbranch
  (tbranch'', fbranch'', hoisted) <- hoistCommon cond' ifsort tbranch' fbranch'
  return (If cond' tbranch'' fbranch'' $ IfDec ts' ifsort, hoisted)
simplifyExp (DoLoop merge form loopbody) = do
  let (params, args) = unzip merge
  params' <- mapM (traverse simplify) params
  args' <- mapM simplify args
  let merge' = zip params' args'
      diets = map (diet . paramDeclType) params'
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
  ((loopstms, loopres), hoisted) <-
    enterLoop . consumeMerge $
      bindMerge (zipWith withRes merge' (bodyResult loopbody)) $
        wrapbody $
          blockIf
            ( hasFree boundnames `orIf` isConsumed
                `orIf` seq_blocker
                `orIf` notWorthHoisting
            )
            $ do
              ((res, uses), stms) <- simplifyBody diets loopbody
              return ((res, uses <> isDoLoopResult res), stms)
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
simplifyExp (Op op) = do
  (op', stms) <- simplifyOp op
  return (Op op', stms)
simplifyExp (WithAcc inputs lam) = do
  (inputs', inputs_stms) <- fmap unzip . forM inputs $ \(shape, arrs, op) -> do
    (op', op_stms) <- case op of
      Nothing ->
        pure (Nothing, mempty)
      Just (op_lam, nes) -> do
        (op_lam', op_lam_stms) <- simplifyLambda op_lam
        nes' <- simplify nes
        return (Just (op_lam', nes'), op_lam_stms)
    (,op_stms) <$> ((,,op') <$> simplify shape <*> simplify arrs)
  (lam', lam_stms) <- simplifyLambda lam
  pure (WithAcc inputs' lam', mconcat inputs_stms <> lam_stms)

-- Special case for simplification of commutative BinOps where we
-- arrange the operands in sorted order.  This can make expressions
-- more identical, which helps CSE.
simplifyExp (BasicOp (BinOp op x y))
  | commutativeBinOp op = do
    x' <- simplify x
    y' <- simplify y
    return (BasicOp $ BinOp op (min x' y') (max x' y'), mempty)
simplifyExp e = do
  e' <- simplifyExpBase e
  return (e', mempty)

simplifyExpBase ::
  SimplifiableRep rep =>
  Exp rep ->
  SimpleM rep (Exp (Wise rep))
simplifyExpBase = mapExpM hoist
  where
    hoist =
      Mapper
        { -- Bodies are handled explicitly because we need to
          -- provide their result diet.
          mapOnBody =
            error "Unhandled body in simplification engine.",
          mapOnSubExp = simplify,
          -- Lambdas are handled explicitly because we need to
          -- bind their parameters.
          mapOnVName = simplify,
          mapOnRetType = simplify,
          mapOnBranchType = simplify,
          mapOnFParam =
            error "Unhandled FParam in simplification engine.",
          mapOnLParam =
            error "Unhandled LParam in simplification engine.",
          mapOnOp =
            error "Unhandled Op in simplification engine."
        }

type SimplifiableRep rep =
  ( ASTRep rep,
    Simplifiable (LetDec rep),
    Simplifiable (FParamInfo rep),
    Simplifiable (LParamInfo rep),
    Simplifiable (RetType rep),
    Simplifiable (BranchType rep),
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
    bnd <- ST.lookupSubExp name <$> askVtable
    case bnd of
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
  PatT dec ->
  SimpleM rep (PatT dec)
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
  Lambda rep ->
  SimpleM rep (Lambda (Wise rep), Stms (Wise rep))
simplifyLambda lam = do
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  simplifyLambdaMaybeHoist par_blocker lam

simplifyLambdaNoHoisting ::
  SimplifiableRep rep =>
  Lambda rep ->
  SimpleM rep (Lambda (Wise rep))
simplifyLambdaNoHoisting lam =
  fst <$> simplifyLambdaMaybeHoist (isFalse False) lam

simplifyLambdaMaybeHoist ::
  SimplifiableRep rep =>
  BlockPred (Wise rep) ->
  Lambda rep ->
  SimpleM rep (Lambda (Wise rep), Stms (Wise rep))
simplifyLambdaMaybeHoist blocked lam@(Lambda params body rettype) = do
  params' <- mapM (traverse simplify) params
  let paramnames = namesFromList $ boundByLambda lam
  ((lamstms, lamres), hoisted) <-
    enterLoop $
      bindLParams params' $
        blockIf (blocked `orIf` hasFree paramnames `orIf` isConsumed) $
          simplifyBody (map (const Observe) rettype) body
  body' <- constructBody lamstms lamres
  rettype' <- simplify rettype
  return (Lambda params' body' rettype', hoisted)

consumeResult :: ST.SymbolTable rep -> [(Diet, SubExpRes)] -> UT.UsageTable
consumeResult vtable = mconcat . map inspect
  where
    inspect (Consume, SubExpRes _ (Var v)) =
      mconcat $ map UT.consumedUsage $ v : namesToList (ST.lookupAliases v vtable)
    inspect _ = mempty

instance Simplifiable Certs where
  simplify (Certs ocs) = Certs . nubOrd . concat <$> mapM check ocs
    where
      check idd = do
        vv <- ST.lookupSubExp idd <$> askVtable
        case vv of
          Just (Constant _, Certs cs) -> return cs
          Just (Var idd', _) -> return [idd']
          _ -> return [idd]

insertAllStms ::
  SimplifiableRep rep =>
  SimpleM rep (SimplifiedBody rep Result) ->
  SimpleM rep (Body (Wise rep))
insertAllStms = uncurry constructBody . fst <=< blockIf (isFalse False)

simplifyFun ::
  SimplifiableRep rep =>
  FunDef rep ->
  SimpleM rep (FunDef (Wise rep))
simplifyFun (FunDef entry attrs fname rettype params body) = do
  rettype' <- simplify rettype
  params' <- mapM (traverse simplify) params
  let ds = map (diet . declExtTypeOf) rettype'
  body' <- bindFParams params $ insertAllStms $ simplifyBody ds body
  return $ FunDef entry attrs fname rettype' params' body'
