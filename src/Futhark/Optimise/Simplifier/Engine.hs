{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
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
-- may prefer to use the "Futhark.Optimise.Simplifier" module.
--
module Futhark.Optimise.Simplifier.Engine
       ( -- * Monadic interface
         SimpleM
       , runSimpleM
       , subSimpleM
       , SimpleOps (..)
       , SimplifyOp
       , bindableSimpleOps

       , Env (envHoistBlockers, envRules)
       , emptyEnv
       , HoistBlockers(..)
       , neverBlocks
       , noExtraHoistBlockers
       , BlockPred
       , orIf
       , hasFree
       , isConsumed
       , isFalse
       , isOp
       , isNotSafe
       , asksEngineEnv
       , changed
       , askVtable
       , localVtable

         -- * Building blocks
       , SimplifiableLore
       , Simplifiable (..)
       , simplifyStms
       , simplifyFun
       , simplifyLambda
       , simplifyLambdaSeq
       , simplifyLambdaNoHoisting
       , simplifyExtLambda
       , simplifyParam
       , bindLParams
       , bindChunkLParams
       , bindLoopVar
       , enterLoop
       , simplifyBody
       , SimplifiedBody

       , blockIf
       , constructBody

       , module Futhark.Optimise.Simplifier.Lore
       ) where

import Control.Monad.Writer
import Control.Monad.RWS.Strict
import Data.Either
import Data.Hashable
import Data.List
import Data.Maybe
import qualified Data.Set as S

import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Usage
import Futhark.Construct
import Futhark.Optimise.Simplifier.Lore
import qualified Futhark.Analysis.ScalExp as SE

data HoistBlockers lore = HoistBlockers
                          { blockHoistPar :: BlockPred (Wise lore)
                            -- ^ Blocker for hoisting out of parallel loops.
                          , blockHoistSeq :: BlockPred (Wise lore)
                            -- ^ Blocker for hoisting out of sequential loops.
                          , getArraySizes :: Stm (Wise lore) -> Names
                            -- ^ gets the sizes of arrays from a binding.
                          , isAllocation  :: Stm (Wise lore) -> Bool
                          }

noExtraHoistBlockers :: HoistBlockers lore
noExtraHoistBlockers = HoistBlockers neverBlocks neverBlocks (const S.empty) (const False)

data Env lore = Env { envRules         :: RuleBook (Wise lore)
                    , envHoistBlockers :: HoistBlockers lore
                    , envVtable        :: ST.SymbolTable (Wise lore)
                    }

emptyEnv :: RuleBook (Wise lore) -> HoistBlockers lore -> Env lore
emptyEnv rules blockers =
  Env { envRules = rules
      , envHoistBlockers = blockers
      , envVtable = mempty
      }

data SimpleOps lore =
  SimpleOps { mkExpAttrS :: ST.SymbolTable (Wise lore)
                         -> Pattern (Wise lore) -> Exp (Wise lore)
                         -> SimpleM lore (ExpAttr (Wise lore))
            , mkBodyS :: ST.SymbolTable (Wise lore)
                      -> Stms (Wise lore) -> Result
                      -> SimpleM lore (Body (Wise lore))
            , mkLetNamesS :: ST.SymbolTable (Wise lore)
                          -> [(VName,Bindage)] -> Exp (Wise lore)
                          -> SimpleM lore (Stm (Wise lore), Stms (Wise lore))
            , simplifyOpS :: SimplifyOp lore
            }

type SimplifyOp lore = Op lore -> SimpleM lore (OpWithWisdom (Op lore), Stms (Wise lore))

bindableSimpleOps :: (SimplifiableLore lore, Bindable lore) =>
                     SimplifyOp lore -> SimpleOps lore
bindableSimpleOps = SimpleOps mkExpAttrS' mkBodyS' mkLetNamesS'
  where mkExpAttrS' _ pat e = return $ mkExpAttr pat e
        mkBodyS' _ bnds res = return $ mkBody bnds res
        mkLetNamesS' _ name e = (,) <$> mkLetNames name e <*> pure mempty

newtype SimpleM lore a =
  SimpleM (RWS (SimpleOps lore, Env lore) Certificates (VNameSource, Bool) a)
  deriving (Applicative, Functor, Monad,
            MonadReader (SimpleOps lore, Env lore),
            MonadState (VNameSource, Bool),
            MonadWriter Certificates)

instance MonadFreshNames (SimpleM lore) where
  putNameSource src = modify $ \(_, b) -> (src, b)
  getNameSource = gets fst

instance SimplifiableLore lore => HasScope (Wise lore) (SimpleM lore) where
  askScope = ST.toScope <$> askVtable
  lookupType name = do
    vtable <- askVtable
    case ST.lookupType name vtable of
      Just t -> return t
      Nothing -> fail $
                 "SimpleM.lookupType: cannot find variable " ++
                 pretty name ++ " in symbol table."

instance SimplifiableLore lore =>
         LocalScope (Wise lore) (SimpleM lore) where
  localScope types = localVtable (<>ST.fromScope types)

runSimpleM :: SimpleM lore a
           -> SimpleOps lore
           -> Env lore
           -> VNameSource
           -> ((a, Bool), VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (src', b), _) = runRWS m (simpl, env) (src, False)
  in ((x, b), src')

subSimpleM :: (SimplifiableLore lore,
               MonadFreshNames m,
               SameScope outerlore lore,
               ExpAttr outerlore ~ ExpAttr lore,
               BodyAttr outerlore ~ BodyAttr lore,
               RetType outerlore ~ RetType lore,
               BranchType outerlore ~ BranchType lore) =>
              SimpleOps lore
           -> Env lore
           -> ST.SymbolTable (Wise outerlore)
           -> SimpleM lore a
           -> m (a, Bool)
subSimpleM simpl env outer_vtable m = do
  let inner_vtable = ST.castSymbolTable outer_vtable
  src <- getNameSource
  let SimpleM m' = localVtable (<>inner_vtable) m
      (x, (src', b), _) = runRWS m' (simpl, env) (src, False)
  putNameSource src'
  return (x, b)

askEngineEnv :: SimpleM lore (Env lore)
askEngineEnv = snd <$> ask

asksEngineEnv :: (Env lore -> a) -> SimpleM lore a
asksEngineEnv f = f <$> askEngineEnv

askVtable :: SimpleM lore (ST.SymbolTable (Wise lore))
askVtable = asksEngineEnv envVtable

localVtable :: SimplifiableLore lore =>
               (ST.SymbolTable (Wise lore) -> ST.SymbolTable (Wise lore))
            -> SimpleM lore a -> SimpleM lore a
localVtable f = local $ \(ops, env) -> (ops, env { envVtable = f $ envVtable env })

collectCerts :: SimplifiableLore lore => SimpleM lore a -> SimpleM lore (a, Certificates)
collectCerts m = pass $ do (x, cs) <- listen m
                           return ((x, cs), const mempty)

-- | Mark that we have changed something and it would be a good idea
-- to re-run the simplifier.
changed :: SimpleM lore ()
changed = modify $ \(src, _) -> (src, True)

usedCerts :: Certificates -> SimpleM lore ()
usedCerts = tell

enterLoop :: SimplifiableLore lore => SimpleM lore a -> SimpleM lore a
enterLoop = localVtable ST.deepen

bindFParams :: SimplifiableLore lore =>
               [FParam (Wise lore)] -> SimpleM lore a -> SimpleM lore a
bindFParams params =
  localVtable $ ST.insertFParams params

bindLParams :: SimplifiableLore lore =>
               [LParam (Wise lore)] -> SimpleM lore a -> SimpleM lore a
bindLParams params =
  localVtable $ \vtable ->
    foldr ST.insertLParam vtable params

bindArrayLParams :: SimplifiableLore lore =>
                    [(LParam (Wise lore),Maybe VName)] -> SimpleM lore a -> SimpleM lore a
bindArrayLParams params =
  localVtable $ \vtable ->
    foldr (uncurry ST.insertArrayLParam) vtable params

bindChunkLParams :: SimplifiableLore lore =>
                    VName -> [(LParam (Wise lore),VName)] -> SimpleM lore a -> SimpleM lore a
bindChunkLParams offset params =
  localVtable $ \vtable ->
    foldr (uncurry $ ST.insertChunkLParam offset) vtable params

bindLoopVar :: SimplifiableLore lore =>
               VName -> IntType -> SubExp -> SimpleM lore a -> SimpleM lore a
bindLoopVar var it bound =
  localVtable $ clampUpper . clampVar
  where clampVar = ST.insertLoopVar var it bound
        -- If we enter the loop, then 'bound' is at least one.
        clampUpper = case bound of Var v -> ST.isAtLeast v 1
                                   _     -> id

-- | We are willing to hoist potentially unsafe statements out of
-- loops, but they most be protected by adding a branch on top of
-- them.
protectLoopHoisted :: SimplifiableLore lore =>
                      [(FParam (Wise lore),SubExp)]
                   -> [(FParam (Wise lore),SubExp)]
                   -> LoopForm (Wise lore)
                   -> SimpleM lore (a, Stms (Wise lore))
                   -> SimpleM lore (a, Stms (Wise lore))
protectLoopHoisted ctx val form m = do
  (x, stms) <- m
  runBinder $ do
    if any (not . safeExp . stmExp) stms
      then do is_nonempty <- checkIfNonEmpty
              mapM_ (protectUnsafe is_nonempty) stms
      else addStms stms
    return x
  where checkIfNonEmpty =
          case form of
            WhileLoop cond
              | Just (_, cond_init) <-
                  find ((==cond) . paramName . fst) $ ctx ++ val ->
                    return cond_init
              | otherwise -> return $ constant True -- infinite loop
            ForLoop _ it bound _ ->
              letSubExp "loop_nonempty" $
              BasicOp $ CmpOp (CmpSlt it) (intConst it 0) bound

        protectUnsafe is_nonempty (Let pat (StmAux cs _) e)
          | not $ safeExp e = do
              nonempty_body <- eBody [pure e]
              empty_body <- eBody $ map (emptyOfType $ patternContextNames pat)
                                        (patternValueTypes pat)
              if_ts <- expTypesFromPattern pat
              certifying cs $
                letBind_ pat $ If is_nonempty nonempty_body empty_body $
                IfAttr if_ts IfFallback
        protectUnsafe _ stm =
          addStm stm

        emptyOfType _ Mem{} =
          fail "protectLoopHoisted: Cannot hoist non-existential memory."
        emptyOfType _ (Prim pt) =
          return $ BasicOp $ SubExp $ Constant $ blankPrimValue pt
        emptyOfType ctx_names (Array pt shape _) = do
          let dims = map zeroIfContext $ shapeDims shape
          return $ BasicOp $ Scratch pt dims
          where zeroIfContext (Var v) | v `elem` ctx_names = intConst Int32 0
                zeroIfContext se = se

-- | Statements that are not worth hoisting out of loops, because they
-- are unsafe, and added safety (by 'protectLoopHoisted') may inhibit
-- further optimisation..
notWorthHoisting :: Attributes lore => BlockPred lore
notWorthHoisting _ (Let pat _ e) =
  not (safeExp e) && any (>0) (map arrayRank $ patternTypes pat)

hoistStms :: SimplifiableLore lore =>
             RuleBook (Wise lore) -> BlockPred (Wise lore)
          -> ST.SymbolTable (Wise lore) -> UT.UsageTable
          -> Stms (Wise lore)
          -> SimpleM lore (Stms (Wise lore),
                           Stms (Wise lore))
hoistStms rules block vtable uses orig_stms = do
  (blocked, hoisted) <- simplifyStmsBottomUp vtable uses orig_stms
  unless (null hoisted) changed
  return (stmsFromList blocked, stmsFromList hoisted)
  where simplifyStmsBottomUp vtable' uses' stms = do
          (_, stms') <- simplifyStmsBottomUp' vtable' uses' stms
          -- We need to do a final pass to ensure that nothing is
          -- hoisted past something that it depends on.
          let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps stms'
          return (blocked, hoisted)

        simplifyStmsBottomUp' vtable' uses' stms =
          foldM hoistable (uses',[]) $ reverse $ zip (stmsToList stms) vtables
            where vtables = scanl (flip ST.insertStm) vtable' $ stmsToList stms

        hoistable (uses',stms) (stm, vtable')
          | not $ uses' `UT.contains` provides stm = -- Dead binding.
            return (uses', stms)
          | otherwise = do
            res <- localVtable (const vtable') $
                   bottomUpSimplifyStm rules (vtable', uses') stm
            case res of
              Nothing -- Nothing to optimise - see if hoistable.
                | block uses' stm ->
                  return (expandUsage uses' stm `UT.without` provides stm,
                          Left stm : stms)
                | otherwise ->
                  return (expandUsage uses' stm, Right stm : stms)
              Just optimstms -> do
                changed
                (uses'',stms') <- simplifyStmsBottomUp' vtable' uses' optimstms
                return (uses'', stms'++stms)

blockUnhoistedDeps :: Attributes lore =>
                      [Either (Stm lore) (Stm lore)]
                   -> [Either (Stm lore) (Stm lore)]
blockUnhoistedDeps = snd . mapAccumL block S.empty
  where block blocked (Left need) =
          (blocked <> S.fromList (provides need), Left need)
        block blocked (Right need)
          | blocked `intersects` requires need =
            (blocked <> S.fromList (provides need), Left need)
          | otherwise =
            (blocked, Right need)

provides :: Stm lore -> [VName]
provides = patternNames . stmPattern

requires :: Attributes lore => Stm lore -> Names
requires = freeInStm

expandUsage :: (Attributes lore, Aliased lore, UsageInOp (Op lore)) =>
               UT.UsageTable -> Stm lore -> UT.UsageTable
expandUsage utable bnd = utable <> usageInStm bnd <> usageThroughAliases
  where pat = stmPattern bnd
        usageThroughAliases =
          mconcat $ mapMaybe usageThroughBindeeAliases $
          zip (patternNames pat) (patternAliases pat)
        usageThroughBindeeAliases (name, aliases) = do
          uses <- UT.lookup name utable
          return $ mconcat $ map (`UT.usage` uses) $ S.toList aliases

intersects :: (Ord a, Hashable a) => S.Set a -> S.Set a -> Bool
intersects a b = not $ S.null $ a `S.intersection` b

type BlockPred lore = UT.UsageTable -> Stm lore -> Bool

neverBlocks :: BlockPred lore
neverBlocks _ _ = False

isFalse :: Bool -> BlockPred lore
isFalse b _ _ = not b

orIf :: BlockPred lore -> BlockPred lore -> BlockPred lore
orIf p1 p2 body need = p1 body need || p2 body need

isConsumed :: BlockPred lore
isConsumed utable = any (`UT.isConsumed` utable) . patternNames . stmPattern

isOp :: BlockPred lore
isOp _ (Let _ _ Op{}) = True
isOp _ _ = False

constructBody :: SimplifiableLore lore => Stms (Wise lore) -> Result
              -> SimpleM lore (Body (Wise lore))
constructBody stms res =
  fmap fst $ runBinder $ insertStmsM $ do addStms stms
                                          resultBodyM res

type SimplifiedBody lore a = ((a, UT.UsageTable), Stms (Wise lore))

blockIf :: SimplifiableLore lore =>
           BlockPred (Wise lore)
        -> SimpleM lore (SimplifiedBody lore a)
        -> SimpleM lore ((Stms (Wise lore), a), Stms (Wise lore))
blockIf block m = do
  ((x, usages), stms) <- m
  vtable <- askVtable
  rules <- asksEngineEnv envRules
  (blocked, hoisted) <- hoistStms rules block vtable usages stms
  return ((blocked, x), hoisted)

insertAllStms :: SimplifiableLore lore =>
                 SimpleM lore (SimplifiedBody lore Result)
              -> SimpleM lore (Body (Wise lore))
insertAllStms = uncurry constructBody . fst <=< blockIf (isFalse False)

hasFree :: Attributes lore => Names -> BlockPred lore
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: Attributes lore => BlockPred lore
isNotSafe _ = not . safeExp . stmExp

isInPlaceBound :: BlockPred m
isInPlaceBound _ = not . all ((==BindVar) . patElemBindage) .
                   patternElements . stmPattern

isNotCheap :: Attributes lore => BlockPred lore
isNotCheap _ = not . cheapStm
  where cheapStm = cheap . stmExp
        cheap (BasicOp BinOp{})        = True
        cheap (BasicOp SubExp{})       = True
        cheap (BasicOp UnOp{})         = True
        cheap (BasicOp CmpOp{})        = True
        cheap (BasicOp ConvOp{})       = True
        cheap (BasicOp Copy{})         = False
        cheap DoLoop{}                 = False
        cheap (If _ tbranch fbranch _) = all cheapStm (bodyStms tbranch) &&
                                         all cheapStm (bodyStms fbranch)
        cheap (Op op)                  = cheapOp op
        cheap _                        = True -- Used to be False, but
                                              -- let's try it out.
hoistCommon :: SimplifiableLore lore =>
               SimplifiedBody lore Result
            -> SimplifiedBody lore Result
            -> SimpleM lore (Body (Wise lore), Body (Wise lore), Stms (Wise lore))
hoistCommon ((res1, usages1), stms1) ((res2, usages2), stms2) = do
  is_alloc_fun <- asksEngineEnv $ isAllocation  . envHoistBlockers
  getArrSz_fun <- asksEngineEnv $ getArraySizes . envHoistBlockers
  let hoistbl_nms = filterBnds is_alloc_fun getArrSz_fun $
                    stmsToList $ stms1<>stms2
      -- "isNotHoistableBnd hoistbl_nms" ensures that only the (transitive closure)
      -- of the bindings used for allocations and shape computations are if-hoistable.
      block = isNotSafe `orIf` isNotCheap `orIf` isInPlaceBound `orIf`
              isNotHoistableBnd hoistbl_nms
  vtable <- askVtable
  rules <- asksEngineEnv envRules
  (body1_bnds', safe1) <- hoistStms rules block vtable usages1 stms1
  (body2_bnds', safe2) <- hoistStms rules block vtable usages2 stms2
  let hoistable = safe1 <> safe2
  body1' <- constructBody body1_bnds' res1
  body2' <- constructBody body2_bnds' res2
  return (body1', body2', hoistable)
  where filterBnds is_alloc_fn getArrSz_fn all_bnds =
          let sz_nms     = mconcat $ map getArrSz_fn all_bnds
              sz_needs   = transClosSizes all_bnds sz_nms []
              alloc_bnds = filter is_alloc_fn all_bnds
              sel_nms    = S.fromList $
                           concatMap (patternNames . stmPattern)
                                     (sz_needs ++ alloc_bnds)
          in  sel_nms
        transClosSizes all_bnds scal_nms hoist_bnds =
          let new_bnds = filter (hasPatName scal_nms) all_bnds
              new_nms  = mconcat $ map (freeInExp . stmExp) new_bnds
          in  if null new_bnds
              then hoist_bnds
              else transClosSizes all_bnds new_nms (new_bnds ++ hoist_bnds)
        hasPatName nms bnd = intersects nms $ S.fromList $
                             patternNames $ stmPattern bnd
        isNotHoistableBnd :: Names -> BlockPred m
        isNotHoistableBnd nms _ bnd = not $ hasPatName nms bnd

-- | Simplify a single 'Body'.
simplifyBody :: SimplifiableLore lore =>
                [Diet] -> Body lore -> SimpleM lore (SimplifiedBody lore Result)
simplifyBody ds (Body _ bnds res) =
  simplifyStms bnds $ do res' <- simplifyResult ds res
                         return (res', mempty)

-- | Simplify a single 'Result'.
simplifyResult :: SimplifiableLore lore =>
                  [Diet] -> Result -> SimpleM lore (Result, UT.UsageTable)
simplifyResult ds es = do
  -- Use a special copy propagation function to avoid copy propagation
  -- when certificates are involved - there is nowhere to put them!
  es' <- mapM simplify' es
  let usages = consumeResult $ zip ds es'
  return (es', usages)
  where simplify' (Var name) = do
          bnd <- ST.lookupSubExp name <$> askVtable
          case bnd of
            Just (Constant v, cs)
              | cs == mempty -> return $ Constant v
            Just (Var id', cs)
              | cs == mempty -> return $ Var id'
            _                -> return $ Var name
        simplify' (Constant v) =
          return $ Constant v

isDoLoopResult :: Result -> UT.UsageTable
isDoLoopResult = mconcat . map checkForVar
  where checkForVar (Var ident) = UT.inResultUsage ident
        checkForVar _           = mempty

simplifyStms :: SimplifiableLore lore =>
                Stms lore -> SimpleM lore (a, Stms (Wise lore))
             -> SimpleM lore (a, Stms (Wise lore))
simplifyStms stms m =
  case stmsHead stms of
    Nothing -> inspectStms mempty m
    Just (Let pat (StmAux stm_cs attr) e, stms') -> do
      stm_cs' <- simplify stm_cs
      ((e', e_stms), e_cs) <- collectCerts $ simplifyExp e
      (pat', pat_cs) <- collectCerts $ simplifyPattern pat
      let cs = stm_cs'<>e_cs<>pat_cs
      inspectStms e_stms $
        inspectStm (mkWiseLetStm pat' (StmAux cs attr) e') $
        simplifyStms stms' m

inspectStm :: SimplifiableLore lore =>
              Stm (Wise lore) -> SimpleM lore (a, Stms (Wise lore))
           -> SimpleM lore (a, Stms (Wise lore))
inspectStm = inspectStms . oneStm

inspectStms :: SimplifiableLore lore =>
               Stms (Wise lore)
            -> SimpleM lore (a, Stms (Wise lore))
            -> SimpleM lore (a, Stms (Wise lore))
inspectStms stms m =
  case stmsHead stms of
    Nothing -> m
    Just (stm, stms') -> do
      vtable <- askVtable
      rules <- asksEngineEnv envRules
      simplified <- topDownSimplifyStm rules vtable stm
      case simplified of
        Just newbnds -> changed >> inspectStms (newbnds <> stms') m
        Nothing      -> do (x, stms'') <- localVtable (ST.insertStm stm) $ inspectStms stms' m
                           return (x, oneStm stm <> stms'')

simplifyOp :: Op lore -> SimpleM lore (Op (Wise lore), Stms (Wise lore))
simplifyOp op = do f <- asks $ simplifyOpS . fst
                   f op

simplifyExp :: SimplifiableLore lore =>
               Exp lore -> SimpleM lore (Exp (Wise lore), Stms (Wise lore))

simplifyExp (If cond tbranch fbranch (IfAttr ts ifsort)) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We should also try to do CSE
  -- across branches.
  cond' <- simplify cond
  ts' <- mapM simplify ts
  let ds = map (const Observe) (bodyResult tbranch)
  tbranch' <- localVtable (ST.updateBounds True cond) $ simplifyBody ds tbranch
  fbranch' <- localVtable (ST.updateBounds False cond) $ simplifyBody ds fbranch
  (tbranch'',fbranch'', hoisted) <- hoistCommon tbranch' fbranch'
  return (If cond' tbranch'' fbranch'' $ IfAttr ts' ifsort, hoisted)

simplifyExp (DoLoop ctx val form loopbody) = do
  let (ctxparams, ctxinit) = unzip ctx
      (valparams, valinit) = unzip val
  ctxparams' <- mapM (simplifyParam simplify) ctxparams
  ctxinit' <- mapM simplify ctxinit
  valparams' <- mapM (simplifyParam simplify) valparams
  valinit' <- mapM simplify valinit
  let ctx' = zip ctxparams' ctxinit'
      val' = zip valparams' valinit'
      diets = map (diet . paramDeclType) $ ctxparams' ++ valparams'
  (form', boundnames, wrapbody) <- case form of
    ForLoop loopvar it boundexp loopvars -> do
      boundexp' <- simplify boundexp
      let (loop_params, loop_arrs) = unzip loopvars
      loop_params' <- mapM (simplifyParam simplify) loop_params
      loop_arrs' <- mapM simplify loop_arrs
      let form' = ForLoop loopvar it boundexp' (zip loop_params' loop_arrs')
      return (form',
              S.fromList (loopvar : map paramName loop_params') <> fparamnames,
              bindLoopVar loopvar it boundexp' .
              protectLoopHoisted ctx' val' form' .
              bindArrayLParams (zip loop_params' (map Just loop_arrs')))
    WhileLoop cond -> do
      cond' <- simplify cond
      return (WhileLoop cond',
              fparamnames,
              protectLoopHoisted ctx' val' (WhileLoop cond'))
  seq_blocker <- asksEngineEnv $ blockHoistSeq . envHoistBlockers
  ((loopstms, loopres), hoisted) <-
    enterLoop $
    bindFParams (ctxparams'++valparams') $ wrapbody $
    blockIf
    (hasFree boundnames `orIf` isConsumed
     `orIf` seq_blocker `orIf` notWorthHoisting) $ do
      ((res, uses), stms) <- simplifyBody diets loopbody
      return ((res, uses <> isDoLoopResult res), stms)
  loopbody' <- constructBody loopstms loopres
  return (DoLoop ctx' val' form' loopbody', hoisted)
  where fparamnames = S.fromList (map (paramName . fst) $ ctx++val)

simplifyExp (Op op) = do (op', stms) <- simplifyOp op
                         return (Op op', stms)

simplifyExp e = do e' <- simplifyExpBase e
                   return (e', mempty)

simplifyExpBase :: SimplifiableLore lore =>
                   Exp lore -> SimpleM lore (Exp (Wise lore))
simplifyExpBase = mapExpM hoist
  where hoist = Mapper {
                -- Bodies are handled explicitly because we need to
                -- provide their result diet.
                  mapOnBody = fail "Unhandled body in simplification engine."
                , mapOnSubExp = simplify
                -- Lambdas are handled explicitly because we need to
                -- bind their parameters.
                , mapOnVName = simplify
                , mapOnCertificates = simplify
                , mapOnRetType = simplify
                , mapOnBranchType = simplify
                , mapOnFParam =
                  fail "Unhandled FParam in simplification engine."
                , mapOnLParam =
                  fail "Unhandled LParam in simplification engine."
                , mapOnOp =
                  fail "Unhandled Op in simplification engine."
                }

type SimplifiableLore lore = (Attributes lore,
                              Simplifiable (LetAttr lore),
                              Simplifiable (FParamAttr lore),
                              Simplifiable (LParamAttr lore),
                              Simplifiable (RetType lore),
                              Simplifiable (BranchType lore),
                              CanBeWise (Op lore),
                              ST.IndexOp (OpWithWisdom (Op lore)),
                              BinderOps (Wise lore),
                              IsOp (Op lore))

class Simplifiable e where
  simplify :: SimplifiableLore lore => e -> SimpleM lore e

instance (Simplifiable a, Simplifiable b) => Simplifiable (a, b) where
  simplify (x,y) = do
    x' <- simplify x
    y' <- simplify y
    return (x', y')

instance Simplifiable a => Simplifiable (Maybe a) where
  simplify Nothing = return Nothing
  simplify (Just x) = Just <$> simplify x

instance Simplifiable a => Simplifiable [a] where
  simplify = mapM simplify

instance Simplifiable SubExp where
  simplify (Var name) = do
    bnd <- ST.lookupSubExp name <$> askVtable
    case bnd of
      Just (Constant v, cs) -> do changed
                                  usedCerts cs
                                  return $ Constant v
      Just (Var id', cs) -> do changed
                               usedCerts cs
                               return $ Var id'
      _              -> return $ Var name
  simplify (Constant v) =
    return $ Constant v

simplifyPattern :: (SimplifiableLore lore, Simplifiable attr) =>
                   PatternT attr
                -> SimpleM lore (PatternT attr)
simplifyPattern pat =
  Pattern <$>
  mapM inspect (patternContextElements pat) <*>
  mapM inspect (patternValueElements pat)
  where inspect (PatElem name bindage lore) = do
          bindage' <- simplify bindage
          lore'  <- simplify lore
          return $ PatElem name bindage' lore'

instance Simplifiable Bindage where
  simplify BindVar =
    return BindVar
  simplify (BindInPlace src is) =
    BindInPlace <$> simplify src <*> simplify is

simplifyParam :: (attr -> SimpleM lore attr) -> ParamT attr -> SimpleM lore (ParamT attr)
simplifyParam simplifyAttribute (Param name attr) = do
  attr' <- simplifyAttribute attr
  return $ Param name attr'

instance Simplifiable VName where
  simplify v = do
    se <- ST.lookupSubExp v <$> askVtable
    case se of
      Just (Var v', cs) -> do changed
                              usedCerts cs
                              return v'
      _             -> return v

instance Simplifiable d => Simplifiable (ShapeBase d) where
  simplify = fmap Shape . simplify . shapeDims

instance Simplifiable ExtSize where
  simplify (Free se) = Free <$> simplify se
  simplify (Ext x)   = return $ Ext x

instance Simplifiable shape => Simplifiable (TypeBase shape u) where
  simplify (Array et shape u) = do
    shape' <- simplify shape
    return $ Array et shape' u
  simplify (Mem size space) =
    Mem <$> simplify size <*> pure space
  simplify (Prim bt) =
    return $ Prim bt

instance Simplifiable d => Simplifiable (DimIndex d) where
  simplify (DimFix i)       = DimFix <$> simplify i
  simplify (DimSlice i n s) = DimSlice <$> simplify i <*> simplify n <*> simplify s

simplifyLambda :: SimplifiableLore lore =>
                  Lambda lore
               -> [Maybe VName]
               -> SimpleM lore (Lambda (Wise lore), Stms (Wise lore))
simplifyLambda lam arrs = do
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  simplifyLambdaMaybeHoist par_blocker lam arrs

simplifyLambdaSeq :: SimplifiableLore lore =>
                     Lambda lore
                  -> [Maybe VName]
                  -> SimpleM lore (Lambda (Wise lore), Stms (Wise lore))
simplifyLambdaSeq = simplifyLambdaMaybeHoist neverBlocks

simplifyLambdaNoHoisting :: SimplifiableLore lore =>
                            Lambda lore
                         -> [Maybe VName]
                         -> SimpleM lore (Lambda (Wise lore))
simplifyLambdaNoHoisting lam arr =
  fst <$> simplifyLambdaMaybeHoist (isFalse False) lam arr

simplifyLambdaMaybeHoist :: SimplifiableLore lore =>
                            BlockPred (Wise lore) -> Lambda lore
                         -> [Maybe VName]
                         -> SimpleM lore (Lambda (Wise lore), Stms (Wise lore))
simplifyLambdaMaybeHoist blocked lam@(Lambda params body rettype) arrs = do
  params' <- mapM (simplifyParam simplify) params
  let (nonarrayparams, arrayparams) =
        splitAt (length params' - length arrs) params'
      paramnames = S.fromList $ boundByLambda lam
  ((lamstms, lamres), hoisted) <-
    enterLoop $
    bindLParams nonarrayparams $
    bindArrayLParams (zip arrayparams arrs) $
    blockIf (blocked `orIf` hasFree paramnames `orIf` isConsumed) $
      simplifyBody (map (const Observe) rettype) body
  body' <- constructBody lamstms lamres
  rettype' <- simplify rettype
  return (Lambda params' body' rettype', hoisted)

simplifyExtLambda :: SimplifiableLore lore =>
                     ExtLambda lore
                  -> [SubExp]
                  -> [(LParam (Wise lore), SE.ScalExp, SE.ScalExp)]
                  -> SimpleM lore (ExtLambda (Wise lore), UT.UsageTable, Stms (Wise lore))
simplifyExtLambda lam@(ExtLambda params body rettype) nes parbnds = do
  params' <- mapM (simplifyParam simplify) params
  let paramnames = S.fromList $ boundByExtLambda lam
  rettype' <- simplify rettype
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  ((lamstms, lamres), hoisted) <-
    enterLoop $
    bindLParams params' $
    localVtable extendSymTab $
    blockIf (hasFree paramnames `orIf` isConsumed `orIf` par_blocker) $
    simplifyBody (map (const Observe) rettype) body
  body' <- constructBody lamstms lamres

  let consumed_in_body = consumedInBody body'
      paramWasConsumed p (Var arr)
        | p `S.member` consumed_in_body = UT.consumedUsage arr
      paramWasConsumed _ _              = mempty
      accparams = take (length nes) $ drop 1 params
      usage = mconcat $ zipWith paramWasConsumed (map paramName accparams) nes
  return (ExtLambda params' body' rettype', usage, hoisted)
  where extendSymTab vtb =
          foldl (\ vt (i,l,u) ->
                   let i_name = paramName i
                   in  ST.setUpperBound i_name u $
                       ST.setLowerBound i_name l vt
                ) vtb parbnds

consumeResult :: [(Diet, SubExp)] -> UT.UsageTable
consumeResult = mconcat . map inspect
  where inspect (Consume, se) =
          mconcat $ map UT.consumedUsage $ S.toList $ subExpAliases se
        inspect (Observe, se) = UT.usages $ freeIn se

instance Simplifiable Certificates where
  simplify (Certificates ocs) = Certificates . nub . concat <$> mapM check ocs
    where check idd = do
            vv <- ST.lookupSubExp idd <$> askVtable
            case vv of
              Just (Constant Checked, Certificates cs) -> return cs
              Just (Var idd', _) -> return [idd']
              _ -> return [idd]

simplifyFun :: SimplifiableLore lore => FunDef lore -> SimpleM lore (FunDef (Wise lore))
simplifyFun (FunDef entry fname rettype params body) = do
  rettype' <- simplify rettype
  let ds = replicate (length (bodyResult body) - length rettype) Observe ++
           map diet (retTypeValues rettype')
  body' <- bindFParams params $ insertAllStms $ simplifyBody ds body
  return $ FunDef entry fname rettype' params body'
