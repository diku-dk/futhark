{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
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

       , addStmEngine
       , collectStmsEngine
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
       , State
       , emptyState
       , Need (needStms)
       , asksEngineEnv
       , getVtable
       , localVtable
       , insertAllStms
       , simplifyBody
       , inspectStm

         -- * Building blocks
       , SimplifiableLore
       , Simplifiable (..)
       , simplifyStm
       , simplifyResult
       , simplifyExp
       , simplifyPattern
       , simplifyFun
       , simplifyLambda
       , simplifyLambdaSeq
       , simplifyLambdaNoHoisting
       , simplifyExtLambda
       , simplifyParam
       , bindLParams
       , bindArrayLParams
       , bindLoopVar
       , enterLoop
       , consumedName

       , blockIf

       , tapUsage

       , module Futhark.Optimise.Simplifier.Lore
       ) where

import Control.Applicative
import Control.Monad.Writer

import Data.Either
import Data.Hashable
import Data.List
import Data.Maybe

import qualified Data.HashSet as HS
import Data.Foldable (traverse_)

import Prelude

import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Usage
import Futhark.Construct
import Futhark.MonadFreshNames
import Control.Monad.RWS
import Futhark.Optimise.Simplifier.Lore
import qualified Futhark.Analysis.ScalExp as SE

type NeedSet lore = [Stm lore]

data Need lore = Need { needStms :: NeedSet lore
                      , usageTable  :: UT.UsageTable
                      }

instance Monoid (Need lore) where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] UT.empty

data HoistBlockers m = HoistBlockers
                       { blockHoistPar :: BlockPred (Lore m)
                         -- ^ Blocker for hoisting out of parallel loops.
                       , blockHoistSeq :: BlockPred (Lore m)
                         -- ^ Blocker for hoisting out of sequential loops.
                       , getArraySizes :: Stm (Lore m) -> Names
                         -- ^ gets the sizes of arrays from a binding.
                       , isAllocation  :: Stm (Lore m) -> Bool
                       }

noExtraHoistBlockers :: HoistBlockers m
noExtraHoistBlockers = HoistBlockers neverBlocks neverBlocks (const HS.empty) (const False)

data Env m = Env { envRules         :: RuleBook m
                 , envHoistBlockers :: HoistBlockers m
                 }

emptyEnv :: RuleBook (SimpleM lore)
         -> HoistBlockers (SimpleM lore)
         -> Env (SimpleM lore)
emptyEnv rules blockers =
  Env { envRules = rules
      , envHoistBlockers = blockers
      }

data State m = State { stateVtable :: ST.SymbolTable (Lore m)
                     , stateChanged :: Bool -- Should we run again?
                     }

emptyState :: State m
emptyState = State { stateVtable = ST.empty
                   , stateChanged = False
                   }

data SimpleOps lore =
  SimpleOps { mkLetS :: ST.SymbolTable (Wise lore)
                     -> Pattern (Wise lore) -> Exp (Wise lore)
                     -> SimpleM lore (Stm (Wise lore))
            , mkBodyS :: ST.SymbolTable (Wise lore)
                      -> [Stm (Wise lore)] -> Result
                      -> SimpleM lore (Body (Wise lore))
            , mkLetNamesS :: ST.SymbolTable (Wise lore)
                          -> [(VName,Bindage)] -> Exp (Wise lore)
                          -> SimpleM lore (Stm (Wise lore))
            , simplifyOpS :: SimplifyOp lore
            }

type SimplifyOp lore = Op lore -> SimpleM lore (OpWithWisdom (Op lore))

bindableSimpleOps :: (SimplifiableLore lore, Bindable lore) =>
                     SimplifyOp lore -> SimpleOps lore
bindableSimpleOps = SimpleOps mkLetS' mkBodyS' mkLetNamesS'
  where mkLetS' _ pat e = return $
                          mkLet (map asPair $ patternContextElements pat)
                          (map asPair $ patternValueElements pat)
                          e
          where asPair patElem = (patElemIdent patElem, patElemBindage patElem)
        mkBodyS' _ bnds res = return $ mkBody bnds res
        mkLetNamesS' _ = mkLetNames

newtype SimpleM lore a =
  SimpleM (RWS
           (SimpleOps lore, Env (SimpleM lore)) -- Reader
           (Need (Wise lore))                             -- Writer
           (State (SimpleM lore), VNameSource)       -- State
           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Need (Wise lore)),
            MonadReader (SimpleOps lore, Env (SimpleM lore)),
            MonadState (State (SimpleM lore), VNameSource))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance SimplifiableLore lore => HasScope (Wise lore) (SimpleM lore) where
  askScope = ST.typeEnv <$> getVtable
  lookupType name = do
    vtable <- getVtable
    case ST.lookupType name vtable of
      Just t -> return t
      Nothing -> fail $
                 "SimpleM.lookupType: cannot find variable " ++
                 pretty name ++ " in symbol table."

instance SimplifiableLore lore =>
         LocalScope (Wise lore) (SimpleM lore) where
  localScope types = localVtable (<>ST.fromScope types)

instance SimplifiableLore lore => MonadBinder (SimpleM lore) where
  type Lore (SimpleM lore) = Wise lore
  mkLetM pat e = do
    vtable <- getVtable
    simpl <- fst <$> ask
    mkLetS simpl vtable pat e
  mkBodyM bnds res = do
    vtable <- getVtable
    simpl <- fst <$> ask
    mkBodyS simpl vtable bnds res
  mkLetNamesM names e = do
    vtable <- getVtable
    simpl <- fst <$> ask
    mkLetNamesS simpl vtable names e

  addStm      = addStmEngine
  collectStms = collectStmsEngine

runSimpleM :: SimpleM lore a
           -> SimpleOps lore
           -> Env (SimpleM lore)
           -> VNameSource
           -> ((a, Bool), VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (s, src'), _) = runRWS m (simpl, env) (emptyState, src)
  in ((x, stateChanged s), src')

subSimpleM :: (SimplifiableLore lore,
               MonadFreshNames m,
               SameScope outerlore lore,
               ExpAttr outerlore ~ ExpAttr lore,
               BodyAttr outerlore ~ BodyAttr lore,
               RetType outerlore ~ RetType lore) =>
              SimpleOps lore
           -> Env (SimpleM lore)
           -> ST.SymbolTable (Wise outerlore)
           -> SimpleM lore a
           -> m (a, [Stm (Wise lore)])
subSimpleM simpl env outer_vtable m = do
  let inner_vtable = ST.castSymbolTable outer_vtable
  modifyNameSource $ \src ->
    let SimpleM m' = localVtable (<>inner_vtable) m
        (x, (_, src'), need) =
          runRWS m' (simpl, env) (emptyState, src)
    in ((x, needStms need), src')

askEngineEnv :: SimpleM lore (Env (SimpleM lore))
askEngineEnv = snd <$> ask
tellNeed :: Need (Wise lore) -> SimpleM lore ()
tellNeed = tell
listenNeed :: SimpleM lore a -> SimpleM lore (a, Need (Wise lore))
listenNeed = listen
getEngineState :: SimpleM lore (State (SimpleM lore))
getEngineState   = fst <$> get
putEngineState :: State (SimpleM lore) -> SimpleM lore ()
putEngineState x = modify $ \(_, y) -> (x,y)
passNeed :: SimpleM lore (a, Need (Wise lore) -> Need (Wise lore)) -> SimpleM lore a
passNeed = pass

addStmEngine :: SimplifiableLore lore => Stm (Wise lore) -> SimpleM lore ()
addStmEngine bnd = do
  modifyVtable $ ST.insertStm bnd
  case bindingExp bnd of
    BasicOp (Assert se _) -> asserted se
    _                    -> return ()
  needStm bnd

collectStmsEngine :: SimpleM lore a -> SimpleM lore (a, [Stm (Wise lore)])
collectStmsEngine m = passNeed $ do
  (x, need) <- listenNeed m
  return ((x, needStms need),
          const mempty)

asksEngineEnv :: (Env (SimpleM lore) -> a) -> SimpleM lore a
asksEngineEnv f = f <$> askEngineEnv

getsEngineState :: (State (SimpleM lore) -> a) -> SimpleM lore a
getsEngineState f = f <$> getEngineState

modifyEngineState :: (State (SimpleM lore) -> State (SimpleM lore)) -> SimpleM lore ()
modifyEngineState f = do x <- getEngineState
                         putEngineState $ f x

changed :: SimpleM lore ()
changed = modifyEngineState $ \s -> s { stateChanged = True }


needStm :: Stm (Wise lore) -> SimpleM lore ()
needStm bnd = tellNeed $ Need [bnd] UT.empty

boundFree :: Names -> SimpleM lore ()
boundFree fs = tellNeed $ Need [] $ UT.usages fs

usedName :: VName -> SimpleM lore ()
usedName = boundFree . HS.singleton

-- | Register the fact that the given name is consumed.
consumedName :: VName -> SimpleM lore ()
consumedName = tellNeed . Need [] . UT.consumedUsage

inResultName :: VName -> SimpleM lore ()
inResultName = tellNeed . Need [] . UT.inResultUsage

asserted :: SubExp -> SimpleM lore ()
asserted Constant{} =
  return ()
asserted (Var name) = do
  se <- ST.lookupExp name <$> getVtable
  case se of Just (BasicOp (CmpOp CmpEq{} x y)) -> do
               case x of Var xvar ->
                           tellNeed $ Need [] $
                           UT.equalToUsage xvar y
                         _ -> return ()
               case y of Var yvar ->
                           tellNeed $ Need [] $
                           UT.equalToUsage yvar x
                         _ -> return ()
             _ -> return ()

tapUsage :: SimpleM lore a -> SimpleM lore (a, UT.UsageTable)
tapUsage m = do (x,needs) <- listenNeed m
                return (x, usageTable needs)

censorUsage :: (UT.UsageTable -> UT.UsageTable)
            -> SimpleM lore a -> SimpleM lore a
censorUsage f m = passNeed $ do
  x <- m
  return (x, \acc -> acc { usageTable = f $ usageTable acc })

getVtable :: SimpleM lore (ST.SymbolTable (Wise lore))
getVtable = getsEngineState stateVtable

putVtable :: ST.SymbolTable (Wise lore) -> SimpleM lore ()
putVtable vtable = modifyEngineState $ \s -> s { stateVtable = vtable }

modifyVtable :: (ST.SymbolTable (Wise lore) -> ST.SymbolTable (Wise lore))
             -> SimpleM lore ()
modifyVtable f = do vtable <- getVtable
                    putVtable $ f vtable

localVtable :: SimplifiableLore lore =>
               (ST.SymbolTable (Wise lore) -> ST.SymbolTable (Wise lore))
            -> SimpleM lore a -> SimpleM lore a
localVtable f m = do
  vtable <- getVtable
  modifyEngineState $ \env -> env { stateVtable = f vtable }
  (x, need) <- listenNeed m
  let vtable' = foldl (flip ST.insertStm) vtable $ needStms need
  modifyEngineState $ \env -> env { stateVtable = vtable' }
  return x

enterLoop :: SimplifiableLore lore => SimpleM lore a -> SimpleM lore a
enterLoop = enterBody . localVtable ST.deepen

enterBody :: SimplifiableLore lore => SimpleM lore a -> SimpleM lore a
enterBody = censorUsage UT.leftScope

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

bindLoopVar :: SimplifiableLore lore =>
               VName -> IntType -> SubExp -> SimpleM lore a -> SimpleM lore a
bindLoopVar var it bound =
  localVtable $ clampUpper . clampVar
  where clampVar = ST.insertLoopVar var it bound
        -- If we enter the loop, then 'bound' is at least one.
        clampUpper = case bound of Var v -> ST.isAtLeast v 1
                                   _     -> id

hoistStms :: SimplifiableLore lore =>
             RuleBook (SimpleM lore) -> BlockPred (Wise lore)
          -> ST.SymbolTable (Wise lore) -> UT.UsageTable
          -> [Stm (Wise lore)]
          -> SimpleM lore ([Stm (Wise lore)],
                            [Stm (Wise lore)],
                            UT.UsageTable)
hoistStms rules block vtable uses needs = do
  (uses', blocked, hoisted) <- simplifyStms vtable uses needs
  mapM_ addStm blocked
  return (blocked, hoisted, uses')
  where simplifyStms vtable' uses' bnds = do
          (uses'', bnds') <- simplifyStms' vtable' uses' bnds
          -- We need to do a final pass to ensure that nothing is
          -- hoisted past something that it depends on.
          let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps bnds'
          return (uses'', blocked, hoisted)

        simplifyStms' vtable' uses' bnds =
          foldM hoistable (uses',[]) $ reverse $ zip bnds vtables
            where vtables = scanl (flip ST.insertStm) vtable' bnds

        hoistable (uses',bnds) (bnd, vtable')
          | not $ uses' `UT.contains` provides bnd = -- Dead binding.
            return (uses', bnds)
          | otherwise = do
            res <- localVtable (const vtable') $
                   bottomUpSimplifyStm rules (vtable', uses') bnd
            case res of
              Nothing -- Nothing to optimise - see if hoistable.
                | block uses' bnd ->
                  return (expandUsage uses' bnd `UT.without` provides bnd,
                          Left bnd : bnds)
                | otherwise ->
                  return (expandUsage uses' bnd, Right bnd : bnds)
              Just optimbnds -> do
                changed
                (uses'',bnds') <- simplifyStms' vtable' uses' optimbnds
                return (uses'', bnds'++bnds)

blockUnhoistedDeps :: Attributes lore =>
                      [Either (Stm lore) (Stm lore)]
                   -> [Either (Stm lore) (Stm lore)]
blockUnhoistedDeps = snd . mapAccumL block HS.empty
  where block blocked (Left need) =
          (blocked <> HS.fromList (provides need), Left need)
        block blocked (Right need)
          | blocked `intersects` requires need =
            (blocked <> HS.fromList (provides need), Left need)
          | otherwise =
            (blocked, Right need)

provides :: Attributes lore => Stm lore -> [VName]
provides = patternNames . bindingPattern

requires :: Attributes lore => Stm lore -> Names
requires bnd =
  (mconcat (map freeIn $ patternElements $ bindingPattern bnd)
  `HS.difference` HS.fromList (provides bnd)) <>
  freeInExp (bindingExp bnd)

expandUsage :: (Attributes lore, Aliased lore, UsageInOp (Op lore)) =>
               UT.UsageTable -> Stm lore -> UT.UsageTable
expandUsage utable bnd = utable <> usageInStm bnd <> usageThroughAliases
  where pat = bindingPattern bnd
        usageThroughAliases =
          mconcat $ mapMaybe usageThroughBindeeAliases $
          zip (patternNames pat) (patternAliases pat)
        usageThroughBindeeAliases (name, aliases) = do
          uses <- UT.lookup name utable
          return $ mconcat $ map (`UT.usage` uses) $ HS.toList aliases

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

type BlockPred lore = UT.UsageTable -> Stm lore -> Bool

neverBlocks :: BlockPred lore
neverBlocks _ _ = False

isFalse :: Bool -> BlockPred lore
isFalse b _ _ = not b

orIf :: BlockPred lore -> BlockPred lore -> BlockPred lore
orIf p1 p2 body need = p1 body need || p2 body need

isConsumed :: BlockPred lore
isConsumed utable = any (`UT.isConsumed` utable) . patternNames . bindingPattern

isOp :: BlockPred lore
isOp _ (Let _ _ Op{}) = True
isOp _ _ = False

blockIf :: SimplifiableLore lore =>
           BlockPred (Wise lore)
        -> SimpleM lore a -> SimpleM lore (a, [Stm (Wise lore)])
blockIf block m = passNeed $ do
  (x, needs) <- listenNeed m
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (hoisted, hoistable, usages) <-
    hoistStms rules block vtable (usageTable needs) (needStms needs)
  putVtable $ foldl (flip ST.insertStm) vtable hoistable
  return ((x, hoisted),
          const Need { needStms = hoistable
                     , usageTable  = usages
                     })

insertAllStms :: SimplifiableLore lore =>
                 SimpleM lore Result -> SimpleM lore (Body (Wise lore))
insertAllStms = uncurry (flip mkBodyM) <=< blockIf (isFalse False)

hasFree :: Attributes lore => Names -> BlockPred lore
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: Attributes lore => BlockPred lore
isNotSafe _ = not . safeExp . bindingExp

isInPlaceBound :: BlockPred m
isInPlaceBound _ = not . all ((==BindVar) . patElemBindage) .
                   patternElements . bindingPattern

isNotCheap :: BlockPred m
isNotCheap _ = not . cheapBnd
  where cheapBnd = cheap . bindingExp
        cheap (BasicOp BinOp{})   = True
        cheap (BasicOp SubExp{})  = True
        cheap (BasicOp UnOp{})    = True
        cheap (BasicOp CmpOp{})   = True
        cheap (BasicOp ConvOp{})  = True
        cheap DoLoop{}           = False
        cheap _                  = True -- Used to be False, but
                                        -- let's try it out.
hoistCommon :: SimplifiableLore lore =>
               SimpleM lore Result
            -> (ST.SymbolTable (Wise lore)
                -> ST.SymbolTable (Wise lore))
            -> SimpleM lore Result
            -> (ST.SymbolTable (Wise lore)
                -> ST.SymbolTable (Wise lore))
            -> SimpleM lore (Body (Wise lore), Body (Wise lore))
hoistCommon m1 vtablef1 m2 vtablef2 = passNeed $ do
  (res1, needs1) <- listenNeed $ localVtable vtablef1 m1
  (res2, needs2) <- listenNeed $ localVtable vtablef2 m2
  is_alloc_fun <- asksEngineEnv $ isAllocation  . envHoistBlockers
  getArrSz_fun <- asksEngineEnv $ getArraySizes . envHoistBlockers
  let needs1_bnds = needStms needs1
      needs2_bnds = needStms needs2
      hoistbl_nms = filterBnds is_alloc_fun getArrSz_fun (needs1_bnds++needs2_bnds)
      -- "isNotHoistableBnd hoistbl_nms" ensures that only the (transitive closure)
      -- of the bindings used for allocations and shape computations are if-hoistable.
      block = isNotSafe `orIf` isNotCheap `orIf` isInPlaceBound `orIf`
                isNotHoistableBnd hoistbl_nms
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (body1_bnds', safe1, f1) <-
    enterBody $
    localVtable vtablef1 $
    hoistStms rules block vtable (usageTable needs1) needs1_bnds
  (body2_bnds', safe2, f2) <-
    enterBody $
    localVtable vtablef2 $
    hoistStms rules block vtable (usageTable needs2) needs2_bnds
  let hoistable = safe1 <> safe2
  putVtable $ foldl (flip ST.insertStm) vtable hoistable
  body1' <- mkBodyM body1_bnds' res1
  body2' <- mkBodyM body2_bnds' res2
  return ((body1', body2'),
          const Need { needStms = hoistable
                     , usageTable = f1 <> f2
                     })
  where filterBnds is_alloc_fn getArrSz_fn all_bnds =
          let sz_nms     = HS.fromList $
                            concatMap (HS.toList . getArrSz_fn) all_bnds
              sz_needs   = transClosSizes all_bnds sz_nms []
              alloc_bnds = filter is_alloc_fn all_bnds
              sel_nms    = HS.fromList $
                            concatMap (patternNames . bindingPattern)
                                      (sz_needs ++ alloc_bnds)
          in  sel_nms
        transClosSizes all_bnds scal_nms hoist_bnds =
          let new_bnds = filter (hasPatName scal_nms) all_bnds
              new_nms  = HS.fromList $ concatMap (HS.toList . freeInExp . bindingExp) new_bnds
          in  if null new_bnds
              then hoist_bnds
              else transClosSizes all_bnds new_nms (new_bnds ++ hoist_bnds)
        hasPatName nms bnd = not $ HS.null $ HS.intersection nms $
                               HS.fromList $ patternNames $ bindingPattern bnd
        isNotHoistableBnd :: Names -> BlockPred m
        isNotHoistableBnd nms _ bnd = not $ hasPatName nms bnd

-- | Simplify a single 'Body'.
simplifyBody :: SimplifiableLore lore =>
                [Diet] -> Body lore -> SimpleM lore Result
simplifyBody ds (Body _ bnds res) = do
  mapM_ simplifyStm bnds
  simplifyResult ds res

-- | Simplify a single 'Result'.
simplifyResult :: SimplifiableLore lore =>
                  [Diet] -> Result -> SimpleM lore Result
simplifyResult ds es = do
  es' <- mapM simplify es
  consumeResult $ zip ds es'
  return es'

isDoLoopResult :: Result -> SimpleM lore ()
isDoLoopResult = mapM_ checkForVar
  where checkForVar (Var ident) =
          inResultName ident
        checkForVar _ =
          return ()

-- | Simplify the binding, adding it to the program being constructed.
simplifyStm :: SimplifiableLore lore =>
               Stm lore -> SimpleM lore ()

simplifyStm (Let pat _ e) = do
  e' <- simplifyExp e
  pat' <- simplifyPattern pat
  inspectStm =<<
    mkLetM (addWisdomToPattern pat' e') e'

inspectStm :: SimplifiableLore lore => Stm (Wise lore) -> SimpleM lore ()
inspectStm bnd = do
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  simplified <- topDownSimplifyStm rules vtable bnd
  case simplified of
    Just newbnds -> changed >> mapM_ inspectStm newbnds
    Nothing      -> addStm bnd

simplifyOp :: SimplifiableLore lore => Op lore -> SimpleM lore (Op (Wise lore))
simplifyOp op = do f <- asks $ simplifyOpS . fst
                   f op

simplifyExp :: SimplifiableLore lore => Exp lore -> SimpleM lore (Exp (Wise lore))

simplifyExp (If cond tbranch fbranch ts) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We should also try to do CSE
  -- across branches.
  cond' <- simplify cond
  ts' <- mapM simplify ts
  let ds = map (const Observe) ts'
  (tbranch',fbranch') <-
    hoistCommon (simplifyBody ds tbranch) (ST.updateBounds True cond)
                (simplifyBody ds fbranch) (ST.updateBounds False cond)
  return $ If cond' tbranch' fbranch' ts'

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
    ForLoop loopvar it boundexp -> do
      boundexp' <- simplify boundexp
      return (ForLoop loopvar it boundexp',
              loopvar `HS.insert` fparamnames,
              bindLoopVar loopvar it boundexp')
    WhileLoop cond -> do
      cond' <- simplify cond
      return (WhileLoop cond',
              fparamnames,
              id)
  seq_blocker <- asksEngineEnv $ blockHoistSeq . envHoistBlockers
  (loopres', loopbnds') <-
    enterLoop $
    bindFParams (ctxparams'++valparams') $ wrapbody $
    blockIf
    (hasFree boundnames `orIf` isConsumed `orIf` seq_blocker) $ do
      res <- simplifyBody diets loopbody
      isDoLoopResult res
      return res
  loopbody' <- mkBodyM loopbnds' loopres'
  consumeResult $ zip diets $ ctxinit' ++ valinit'
  return $ DoLoop ctx' val' form' loopbody'
  where fparamnames = HS.fromList (map (paramName . fst) $ ctx++val)


simplifyExp e = simplifyExpBase e

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
                , mapOnFParam =
                  fail "Unhandled FParam in simplification engine."
                , mapOnOp =
                  simplifyOp
                }

type SimplifiableLore lore = (Attributes lore,
                              Simplifiable (LetAttr lore),
                              Simplifiable (FParamAttr lore),
                              Simplifiable (LParamAttr lore),
                              Simplifiable (RetType lore),
                              CanBeWise (Op lore))

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

instance Simplifiable SubExp where
  simplify (Var name) = do
    bnd <- getsEngineState $ ST.lookupSubExp name . stateVtable
    case bnd of
      Just (Constant v) -> return $ Constant v
      Just (Var id') -> do usedName id'
                           return $ Var id'
      _              -> do usedName name
                           return $ Var name
  simplify (Constant v) =
    return $ Constant v

instance Simplifiable ExtRetType where
  simplify = fmap ExtRetType . mapM simplify . retTypeValues

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
  simplify (BindInPlace cs src is) =
    BindInPlace <$>
    simplify cs <*>
    simplify src <*>
    mapM simplify is

simplifyParam :: (attr -> SimpleM lore attr) -> ParamT attr -> SimpleM lore (ParamT attr)
simplifyParam simplifyAttribute (Param name attr) = do
  attr' <- simplifyAttribute attr
  return $ Param name attr'

instance Simplifiable VName where
  simplify v = do
    se <- ST.lookupSubExp v <$> getVtable
    case se of
      Just (Var v') -> do usedName v'
                          return v'
      _             -> do usedName v
                          return v

instance Simplifiable (TypeBase ExtShape u) where
  simplify t = do shape <- simplify $ arrayShape t
                  return $ t `setArrayShape` shape

instance Simplifiable ExtShape where
  simplify = fmap ExtShape . mapM simplify . extShapeDims

instance Simplifiable ExtDimSize where
  simplify (Free se) = Free <$> simplify se
  simplify (Ext x)   = return $ Ext x

instance Simplifiable (TypeBase Shape u) where
  simplify (Array et shape u) = do
    dims <- mapM simplify $ shapeDims shape
    return $ Array et (Shape dims) u
  simplify (Mem size space) =
    Mem <$> simplify size <*> pure space
  simplify (Prim bt) =
    return $ Prim bt

instance Simplifiable d => Simplifiable (DimIndex d) where
  simplify (DimFix i) = DimFix <$> simplify i
  simplify (DimSlice i n) = DimSlice <$> simplify i <*> simplify n

simplifyLambda :: SimplifiableLore lore =>
                  Lambda lore
               -> Maybe [SubExp] -> [Maybe VName]
               -> SimpleM lore (Lambda (Wise lore))
simplifyLambda lam nes arrs = do
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  simplifyLambdaMaybeHoist par_blocker lam nes arrs

simplifyLambdaSeq :: SimplifiableLore lore =>
                     Lambda lore
                  -> Maybe [SubExp] -> [Maybe VName]
                  -> SimpleM lore (Lambda (Wise lore))
simplifyLambdaSeq = simplifyLambdaMaybeHoist neverBlocks

simplifyLambdaNoHoisting :: SimplifiableLore lore =>
                            Lambda lore
                         -> Maybe [SubExp] -> [Maybe VName]
                         -> SimpleM lore (Lambda (Wise lore))
simplifyLambdaNoHoisting = simplifyLambdaMaybeHoist $ isFalse False

simplifyLambdaMaybeHoist :: SimplifiableLore lore =>
                            BlockPred (Wise lore) -> Lambda lore
                         -> Maybe [SubExp] -> [Maybe VName]
                         -> SimpleM lore (Lambda (Wise lore))
simplifyLambdaMaybeHoist blocked lam@(Lambda params body rettype) nes arrs = do
  params' <- mapM (simplifyParam simplify) params
  let (nonarrayparams, arrayparams) =
        splitAt (length params' - length arrs) params'
      paramnames = HS.fromList $ boundByLambda lam
  (body_res', body_bnds') <-
    enterLoop $
    bindLParams nonarrayparams $
    bindArrayLParams (zip arrayparams arrs) $
    blockIf (blocked `orIf` hasFree paramnames `orIf` isConsumed) $
      simplifyBody (map (const Observe) rettype) body
  body' <- mkBodyM body_bnds' body_res'
  rettype' <- mapM simplify rettype
  let consumed_in_body = consumedInBody body'
      paramWasConsumed p (Just (Var arr))
        | p `HS.member` consumed_in_body = consumedName arr
      paramWasConsumed _ _ =
        return ()
  zipWithM_ paramWasConsumed (map paramName arrayparams) $ map (fmap Var) arrs
  case nes of
    Just nes' -> do
      let accparams = drop (length nonarrayparams - length nes') nonarrayparams
      zipWithM_ paramWasConsumed (map paramName accparams) $ map Just nes'
    Nothing -> return ()

  return $ Lambda params' body' rettype'

simplifyExtLambda :: SimplifiableLore lore =>
                     ExtLambda lore
                  -> [SubExp]
                  -> [(LParam (Wise lore), SE.ScalExp, SE.ScalExp)]
                  -> SimpleM lore (ExtLambda (Wise lore))
simplifyExtLambda lam@(ExtLambda params body rettype) nes parbnds = do
  params' <- mapM (simplifyParam simplify) params
  let paramnames = HS.fromList $ boundByExtLambda lam
  rettype' <- mapM simplify rettype
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  (body_res', body_bnds') <-
    enterLoop $
    bindLParams params' $
    localVtable extendSymTab $
    blockIf (hasFree paramnames `orIf` isConsumed `orIf` par_blocker) $
    simplifyBody (map (const Observe) rettype) body
  body' <- mkBodyM body_bnds' body_res'
  let consumed_in_body = consumedInBody body'
      paramWasConsumed p (Var arr)
        | p `HS.member` consumed_in_body = consumedName arr
      paramWasConsumed _ _ =
        return ()
      accparams = take (length nes) $ drop 1 params
  zipWithM_ paramWasConsumed (map paramName accparams) nes
  return $ ExtLambda params' body' rettype'
  where extendSymTab vtb =
          foldl (\ vt (i,l,u) ->
                   let i_name = paramName i
                   in  ST.setUpperBound i_name u $
                       ST.setLowerBound i_name l vt
                ) vtb parbnds

consumeResult :: [(Diet, SubExp)] -> SimpleM lore ()
consumeResult = mapM_ inspect
  where inspect (Consume, se) =
          traverse_ consumedName $ subExpAliases se
        inspect (Observe, _) = return ()

instance Simplifiable Certificates where
  simplify = fmap (nub . concat) . mapM check
    where check idd = do
            vv <- getsEngineState $ ST.lookupSubExp idd . stateVtable
            case vv of
              Just (Constant Checked) -> return []
              Just (Var idd') -> do usedName idd'
                                    return [idd']
              _ -> do usedName idd
                      return [idd]

simplifyFun :: SimplifiableLore lore => FunDef lore -> SimpleM lore (FunDef (Wise lore))
simplifyFun (FunDef entry fname rettype params body) = do
  rettype' <- simplify rettype
  body' <- bindFParams params $ insertAllStms $
           simplifyBody (map diet $ retTypeValues rettype') body
  return $ FunDef entry fname rettype' params body'
