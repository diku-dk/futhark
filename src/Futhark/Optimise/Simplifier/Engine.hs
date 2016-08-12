{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
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
         MonadEngine(..)
       , addBindingEngine
       , collectBindingsEngine
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
       , Need (needBindings)
       , asksEngineEnv
       , getVtable
       , localVtable
       , insertAllBindings
       , simplifyBody
       , inspectBinding
         -- * Building blocks
       , SimplifiableOp (..)
       , Simplifiable (..)
       , simplifyBinding
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
       , bindLoopVars
       , enterLoop
       , consumedName

       , blockIf

       , tapUsage
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
import Futhark.Optimise.Simplifier.Lore
import qualified Futhark.Analysis.ScalExp as SE

type NeedSet lore = [Binding lore]

data Need lore = Need { needBindings :: NeedSet lore
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
                       , getArraySizes :: Binding (Lore m) -> Names
                         -- ^ gets the sizes of arrays from a binding.
                       , isAllocation  :: Binding (Lore m) -> Bool
                       }

noExtraHoistBlockers :: HoistBlockers m
noExtraHoistBlockers = HoistBlockers neverBlocks neverBlocks (const HS.empty) (const False)

data Env m = Env { envRules         :: RuleBook m
                 , envHoistBlockers :: HoistBlockers m
                 }

emptyEnv :: MonadEngine m =>
            RuleBook m
         -> HoistBlockers m
         -> Env m
emptyEnv rules blockers =
  Env { envRules = rules
      , envHoistBlockers = blockers
      }

data State m = State { stateVtable :: ST.SymbolTable (Lore m)
                     }

emptyState :: State m
emptyState = State { stateVtable = ST.empty }

class (MonadBinder m,
       Attributes (Lore m),
       Attributes (InnerLore m),
       Lore m ~ Wise (InnerLore m),
       Simplifiable (LetAttr (InnerLore m)),
       Simplifiable (FParamAttr (InnerLore m)),
       Simplifiable (LParamAttr (InnerLore m)),
       Simplifiable (RetType (InnerLore m)),
       SimplifiableOp (InnerLore m) (Op (InnerLore m))) => MonadEngine m where
  type InnerLore m :: *
  askEngineEnv :: m (Env m)
  localEngineEnv :: (Env m -> Env m) -> m a -> m a
  tellNeed :: Need (Lore m) -> m ()
  listenNeed :: m a -> m (a, Need (Lore m))
  getEngineState :: m (State m)
  putEngineState :: State m -> m ()
  passNeed :: m (a, Need (Lore m) -> Need (Lore m)) -> m a

addBindingEngine :: MonadEngine m =>
                    Binding (Lore m) -> m ()
addBindingEngine bnd = do
  modifyVtable $ ST.insertBinding bnd
  case bindingExp bnd of
    PrimOp (Assert se _) -> asserted se
    _                    -> return ()
  needBinding bnd

collectBindingsEngine :: MonadEngine m =>
                         m a -> m (a, [Binding (Lore m)])
collectBindingsEngine m = passNeed $ do
  (x, need) <- listenNeed m
  return ((x, needBindings need),
          const mempty)

asksEngineEnv :: MonadEngine m => (Env m -> a) -> m a
asksEngineEnv f = f <$> askEngineEnv

getsEngineState :: MonadEngine m => (State m -> a) -> m a
getsEngineState f = f <$> getEngineState

modifyEngineState :: MonadEngine m => (State m -> State m) -> m ()
modifyEngineState f = do x <- getEngineState
                         putEngineState $ f x

needBinding :: MonadEngine m => Binding (Lore m) -> m ()
needBinding bnd = tellNeed $ Need [bnd] UT.empty

boundFree :: MonadEngine m => Names -> m ()
boundFree fs = tellNeed $ Need [] $ UT.usages fs

usedName :: MonadEngine m => VName -> m ()
usedName = boundFree . HS.singleton

-- | Register the fact that the given name is consumed.
consumedName :: MonadEngine m => VName -> m ()
consumedName = tellNeed . Need [] . UT.consumedUsage

inResultName :: MonadEngine m => VName -> m ()
inResultName = tellNeed . Need [] . UT.inResultUsage

asserted :: MonadEngine m => SubExp -> m ()
asserted Constant{} =
  return ()
asserted (Var name) = do
  se <- ST.lookupExp name <$> getVtable
  case se of Just (PrimOp (CmpOp CmpEq{} x y)) -> do
               case x of Var xvar ->
                           tellNeed $ Need [] $
                           UT.equalToUsage xvar y
                         _ -> return ()
               case y of Var yvar ->
                           tellNeed $ Need [] $
                           UT.equalToUsage yvar x
                         _ -> return ()
             _ -> return ()

tapUsage :: MonadEngine m => m a -> m (a, UT.UsageTable)
tapUsage m = do (x,needs) <- listenNeed m
                return (x, usageTable needs)

censorUsage :: MonadEngine m =>
               (UT.UsageTable -> UT.UsageTable)
            -> m a -> m a
censorUsage f m = passNeed $ do
  x <- m
  return (x, \acc -> acc { usageTable = f $ usageTable acc })

getVtable :: MonadEngine m => m (ST.SymbolTable (Lore m))
getVtable = getsEngineState stateVtable

putVtable :: MonadEngine m => ST.SymbolTable (Lore m) -> m ()
putVtable vtable = modifyEngineState $ \s -> s { stateVtable = vtable }

modifyVtable :: MonadEngine m => (ST.SymbolTable (Lore m) -> ST.SymbolTable (Lore m))
             -> m ()
modifyVtable f = do vtable <- getVtable
                    putVtable $ f vtable

localVtable :: MonadEngine m =>
               (ST.SymbolTable (Lore m) -> ST.SymbolTable (Lore m))
            -> m a -> m a
localVtable f m = do
  vtable <- getVtable
  modifyEngineState $ \env -> env { stateVtable = f vtable }
  (x, need) <- listenNeed m
  let vtable' = foldl (flip ST.insertBinding) vtable $ needBindings need
  modifyEngineState $ \env -> env { stateVtable = vtable' }
  return x

enterLoop :: MonadEngine m => m a -> m a
enterLoop = enterBody . localVtable ST.deepen

enterBody :: MonadEngine m => m a -> m a
enterBody = censorUsage UT.leftScope

bindFParams :: MonadEngine m =>
               [FParam (Lore m)] -> m a -> m a
bindFParams params =
  localVtable $ ST.insertFParams params

bindLParams :: MonadEngine m =>
               [LParam (Lore m)] -> m a -> m a
bindLParams params =
  localVtable $ \vtable ->
    foldr ST.insertLParam vtable params

bindArrayLParams :: MonadEngine m =>
                    [(LParam (Lore m),Maybe VName)] -> m a -> m a
bindArrayLParams params =
  localVtable $ \vtable ->
    foldr (uncurry ST.insertArrayLParam) vtable params

bindLoopVar :: MonadEngine m => VName -> SubExp -> m a -> m a
bindLoopVar var bound =
  localVtable $ clampUpper . clampVar
  where clampVar = ST.insertLoopVar var bound
        -- If we enter the loop, then 'bound' is at least one.
        clampUpper = case bound of Var v -> ST.isAtLeast v 1
                                   _     -> id

bindLoopVars :: MonadEngine m => [(VName,SubExp)] -> m a -> m a
bindLoopVars []                  m =
  m
bindLoopVars ((var,bound):lvars) m =
  bindLoopVar var bound $ bindLoopVars lvars m

hoistBindings :: MonadEngine m =>
                 RuleBook m -> BlockPred (Lore m)
              -> ST.SymbolTable (Lore m) -> UT.UsageTable
              -> [Binding (Lore m)]
              -> m ([Binding (Lore m)],
                    [Binding (Lore m)],
                    UT.UsageTable)
hoistBindings rules block vtable uses needs = do
  (uses', blocked, hoisted) <- simplifyBindings vtable uses needs
  mapM_ addBinding blocked
  return (blocked, hoisted, uses')
  where simplifyBindings vtable' uses' bnds = do
          (uses'', bnds') <- simplifyBindings' vtable' uses' bnds
          -- We need to do a final pass to ensure that nothing is
          -- hoisted past something that it depends on.
          let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps bnds'
          return (uses'', blocked, hoisted)

        simplifyBindings' vtable' uses' bnds =
          foldM hoistable (uses',[]) $ reverse $ zip bnds vtables
            where vtables = scanl (flip ST.insertBinding) vtable' bnds

        hoistable (uses',bnds) (bnd, vtable')
          | not $ uses' `UT.contains` provides bnd = -- Dead binding.
            return (uses', bnds)
          | otherwise = do
            res <- localVtable (const vtable') $
                   bottomUpSimplifyBinding rules (vtable', uses') bnd
            case res of
              Nothing -- Nothing to optimise - see if hoistable.
                | block uses' bnd ->
                  return (expandUsage uses' bnd `UT.without` provides bnd,
                          Left bnd : bnds)
                | otherwise ->
                  return (expandUsage uses' bnd, Right bnd : bnds)
              Just optimbnds -> do
                (uses'',bnds') <- simplifyBindings' vtable' uses' optimbnds
                return (uses'', bnds'++bnds)

blockUnhoistedDeps :: Attributes lore =>
                      [Either (Binding lore) (Binding lore)]
                   -> [Either (Binding lore) (Binding lore)]
blockUnhoistedDeps = snd . mapAccumL block HS.empty
  where block blocked (Left need) =
          (blocked <> HS.fromList (provides need), Left need)
        block blocked (Right need)
          | blocked `intersects` requires need =
            (blocked <> HS.fromList (provides need), Left need)
          | otherwise =
            (blocked, Right need)

provides :: Attributes lore => Binding lore -> [VName]
provides = patternNames . bindingPattern

requires :: Attributes lore => Binding lore -> Names
requires bnd =
  (mconcat (map freeIn $ patternElements $ bindingPattern bnd)
  `HS.difference` HS.fromList (provides bnd)) <>
  freeInExp (bindingExp bnd)

expandUsage :: (Attributes lore, Aliased lore, UsageInOp (Op lore)) =>
               UT.UsageTable -> Binding lore -> UT.UsageTable
expandUsage utable bnd = utable <> usageInBinding bnd <> usageThroughAliases
  where pat = bindingPattern bnd
        usageThroughAliases =
          mconcat $ mapMaybe usageThroughBindeeAliases $
          zip (patternNames pat) (patternAliases pat)
        usageThroughBindeeAliases (name, aliases) = do
          uses <- UT.lookup name utable
          return $ mconcat $ map (`UT.usage` uses) $ HS.toList aliases

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

type BlockPred lore = UT.UsageTable -> Binding lore -> Bool

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

blockIf :: MonadEngine m =>
           BlockPred (Lore m)
        -> m a -> m (a, [Binding (Lore m)])
blockIf block m = passNeed $ do
  (x, needs) <- listenNeed m
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (hoisted, hoistable, usages) <-
    hoistBindings rules block vtable (usageTable needs) (needBindings needs)
  putVtable $ foldl (flip ST.insertBinding) vtable hoistable
  return ((x, hoisted),
          const Need { needBindings = hoistable
                     , usageTable  = usages
                     })

insertAllBindings :: MonadEngine m => m Result -> m (Body (Lore m))
insertAllBindings = uncurry (flip mkBodyM) <=< blockIf (isFalse False)

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
        cheap (PrimOp BinOp{})   = True
        cheap (PrimOp SubExp{})  = True
        cheap (PrimOp UnOp{})    = True
        cheap (PrimOp CmpOp{})   = True
        cheap (PrimOp ConvOp{})  = True
        cheap DoLoop{}           = False
        cheap _                  = True -- Used to be False, but
                                        -- let's try it out.
hoistCommon :: MonadEngine m =>
               m Result
            -> (ST.SymbolTable (Lore m)
                -> ST.SymbolTable (Lore m))
            -> m Result
            -> (ST.SymbolTable (Lore m)
                -> ST.SymbolTable (Lore m))
            -> m (Body (Lore m), Body (Lore m))
hoistCommon m1 vtablef1 m2 vtablef2 = passNeed $ do
  (res1, needs1) <- listenNeed $ localVtable vtablef1 m1
  (res2, needs2) <- listenNeed $ localVtable vtablef2 m2
  is_alloc_fun <- asksEngineEnv $ isAllocation  . envHoistBlockers
  getArrSz_fun <- asksEngineEnv $ getArraySizes . envHoistBlockers
  let needs1_bnds = needBindings needs1
      needs2_bnds = needBindings needs2
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
    hoistBindings rules block vtable (usageTable needs1) needs1_bnds
  (body2_bnds', safe2, f2) <-
    enterBody $
    localVtable vtablef2 $
    hoistBindings rules block vtable (usageTable needs2) needs2_bnds
  let hoistable = safe1 <> safe2
  putVtable $ foldl (flip ST.insertBinding) vtable hoistable
  body1' <- mkBodyM body1_bnds' res1
  body2' <- mkBodyM body2_bnds' res2
  return ((body1', body2'),
          const Need { needBindings = hoistable
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

-- | Simplify a single 'Body' inside an arbitrary 'MonadEngine'.
simplifyBody :: MonadEngine m =>
                [Diet] -> Body (InnerLore m) -> m Result
simplifyBody ds (Body _ bnds res) = do
  mapM_ simplifyBinding bnds
  simplifyResult ds res

-- | Simplify a single 'Result' inside an arbitrary 'MonadEngine'.
simplifyResult :: MonadEngine m =>
                  [Diet] -> Result -> m Result

simplifyResult ds es = do
  es' <- mapM simplify es
  consumeResult $ zip ds es'
  return es'

isDoLoopResult :: MonadEngine m =>
                  Result -> m ()
isDoLoopResult = mapM_ checkForVar
  where checkForVar (Var ident) =
          inResultName ident
        checkForVar _ =
          return ()

-- | Simplify the binding, adding it to the program being constructed.
simplifyBinding :: MonadEngine m =>
                   Binding (InnerLore m)
                -> m ()

simplifyBinding (Let pat _ e) = do
  e' <- simplifyExp e
  pat' <- simplifyPattern pat
  inspectBinding =<<
    mkLetM (addWisdomToPattern pat' e') e'

inspectBinding :: MonadEngine m =>
                  Binding (Lore m) -> m ()
inspectBinding bnd = do
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  simplified <- topDownSimplifyBinding rules vtable bnd
  case simplified of
    Just newbnds -> mapM_ inspectBinding newbnds
    Nothing      -> addBinding bnd

simplifyExp :: MonadEngine m => Exp (InnerLore m) -> m (Exp (Lore m))

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
    ForLoop loopvar boundexp -> do
      boundexp' <- simplify boundexp
      return (ForLoop loopvar boundexp',
              loopvar `HS.insert` fparamnames,
              bindLoopVar loopvar boundexp')
    WhileLoop cond -> do
      cond' <- simplify cond
      return (WhileLoop cond',
              fparamnames,
              id)
  seq_blocker <- asksEngineEnv $ blockHoistSeq . envHoistBlockers
  (loopres', loopbnds') <-
    enterLoop $
    bindFParams (ctxparams'++valparams') $
    blockIf
    (hasFree boundnames `orIf` isConsumed `orIf` seq_blocker) $
    wrapbody $ do
      res <- simplifyBody diets loopbody
      isDoLoopResult res
      return res
  loopbody' <- mkBodyM loopbnds' loopres'
  consumeResult $ zip diets $ ctxinit' ++ valinit'
  return $ DoLoop ctx' val' form' loopbody'
  where fparamnames = HS.fromList (map (paramName . fst) $ ctx++val)


simplifyExp e = simplifyExpBase e

simplifyExpBase :: MonadEngine m => Exp (InnerLore m) -> m (Exp (Lore m))
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

class (CanBeWise op, UsageInOp (OpWithWisdom op)) => SimplifiableOp lore op where
  simplifyOp :: (MonadEngine m, InnerLore m ~ lore) => op -> m (OpWithWisdom op)

instance SimplifiableOp lore () where
  simplifyOp () = return ()


class Simplifiable e where
  simplify :: MonadEngine m => e -> m e

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

simplifyPattern :: (MonadEngine m, Simplifiable attr) =>
                   PatternT attr
                -> m (PatternT attr)
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

simplifyParam :: MonadEngine m =>
                 (attr -> m attr) -> ParamT attr -> m (ParamT attr)
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

simplifyLambda :: MonadEngine m =>
                  Lambda (InnerLore m)
               -> Maybe [SubExp] -> [Maybe VName]
               -> m (Lambda (Lore m))
simplifyLambda lam nes arrs = do
  par_blocker <- asksEngineEnv $ blockHoistPar . envHoistBlockers
  simplifyLambdaMaybeHoist par_blocker lam nes arrs

simplifyLambdaSeq :: MonadEngine m =>
                  Lambda (InnerLore m)
               -> Maybe [SubExp] -> [Maybe VName]
               -> m (Lambda (Lore m))
simplifyLambdaSeq = simplifyLambdaMaybeHoist neverBlocks

simplifyLambdaNoHoisting :: MonadEngine m =>
                            Lambda (InnerLore m)
                         -> Maybe [SubExp] -> [Maybe VName]
                         -> m (Lambda (Lore m))
simplifyLambdaNoHoisting = simplifyLambdaMaybeHoist $ isFalse False

simplifyLambdaMaybeHoist :: MonadEngine m =>
                            BlockPred (Lore m) -> Lambda (InnerLore m)
                         -> Maybe [SubExp] -> [Maybe VName]
                         -> m (Lambda (Lore m))
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

simplifyExtLambda :: MonadEngine m =>
                     ExtLambda (InnerLore m)
                  -> [SubExp]
                  -> [(LParam (Lore m), SE.ScalExp, SE.ScalExp)]
                  -> m (ExtLambda (Lore m))
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

consumeResult :: MonadEngine m =>
                 [(Diet, SubExp)] -> m ()
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

simplifyFun :: MonadEngine m =>
               FunDef (InnerLore m) -> m (FunDef (Lore m))
simplifyFun (FunDef entry fname rettype params body) = do
  rettype' <- simplify rettype
  body' <- bindFParams params $ insertAllBindings $
           simplifyBody (map diet $ retTypeValues rettype') body
  return $ FunDef entry fname rettype' params body'
