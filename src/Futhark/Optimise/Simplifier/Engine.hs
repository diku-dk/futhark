{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
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
-- may prefer to use the "Futhark.Optimise.Simplifier" or
-- "Futhark.Optimise.Simplifier.Simplifiable" modules.
--
module Futhark.Optimise.Simplifier.Engine
       ( -- * Monadic interface
         MonadEngine(..)
       , addBindingEngine
       , collectBindingsEngine
       , Env
       , emptyEnv
       , State
       , emptyState
       , Need
       , asksEngineEnv
       , getVtable
       , localVtable
       , insertAllBindings
       , defaultSimplifyBody
       , defaultInspectBinding
         -- * Building blocks
       , simplifyBinding
       , simplifyResult
       , simplifyExp
       , simplifyFun
       , simplifyLambda
       , simplifySubExp
       , simplifyIdent
       , simplifyExtType
       , simplifyExtShape
       ) where

import Control.Applicative
import Control.Monad.Writer

import Data.Either
import Data.Graph
import Data.Hashable
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Foldable (traverse_)

import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST
import Futhark.Representation.Aliases (Aliases)
import qualified Futhark.Representation.Aliases as Aliases
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.CSE
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Usage
import Futhark.Optimise.Simplifier.Apply
import Futhark.Tools

type NeedSet lore = [Binding lore]

data Need lore = Need { needBindings :: NeedSet lore
                      , usageTable  :: UT.UsageTable
                      }

instance Monoid (Need lore) where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] UT.empty

type AliasMap = HM.HashMap VName Names

data Env m = Env { envDupeState :: DupeState (Lore m)
                 , envProgram   :: Maybe (Prog (InnerLore m))
                 , envRules     :: RuleBook m
                 , envAliases   :: AliasMap
                 }

emptyEnv :: MonadEngine m =>
            RuleBook m
         -> Maybe (Prog (InnerLore m)) -> Env m
emptyEnv rules prog =
  Env { envDupeState = newDupeState
      , envProgram = prog
      , envRules = rules
      , envAliases = mempty
      }

data State m = State { stateVtable :: ST.SymbolTable (Lore m)
                     }

emptyState :: State m
emptyState = State { stateVtable = ST.empty }

class (MonadBinder m,
       Proper (Lore m),
       Lore m ~ Aliases (InnerLore m),
       Proper (InnerLore m)) => MonadEngine m where
  type InnerLore m
  askEngineEnv :: m (Env m)
  localEngineEnv :: (Env m -> Env m) -> m a -> m a
  tellNeed :: Need (Lore m) -> m ()
  listenNeed :: m a -> m (a, Need (Lore m))
  getEngineState :: m (State m)
  putEngineState :: State m -> m ()
  passNeed :: m (a, Need (Lore m) -> Need (Lore m)) -> m a

  simplifyBody :: [Diet] -> Body (InnerLore m) -> m Result
  simplifyBody = defaultSimplifyBody
  inspectBinding :: Binding (Lore m) -> m ()
  inspectBinding = defaultInspectBinding
  simplifyLetBoundLore :: Lore.LetBound (InnerLore m)
                       -> m (Lore.LetBound (InnerLore m))
  simplifyFParamLore :: Lore.FParam (InnerLore m)
                     -> m (Lore.FParam (InnerLore m))
  simplifyRetType :: Lore.RetType (InnerLore m)
                  -> m (Lore.RetType (InnerLore m))

addBindingEngine :: MonadEngine m =>
                    Binding (Lore m) -> m ()
addBindingEngine bnd = do
  modifyVtable $ ST.insertBinding bnd
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

consumedName :: MonadEngine m => VName -> m ()
consumedName = tellNeed . Need [] . UT.consumedUsage

inResultName :: MonadEngine m => VName -> m ()
inResultName = tellNeed . Need [] . UT.inResultUsage

tapUsage :: MonadEngine m => m a -> m (a, UT.UsageTable)
tapUsage m = do (x,needs) <- listenNeed m
                return (x, usageTable needs)

blockUsage :: MonadEngine m => m a -> m a
blockUsage m = passNeed $ do
  (x, _) <- listenNeed m
  return (x, const mempty)

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
  x <- m
  modifyEngineState $ \env -> env { stateVtable = vtable }
  return x

enterLoop :: MonadEngine m => m a -> m a
enterLoop = localVtable ST.deepen

bindFParams :: MonadEngine m =>
               [FParam (Lore m)] -> m a -> m a
bindFParams params =
  localVtable $ \vtable ->
    foldr ST.insertFParam vtable params

bindLParams :: MonadEngine m =>
               [Param] -> m a -> m a
bindLParams params =
  localVtable $ \vtable ->
    foldr ST.insertLParam vtable params

bindArrayLParams :: MonadEngine m =>
                    [(Param,Maybe SubExp)] -> m a -> m a
bindArrayLParams params =
  localVtable $ \vtable ->
    foldr (uncurry ST.insertArrayLParam) vtable params

bindLoopVar :: MonadEngine m => Ident -> SubExp -> m a -> m a
bindLoopVar var bound =
  localVtable $ clampUpper . clampVar
  where clampVar = ST.insertLoopVar (identName var) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampUpper = case bound of Var v -> ST.isAtLeast (identName v) 1
                                   _     -> id

hoistBindings :: MonadEngine m =>
                 RuleBook m -> BlockPred (Lore m)
              -> ST.SymbolTable (Lore m) -> UT.UsageTable -> DupeState (Lore m)
              -> [Binding (Lore m)] -> Result
              -> m (Body (Lore m),
                    [Binding (Lore m)],
                    UT.UsageTable)
hoistBindings rules block vtable uses dupes needs result = do
  (uses', blocked, hoisted) <-
    simplifyBindings vtable uses $
    concat $ snd $ mapAccumL pick dupes $ inDepOrder needs
  body <- mkBodyM blocked result
  return (body, hoisted, uses')
  where simplifyBindings vtable' uses' bnds = do
          (uses'', bnds') <- simplifyBindings' vtable' uses' bnds
          let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps bnds'
          -- We need to do a final pass to ensure that nothing is
          -- hoisted past something that it depends on.
          return (uses'', blocked, hoisted)

        simplifyBindings' vtable' uses' bnds =
          foldM hoistable (uses',[]) (reverse $ zip bnds vtables)
            where vtables = scanl insertBnd vtable' bnds
                  insertBnd vtable'' bnd =
                    ST.insertBinding bnd vtable''

        hoistable (uses',bnds) (bnd, vtable')
          | not $ uses' `UT.contains` provides bnd = -- Dead binding.
            return (uses', bnds)
          | otherwise = do
            res <- bottomUpSimplifyBinding rules (vtable', uses') bnd
            case res of
              Nothing -- Nothing to optimise - see if hoistable.
                | block uses' bnd ->
                  return (expandUsage uses' bnd `UT.without` provides bnd,
                          Left bnd : bnds)
                | otherwise ->
                  return (expandUsage uses' bnd, Right bnd : bnds)
              Just optimbnds -> do
                (uses'',bnds') <-
                  simplifyBindings' vtable' uses' optimbnds
                return (uses'', bnds'++bnds)

        pick ds bnd =
          (ds,[bnd])

blockUnhoistedDeps :: Proper lore =>
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

inDepOrder :: (Proper lore, Aliased lore) =>
              [Binding lore] -> [Binding lore]
inDepOrder = flattenSCCs . stronglyConnComp . buildGraph
  where buildGraph bnds =
          [ (bnd, rep $ provides bnd, deps) |
            bnd <- bnds,
            let deps = [ rep $ provides dep
                         | dep <- bnds, dep `mustPrecede` bnd ] ]

        -- As all names are unique, a pattern can be uniquely
        -- represented by any of its names.  If the pattern has no
        -- names, then it doesn't matter anyway.
        rep []    = Nothing
        rep (x:_) = Just x

mustPrecede :: (Proper lore, Aliased lore) =>
               Binding lore -> Binding lore -> Bool
bnd1 `mustPrecede` bnd2 =
  not $ HS.null $ HS.fromList (filter (`HS.member` req2) $ provides bnd1)
                  `HS.union`
                  (consumedInExp (bindingExp bnd2) `HS.intersection`
                   requires bnd1)
  where req2 = requires bnd2

provides :: Proper lore => Binding lore -> [VName]
provides = patternNames . bindingPattern

requires :: Proper lore => Binding lore -> Names
requires bnd =
  (mconcat (map freeNamesIn $ patternBindees $ bindingPattern bnd)
  `HS.difference` HS.fromList (provides bnd)) <>
  freeNamesInExp (bindingExp bnd)

expandUsage :: (Proper lore, Aliased lore) =>
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

orIf :: BlockPred lore -> BlockPred lore -> BlockPred lore
orIf p1 p2 body need = p1 body need || p2 body need

blockIf :: MonadEngine m =>
           BlockPred (Lore m)
        -> m Result -> m (Body (Lore m))
blockIf block m = passNeed $ do
  (body, needs) <- listenNeed m
  ds <- asksEngineEnv envDupeState
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (e, hoistable, usages) <-
    hoistBindings rules block vtable (usageTable needs) ds (needBindings needs) body
  putVtable $ foldl (flip ST.insertBinding) vtable hoistable
  return (e,
          const Need { needBindings = hoistable
                     , usageTable  = usages
                     })

insertAllBindings :: MonadEngine m => m Result -> m (Body (Lore m))
insertAllBindings = blockIf $ \_ _ -> True

hasFree :: Proper lore => Names -> BlockPred lore
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred m
isNotSafe _ = not . safeExp . bindingExp

isNotCheap :: BlockPred m
isNotCheap _ = not . cheapBnd
  where cheapBnd = cheap . bindingExp
        cheap (PrimOp (BinOp {}))   = True
        cheap (PrimOp (SubExp {}))  = True
        cheap (PrimOp (Not {}))     = True
        cheap (PrimOp (Negate {}))  = True
        cheap (LoopOp {})           = False
        cheap _                     = True -- Used to be False, but
                                           -- let's try it out.

isUnique :: BlockPred lore
isUnique _ = any unique . patternTypes . bindingPattern

isAlloc :: BlockPred lore
isAlloc _ (Let _ _ (PrimOp (Alloc {}))) = True
isAlloc _ _                             = False

isResultAlloc :: BlockPred lore
isResultAlloc usage (Let (Pattern [bindee]) _
                     (PrimOp (Alloc {}))) =
  UT.isInResult (bindeeName bindee) usage
isResultAlloc _ _ = False

hoistCommon :: MonadEngine m =>
               m Result
            -> (ST.SymbolTable (Lore m)
                -> ST.SymbolTable (Lore m))
            -> m Result
            -> (ST.SymbolTable (Lore m)
                -> ST.SymbolTable (Lore m))
            -> m (Body (Lore m), Body (Lore m))
hoistCommon m1 vtablef1 m2 vtablef2 = passNeed $ do
  (body1, needs1) <- listenNeed $ localVtable vtablef1 m1
  (body2, needs2) <- listenNeed $ localVtable vtablef2 m2
  let block = isNotSafe `orIf` isNotCheap
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (body1', safe1, f1) <-
    localVtable vtablef1 $
    hoistBindings rules block vtable (usageTable needs1)
    newDupeState (needBindings needs1) body1
  (body2', safe2, f2) <-
    localVtable vtablef2 $
    hoistBindings rules block vtable (usageTable needs2)
    newDupeState (needBindings needs2) body2
  let hoistable = safe1 <> safe2
  putVtable $ foldl (flip ST.insertBinding) vtable hoistable
  return ((body1', body2'),
          const Need { needBindings = hoistable
                     , usageTable = f1 <> f2
                     })

-- | Simplify a single 'Body' inside an arbitrary 'MonadEngine'.
defaultSimplifyBody :: MonadEngine m =>
                       [Diet] -> Body (InnerLore m) -> m Result

defaultSimplifyBody ds (Body _ [] res) =
  simplifyResult ds res

defaultSimplifyBody ds (Body lore (bnd:bnds) res) = do
  simplifyBinding bnd
  simplifyBody ds $ Body lore bnds res

-- | Simplify a single 'Result' inside an arbitrary 'MonadEngine'.
simplifyResult :: MonadEngine m =>
                  [Diet] -> Result -> m Result

simplifyResult ds (Result cs es loc) = do
  es' <- mapM simplifySubExp es
  consumeResult $ zip ds es'
  Result <$> simplifyCerts cs <*> pure es' <*> pure loc

isDoLoopResult :: MonadEngine m =>
                  Result -> m ()
isDoLoopResult = mapM_ checkForVar . resultSubExps
  where checkForVar (Var ident) =
          inResultName $ identName ident
        checkForVar _ =
          return ()

-- | Simplify the binding, adding it to the program being constructed.
simplifyBinding :: MonadEngine m =>
                   Binding (InnerLore m)
                -> m ()
-- The simplification rules cannot handle Apply, because it requires
-- access to the full program.  This is a bit of a hack.
simplifyBinding (Let pat _ (Apply fname args rettype loc)) = do
  args' <- mapM (simplifySubExp . fst) args
  rettype' <- simplifyRetType rettype
  prog <- asksEngineEnv envProgram
  vtable <- getVtable
  case join $ pure simplifyApply <*> prog <*> pure vtable <*> pure fname <*> pure args of
    -- Array values are non-unique, so we may need to copy them.
    Just vs -> do let vs' = valueShapeContext (resTypeValues rettype) vs ++ vs
                  bnds <- forM (zip (patternIdents pat) vs') $ \(p,v) ->
                    case uniqueness $ identType p of
                      Unique    -> mkLetNamesM [identName p] =<< eCopy (eValue v loc)
                      Nonunique -> mkLetNamesM [identName p] =<< eValue v loc
                  mapM_ (simplifyBinding . Aliases.removeBindingAliases) bnds
    Nothing -> do let e' = Apply fname (zip args' $ map snd args) rettype' loc
                  pat' <- blockUsage $ simplifyPattern pat
                  inspectBinding =<<
                    mkLetM (Aliases.addAliasesToPattern pat' e') e'

simplifyBinding (Let pat _ e) = do
  e' <- simplifyExp e
  pat' <- simplifyPattern pat
  inspectBinding =<<
    mkLetM (Aliases.addAliasesToPattern pat' e') e'

defaultInspectBinding :: MonadEngine m =>
                         Binding (Lore m) -> m ()

defaultInspectBinding bnd = do
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  simplified <- topDownSimplifyBinding rules vtable bnd
  case simplified of
    Just newbnds -> mapM_ inspectBinding newbnds
    Nothing      -> addBinding bnd

simplifyExp :: MonadEngine m => Exp (InnerLore m) -> m (Exp (Lore m))

simplifyExp (If cond tbranch fbranch ts loc) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We should also try to do CSE
  -- across branches.
  cond' <- simplifySubExp cond
  ts' <- mapM simplifyExtType ts
  let ds = map diet ts'
  (tbranch',fbranch') <-
    hoistCommon (simplifyBody ds tbranch) (ST.updateBounds True cond)
                (simplifyBody ds fbranch) (ST.updateBounds False cond)
  return $ If cond' tbranch' fbranch' ts' loc

simplifyExp (LoopOp op) = LoopOp <$> simplifyLoopOp op

simplifyExp e = simplifyExpBase e

simplifyExpBase :: MonadEngine m => Exp (InnerLore m) -> m (Exp (Lore m))
simplifyExpBase = mapExpM hoist
  where hoist = Mapper {
                  mapOnBinding = fail "Unhandled binding in simplification engine"
                -- Bodies are handled explicitly because we need to
                -- provide their result diet.
                , mapOnBody = fail "Unhandled body in simplification engine."
                , mapOnSubExp = simplifySubExp
                -- Lambdas are handled explicitly because we need to
                -- bind their parameters.
                , mapOnLambda = fail "Unhandled lambda in simplification engine."
                , mapOnIdent = simplifyIdent
                , mapOnType = simplifyType
                , mapOnValue = return
                , mapOnCertificates = simplifyCerts
                , mapOnRetType = simplifyRetType
                , mapOnFParam =
                  fail "Unhandled FParam in simplification engine."
                }

simplifyLoopOp :: MonadEngine m => LoopOp (InnerLore m) -> m (LoopOp (Lore m))

simplifyLoopOp (DoLoop respat merge loopvar boundexp loopbody loc) = do
  let (mergepat, mergeexp) = unzip merge
  respat'   <- mapM simplifyIdentBinding respat
  mergepat' <- mapM simplifyFParam mergepat
  mergeexp' <- mapM simplifySubExp mergeexp
  boundexp' <- simplifySubExp boundexp
  let diets = map (diet . bindeeType) mergepat'
  -- Blocking hoisting of all unique bindings is probably too
  -- conservative, but there is currently no nice way to mark
  -- consumption of the loop body result.
  loopbody' <- blockIf
               (hasFree boundnames `orIf` isUnique `orIf` isResultAlloc) $
               enterLoop $
               bindFParams mergepat' $
               bindLoopVar loopvar boundexp' $ do
                 res <- simplifyBody diets loopbody
                 isDoLoopResult res
                 return res
  let merge' = zip mergepat' mergeexp'
  consumeResult $ zip diets mergeexp'
  return $ DoLoop respat' merge' loopvar boundexp' loopbody' loc
  where boundnames = identName loopvar `HS.insert`
                     HS.fromList (map (bindeeName . fst) merge)
        simplifyFParam (Bindee ident lore) = do
          ident' <- simplifyIdentBinding ident
          lore' <- simplifyFParamLore lore
          return $ Bindee ident' lore'

simplifyLoopOp (Map cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun $ map Just arrs'
  return $ Map cs' fun' arrs' loc

simplifyLoopOp (Filter cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun $ map Just arrs'
  return $ Filter cs' fun' arrs' loc

simplifyLoopOp (Reduce cs fun input loc) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun $ map Just arrs'
  return $ Reduce cs' fun' (zip acc' arrs') loc

simplifyLoopOp (Scan cs fun input loc) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun $ map Just arrs'
  return $ Scan cs' fun' (zip acc' arrs') loc

simplifyLoopOp (Redomap cs outerfun innerfun acc arrs loc) = do
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  outerfun' <- simplifyLambda outerfun $
               replicate (length $ lambdaParams outerfun) Nothing
  (innerfun', used) <- tapUsage $ simplifyLambda innerfun $ map Just arrs
  (innerfun'', arrs'') <- removeUnusedParams used innerfun' arrs'
  return $ Redomap cs' outerfun' innerfun'' acc' arrs'' loc
  where removeUnusedParams used lam arrinps
          | (accparams, arrparams@(firstparam:_)) <-
            splitAt (length acc) $ lambdaParams lam,
            firstarr : _ <- arrinps =
              case unzip $ filter ((`UT.used` used) . identName . fst) $
                   zip arrparams arrinps of
               ([],[]) -> do
                 -- Avoid having zero inputs to redomap, as that would
                 -- set the number of iterations to zero, possibly
                 -- changing semantics.  Ideally, we should pick the
                 -- "simplest" size instead of just the one of the
                 -- first array, but I do not think it matters much.
                 let outerSize = arraySize 0 $ subExpType firstarr
                 input <- newIdent "unused_input"
                          (arrayOf (Basic Int) (Shape [outerSize]) Nonunique) $
                          srclocOf firstarr
                 letBindNames_ [identName input] $
                   PrimOp $ Iota outerSize $ srclocOf firstarr
                 return (lam { lambdaParams =
                                  accparams ++
                                  [firstparam { identType = Basic Int }] },
                         [Var input])
               (arrparams', arrinps') ->
                 return (lam { lambdaParams = accparams ++ arrparams' }, arrinps')
          | otherwise = return (lam, arrinps)

simplifySubExp :: MonadEngine m => SubExp -> m SubExp
simplifySubExp (Var (Ident vnm t loc)) = do
  bnd <- getsEngineState $ ST.lookupSubExp vnm . stateVtable
  t' <- simplifyType t
  case bnd of
    Just (Constant v _) -> return $ Constant v loc
    Just (Var id') -> do usedName $ identName id'
                         return $ Var $ Ident (identName id') t' loc
    _              -> do usedName vnm
                         return $ Var $ Ident vnm t' loc
simplifySubExp (Constant v loc) = return $ Constant v loc

simplifyPattern :: MonadEngine m =>
                   Pattern (InnerLore m)
                -> m (Pattern (InnerLore m))
simplifyPattern =
  liftM Pattern . mapM inspect . patternBindees
  where inspect (Bindee ident lore) = do
          ident' <- simplifyIdentBinding ident
          lore'  <- simplifyLetBoundLore lore
          return $ Bindee ident' lore'

simplifyIdentBinding :: MonadEngine m => Ident -> m Ident
simplifyIdentBinding v = do
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyIdent :: MonadEngine m => Ident -> m Ident
simplifyIdent v = do
  se <- ST.lookupSubExp (identName v) <$> getVtable
  t' <- simplifyType $ identType v
  case se of
    Just (Var v') -> do usedName $ identName v'
                        return v { identType = t'
                                 , identName = identName v'
                                 }
    _             -> do usedName $ identName v
                        return v { identType = t' }

simplifyExtType :: MonadEngine m =>
                   TypeBase ExtShape -> m (TypeBase ExtShape)
simplifyExtType t = do shape <- simplifyExtShape $ arrayShape t
                       return $ t `setArrayShape` shape

simplifyExtShape :: MonadEngine m =>
                    ExtShape -> m ExtShape
simplifyExtShape = liftM ExtShape . mapM simplifyDim . extShapeDims
  where simplifyDim (Free se) = Free <$> simplifySubExp se
        simplifyDim (Ext x)   = return $ Ext x

simplifyType :: MonadEngine m => Type -> m Type
simplifyType (Array et shape u) = do
  dims <- mapM simplifySubExp $ shapeDims shape
  return $ Array et (Shape dims) u
simplifyType (Mem size) =
  Mem <$> simplifySubExp size
simplifyType (Basic bt) =
  return $ Basic bt

simplifyLambda :: MonadEngine m =>
                  Lambda (InnerLore m) -> [Maybe SubExp]
               -> m (Lambda (Lore m))
simplifyLambda (Lambda params body rettype loc) arrs = do
  params' <- mapM simplifyIdentBinding params
  let (nonarrayparams, arrayparams) =
        splitAt (length params' - length arrs) params'
      paramnames = HS.fromList $ map identName params'
  body' <-
    blockIf (hasFree paramnames `orIf` isUnique `orIf` isAlloc) $
    enterLoop $
    bindLParams nonarrayparams $
    bindArrayLParams (zip arrayparams arrs) $
      simplifyBody (map diet rettype) body
  rettype' <- mapM simplifyType rettype
  return $ Lambda params' body' rettype' loc

consumeResult :: MonadEngine m =>
                 [(Diet, SubExp)] -> m ()
consumeResult = mapM_ inspect
  where inspect (Consume, se) =
          traverse_ consumedName $ subExpAliases se
        inspect (Observe, _) = return ()

simplifyCerts :: MonadEngine m =>
                 Certificates -> m Certificates
simplifyCerts = liftM (nub . concat) . mapM check
  where check idd = do
          vv <- getsEngineState $ ST.lookupSubExp (identName idd) . stateVtable
          case vv of
            Just (Constant Checked _) -> return []
            Just (Var idd') -> do usedName $ identName idd'
                                  return [idd']
            _ -> do usedName $ identName idd
                    return [idd]

simplifyFun :: MonadEngine m =>
               FunDec (InnerLore m) -> m (FunDec (Lore m))
simplifyFun (FunDec fname rettype params body loc) = do
  rettype' <- simplifyRetType rettype
  body' <- insertAllBindings $ bindFParams params $
           simplifyBody (map diet $ resTypeValues rettype') body
  return $ FunDec fname rettype' params body' loc
