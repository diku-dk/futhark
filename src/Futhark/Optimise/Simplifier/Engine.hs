{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
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
       , simplifyBinding
       , simplifyResult
       , simplifyExp
       , defaultInspectBinding
         -- * Simple interface
       , simplifyProg
       , simplifyOneFun
       , simplifyOneLambda
       ) where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.RWS

import Data.Either
import Data.Graph
import Data.Hashable
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Foldable (traverse_)

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

patNameSet :: [Ident] -> Names
patNameSet = HS.fromList . map identName

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

class (Proper (Lore m), MonadBinder m,
       Lore m ~ Aliases (InnerLore m)) => MonadEngine m where
  type InnerLore m
  askEngineEnv :: m (Env m)
  localEngineEnv :: (Env m -> Env m) -> m a -> m a
  tellNeed :: Need (Lore m) -> m ()
  listenNeed :: m a -> m (a, Need (Lore m))
  getEngineState :: m (State m)
  putEngineState :: State m -> m ()
  passNeed :: m (a, Need (Lore m) -> Need (Lore m)) -> m a

  simplifyBody :: [Diet] -> Body (InnerLore m) -> m (Body (Lore m))
  simplifyBody = defaultSimplifyBody
  inspectBinding :: Binding (Lore m) -> m ()
  inspectBinding = defaultInspectBinding

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

descendIntoLoop :: MonadEngine m => m a -> m a
descendIntoLoop = localVtable ST.deepen

bindParams :: MonadEngine m =>
              [Param] -> m a -> m a
bindParams params =
  localVtable $ \vtable ->
    foldr ST.insertParam vtable params

bindArrayParams :: MonadEngine m =>
                   [(Param,SubExp)] -> m a -> m a
bindArrayParams params =
  localVtable $ \vtable ->
    foldr (uncurry ST.insertArrayParam) vtable params

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
              -> [Binding (Lore m)] -> Body (Lore m)
              -> m (Body (Lore m),
                    [Binding (Lore m)],
                    UT.UsageTable)
hoistBindings rules block vtable uses dupes needs body = do
  (uses', blocked, hoisted) <-
    simplifyBindings vtable uses $
    concat $ snd $ mapAccumL pick dupes $ inDepOrder needs
  body' <- insertBindingsM $ do
    mapM_ addBinding blocked
    return body
  return (body', hoisted, uses')
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
          [ (bnd, representative $ provides bnd, deps) |
            bnd <- bnds,
            let deps = [ representative $ provides dep
                         | dep <- bnds, dep `mustPrecede` bnd ] ]

        -- As all names are unique, a pattern can be uniquely
        -- represented by any of its names.  If the pattern has no
        -- names, then it doesn't matter anyway.
        representative []    = Nothing
        representative (x:_) = Just x

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
  (mconcat (map freeNamesIn $ patternIdents $ bindingPattern bnd)
  `HS.difference` HS.fromList (provides bnd)) <>
  freeNamesInExp (bindingExp bnd)

usage :: (Proper lore, Aliased lore) => Binding lore -> UT.UsageTable
usage = usageInBinding

expandUsage :: (Proper lore, Aliased lore) =>
               UT.UsageTable -> Binding lore -> UT.UsageTable
expandUsage utable bnd = utable <> usage bnd <> usageThroughAliases
  where e = bindingExp bnd
        aliases = aliasesOf $ bindingExp bnd
        tagged = zip (drop (contextSize $ typeOf e) $ provides bnd) aliases
        usageThroughAliases = mconcat $ catMaybes $ do
          (name,als) <- tagged
          return $ do uses <- UT.lookup name utable
                      return $ mconcat $ map (`UT.usage` uses) $ HS.toList als


intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

type BlockPred lore = UT.UsageTable -> Binding lore -> Bool

orIf :: BlockPred lore -> BlockPred lore -> BlockPred lore
orIf p1 p2 body need = p1 body need || p2 body need

blockIfSeq :: MonadEngine m => [BlockPred (Lore m)] -> m (Body (Lore m)) -> m (Body (Lore m))
blockIfSeq ps m = foldl (flip blockIf) m ps

blockIf :: MonadEngine m => BlockPred (Lore m)
        -> m (Body (Lore m)) -> m (Body (Lore m))
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

insertAllBindings :: MonadEngine m => m (Body (Lore m)) -> m (Body (Lore m))
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

isConsumed :: BlockPred lore
isConsumed uses =
  any (`UT.isConsumed` uses) . patternNames . bindingPattern

hoistCommon :: MonadEngine m =>
               m (Body (Lore m))
            -> (ST.SymbolTable (Lore m)
                -> ST.SymbolTable (Lore m))
            -> m (Body (Lore m))
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
                       [Diet] -> Body (InnerLore m) -> m (Body (Lore m))

defaultSimplifyBody ds (Body _ [] res) =
  mkBodyM [] =<< simplifyResult ds res

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

-- | Simplify the binding, adding it to the program being constructed.
simplifyBinding :: MonadEngine m =>
                   Binding (InnerLore m)
                -> m ()
-- The simplification rules cannot handle Apply, because it requires
-- access to the full program.  This is a bit of a hack.
simplifyBinding (Let pat _ (Apply fname args rettype loc)) = do
  args' <- mapM (simplifySubExp . fst) args
  rettype' <- mapM simplifyExtType rettype
  prog <- asksEngineEnv envProgram
  vtable <- getVtable
  case join $ pure simplifyApply <*> prog <*> pure vtable <*> pure fname <*> pure args of
    -- Array values are non-unique, so we may need to copy them.
    Just vs -> do let vs' = valueShapeContext rettype vs ++ vs
                  bnds <- forM (zip (patternIdents pat) vs') $ \(p,v) ->
                    case uniqueness $ identType p of
                      Unique    -> mkLetM [p] $ PrimOp $ Copy (Constant v loc) loc
                      Nonunique -> mkLetM [p] $ PrimOp $ SubExp $ Constant v loc
                  mapM_ (simplifyBinding . Aliases.removeBindingAliases) bnds
    Nothing -> do let e' = Apply fname (zip args' $ map snd args) rettype' loc
                  pat' <- blockUsage $ simplifyPattern pat
                  inspectBinding =<< mkLetM pat' e'

simplifyBinding (Let pat _ e) = do
  e' <- simplifyExp e
  pat' <- blockUsage $ simplifyPattern pat
  inspectBinding =<< mkLetM pat' e'

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

simplifyExp (If cond tbranch fbranch t loc) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We also try to do CSE across
  -- branches.
  cond' <- simplifySubExp cond
  t' <- mapM simplifyExtType t
  (tbranch',fbranch') <-
    hoistCommon (simplifyBody (map diet t') tbranch) (ST.updateBounds True cond)
                (simplifyBody (map diet t') fbranch) (ST.updateBounds False cond)
  return $ If cond' tbranch' fbranch' t' loc

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
                }

simplifyLoopOp :: MonadEngine m => LoopOp (InnerLore m) -> m (LoopOp (Lore m))

simplifyLoopOp (DoLoop respat merge loopvar boundexp loopbody loc) = do
  let (mergepat, mergeexp) = unzip merge
  respat'   <- mapM simplifyIdentBinding respat
  mergepat' <- mapM simplifyIdentBinding mergepat
  mergeexp' <- mapM simplifySubExp mergeexp
  boundexp' <- simplifySubExp boundexp
  let diets = map (diet . identType) mergepat'
  -- Blocking hoisting of all unique bindings is probably too
  -- conservative, but there is currently no nice way to mark
  -- consumption of the loop body result.
  loopbody' <- blockIfSeq [hasFree boundnames, isConsumed] $
               descendIntoLoop $ bindLoopVar loopvar boundexp' $
               simplifyBody diets loopbody
  let merge' = zip mergepat' mergeexp'
  consumeResult $ zip diets mergeexp'
  return $ DoLoop respat' merge' loopvar boundexp' loopbody' loc
  where boundnames = identName loopvar `HS.insert`
                     patNameSet (map fst merge)

simplifyLoopOp (Map cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Map cs' fun' arrs' loc

simplifyLoopOp (Filter cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Filter cs' fun' arrs' loc

simplifyLoopOp (Reduce cs fun input loc) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Reduce cs' fun' (zip acc' arrs') loc

simplifyLoopOp (Scan cs fun input loc) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Scan cs' fun' (zip acc' arrs') loc

simplifyLoopOp (Redomap cs outerfun innerfun acc arrs loc) = do
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  outerfun' <- simplifyLambda outerfun []
  (innerfun', used) <- tapUsage $ simplifyLambda innerfun arrs
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
                 letBind [input] $ PrimOp $ Iota outerSize $ srclocOf firstarr
                 return (lam { lambdaParams =
                                  accparams ++
                                  [firstparam { identType = Basic Int }] },
                         [Var input])
               (arrparams', arrinps') ->
                 return (lam { lambdaParams = accparams ++ arrparams' }, arrinps')
          | otherwise = return (lam, arrinps)

simplifySubExp :: MonadEngine m => SubExp -> m SubExp
simplifySubExp (Var ident@(Ident vnm _ loc)) = do
  bnd <- getsEngineState $ ST.lookupSubExp vnm . stateVtable
  case bnd of
    Just (Constant v _)
      | isBasicTypeVal v  -> return $ Constant v loc
    Just (Var id') -> do usedName $ identName id'
                         return $ Var id'
    _              -> Var <$> simplifyIdent ident
  where isBasicTypeVal = basicType . valueType
simplifySubExp (Constant v loc) = return $ Constant v loc

simplifyPattern :: MonadEngine m => Pattern lore -> m [Ident]
simplifyPattern pat =
  mapM simplifyIdentBinding (patternIdents pat)

simplifyIdentBinding :: MonadEngine m => Ident -> m Ident
simplifyIdentBinding v = do
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyIdent :: MonadEngine m => Ident -> m Ident
simplifyIdent v = do
  usedName $ identName v
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyExtType :: MonadEngine m =>
                   TypeBase ExtShape -> m (TypeBase ExtShape)
simplifyExtType t = do dims <- mapM simplifyDim $ extShapeDims $ arrayShape t
                       return $ t `setArrayShape` ExtShape dims
  where simplifyDim (Free se) = Free <$> simplifySubExp se
        simplifyDim (Ext x)   = return $ Ext x

simplifyType :: MonadEngine m => Type -> m Type
simplifyType t = do dims <- mapM simplifySubExp $ arrayDims t
                    return $ t `setArrayShape` Shape dims

simplifyLambda :: MonadEngine m =>
                  Lambda (InnerLore m) -> [SubExp]
               -> m (Lambda (Lore m))
simplifyLambda (Lambda params body rettype loc) arrs = do
  body' <-
    blockIf (hasFree params' `orIf` isConsumed) $
    descendIntoLoop $
    bindParams nonarrayparams $
    bindArrayParams (zip arrayparams arrs) $
      simplifyBody (map diet rettype) body
  rettype' <- mapM simplifyType rettype
  return $ Lambda params body' rettype' loc
  where params' = patNameSet params
        (nonarrayparams, arrayparams) =
          splitAt (length params - length arrs) params

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
            Just (Constant (BasicVal Checked) _) -> return []
            Just (Var idd') -> do usedName $ identName idd'
                                  return [idd']
            _ -> do usedName $ identName idd
                    return [idd]

-- Simple implementation of the MonadEngine class, and simple
-- interface to running the simplifier on various things.

newtype SimpleM lore a = SimpleM (RWS
                                  (Env (SimpleM lore))                     -- Reader
                                  (Need (Aliases lore))                    -- Writer
                                  (State (SimpleM lore), NameSource VName) -- State
                                  a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Need (Aliases lore)),
            MonadReader (Env (SimpleM lore)),
            MonadState (State (SimpleM lore), NameSource VName))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance (Proper lore, Bindable lore) =>
         MonadBinder (SimpleM lore) where
  addBinding      = addBindingEngine
  collectBindings = collectBindingsEngine

instance (Proper lore, Bindable lore) =>
         MonadEngine (SimpleM lore) where
  type InnerLore (SimpleM lore) = lore
  askEngineEnv = ask
  localEngineEnv = local
  tellNeed = tell
  listenNeed = listen
  getEngineState   = fst <$> get
  putEngineState x = modify $ \(_, y) -> (x,y)
  passNeed = pass
  simplifyBody = defaultSimplifyBody

instance (Proper lore, Bindable lore) => BindableM (SimpleM lore) where
  type Lore (SimpleM lore) = Aliases lore
  mkLetM pat e = do
    let Let pat' explore _ = mkLet pat $ Aliases.removeExpAliases e
    return $ Aliases.mkAliasedLetBinding pat' explore e
  mkBodyM bnds res = do
    let Body bodylore _ _ = mkBody (map Aliases.removeBindingAliases bnds) res
    return $ Aliases.mkAliasedBody bodylore bnds res

runSimpleM :: SimpleM lore a
           -> Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) env src = let (x, (_, src'), _) = runRWS m env (emptyState, src)
                                 in (x, src')

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: (Proper lore, Bindable lore) =>
                RuleBook (SimpleM lore)
             -> Prog lore -> Prog (Aliases lore)
simplifyProg rules prog =
  Prog $ fst $ runSimpleM (mapM simplifyFun $ progFunctions prog)
               (emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

simplifyFun :: MonadEngine m =>
               FunDec (InnerLore m) -> m (FunDec (Lore m))
simplifyFun (FunDec fname rettype params body loc) = do
  body' <- insertAllBindings $ bindParams (map bindeeIdent params) $
           simplifyBody (map diet rettype) body
  return $ FunDec fname rettype params body' loc

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyOneFun :: (MonadFreshNames m, Proper lore,
                   Bindable lore) =>
                  RuleBook (SimpleM lore)
               -> FunDec lore -> m (FunDec (Aliases lore))
simplifyOneFun rules fundec =
  modifyNameSource $ runSimpleM (simplifyFun fundec) (emptyEnv rules Nothing)

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: (MonadFreshNames m,
                      Proper lore, Bindable lore) =>
                     RuleBook (SimpleM lore)
                  -> Maybe (Prog lore) -> Lambda lore
                  -> m (Lambda (Aliases lore))
simplifyOneLambda rules prog lam = do
  let simplifyOneLambda' = insertAllBindings $
                           bindParams (lambdaParams lam) $
                           simplifyBody (map diet $ lambdaReturnType lam) $ lambdaBody lam
  body' <- modifyNameSource $ runSimpleM simplifyOneLambda' $ emptyEnv rules prog
  return $ lam { lambdaBody = body' }
