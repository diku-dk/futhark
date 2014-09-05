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
import qualified Data.HashSet as HS
import Data.Foldable (traverse_)

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.CSE
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Optimise.Simplifier.Apply
import Futhark.Optimise.Simplifier.TaggedBinding
import Futhark.Binder

type NeedSet lore = [TaggedBinding lore]

patNameSet :: [Ident] -> Names
patNameSet = HS.fromList . map identName

data Need lore = Need { needBindings :: NeedSet lore
                      , usageTable  :: UT.UsageTable
                      }

instance Monoid (Need lore) where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] UT.empty

data Env m = Env { envDupeState :: DupeState (Lore m)
                 , envProgram   :: Maybe (Prog (Lore m))
                 , envRules     :: RuleBook m
                 }

emptyEnv :: MonadEngine m =>
            RuleBook m
         -> Maybe (Prog (Lore m)) -> Env m
emptyEnv rules prog =
  Env { envDupeState = newDupeState
      , envProgram = prog
      , envRules = rules
      }

data State m = State { stateVtable :: ST.SymbolTable (Lore m)
                     }

emptyState :: State m
emptyState = State { stateVtable = ST.empty }

class (Proper (Lore m), MonadBinder m) => MonadEngine m where
  askEngineEnv :: m (Env m)
  localEngineEnv :: (Env m -> Env m) -> m a -> m a
  tellNeed :: Need (Lore m) -> m ()
  listenNeed :: m a -> m (a, Need (Lore m))
  getEngineState :: m (State m)
  putEngineState :: State m -> m ()
  passNeed :: m (a, Need (Lore m) -> Need (Lore m)) -> m a

  simplifyBody :: [Diet] -> Body lore -> m (Body (Lore m))
  simplifyBody = defaultSimplifyBody
  inspectBinding :: Binding (Lore m) -> m ()
  inspectBinding = defaultInspectBinding

addBindingEngine :: MonadEngine m =>
                    Binding (Lore m) -> m ()
addBindingEngine bnd = do
  vtable <- getVtable
  modifyVtable $ ST.insertBinding bnd
  needTagged $ tagBinding vtable bnd

collectBindingsEngine :: MonadEngine m =>
                         m a -> m (a, [Binding (Lore m)])
collectBindingsEngine m = passNeed $ do
  (x, need) <- listenNeed m
  return ((x, map untagBinding $ needBindings need),
          const mempty)

asksEngineEnv :: MonadEngine m => (Env m -> a) -> m a
asksEngineEnv f = f <$> askEngineEnv

getsEngineState :: MonadEngine m => (State m -> a) -> m a
getsEngineState f = f <$> getEngineState

modifyEngineState :: MonadEngine m => (State m -> State m) -> m ()
modifyEngineState f = do x <- getEngineState
                         putEngineState $ f x

needTagged :: MonadEngine m => TaggedBinding (Lore m) -> m ()
needTagged bnd = tellNeed $ Need [bnd] UT.empty

needToBinding :: TaggedBinding lore -> Binding lore
needToBinding (TaggedLet (pat, _) lore (e, _)) = Let pat lore e

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
              -> [TaggedBinding (Lore m)] -> Body (Lore m)
              -> m (Body (Lore m),
                    [TaggedBinding (Lore m)],
                    UT.UsageTable)
hoistBindings rules block vtable uses dupes needs body = do
  (uses', blocked, hoisted) <-
    simplifyBindings vtable uses $
    concat $ snd $ mapAccumL pick dupes $ inDepOrder needs
  return (insertBindings blocked body, hoisted, uses')
  where simplifyBindings vtable' uses' bnds = do
          (uses'', bnds') <- simplifyBindings' vtable' uses' bnds
          let (blocked, hoisted) = partitionEithers $ blockUnhoistedDeps bnds'
          -- We need to do a final pass to ensure that nothing is
          -- hoisted past something that it depends on.
          return (uses'', map needToBinding blocked, hoisted)

        simplifyBindings' vtable' uses' bnds =
          foldM hoistable (uses',[]) (reverse $ zip bnds vtables)
            where vtables = scanl insertBnd vtable' bnds
                  insertBnd vtable'' bnd =
                    ST.insertBinding (untagBinding bnd) vtable''

        hoistable (uses',bnds) (bnd, vtable')
          | not $ uses' `UT.contains` provides bnd = -- Dead binding.
            return (uses', bnds)
          | otherwise = do
            res <- bottomUpSimplifyBinding rules (vtable', uses') $ untagBinding bnd
            case res of
              Nothing -- Nothing to optimise - see if hoistable.
                | block uses' bnd ->
                  return ((uses' <> usage bnd) `UT.without` provides bnd,
                          Left bnd : bnds)
                | otherwise ->
                  return (uses' <> usage bnd, Right bnd : bnds)
              Just optimbnds -> do
                (uses'',bnds') <-
                  simplifyBindings' vtable' uses' $ map (tagBinding vtable') optimbnds
                return (uses'', bnds'++bnds)

        pick ds bnd =
          (ds,[bnd])

blockUnhoistedDeps :: FreeIn (Lore.Exp lore) =>
                      [Either (TaggedBinding lore) (TaggedBinding lore)]
                   -> [Either (TaggedBinding lore) (TaggedBinding lore)]
blockUnhoistedDeps = snd . mapAccumL block HS.empty
  where block blocked (Left need) =
          (blocked <> HS.fromList (provides need), Left need)
        block blocked (Right need)
          | blocked `intersects` requires need =
            (blocked <> HS.fromList (provides need), Left need)
          | otherwise =
            (blocked, Right need)

inDepOrder :: Proper lore => [TaggedBinding lore] -> [TaggedBinding lore]
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

mustPrecede :: FreeIn (Lore.Exp lore) => TaggedBinding lore -> TaggedBinding lore -> Bool
bnd1 `mustPrecede` bnd2 =
  not $ HS.null $ HS.fromList (filter (`HS.member` req2) $ provides bnd1)
                  `HS.union`
                  (consumedInBody e2 `HS.intersection` requires bnd1)
  where e2 = asTail bnd2
        req2 = requires bnd2

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

type BlockPred lore = UT.UsageTable -> TaggedBinding lore -> Bool

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
  putVtable $ foldl (flip ST.insertBinding) vtable $ map untagBinding hoistable
  return (e,
          const Need { needBindings = hoistable
                     , usageTable  = usages
                     })

insertAllBindings :: MonadEngine m => m (Body (Lore m)) -> m (Body (Lore m))
insertAllBindings = blockIf $ \_ _ -> True

hasFree :: FreeIn (Lore.Exp lore) => Names -> BlockPred lore
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred m
isNotSafe _ (TaggedLet _ _ (e,_)) = not $ safeExp e

isNotCheap :: BlockPred m
isNotCheap _ = not . cheapBnd
  where cheapBnd (TaggedLet _ _ (e,_)) = cheap e
        cheap (BinOp {})   = True
        cheap (SubExp {})  = True
        cheap (Not {})     = True
        cheap (Negate {})  = True
        cheap (DoLoop {})  = False
        cheap _            = True -- Used to be False, but mkLets try
                                  -- it out.

isConsumed :: BlockPred lore
isConsumed uses (TaggedLet (pat,_) _ _) =
  any (`UT.isConsumed` uses) $ patternNames pat

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
  putVtable $ foldl (flip ST.insertBinding) vtable $ map untagBinding hoistable
  return ((body1', body2'),
          const Need { needBindings = hoistable
                     , usageTable = f1 <> f2
                     })

-- | Simplify a single 'Body' inside an arbitrary 'MonadEngine'.
defaultSimplifyBody :: MonadEngine m =>
                       [Diet] -> Body lore -> m (Body (Lore m))

defaultSimplifyBody ds (Body [] res) =
  Body [] <$> simplifyResult ds res

defaultSimplifyBody ds (Body (bnd:bnds) res) = do
  simplifyBinding bnd
  simplifyBody ds $ Body bnds res

-- | Simplify a single 'Result' inside an arbitrary 'MonadEngine'.
simplifyResult :: MonadEngine m =>
                  [Diet] -> Result -> m Result

simplifyResult ds (Result cs es loc) = do
  es' <- mapM simplifySubExp es
  consumeResult $ zip ds es'
  Result <$> simplifyCerts cs <*> pure es' <*> pure loc

-- | Simplify the binding, adding it to the program being constructed.
simplifyBinding :: MonadEngine m =>
                   Binding lore
                -> m ()
-- The simplification rules cannot handle Apply, because it requires
-- access to the full program.  This is a bit of a hack.
simplifyBinding (Let pat _ (Apply fname args rettype loc)) = do
  pat' <- blockUsage $ simplifyPattern pat
  args' <- mapM (simplifySubExp . fst) args
  rettype' <- mapM simplifyExtType rettype
  prog <- asksEngineEnv envProgram
  vtable <- getVtable
  case join $ pure simplifyApply <*> prog <*> pure vtable <*> pure fname <*> pure args of
    -- Array values are non-unique, so we may need to copy them.
    Just vs -> do let vs' = valueShapeContext rettype vs ++ vs
                  bnds <- forM (zip (patternIdents pat) vs') $ \(p,v) ->
                    case uniqueness $ identType p of
                      Unique    -> mkLetM [p] $ Copy (Constant v loc) loc
                      Nonunique -> mkLetM [p] $ SubExp $ Constant v loc
                  mapM_ inspectBinding bnds
    Nothing -> do let e' = Apply fname (zip args' $ map snd args) rettype' loc
                  lore <- loreForExpM e'
                  inspectBinding $ Let pat' lore e'

simplifyBinding (Let pat _ e) = do
  pat' <- blockUsage $ simplifyPattern pat
  e' <- simplifyExp e
  lore <- loreForExpM e'
  inspectBinding $ Let pat' lore e'

defaultInspectBinding :: MonadEngine m =>
                         Binding (Lore m) -> m ()

defaultInspectBinding bnd = do
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  simplified <- topDownSimplifyBinding rules vtable bnd
  case simplified of
    Just newbnds -> mapM_ inspectBinding newbnds
    Nothing      -> addBinding bnd

simplifyExp :: MonadEngine m => Exp lore -> m (Exp (Lore m))

simplifyExp (DoLoop respat merge loopvar boundexp loopbody loc) = do
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

simplifyExp (Map cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Map cs' fun' arrs' loc

simplifyExp (Filter cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Filter cs' fun' arrs' loc

simplifyExp (Reduce cs fun input loc) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Reduce cs' fun' (zip acc' arrs') loc

simplifyExp (Scan cs fun input loc) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Scan cs' fun' (zip acc' arrs') loc

simplifyExp (Redomap cs outerfun innerfun acc arrs loc) = do
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
                 letBind [input] $ Iota outerSize $ srclocOf firstarr
                 return (lam { lambdaParams =
                                  accparams ++
                                  [firstparam { identType = Basic Int }] },
                         [Var input])
               (arrparams', arrinps') ->
                 return (lam { lambdaParams = accparams ++ arrparams' }, arrinps')
          | otherwise = return (lam, arrinps)

simplifyExp e = simplifyExpBase e

simplifyExpBase :: MonadEngine m => Exp lore -> m (Exp (Lore m))
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

simplifyPattern :: MonadEngine m => Pattern lore -> m (Pattern (Lore m))
simplifyPattern pat =
  mkPatternM =<< mapM simplifyIdentBinding (patternIdents pat)

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
                   TypeBase als ExtShape -> m (TypeBase als ExtShape)
simplifyExtType t = do dims <- mapM simplifyDim $ extShapeDims $ arrayShape t
                       return $ t `setArrayShape` ExtShape dims
  where simplifyDim (Free se) = Free <$> simplifySubExp se
        simplifyDim (Ext x)   = return $ Ext x

simplifyType :: MonadEngine m =>
                TypeBase als Shape -> m (TypeBase als Shape)
simplifyType t = do dims <- mapM simplifySubExp $ arrayDims t
                    return $ t `setArrayShape` Shape dims

simplifyLambda :: MonadEngine m =>
                  Lambda lore -> [SubExp] -> m (Lambda (Lore m))
simplifyLambda (Lambda params body rettype loc) arrs = do
  body' <-
    blockIf (hasFree params' `orIf` isConsumed) $
    descendIntoLoop $
    bindParams nonarrayparams $
    bindArrayParams (zip arrayparams arrs) $
      simplifyBody (map diet rettype) body
  rettype' <- mapM simplifyType rettype
  return $ Lambda params body' rettype' loc
  where params' = patNameSet $ map fromParam params
        (nonarrayparams, arrayparams) =
          splitAt (length params - length arrs) params

consumeResult :: MonadEngine m =>
                 [(Diet, SubExp)] -> m ()
consumeResult = mapM_ inspect
  where inspect (Consume, se) =
          traverse_ consumedName $ aliases $ subExpType se
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
                                  (Env (SimpleM lore)) -- Reader
                                  (Need lore)          -- Writer
                                  (State (SimpleM lore), NameSource VName)   -- State
                                  a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Need lore),
            MonadReader (Env (SimpleM lore)),
            MonadState (State (SimpleM lore), NameSource VName))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance (Proper lore, Bindable lore) =>
         MonadBinder (SimpleM lore) where
  addBinding      = addBindingEngine
  collectBindings = collectBindingsEngine

instance (Proper lore, Bindable lore) => MonadEngine (SimpleM lore) where
  askEngineEnv = ask
  localEngineEnv = local
  tellNeed = tell
  listenNeed = listen
  getEngineState   = fst <$> get
  putEngineState x = modify $ \(_, y) -> (x,y)
  passNeed = pass
  simplifyBody = defaultSimplifyBody

instance (Proper lore, Bindable lore) => ProperBinder (SimpleM lore) where

instance (Proper lore, Bindable lore) => BindableM (SimpleM lore) where
  type Lore (SimpleM lore) = lore
  loreForExpM = return . loreForExp
  loreForBindingM = return . loreForBinding . (Const :: Ident -> Const Ident lore)

runSimpleM :: SimpleM lore a
           -> Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) env src = let (x, (_, src'), _) = runRWS m env (emptyState, src)
                                 in (x, src')

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyProg :: (Proper lore, Bindable lore) =>
                RuleBook (SimpleM lore)
             -> Prog lore -> Prog lore
simplifyProg rules prog =
  Prog $ fst $ runSimpleM (mapM simplifyFun $ progFunctions prog)
               (emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

simplifyFun :: (Proper lore, Bindable lore) =>
               FunDec oldlore -> SimpleM lore (FunDec lore)
simplifyFun (fname, rettype, params, body, loc) = do
  body' <- insertAllBindings $ bindParams params $
           simplifyBody (map diet rettype) body
  return (fname, rettype, params, body', loc)

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyOneFun :: (MonadFreshNames m, Proper lore, Bindable lore) =>
                  RuleBook (SimpleM lore)
               -> FunDec oldlore -> m (FunDec lore)
simplifyOneFun rules fundec =
  modifyNameSource $ runSimpleM (simplifyFun fundec) (emptyEnv rules Nothing)

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: (MonadFreshNames m, Proper lore, Bindable lore) =>
                     RuleBook (SimpleM lore)
                  -> Maybe (Prog lore) -> Lambda oldlore
                  -> m (Lambda lore)
simplifyOneLambda rules prog lam = do
  let simplifyOneLambda' = insertAllBindings $
                           bindParams (lambdaParams lam) $
                           simplifyBody (map diet $ lambdaReturnType lam) $ lambdaBody lam
  body' <- modifyNameSource $ runSimpleM simplifyOneLambda' $ emptyEnv rules prog
  return $ lam { lambdaBody = body' }
