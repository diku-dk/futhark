{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
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
       , Env (envVtable)
       , Need
       , emptyEnv
       , asksEngineEnv
       , localVtable
       , insertAllBindings
       , defaultSimplifyBody
       , simplifyBinding
       , simplifyResult
       , bindLet
       , bindLetWith
         -- * Simple interface
       , simplifyProg
       , simplifyOneFun
       , simplifyOneLambda
       , simplifyOneBody
       ) where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Either
import Data.Graph
import Data.Hashable
import Data.List
import Data.Loc
import qualified Data.HashSet as HS
import Data.Foldable (traverse_)

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.CSE
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Optimise.Simplifier.Apply
import Futhark.Optimise.Simplifier.TaggedBinding
import Futhark.Substitute

type NeedSet u = [TaggedBinding u]

patNameSet :: [Ident] -> Names
patNameSet = HS.fromList . map identName

data Need u = Need { needBindings :: NeedSet u
                   , usageTable  :: UT.UsageTable
                   }

instance Monoid (Need u) where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] UT.empty

data Env u = Env { envDupeState :: DupeState
                 , envVtable    :: ST.SymbolTable u
                 , envProgram   :: Maybe Prog
                 , envRules     :: RuleBook u
                 }

emptyEnv :: ST.UserAnnotation u =>
            RuleBook u -> Maybe Prog -> Env u
emptyEnv rules prog =
  Env { envDupeState = newDupeState
      , envVtable = ST.empty
      , envProgram = prog
      , envRules = rules
      }

class (MonadFreshNames m, ST.UserAnnotation u) => MonadEngine u m | m -> u where
  askEngineEnv :: m (Env u)
  localEngineEnv :: (Env u -> Env u) -> m a -> m a
  tellNeed :: Need u -> m ()
  listenNeed :: m a -> m (a, Need u)
  passNeed :: m (a, Need u -> Need u) -> m a

  simplifyBody :: MonadEngine u m => Body -> m Body

asksEngineEnv :: MonadEngine u m => (Env u -> a) -> m a
asksEngineEnv f = f <$> askEngineEnv

descendIntoLoop :: MonadEngine u m => m a -> m a
descendIntoLoop = localEngineEnv $ \env -> env { envVtable = ST.deepen $ envVtable env }

needTagged :: MonadEngine u m => TaggedBinding u -> m ()
needTagged bnd = tellNeed $ Need [bnd] UT.empty

needUntagged :: MonadEngine u m => Binding -> m ()
needUntagged bnd = do
  vtable <- asksEngineEnv envVtable
  needTagged $ tagBinding vtable bnd

needToBinding :: TaggedBinding u -> Binding
needToBinding (TaggedLet (pat, _) (e, _)) = Let pat e

boundFree :: MonadEngine u m => Names -> m ()
boundFree fs = tellNeed $ Need [] $ UT.usages fs

usedName :: MonadEngine u m => VName -> m ()
usedName = boundFree . HS.singleton

consumedName :: MonadEngine u m => VName -> m ()
consumedName = tellNeed . Need [] . UT.consumedUsage

tapUsage :: MonadEngine u m => m a -> m (a, UT.UsageTable)
tapUsage m = do (x,needs) <- listenNeed m
                return (x, usageTable needs)

blockUsage :: MonadEngine u m => m a -> m a
blockUsage m = passNeed $ do
  (x, _) <- listenNeed m
  return (x, const mempty)

localVtable :: MonadEngine u m =>
               (ST.SymbolTable u -> ST.SymbolTable u) -> m a -> m a
localVtable f = localEngineEnv $ \env -> env { envVtable = f $ envVtable env }

binding :: MonadEngine u m =>
           [TaggedBinding u] -> m a -> m a
binding bnds =
  localVtable $ ST.insertEntries $ concatMap bindingEntries bnds

bindParams :: MonadEngine u m =>
              [Param] -> m a -> m a
bindParams params =
  localVtable $ \vtable ->
    foldr ST.insertParam vtable params

bindArrayParams :: MonadEngine u m =>
                   [(Param,SubExp)] -> m a -> m a
bindArrayParams params =
  localVtable $ \vtable ->
    foldr (uncurry ST.insertArrayParam) vtable params

bindLoopVar :: MonadEngine u m => Ident -> SubExp -> m a -> m a
bindLoopVar var bound =
  localVtable $ clampUpper . clampVar
  where clampVar = ST.insertLoopVar (identName var) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampUpper = case bound of Var v -> ST.isAtLeast (identName v) 1
                                   _     -> id

bindLet :: MonadEngine u m => [Ident] -> Exp -> m a -> m a

bindLet pat e m = do
  ds <- asksEngineEnv envDupeState
  vtable <- asksEngineEnv envVtable
  let (bnds, ds') = performCSE ds $ tagBinding vtable $ Let pat e
  mapM_ needTagged bnds
  binding bnds $
    localEngineEnv (\env -> env { envDupeState = ds'}) m

-- | As 'bindLet', but use a different annotation function in the
-- symbol table when creating just this binding.
bindLetWith :: MonadEngine u m =>
               (ST.Entry () -> u) -> [Ident] -> Exp -> m a -> m a
bindLetWith f pat e m = do
  origAnnotationFun <- asksEngineEnv $ ST.annotationFunction . envVtable
  localEngineEnv (setAnnotationFun f) $
    bindLet pat e $
    localEngineEnv (setAnnotationFun origAnnotationFun) m
  where setAnnotationFun g env =
          env { envVtable = (envVtable env) { ST.annotationFunction = g } }

hoistBindings :: (Substitute u, MonadFreshNames m) =>
                 RuleBook u -> BlockPred u
              -> ST.SymbolTable u -> UT.UsageTable -> DupeState
              -> [TaggedBinding u] -> Body
              -> m (Body, [TaggedBinding u], UT.UsageTable)
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
          let (bnds,ds') = performCSE ds bnd
          in (ds', bnds)

blockUnhoistedDeps :: [Either (TaggedBinding u) (TaggedBinding u)]
                   -> [Either (TaggedBinding u) (TaggedBinding u)]
blockUnhoistedDeps = snd . mapAccumL block HS.empty
  where block blocked (Left need) =
          (blocked <> HS.fromList (provides need), Left need)
        block blocked (Right need)
          | blocked `intersects` requires need =
            (blocked <> HS.fromList (provides need), Left need)
          | otherwise =
            (blocked, Right need)

inDepOrder :: [TaggedBinding u] -> [TaggedBinding u]
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

mustPrecede :: TaggedBinding u -> TaggedBinding u -> Bool
bnd1 `mustPrecede` bnd2 =
  not $ HS.null $ HS.fromList (filter (`HS.member` req2) $ provides bnd1)
                  `HS.union`
                  (consumedInBody e2 `HS.intersection` requires bnd1)
  where e2 = asTail bnd2
        req2 = requires bnd2

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

type BlockPred u = UT.UsageTable -> TaggedBinding u -> Bool

orIf :: BlockPred u -> BlockPred u -> BlockPred u
orIf p1 p2 body need = p1 body need || p2 body need

blockIfSeq :: MonadEngine u m => [BlockPred u] -> m Body -> m Body
blockIfSeq ps m = foldl (flip blockIf) m ps

blockIf :: MonadEngine u m => BlockPred u -> m Body -> m Body
blockIf block m = passNeed $ do
  (body, needs) <- listenNeed m
  ds <- asksEngineEnv envDupeState
  vtable <- asksEngineEnv envVtable
  rules <- asksEngineEnv envRules
  (e, hoistable, usages) <-
    hoistBindings rules block vtable (usageTable needs) ds (needBindings needs) body
  return (e,
          const Need { needBindings = hoistable
                     , usageTable  = usages
                     })

insertAllBindings :: MonadEngine u m => m Body -> m Body
insertAllBindings = blockIf $ \_ _ -> True

hasFree :: Names -> BlockPred u
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred u
isNotSafe _ (TaggedLet _ (e,_)) = not $ safeExp e

isNotCheap :: BlockPred u
isNotCheap _ = not . cheapBnd
  where cheapBnd (TaggedLet _ (e,_)) = cheap e
        cheap (BinOp {})   = True
        cheap (SubExp {})  = True
        cheap (Not {})     = True
        cheap (Negate {})  = True
        cheap (DoLoop {})  = False
        cheap _            = True -- Used to be False, but let's try
                                  -- it out.

isConsumed :: BlockPred u
isConsumed uses (TaggedLet (pat,_) _) =
  any ((`UT.isConsumed` uses) . identName) pat

hoistCommon :: MonadEngine u m =>
               m Body -> (ST.SymbolTable u -> ST.SymbolTable u)
            -> m Body -> (ST.SymbolTable u -> ST.SymbolTable u)
            -> m (Body, Body)
hoistCommon m1 vtablef1 m2 vtablef2 = passNeed $ do
  (body1, needs1) <- listenNeed $ localVtable vtablef1 m1
  (body2, needs2) <- listenNeed $ localVtable vtablef2 m2
  let block = isNotSafe `orIf` isNotCheap
  vtable <- asksEngineEnv envVtable
  rules <- asksEngineEnv envRules
  (body1', safe1, f1) <-
    localVtable vtablef1 $
    hoistBindings rules block vtable (usageTable needs1)
    newDupeState (needBindings needs1) body1
  (body2', safe2, f2) <-
    localVtable vtablef2 $
    hoistBindings rules block vtable (usageTable needs2)
    newDupeState (needBindings needs2) body2
  return ((body1', body2'),
          const Need { needBindings = safe1 <> safe2
                     , usageTable = f1 <> f2
                     })

-- | Simplify a single 'Body' inside an arbitrary 'MonadEngine'.
defaultSimplifyBody :: MonadEngine u m => Body -> m Body

defaultSimplifyBody (Body [] res) =
  Body [] <$> simplifyResult res

defaultSimplifyBody (Body (bnd:bnds) res) = do
  (hoisted, simplified) <- simplifyBinding bnd
  localVtable (ST.insertEntries hoisted) $
    case simplified of
      Left newbnds ->
        simplifyBody $ Body (newbnds++bnds) res
      Right (Let pat' e') ->
        bindLet pat' e' $ simplifyBody $ Body bnds res

-- | Simplify a single 'Result' inside an arbitrary 'MonadEngine'.
simplifyResult :: MonadEngine u m => Result -> m Result

simplifyResult (Result cs es loc) =
  Result <$> simplifyCerts cs <*>
             mapM simplifySubExp es <*> pure loc

-- | Simplify the binding, but also return anything that has been
-- hoisted out (as a 'Left' value).
simplifyBinding :: MonadEngine u m =>
                   Binding -> m ([(VName, ST.Entry u)],
                                 Either [Binding] Binding)
simplifyBinding bnd = do
  (bnd', need) <- listenNeed $ simplifyBinding' bnd
  return (concatMap bindingEntries $ needBindings need,
          bnd')

simplifyBinding' :: MonadEngine u m =>
                   Binding -> m (Either [Binding] Binding)

-- The simplification rules cannot handle Apply, because it requires
-- access to the full program.
simplifyBinding' (Let pat (Apply fname args rettype loc)) = do
  pat' <- blockUsage $ mapM simplifyIdentBinding pat
  args' <- mapM (simplifySubExp . fst) args
  rettype' <- mapM simplifyExtType rettype
  prog <- asksEngineEnv envProgram
  vtable <- asksEngineEnv envVtable
  case join $ pure simplifyApply <*> prog <*> pure vtable <*> pure fname <*> pure args of
    -- Array values are non-unique, so we may need to copy them.
    Just vs -> do let vs' = valueShapeContext rettype vs ++ vs
                      newbnds = flip map (zip pat vs') $ \(p,v) ->
                        case uniqueness $ identType p of
                          Unique    -> Let [p] $ Copy (Constant v loc) loc
                          Nonunique -> Let [p] $ SubExp $ Constant v loc
                  return $ Left newbnds
    Nothing -> let e' = Apply fname (zip args' $ map snd args) rettype' loc
               in return $ Right $ Let pat' e'

simplifyBinding' (Let pat e) = do
  pat' <- blockUsage $ mapM simplifyIdentBinding pat
  e' <- simplifyExp e
  vtable <- asksEngineEnv envVtable
  rules <- asksEngineEnv envRules
  simplified <- topDownSimplifyBinding rules vtable (Let pat e')
  case simplified of
    Just newbnds -> return $ Left newbnds
    Nothing      -> return $ Right $ Let pat' e'

simplifyExp :: MonadEngine u m => Exp -> m Exp

simplifyExp (DoLoop respat merge loopvar boundexp loopbody loc) = do
  let (mergepat, mergeexp) = unzip merge
  respat'   <- mapM simplifyIdentBinding respat
  mergepat' <- mapM simplifyIdentBinding mergepat
  mergeexp' <- mapM simplifySubExp mergeexp
  boundexp' <- simplifySubExp boundexp
  -- Blocking hoisting of all unique bindings is probably too
  -- conservative, but there is currently no nice way to mark
  -- consumption of the loop body result.
  loopbody' <- descendIntoLoop $ bindLoopVar loopvar boundexp' $
               blockIfSeq [hasFree boundnames, isConsumed] $
               simplifyBody loopbody
  let merge' = zip mergepat' mergeexp'
  consumeResult $ zip (map identType mergepat') mergeexp'
  return $ DoLoop respat' merge' loopvar boundexp' loopbody' loc
  where boundnames = identName loopvar `HS.insert`
                     patNameSet (map fst merge)

simplifyExp (If cond tbranch fbranch t loc) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We also try to do CSE across
  -- branches.
  cond' <- simplifySubExp cond
  (tbranch',fbranch') <-
    hoistCommon (simplifyBody tbranch) (ST.updateBounds True cond)
                (simplifyBody fbranch) (ST.updateBounds False cond)
  t' <- mapM simplifyExtType t
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
                 needUntagged $ Let [input] $ Iota outerSize $ srclocOf firstarr
                 return (lam { lambdaParams =
                                  accparams ++
                                  [firstparam { identType = Basic Int }] },
                         [Var input])
               (arrparams', arrinps') ->
                 return (lam { lambdaParams = accparams ++ arrparams' }, arrinps')
          | otherwise = return (lam, arrinps)

simplifyExp e = simplifyExpBase e

simplifyExpBase :: MonadEngine u m => Exp -> m Exp
simplifyExpBase = mapExpM hoist
  where hoist = Mapper {
                  mapOnExp = simplifyExp
                , mapOnBody = simplifyBody
                , mapOnSubExp = simplifySubExp
                -- Lambdas are handled explicitly because we need to
                -- bind their parameters.
                , mapOnLambda = fail "Unhandled lambda in simplification engine."
                , mapOnIdent = simplifyIdent
                , mapOnType = simplifyType
                , mapOnValue = return
                , mapOnCertificates = simplifyCerts
                }

simplifySubExp :: MonadEngine u m => SubExp -> m SubExp
simplifySubExp (Var ident@(Ident vnm _ loc)) = do
  bnd <- asksEngineEnv $ ST.lookupSubExp vnm . envVtable
  case bnd of
    Just (Constant v _)
      | isBasicTypeVal v  -> return $ Constant v loc
    Just (Var id') -> do usedName $ identName id'
                         return $ Var id'
    _              -> Var <$> simplifyIdent ident
  where isBasicTypeVal = basicType . valueType
simplifySubExp (Constant v loc) = return $ Constant v loc

simplifyIdentBinding :: MonadEngine u m => Ident -> m Ident
simplifyIdentBinding v = do
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyIdent :: MonadEngine u m => Ident -> m Ident
simplifyIdent v = do
  usedName $ identName v
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyExtType :: MonadEngine u m =>
                   TypeBase als ExtShape -> m (TypeBase als ExtShape)
simplifyExtType t = do dims <- mapM simplifyDim $ extShapeDims $ arrayShape t
                       return $ t `setArrayShape` ExtShape dims
  where simplifyDim (Free se) = Free <$> simplifySubExp se
        simplifyDim (Ext x)   = return $ Ext x

simplifyType :: MonadEngine u m =>
                TypeBase als Shape -> m (TypeBase als Shape)
simplifyType t = do dims <- mapM simplifySubExp $ arrayDims t
                    return $ t `setArrayShape` Shape dims

simplifyLambda :: MonadEngine u m =>
                  Lambda -> [SubExp] -> m Lambda
simplifyLambda (Lambda params body rettype loc) arrs = do
  body' <-
    blockIf (hasFree params' `orIf` isConsumed) $
    descendIntoLoop $
    bindParams nonarrayparams $
    bindArrayParams (zip arrayparams arrs) $ do
      consumeResult $ zip rettype $ resultSubExps $ bodyResult body
      simplifyBody body
  rettype' <- mapM simplifyType rettype
  return $ Lambda params body' rettype' loc
  where params' = patNameSet $ map fromParam params
        (nonarrayparams, arrayparams) =
          splitAt (length params - length arrs) params

consumeResult :: MonadEngine u m =>
                 [(TypeBase als shape, SubExp)] -> m ()
consumeResult = mapM_ inspect
  where inspect (t, se)
          | unique t = traverse_ consumedName $ aliases $ subExpType se
          | otherwise = return ()

simplifyCerts :: MonadEngine u m =>
                 Certificates -> m Certificates
simplifyCerts = liftM (nub . concat) . mapM check
  where check idd = do
          vv <- asksEngineEnv $ ST.lookupSubExp (identName idd) . envVtable
          case vv of
            Just (Constant (BasicVal Checked) _) -> return []
            Just (Var idd') -> do usedName $ identName idd'
                                  return [idd']
            _ -> do usedName $ identName idd
                    return [idd]

-- Simple implementation of the MonadEngine class, and simple
-- interface to running the simplifier on various things.

newtype SimpleM u a = SimpleM (RWS
                               (Env u)            -- Reader
                               (Need u)           -- Writer
                               (NameSource VName) -- State
                               a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Need u),
            MonadReader (Env u),
            MonadState (NameSource VName))

instance MonadFreshNames (SimpleM u) where
  getNameSource = get
  putNameSource = put

instance ST.UserAnnotation u => MonadEngine u (SimpleM u) where
  askEngineEnv = ask
  localEngineEnv = local
  tellNeed = tell
  listenNeed = listen
  passNeed = pass
  simplifyBody = defaultSimplifyBody

runSimpleM :: SimpleM u a -> Env u -> VNameSource -> (a, VNameSource)
runSimpleM (SimpleM m) env src = let (x, src', _) = runRWS m env src
                                 in (x, src')

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyProg :: ST.UserAnnotation u => RuleBook u -> Prog -> Prog
simplifyProg rules prog =
  Prog $ fst $ runSimpleM (mapM simplifyFun $ progFunctions prog)
               (emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

simplifyFun :: ST.UserAnnotation u => FunDec -> SimpleM u FunDec
simplifyFun (fname, rettype, params, body, loc) = do
  body' <- insertAllBindings $ bindParams params $ simplifyBody body
  return (fname, rettype, params, body', loc)

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyOneFun :: (ST.UserAnnotation u, MonadFreshNames m) =>
                  RuleBook u -> FunDec -> m FunDec
simplifyOneFun rules fundec =
  modifyNameSource $ runSimpleM (simplifyFun fundec) (emptyEnv rules Nothing)

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: (ST.UserAnnotation u, MonadFreshNames m) =>
                     RuleBook u -> Maybe Prog -> Lambda -> m Lambda
simplifyOneLambda rules prog lam = do
  let simplifyOneLambda' = insertAllBindings $
                           bindParams (lambdaParams lam) $
                           simplifyBody $ lambdaBody lam
  body' <- modifyNameSource $ runSimpleM simplifyOneLambda' $ emptyEnv rules prog
  return $ lam { lambdaBody = body' }

-- | Simplify the given body.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyOneBody :: ST.UserAnnotation u =>
                   MonadFreshNames m => RuleBook u -> Maybe Prog -> Body -> m Body
simplifyOneBody rules prog body =
  modifyNameSource $
  runSimpleM (insertAllBindings $ simplifyBody body) (emptyEnv rules prog)
