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
-- "Futhark.Optimise.Simplifier.Simplify" modules.
--
module Futhark.Optimise.Simplifier.Engine
       ( -- * Monadic interface
         Simplifiable
       , MonadEngine(..)
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
       , simplifyLambdaNoHoisting
       , simplifyExtLambda
       , simplifySubExp
       , simplifyVName
       , simplifyExtType
       , simplifyExtShape
       ) where

import Control.Applicative
import Control.Monad.Writer

import Data.Either
import Data.Hashable
import Data.List
import qualified Data.Traversable
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Foldable (traverse_)

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Usage
import Futhark.Optimise.Simplifier.Apply
import Futhark.Construct
import qualified Futhark.Analysis.ScalExp as SExp
import Futhark.Optimise.Simplifier.Simplifiable
import Futhark.Optimise.Simplifier.Lore

type NeedSet lore = [Binding lore]

data Need lore = Need { needBindings :: NeedSet lore
                      , usageTable  :: UT.UsageTable
                      }

instance Monoid (Need lore) where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] UT.empty

type AliasMap = HM.HashMap VName Names

data Env m = Env { envProgram   :: Maybe (Prog (InnerLore m))
                 , envRules     :: RuleBook m
                 , envAliases   :: AliasMap
                 }

emptyEnv :: MonadEngine m =>
            RuleBook m
         -> Maybe (Prog (InnerLore m)) -> Env m
emptyEnv rules prog =
  Env { envProgram = prog
      , envRules = rules
      , envAliases = mempty
      }
data State m = State { stateVtable :: ST.SymbolTable (Lore m)
                     }

emptyState :: State m
emptyState = State { stateVtable = ST.empty }

class (MonadBinder m,
       Proper (Lore m),
       Lore m ~ Wise (InnerLore m),
       Simplifiable (InnerLore m)) => MonadEngine m where
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
  simplifyLetBoundLore :: Annotations.LetBound (InnerLore m)
                       -> m (Annotations.LetBound (InnerLore m))
  simplifyFParamLore :: Annotations.FParam (InnerLore m)
                     -> m (Annotations.FParam (InnerLore m))
  simplifyLParamLore :: Annotations.LParam (InnerLore m)
                     -> m (Annotations.LParam (InnerLore m))
  simplifyRetType :: Annotations.RetType (InnerLore m)
                  -> m (Annotations.RetType (InnerLore m))

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

consumedName :: MonadEngine m => VName -> m ()
consumedName = tellNeed . Need [] . UT.consumedUsage

inResultName :: MonadEngine m => VName -> m ()
inResultName = tellNeed . Need [] . UT.inResultUsage

asserted :: MonadEngine m => SubExp -> m ()
asserted (Constant {}) =
  return ()
asserted (Var name) = do
  se <- ST.lookupExp name <$> getVtable
  case se of Just (PrimOp (BinOp Equal x y _)) -> do
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

blockUsage :: MonadEngine m => m a -> m a
blockUsage m = passNeed $ do
  (x, _) <- listenNeed m
  return (x, const mempty)

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
enterLoop = localVtable ST.deepen

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
              -> [Binding (Lore m)] -> Result
              -> m (Body (Lore m),
                    [Binding (Lore m)],
                    UT.UsageTable)
hoistBindings rules block vtable uses needs result = do
  (uses', blocked, hoisted) <- simplifyBindings vtable uses needs
  mapM_ addBinding blocked
  body <- mkBodyM blocked result
  return (body, hoisted, uses')
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

provides :: Proper lore => Binding lore -> [VName]
provides = patternNames . bindingPattern

requires :: Proper lore => Binding lore -> Names
requires bnd =
  (mconcat (map freeIn $ patternElements $ bindingPattern bnd)
  `HS.difference` HS.fromList (provides bnd)) <>
  freeInExp (bindingExp bnd)

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

isFalse :: Bool -> BlockPred lore
isFalse b _ _ = not b

orIf :: BlockPred lore -> BlockPred lore -> BlockPred lore
orIf p1 p2 body need = p1 body need || p2 body need

blockIf :: MonadEngine m =>
           BlockPred (Lore m)
        -> m Result -> m (Body (Lore m))
blockIf block m = passNeed $ do
  (body, needs) <- listenNeed m
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (e, hoistable, usages) <-
    hoistBindings rules block vtable (usageTable needs) (needBindings needs) body
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

isInPlaceBound :: BlockPred m
isInPlaceBound _ = not . all ((==BindVar) . patElemBindage) .
                   patternElements . bindingPattern

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
isResultAlloc usage (Let (Pattern [] [bindee]) _
                     (PrimOp (Alloc {}))) =
  UT.isInResult (patElemName bindee) usage
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
  let block = isNotSafe `orIf` isNotCheap `orIf` isInPlaceBound
  vtable <- getVtable
  rules <- asksEngineEnv envRules
  (body1', safe1, f1) <-
    enterBody $
    localVtable vtablef1 $
    hoistBindings rules block vtable (usageTable needs1)
    (needBindings needs1) body1
  (body2', safe2, f2) <-
    enterBody $
    localVtable vtablef2 $
    hoistBindings rules block vtable (usageTable needs2)
    (needBindings needs2) body2
  let hoistable = safe1 <> safe2
  putVtable $ foldl (flip ST.insertBinding) vtable hoistable
  return ((body1', body2'),
          const Need { needBindings = hoistable
                     , usageTable = f1 <> f2
                     })

-- | Simplify a single 'Body' inside an arbitrary 'MonadEngine'.
defaultSimplifyBody :: MonadEngine m =>
                       [Diet] -> Body (InnerLore m) -> m Result

defaultSimplifyBody ds (Body _ bnds res) = do
  mapM_ simplifyBinding bnds
  simplifyResult ds res

-- | Simplify a single 'Result' inside an arbitrary 'MonadEngine'.
simplifyResult :: MonadEngine m =>
                  [Diet] -> Result -> m Result

simplifyResult ds es = do
  es' <- mapM simplifySubExp es
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
-- The simplification rules cannot handle Apply, because it requires
-- access to the full program.  This is a bit of a hack.
simplifyBinding (Let pat _ (Apply fname args rettype)) = do
  args' <- mapM (simplifySubExp . fst) args
  rettype' <- simplifyRetType rettype
  prog <- asksEngineEnv envProgram
  vtable <- getVtable
  case join $ pure simplifyApply <*> prog <*> pure vtable <*> pure fname <*> pure args of
    -- Array values are non-unique, so we may need to copy them.
    Just vs -> do let vs' = valueShapeContext (retTypeValues rettype) vs ++ vs
                  bnds <- forM (zip (patternIdents pat) vs') $ \(p,v) ->
                    case uniqueness $ identType p of
                      Unique    -> mkLetNamesM' [identName p] =<< eCopy (eValue v)
                      Nonunique -> mkLetNamesM' [identName p] =<< eValue v
                  mapM_ (simplifyBinding . removeBindingWisdom) bnds
    Nothing -> do let e' = Apply fname (zip args' $ map snd args) rettype'
                  t <- expExtType e'
                  pat' <- blockUsage $ simplifyPattern pat t
                  inspectBinding =<<
                    mkLetM (addWisdomToPattern pat' e') e'

simplifyBinding (Let pat _ lss@(LoopOp Stream{})) = do
  lss' <- simplifyExp lss
  rtp <- expExtType lss
  rtp' <- expExtType lss'
  let patels      = patternElements pat
      argpattps   = map patElemType $ drop (length patels - length rtp) patels
  (newpats,newsubexps) <- unzip <$> reverse <$>
                          foldM gatherPat [] (zip3 rtp rtp' argpattps)
  newexps' <- forM newsubexps $ simplifyExp . PrimOp . SubExp
  newpats' <- forM (zip newpats newexps') $ \(p,e) ->
                    simplifyPattern p =<< expExtType e
  let rmvdpatels = concatMap patternElements newpats
      patels' = concatMap (\p->if p `elem` rmvdpatels then [] else [p]) patels
  pat' <- let (ctx,vals) = splitAt (length patels' - length rtp') patels'
          in simplifyPattern (Pattern ctx vals) rtp'
  let newpatexps' = zip newpats' newexps' ++ [(pat',lss')]
  newpats'' <- forM newpatexps' $ \(p,e)->simplifyPattern p =<< expExtType e
  let (_,newexps'') = unzip newpatexps'
  let newpatexps''= zip newpats'' newexps''
  _ <- forM newpatexps'' $ \(p,e) -> inspectBinding =<<
                            mkLetM (addWisdomToPattern p e) e
  return ()
    where gatherPat acc (_, Basic _, _) = return acc
          gatherPat acc (_, Mem {}, _) = return acc
          gatherPat acc (Array _ shp _, Array _ shp' _, Array _ pshp _) =
            foldM gatherShape acc (zip3 (extShapeDims shp) (extShapeDims shp') (shapeDims pshp))
          gatherPat _ _ =
            fail $ "In simplifyBinding \"let pat = stream()\": "++
                   " reached unreachable case!"
          gatherShape acc (Ext i, Free se', Var pid) = do
            let patind  = elemIndex pid $
                          map patElemName $ patternElements pat
            case patind of
              Just k -> return $ (Pattern [] [patternElements pat !! k], se') : acc
              Nothing-> fail $ "In simplifyBinding \"let pat = stream()\": pat "++
                               "element of known dim not found: "++pretty pid++" "++show i++" "++pretty se'++"."
          gatherShape _ (Free se, Ext i', _) =
            fail $ "In simplifyBinding \"let pat = stream()\": "++
                   " previous known dimension: " ++ pretty se ++
                   " becomes existential: ?" ++ show i' ++ "!"
          gatherShape acc _ = return acc

simplifyBinding (Let pat _ e) = do
  e' <- simplifyExp e
  pat' <- simplifyPattern pat =<< expExtType e'
  inspectBinding =<<
    mkLetM (addWisdomToPattern pat' e') e'

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

simplifyExp (If cond tbranch fbranch ts) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.  We should also try to do CSE
  -- across branches.
  cond' <- simplifySubExp cond
  ts' <- mapM simplifyExtType ts
  let ds = map diet ts'
  (tbranch',fbranch') <-
    hoistCommon (simplifyBody ds tbranch) (ST.updateBounds True cond)
                (simplifyBody ds fbranch) (ST.updateBounds False cond)
  return $ If cond' tbranch' fbranch' ts'

simplifyExp (LoopOp op) = LoopOp <$> simplifyLoopOp op

simplifyExp (SegOp op) = SegOp <$> simplifySegOp op

simplifyExp e = simplifyExpBase e

simplifyExpBase :: MonadEngine m => Exp (InnerLore m) -> m (Exp (Lore m))
simplifyExpBase = mapExpM hoist
  where hoist = Mapper {
                -- Bodies are handled explicitly because we need to
                -- provide their result diet.
                  mapOnBody = fail "Unhandled body in simplification engine."
                , mapOnSubExp = simplifySubExp
                -- Lambdas are handled explicitly because we need to
                -- bind their parameters.
                , mapOnLambda = fail "Unhandled lambda in simplification engine."
                , mapOnExtLambda = fail "Unhandled existential lambda in simplification engine."
                , mapOnVName = simplifyVName
                , mapOnCertificates = simplifyCerts
                , mapOnRetType = simplifyRetType
                , mapOnFParam =
                  fail "Unhandled FParam in simplification engine."
                }

simplifyLoopOp :: MonadEngine m => LoopOp (InnerLore m) -> m (LoopOp (Lore m))

simplifyLoopOp (DoLoop respat merge form loopbody) = do
  let (mergepat, mergeexp) = unzip merge
  mergepat' <- mapM (simplifyParam simplifyFParamLore) mergepat
  mergeexp' <- mapM simplifySubExp mergeexp
  let diets = map (diet . paramType) mergepat'
  (form', boundnames, wrapbody) <- case form of
    ForLoop loopvar boundexp -> do
      boundexp' <- simplifySubExp boundexp
      return (ForLoop loopvar boundexp',
              loopvar `HS.insert` fparamnames,
              bindLoopVar loopvar boundexp')
    WhileLoop cond -> do
      cond' <- simplifyVName cond
      return (WhileLoop cond',
              fparamnames,
              id)
  -- Blocking hoisting of all unique bindings is probably too
  -- conservative, but there is currently no nice way to mark
  -- consumption of the loop body result.
  loopbody' <- enterLoop $ enterBody $
               bindFParams mergepat' $
               blockIf
               (hasFree boundnames `orIf` isUnique `orIf` isResultAlloc) $
               wrapbody $ do
                 res <- simplifyBody diets loopbody
                 isDoLoopResult res
                 return res
  let merge' = zip mergepat' mergeexp'
  consumeResult $ zip diets mergeexp'
  return $ DoLoop respat merge' form' loopbody'
  where fparamnames = HS.fromList (map (paramName . fst) merge)

simplifyLoopOp (Stream cs outerdim form lam arr ii) = do
  cs' <- simplifyCerts      cs
  outerdim' <- simplifySubExp outerdim
  form' <- simplifyStreamForm outerdim' form
  arr' <- mapM simplifyVName  arr
  vtab <- getVtable
  let (chunk:_) = extLambdaParams lam
      se_outer = case outerdim' of
                    Var idd    -> fromMaybe (SExp.Id idd Int) (ST.lookupScalExp idd vtab)
                    Constant c -> SExp.Val c
      se_1 = SExp.Val $ IntVal 1
      -- extension: one may similarly treat iota stream-array case,
      -- by setting the bounds to [0, se_outer-1]
      parbnds  = [ (chunk, se_1, se_outer) ]
  lam' <- simplifyExtLambda parbnds lam outerdim'
  return $ Stream cs' outerdim' form' lam' arr' ii
  where simplifyStreamForm _ (MapLike o) = return $ MapLike o
        simplifyStreamForm outerdim' (RedLike o lam0 acc) = do
            acc'  <- mapM simplifySubExp acc
            lam0' <- simplifyLambda lam0 outerdim' $
                     replicate (length $ lambdaParams lam0) Nothing
            return $ RedLike o lam0' acc'
        simplifyStreamForm _ (Sequential acc) = do
            acc'  <- mapM simplifySubExp acc
            return $ Sequential acc'

simplifyLoopOp (Kernel cs w index ispace inps returns body) = do
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  ispace' <- forM ispace $ \(i, bound) -> do
    bound' <- simplifySubExp bound
    return (i, bound')
  returns' <- forM returns $ \(t, perm) -> do
    t' <- simplifyType t
    return (t', perm)
  enterLoop $ enterBody $ bindLoopVars ((index,w) : ispace) $ do
    inps' <- mapM simplifyKernelInput inps
    body' <- bindFParams (map kernelInputParam inps') $
             blockIf (hasFree bound_here `orIf` isUnique `orIf` isAlloc) $
             simplifyBody (map (diet . fst) returns) body
    return $ Kernel cs' w' index ispace' inps' returns' body'
  where bound_here = HS.fromList $ map kernelInputName inps ++ map fst ispace

simplifyLoopOp (ReduceKernel cs w
                (KernelSize num_groups group_size thread_chunk offset_multiple)
                parlam seqlam nes arrs) = do
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  num_groups' <- simplifySubExp num_groups
  group_size' <- simplifySubExp group_size
  thread_chunk' <- simplifySubExp thread_chunk
  offset_multiple' <- simplifySubExp offset_multiple
  nes' <- mapM simplifySubExp nes
  arrs' <- mapM simplifyVName arrs
  parlam' <- simplifyLambda parlam w' $ map (const Nothing) nes
  seqlam' <- simplifyLambda seqlam w' $ map (const Nothing) nes
  return $ ReduceKernel cs' w'
    (KernelSize num_groups' group_size' thread_chunk' offset_multiple')
    parlam' seqlam' nes' arrs'

simplifyLoopOp (Map cs w fun arrs) = do
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  arrs' <- mapM simplifyVName arrs
  fun' <- simplifyLambda fun w $ map Just arrs'
  return $ Map cs' w' fun' arrs'

simplifyLoopOp (ConcatMap cs w fun arrs) = do
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  arrs' <- mapM (mapM simplifyVName) arrs
  fun' <- simplifyLambda fun w $ map (const Nothing) $ lambdaParams fun
  return $ ConcatMap cs' w' fun' arrs'

simplifyLoopOp (Reduce cs w fun input) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifyVName arrs
  fun' <- simplifyLambda fun w $ map Just arrs'
  return $ Reduce cs' w' fun' (zip acc' arrs')

simplifyLoopOp (Scan cs w fun input) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifyVName arrs
  fun' <- simplifyLambda fun w $ map Just arrs'
  return $ Scan cs' w' fun' (zip acc' arrs')

simplifyLoopOp (Redomap cs w outerfun innerfun acc arrs) = do
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifyVName arrs
  outerfun' <- simplifyLambda outerfun w $
               replicate (length $ lambdaParams outerfun) Nothing
  (innerfun', used) <- tapUsage $ simplifyLambda innerfun w $ map Just arrs
  (innerfun'', arrs'') <- removeUnusedParams used innerfun' arrs'
  return $ Redomap cs' w' outerfun' innerfun'' acc' arrs''
  where removeUnusedParams used lam arrinps
          | (accparams, arrparams) <- splitAt (length acc) $ lambdaParams lam =
              let (arrparams', arrinps') =
                    unzip $ filter ((`UT.used` used) . paramName . fst) $
                    zip arrparams arrinps
              in return (lam { lambdaParams = accparams ++ arrparams' },
                         arrinps')
          | otherwise = return (lam, arrinps)

simplifySegOp :: MonadEngine m => SegOp (InnerLore m) -> m (SegOp (Lore m))
simplifySegOp (SegReduce cs w fun input descp) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifyVName arrs
  fun' <- simplifyLambda fun w $ map Just arrs'
  descp' <- simplifyVName descp
  return $ SegReduce cs' w' fun' (zip acc' arrs') descp'

simplifySegOp (SegScan cs w st fun input descp) = do
  let (acc, arrs) = unzip input
  cs' <- simplifyCerts cs
  w' <- simplifySubExp w
  acc' <- mapM simplifySubExp acc
  arrs' <- mapM simplifyVName arrs
  fun' <- simplifyLambda fun w $ map Just arrs'
  descp' <- simplifyVName descp
  return $ SegScan cs' w' st fun' (zip acc' arrs') descp'

simplifySegOp (SegReplicate cs counts dataarr seg) = do
  cs' <- simplifyCerts cs
  counts' <- simplifyVName counts
  dataarr' <- simplifyVName dataarr
  seg' <- Data.Traversable.mapM simplifyVName seg
  return $ SegReplicate cs' counts' dataarr' seg'

simplifySubExp :: MonadEngine m => SubExp -> m SubExp
simplifySubExp (Var name) = do
  bnd <- getsEngineState $ ST.lookupSubExp name . stateVtable
  case bnd of
    Just (Constant v) -> return $ Constant v
    Just (Var id') -> do usedName id'
                         return $ Var id'
    _              -> do usedName name
                         return $ Var name
simplifySubExp (Constant v) = return $ Constant v

simplifyPattern :: MonadEngine m =>
                   Pattern (InnerLore m)
                -> [ExtType]
                -> m (Pattern (InnerLore m))
simplifyPattern pat ets =
  Pattern <$>
  zipWithM inspect (patternContextElements pat) (repeat Nonunique) <*>
  zipWithM inspect (patternValueElements pat) (map uniqueness ets)
  where inspect (PatElem ident bindage lore) u = do
          t <- simplifyType $ identType ident
          let ident' =
                case bindage of
                  BindVar -> ident { identType = t `setUniqueness` u }
                  _       -> ident { identType = t }
          bindage' <- simplifyBindage bindage
          lore'  <- simplifyLetBoundLore lore
          return $ PatElem ident' bindage' lore'

simplifyBindage :: MonadEngine m =>
                   Bindage
                -> m Bindage
simplifyBindage BindVar =
  return BindVar
simplifyBindage (BindInPlace cs src is) =
  BindInPlace <$>
  simplifyCerts cs <*>
  simplifyVName src <*>
  mapM simplifySubExp is

simplifyIdentBinding :: MonadEngine m => Ident -> m Ident
simplifyIdentBinding v = do
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyParam :: MonadEngine m =>
                 (attr -> m attr) -> ParamT attr -> m (ParamT attr)
simplifyParam simplifyAttribute (Param ident attr) = do
  ident' <- simplifyIdentBinding ident
  attr' <- simplifyAttribute attr
  return $ Param ident' attr'

simplifyVName :: MonadEngine m => VName -> m VName
simplifyVName v = do
  se <- ST.lookupSubExp v <$> getVtable
  case se of
    Just (Var v') -> do usedName v'
                        return v'
    _             -> do usedName v
                        return v

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
simplifyType (Mem size space) =
  Mem <$> simplifySubExp size <*> pure space
simplifyType (Basic bt) =
  return $ Basic bt

simplifyLambda :: MonadEngine m =>
                  Lambda (InnerLore m)
               -> SubExp -> [Maybe VName]
               -> m (Lambda (Lore m))
simplifyLambda = simplifyLambdaMaybeHoist True

simplifyLambdaNoHoisting :: MonadEngine m =>
                            Lambda (InnerLore m)
                         -> SubExp -> [Maybe VName]
                         -> m (Lambda (Lore m))
simplifyLambdaNoHoisting = simplifyLambdaMaybeHoist False

simplifyLambdaMaybeHoist :: MonadEngine m =>
                            Bool -> Lambda (InnerLore m)
                         -> SubExp -> [Maybe VName]
                         -> m (Lambda (Lore m))
simplifyLambdaMaybeHoist hoisting lam@(Lambda i params body rettype) w arrs = do
  params' <- mapM (simplifyParam simplifyLParamLore) params
  let (nonarrayparams, arrayparams) =
        splitAt (length params' - length arrs) params'
      paramnames = HS.fromList $ boundByLambda lam
  body' <-
    enterLoop $ enterBody $
    bindLoopVar i w $
    bindLParams nonarrayparams $
    bindArrayLParams (zip arrayparams arrs) $
    blockIf (isFalse hoisting `orIf` hasFree paramnames `orIf` isUnique `orIf` isAlloc) $
      simplifyBody (map diet rettype) body
  rettype' <- mapM simplifyType rettype
  return $ Lambda i params' body' rettype'


simplifyExtLambda :: MonadEngine m =>
                    [(LParam (Lore m), SExp.ScalExp, SExp.ScalExp)]
                  ->    ExtLambda (InnerLore m)
                  -> SubExp
                  -> m (ExtLambda (Lore m))
simplifyExtLambda parbnds lam@(ExtLambda index params body rettype) w = do
  params' <- mapM (simplifyParam simplifyLParamLore) params
  let paramnames = HS.fromList $ boundByExtLambda lam
  rettype' <- mapM simplifyExtType rettype
  body' <- enterLoop $ enterBody $
           bindLoopVar index w $
           bindLParams params' $
           blockIf (hasFree paramnames `orIf` isUnique) $
           localVtable extendSymTab $
           simplifyBody (map diet rettype) body
  let bodyres = bodyResult body'
      bodyenv = typeEnvFromBindings $ bodyBindings body'
  rettype'' <- bindLParams params' $
               zipWithM (refineArrType bodyenv params') bodyres rettype'
  return $ ExtLambda index params' body' rettype''
    where extendSymTab vtb =
            foldl (\ vt (i,l,u) ->
                        let i_name = paramName i
                        in  ST.setUpperBound i_name u $
                            ST.setLowerBound i_name l vt
                  ) vtb parbnds
          refineArrType :: MonadEngine m =>
                           TypeEnv -> [LParam (Lore m)] -> SubExp -> ExtType -> m ExtType
          refineArrType bodyenv pars x (Array btp shp u) = do
            vtab <- ST.bindings <$> getVtable
            dsx <- flip extendedTypeEnv bodyenv $
                   shapeDims <$> arrayShape <$> subExpType x
            let parnms = map paramName pars
                dsrtpx =  extShapeDims shp
                (resdims,_) =
                    foldl (\ (lst,i) el ->
                            case el of
                              (Free (Constant c), _) -> (lst++[Free (Constant c)], i)
                              ( _,      Constant c ) -> (lst++[Free (Constant c)], i)
                              (Free (Var tid), Var pid) ->
                                if not (HM.member tid vtab) &&
                                        HM.member pid vtab
                                then (lst++[Free (Var pid)], i)
                                else (lst++[Free (Var tid)], i)
                              (Ext _, Var pid) ->
                                if HM.member pid vtab ||
                                   pid `elem` parnms
                                then (lst ++ [Free (Var pid)], i)
                                else (lst ++ [Ext i],        i+1)
                          ) ([],0) (zip dsrtpx dsx)
            return $ Array btp (ExtShape resdims) u
          refineArrType _ _ _ tp = return tp

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
          vv <- getsEngineState $ ST.lookupSubExp idd . stateVtable
          case vv of
            Just (Constant Checked) -> return []
            Just (Var idd') -> do usedName idd'
                                  return [idd']
            _ -> do usedName idd
                    return [idd]

simplifyKernelInput :: MonadEngine m =>
                       KernelInput (InnerLore m) -> m (KernelInput (Lore m))
simplifyKernelInput (KernelInput param arr is) = do
  param' <- simplifyParam simplifyFParamLore param
  arr' <- simplifyVName arr
  is' <- mapM simplifySubExp is
  return $ KernelInput param' arr' is'

simplifyFun :: MonadEngine m =>
               FunDec (InnerLore m) -> m (FunDec (Lore m))
simplifyFun (FunDec fname rettype params body) = do
  rettype' <- simplifyRetType rettype
  body' <- bindFParams params $ insertAllBindings $
           simplifyBody (map diet $ retTypeValues rettype') body
  return $ FunDec fname rettype' params body'
