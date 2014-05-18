{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Futhark.Optimise.Simplifier.Engine
       ( simplifyProg
       , simplifyOneFun
       , simplifyOneLambda
       , simplifyOneBody
       ) where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Graph
import Data.Hashable
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashSet as HS

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.CSE
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Optimise.SymbolTable as ST
import qualified Futhark.Optimise.UsageTable as UT
import Futhark.Optimise.Simplifier.Apply

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyProg :: RuleBook -> Prog -> Prog
simplifyProg rules prog =
  Prog $ fst $ runSimpleM (mapM simplifyFun $ progFunctions prog)
               (emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyOneFun :: MonadFreshNames m =>
                  RuleBook -> FunDec -> m FunDec
simplifyOneFun rules fundec =
  modifyNameSource $ runSimpleM (simplifyFun fundec) (emptyEnv rules Nothing)

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: MonadFreshNames m => RuleBook -> Maybe Prog -> Lambda -> m Lambda
simplifyOneLambda rules prog lam = do
  let simplifyOneLambda' = blockAllHoisting $
                           bindParams (lambdaParams lam) $
                           simplifyBody $ lambdaBody lam
  body' <- modifyNameSource $ runSimpleM simplifyOneLambda' $ emptyEnv rules prog
  return $ lam { lambdaBody = body' }

-- | Simplify the given body.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyOneBody :: MonadFreshNames m => RuleBook -> Maybe Prog -> Body -> m Body
simplifyOneBody rules prog body =
  modifyNameSource $
  runSimpleM (blockAllHoisting $ simplifyBody body) (emptyEnv rules prog)

simplifyFun :: FunDec -> SimpleM FunDec
simplifyFun (fname, rettype, params, body, loc) = do
  body' <- blockAllHoisting $ bindParams params $ simplifyBody body
  return (fname, rettype, params, body', loc)

data BindNeed = LetNeed ([Ident],[VName]) (Exp, Names)
                -- The [VName] is just the names of the idents, as a
                -- cache.  Similarly, the expression is tagged with
                -- what is free in it.
                deriving (Show, Eq)

type NeedSet = [BindNeed]

asTail :: BindNeed -> Body
asTail (LetNeed (pat,_) (e,_)) = Body [Let pat e] $ Result [] [] loc
  where loc = srclocOf pat

requires :: BindNeed -> HS.HashSet VName
requires (LetNeed (pat,_) (_,freeInE)) =
  freeInE `mappend` freeInPat
  where freeInPat  = mconcat $ map (freeInType . identType) pat

provides :: BindNeed -> [VName]
provides (LetNeed (_,provs) _) = provs

patNameSet :: [Ident] -> HS.HashSet VName
patNameSet = HS.fromList . map identName

freeInType :: Type -> HS.HashSet VName
freeInType = mconcat . map (freeNamesInExp . SubExp) . arrayDims

data Need = Need { needBindings :: NeedSet
                 , freeInBound  :: UT.UsageTable
                 }

instance Monoid Need where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] UT.empty

data Env = Env { envDupeState :: DupeState
               , envVtable    :: ST.SymbolTable
               , envProgram   :: Maybe Prog
               , envRules     :: RuleBook
               }

emptyEnv :: RuleBook -> Maybe Prog -> Env
emptyEnv rules prog =
  Env { envDupeState = newDupeState
      , envVtable = ST.empty
      , envProgram = prog
      , envRules = rules
      }

newtype SimpleM a = SimpleM (RWS
                           Env                -- Reader
                           Need               -- Writer
                           (NameSource VName) -- State
                           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter Need, MonadReader Env, MonadState (NameSource VName))

instance MonadFreshNames SimpleM where
  getNameSource = get
  putNameSource = put

runSimpleM :: SimpleM a -> Env -> VNameSource -> (a, VNameSource)
runSimpleM (SimpleM m) env src = let (x, src', _) = runRWS m env src
                                 in (x, src')

needThis :: Binding -> SimpleM ()
needThis (Let pat e) = tell $ Need [need] UT.empty
  where need = LetNeed (pat, map identName pat) (e, freeNamesInExp e)

boundFree :: HS.HashSet VName -> SimpleM ()
boundFree fs = tell $ Need [] $ UT.usages fs

usedName :: VName -> SimpleM ()
usedName = boundFree . HS.singleton

localVtable :: (ST.SymbolTable -> ST.SymbolTable) -> SimpleM a -> SimpleM a
localVtable f = local $ \env -> env { envVtable = f $ envVtable env }

binding :: [(VName, Exp)] -> SimpleM a -> SimpleM a
binding = localVtable . flip (foldr $ uncurry ST.insert)

bindParams :: [Param] -> SimpleM a -> SimpleM a
bindParams params =
  localVtable $ \vtable ->
    foldr ST.insertParam vtable params

bindArrayParams :: [(Param,SubExp)] -> SimpleM a -> SimpleM a
bindArrayParams params =
  localVtable $ \vtable ->
    foldr (uncurry ST.insertArrayParam) vtable params

bindLoopVar :: Ident -> SubExp -> SimpleM a -> SimpleM a
bindLoopVar var bound =
  localVtable $ clampUpper . clampVar
  where clampVar = ST.insertLoopVar (identName var) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampUpper = case bound of Var v -> ST.isAtLeast (identName v) 1
                                   _     -> id

withBinding :: [Ident] -> Exp
               -> SimpleM a -> SimpleM a
withBinding pat e m = do
  ds <- asks envDupeState
  let (bnds, ds') = performCSE ds pat e
      patbnds = mapMaybe varBnd bnds
  mapM_ needThis bnds
  binding patbnds $
    local (\env -> env { envDupeState = ds'}) m

varBnd :: Binding -> Maybe (VName, Exp)
varBnd (Let [Ident var _ _] e) = Just (var, e)
varBnd _                       = Nothing

bindLet :: [Ident] -> Exp -> SimpleM a -> SimpleM a

bindLet = withBinding

addBindings :: MonadFreshNames m =>
               RuleBook -> ST.SymbolTable -> DupeState -> [BindNeed] -> Body -> UT.UsageTable
            -> m (Body, UT.UsageTable)
addBindings rules vtable dupes needs body uses = do
  (uses',bnds) <- simplifyBindings vtable uses $
                  concat $ snd $ mapAccumL pick dupes needs
  return (insertBindings bnds body, uses')
  where simplifyBindings vtable' uses' bnds =
          foldM comb (uses',[]) $ reverse $ zip bnds vtables
            where vtables = scanl insertBnd vtable' bnds
                  insertBnd vtable'' (Let [v] e, _, _) =
                    ST.insert (identName v) e vtable''
                  insertBnd vtable'' _ = vtable''

        -- Do not actually insert binding if it is not used.
        comb (uses',bnds) ((bnd, provs, reqs), vtable')
          | uses' `UT.contains` provs = do
            res <- bottomUpSimplifyBinding rules (vtable', uses') bnd
            case res of
              Nothing ->
                return ((uses' <> reqs) `UT.without` provs,
                        bnd:bnds)
              Just optimbnds -> do
                (uses'',optimbnds') <-
                  simplifyBindings vtable' uses' $ map attachUsage optimbnds
                return (uses'', optimbnds'++bnds)
          | otherwise = -- Dead binding.
            return (uses', bnds)

        attachUsage bnd = (bnd, providedByBnd bnd, usedInBnd bnd)
        providedByBnd (Let pat _) = map identName pat
        usedInBnd (Let _ e) = usedInExp e
        usedInExp e = extra <> UT.usages (freeNamesInExp e)
          where extra =
                  case e of Assert (Var v) _ -> UT.predicateUsage $ identName v
                            _                -> UT.empty


        pick ds (LetNeed (pat,_) (e,_)) =
          let (bnds,ds') = performCSE ds pat e
          in (ds',
              [ (Let pat' e',
                 map identName pat,
                 usedInExp e')
              | Let pat' e' <- bnds ])

inDepOrder :: [BindNeed] -> [BindNeed]
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

mustPrecede :: BindNeed -> BindNeed -> Bool
bnd1 `mustPrecede` bnd2 =
  not $ HS.null $ HS.fromList (filter (`HS.member` req2) $ provides bnd1)
                  `HS.union`
                  (consumedInBody e2 `HS.intersection` requires bnd1)
  where e2 = asTail bnd2
        req2 = requires bnd2

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

data BodyInfo = BodyInfo { bodyConsumes :: HS.HashSet VName
                         }

bodyInfo :: Body -> BodyInfo
bodyInfo b = BodyInfo {
               bodyConsumes = consumedInBody b
             }

type BlockPred = BodyInfo -> BindNeed -> Bool

orIf :: BlockPred -> BlockPred -> BlockPred
orIf p1 p2 body need = p1 body need || p2 body need

splitHoistable :: BlockPred -> Body -> NeedSet -> ([BindNeed], NeedSet)
splitHoistable block body needs =
  let (blocked, hoistable, _) =
        foldl split ([], [], HS.empty) $ inDepOrder needs
  in (reverse blocked, hoistable)
  where block' = block $ bodyInfo body
        split (blocked, hoistable, ks) need@(LetNeed (pat,provs) e) =
          let bad (e',free) = block' (LetNeed (pat,provs) (e',free)) ||
                              ks `intersects` free
          in if bad e
             then (need : blocked, hoistable,
                   HS.fromList provs `HS.union` ks)
             else (blocked, LetNeed (pat,provs) e : hoistable,
                   ks)

blockIfSeq :: [BlockPred] -> SimpleM Body -> SimpleM Body
blockIfSeq ps m = foldl (flip blockIf) m ps

blockIf :: BlockPred -> SimpleM Body -> SimpleM Body
blockIf block m = pass $ do
  (body, needs) <- listen m
  ds <- asks envDupeState
  vtable <- asks envVtable
  rules <- asks envRules
  let (blocked, hoistable) = splitHoistable block body $ needBindings needs
  (e, fs) <- addBindings rules vtable ds blocked body $ freeInBound needs
  return (e,
          const Need { needBindings = hoistable
                     , freeInBound  = fs
                     })

blockAllHoisting :: SimpleM Body -> SimpleM Body
blockAllHoisting = blockIf $ \_ _ -> True

hasFree :: HS.HashSet VName -> BlockPred
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred
isNotSafe _ (LetNeed _ (e,_)) = not $ safeExp e

isNotCheap :: BlockPred
isNotCheap _ = not . cheapBnd
  where cheapBnd (LetNeed _ (e,_)) = cheap e
        cheap (BinOp {})   = True
        cheap (SubExp {})  = True
        cheap (Not {})     = True
        cheap (Negate {})  = True
        cheap (DoLoop {})  = False
        cheap _            = True -- Used to be False, but let's try
                                  -- it out.

uniqPat :: [Ident] -> Bool
uniqPat = any $ unique . identType

isUniqueBinding :: BlockPred
isUniqueBinding _ (LetNeed (pat,_) _) = uniqPat pat

hoistCommon :: SimpleM Body -> (ST.SymbolTable -> ST.SymbolTable)
            -> SimpleM Body -> (ST.SymbolTable -> ST.SymbolTable)
            -> SimpleM (Body, Body)
hoistCommon m1 vtablef1 m2 vtablef2 = pass $ do
  (body1, needs1) <- listen $ localVtable vtablef1 m1
  (body2, needs2) <- listen $ localVtable vtablef2 m2
  let splitOK = splitHoistable $ isNotSafe `orIf` isNotCheap
      (needs1', safe1) = splitOK body1 $ needBindings needs1
      (needs2', safe2) = splitOK body2 $ needBindings needs2
  vtable <- asks envVtable
  rules <- asks envRules
  (e1, f1) <- localVtable vtablef1 $ addBindings rules vtable newDupeState needs1' body1 $ freeInBound needs1
  (e2, f2) <- localVtable vtablef2 $ addBindings rules vtable newDupeState needs2' body2 $ freeInBound needs2
  return ((e1, e2),
          const Need { needBindings = safe1 <> safe2
                     , freeInBound = f1 <> f2
                     })

simplifyBody :: Body -> SimpleM Body

simplifyBody (Body [] (Result cs es loc)) =
  resultBody <$> simplifyCerts cs <*>
                 mapM simplifySubExp es <*> pure loc

-- The simplification rules cannot handle Apply, because it requires
-- access to the full program.
simplifyBody (Body (Let pat (Apply fname args tp loc):bnds) res) = do
  pat' <- mapM simplifyIdentBinding pat
  args' <- mapM (simplifySubExp . fst) args
  tp' <- mapM simplifyType tp
  prog <- asks envProgram
  vtable <- asks envVtable
  case join $ pure simplifyApply <*> prog <*> pure vtable <*> pure fname <*> pure args of
    -- Array values are non-unique, so we may need to copy them.
    Just vs -> do let newbnds = flip map (zip3 pat vs tp') $ \(p,v,t) ->
                        case uniqueness t of
                          Unique    -> Let [p] $ Copy (Constant v loc) loc
                          Nonunique -> Let [p] $ SubExp $ Constant v loc
                  simplifyBody $ Body (newbnds++bnds) res
    Nothing -> let e' = Apply fname (zip args' $ map snd args) tp' loc
               in bindLet pat' e' $ simplifyBody $ Body bnds res

simplifyBody (Body (Let pat e:bnds) res) = do
  pat' <- mapM simplifyIdentBinding pat
  e' <- simplifyExp e
  vtable <- asks envVtable
  rules <- asks envRules
  simplified <- topDownSimplifyBinding rules vtable (Let pat e')
  case simplified of
    Just newbnds ->
      simplifyBody $ Body (newbnds++bnds) res
    Nothing      ->
      bindLet pat' e' $ simplifyBody $ Body bnds res

simplifyExp :: Exp -> SimpleM Exp

simplifyExp (DoLoop respat merge loopvar boundexp loopbody loc) = do
  let (mergepat, mergeexp) = unzip merge
  respat'   <- mapM simplifyIdentBinding respat
  mergepat' <- mapM simplifyIdentBinding mergepat
  mergeexp' <- mapM simplifySubExp mergeexp
  boundexp' <- simplifySubExp boundexp
  -- Blocking hoisting of all unique bindings is probably too
  -- conservative, but there is currently no nice way to mark
  -- consumption of the loop body result.
  loopbody' <- bindLoopVar loopvar boundexp' $
               blockIfSeq [hasFree boundnames, isUniqueBinding] $
               simplifyBody loopbody
  let merge' = zip mergepat' mergeexp'
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
  t' <- mapM simplifyType t
  return $ If cond' tbranch' fbranch' t' loc

simplifyExp (Map cs fun arrs loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  return $ Map cs' fun' arrs' loc

simplifyExp (Filter cs fun arrs size loc) = do
  cs' <- simplifyCerts cs
  arrs' <- mapM simplifySubExp arrs
  fun' <- simplifyLambda fun arrs'
  size' <- simplifySubExp size
  return $ Filter cs' fun' arrs' size' loc

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
  innerfun' <- simplifyLambda innerfun arrs
  return $ Redomap cs' outerfun' innerfun' acc' arrs' loc

simplifyExp e = simplifyExpBase e

simplifyExpBase :: Exp -> SimpleM Exp
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

simplifySubExp :: SubExp -> SimpleM SubExp
simplifySubExp (Var ident@(Ident vnm _ pos)) = do
  bnd <- asks $ ST.lookup vnm . envVtable
  case bnd of
    Just (ST.Value v)
      | isBasicTypeVal v  -> return $ Constant v pos
    Just (ST.VarId  id' tp1) -> do usedName id'
                                   return $ Var $ Ident id' tp1 pos
    Just (ST.SymExp (SubExp se)) -> return se
    _                            -> Var <$> simplifyIdent ident
  where isBasicTypeVal = basicType . valueType
simplifySubExp (Constant v loc) = return $ Constant v loc

simplifyIdentBinding :: Ident -> SimpleM Ident
simplifyIdentBinding v = do
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyIdent :: Ident -> SimpleM Ident
simplifyIdent v = do
  usedName $ identName v
  t' <- simplifyType $ identType v
  return v { identType = t' }

simplifyType :: TypeBase als Shape -> SimpleM (TypeBase als Shape)
simplifyType t = do dims <- mapM simplifySubExp $ arrayDims t
                    return $ t `setArrayShape` Shape dims

simplifyLambda :: Lambda -> [SubExp] -> SimpleM Lambda
simplifyLambda (Lambda params body rettype loc) arrs = do
  body' <- blockIf (hasFree params' `orIf` isUniqueBinding) $
           bindParams nonarrayparams $
           bindArrayParams (zip arrayparams arrs) $
           simplifyBody body
  rettype' <- mapM simplifyType rettype
  return $ Lambda params body' rettype' loc
  where params' = patNameSet $ map fromParam params
        (nonarrayparams, arrayparams) =
          splitAt (length params - length arrs) params

simplifyCerts :: Certificates -> SimpleM Certificates
simplifyCerts = liftM (nub . concat) . mapM check
  where check idd = do
          vv <- asks $ ST.lookup (identName idd) . envVtable
          case vv of
            Just (ST.Value (BasicVal Checked)) -> return []
            Just (ST.VarId  id' tp1) -> do usedName id'
                                           return [Ident id' tp1 loc]
            _ -> do usedName $ identName idd
                    return [idd]
          where loc = srclocOf idd
