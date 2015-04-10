{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}
module Futhark.Optimise.SuffCond.OptPredicates
       (
         optimisePredicates
       )
       where

import Control.Applicative
import Control.Arrow (second)
import Data.Foldable (any)
import Data.Maybe
import Data.Monoid
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.Renamer
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Analysis.ScalExp (ScalExp)
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Analysis.AlgSimplify as AS
import Futhark.Tools
import Futhark.Analysis.Rephrase
import qualified Futhark.Optimise.Simplifier.Engine as Simplify
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Optimise.Simplifier
  (simplifyFunWithRules, basicRules)
import Futhark.Optimise.Simplifier.Simplify
  (bindableSimpleOps)
import Futhark.Substitute
import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as S
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Optimise.Simplifier.Lore
  (Wise, mkWiseBody, mkWiseLetBinding, removeExpWisdom, addWisdomToPattern)
import Futhark.Representation.AST.Attributes.Ranges

import Prelude hiding (any)

optimisePredicates :: MonadFreshNames m =>
                      RuleBook (VariantM m) -> Prog -> m Prog
optimisePredicates rules prog = do
  optimPreds <- mapM (maybeOptimiseFun rules) origfuns
  let newfuns = concat optimPreds
      subst = HM.fromList $
              zip (map funDecName origfuns) $
              map (map funDecName) optimPreds
  insertPredicateCalls subst $ Prog $ origfuns ++ newfuns
  where origfuns = progFunctions prog

insertPredicateCalls :: MonadFreshNames m =>
                        HM.HashMap Name [Name] -> Prog -> m Prog
insertPredicateCalls subst prog =
  Prog <$> mapM treatFunction (progFunctions prog)
  where treatFunction fundec = do
          fbody' <- treatBody $ funDecBody fundec
          return fundec { funDecBody = fbody' }
        treatBody (Body _ bnds res) = do
          bnds' <- mapM treatBinding bnds
          return $ mkBody (concat bnds') res
        treatLambda lam = do
          body <- treatBody $ lambdaBody lam
          return $ lam { lambdaBody = body }
        treatBinding (Let pat _ e) = do
          (e', bnds) <- treatExp e
          return $ bnds ++ [Let pat () e']
        treatExp e@(Apply predf predargs predt)
          | Just preds <- HM.lookup predf subst =
            runBinderEmptyEnv $ callPreds predt preds e $ \predf' ->
            Apply predf' predargs predt
        treatExp e = do
          e' <- mapExpM mapper e
          return (e', [])
          where mapper = identityMapper { mapOnBody = treatBody
                                        , mapOnLambda = treatLambda
                                        }
        callPreds _ [] e _            = return e
        callPreds predt (f:fs) e call = do
          c <- letSubExp (nameToString f ++ "_result") $ call f
          eIf (pure $ PrimOp $ SubExp c)
            (eBody [pure $ PrimOp $ SubExp $ constant True])
            (eBody [callPreds predt fs e call])

maybeOptimiseFun :: MonadFreshNames m =>
                    RuleBook (VariantM m) -> FunDec -> m [FunDec]
maybeOptimiseFun rules fundec@(FunDec _ ret _ body)
  | [Basic Bool] <- retTypeValues ret = do
  let sctable = analyseBody (ST.empty :: ST.SymbolTable Basic) mempty body
  generateOptimisedPredicates rules fundec sctable
maybeOptimiseFun _ _ = return []

generateOptimisedPredicates :: MonadFreshNames m =>
                               RuleBook (VariantM m) -> FunDec -> SCTable -> m [FunDec]
generateOptimisedPredicates rules fundec sctable = do
--  o1pred <- generateOptimisedPredicates' rules fundec "_0" sctable 0
  onpred <- generateOptimisedPredicates' rules fundec "_1" sctable 1
  return $ catMaybes [{-o1pred , -}onpred]

generateOptimisedPredicates' :: MonadFreshNames m =>
                                RuleBook (VariantM m) -> FunDec -> String
                             -> SCTable -> Int -> m (Maybe FunDec)
generateOptimisedPredicates'
  rules (FunDec fname rettype params body) suff sctable depth = do
  res <- runVariantM env $ Simplify.insertAllBindings $
         Simplify.simplifyBody (map diet $ retTypeValues rettype) $
         rephraseWithInvariance body
  case res of
    (body', True) -> do
      fundec <- simplifyFunWithRules bindableSimpleOps basicRules $
                FunDec fname' rettype params (removeBodyLore body')
      return $ Just fundec
    _          -> return Nothing
  where fname' = fname <> nameFromString suff
        env = Env { envSimplifyEnv = Simplify.emptyEnv rules Nothing
                  , envSCTable = sctable
                  , envMaxLoops = depth
                  , envGeneratingSuff = Sufficient
                  }

rephraseWithInvariance :: Body -> S.Body Invariance'
rephraseWithInvariance = rephraseBody rephraser
  where rephraser = Rephraser { rephraseExpLore = const TooVariant
                              , rephraseLetBoundLore = const Nothing
                              , rephraseFParamLore = const ()
                              , rephraseBodyLore = const ()
                              , rephraseRetType = id
                              }

data SCEntry = SufficientCond [[ScalExp]]
             deriving (Eq, Show)

type SCTable = HM.HashMap VName SCEntry

analyseBody :: ST.SymbolTable Basic -> SCTable -> Body -> SCTable
analyseBody _ sctable (Body _ [] _) =
  sctable

analyseBody vtable sctable (Body bodylore (bnd@(Let (Pattern [patElem]) _ e):bnds) res) =
  let vtable' = ST.insertBinding bnd vtable
      -- Construct a new sctable for recurrences.
      sctable' = case (analyseExp vtable e,
                       simplify <$> ST.lookupScalExp name vtable') of
        (Nothing, Just (Right se@(SE.RelExp SE.LTH0 ine)))
          | Int <- runReader (SE.scalExpType ine) types ->
          case AS.mkSuffConds se ranges types of
            Left err  -> error $ show err -- Why can this even fail?
            Right ses -> HM.insert name (SufficientCond ses) sctable
        (Just eSCTable, _) -> sctable <> eSCTable
        _                  -> sctable
  in analyseBody vtable' sctable' $ Body bodylore bnds res
  where name = patElemName patElem
        ranges = rangesRep vtable
        types = ST.typeEnv vtable
        simplify se = AS.simplify se ranges undefined
analyseBody vtable sctable (Body bodylore (bnd:bnds) res) =
  analyseBody (ST.insertBinding bnd vtable) sctable $ Body bodylore bnds res

rangesRep :: ST.SymbolTable lore -> AS.RangesRep
rangesRep = HM.filter nonEmptyRange . HM.map toRep . ST.bindings
  where toRep entry =
          (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper

analyseExp :: ST.SymbolTable Basic -> Exp -> Maybe SCTable
analyseExp vtable (LoopOp (DoLoop _ _ (ForLoop i bound) body)) =
  Just $ analyseExpBody vtable' body
  where vtable' = clampLower $ clampUpper vtable
        clampUpper = ST.insertLoopVar i bound
        -- If we enter the loop, then 'bound' is at least one.
        clampLower = case bound of Var v       -> v `ST.isAtLeast` 1
                                   Constant {} -> id
analyseExp vtable (LoopOp (DoLoop _ _ _ body)) =
  Just $ analyseExpBody vtable body
analyseExp vtable (LoopOp (Map _ fun arrs)) =
  Just $ analyseExpBody vtable' $ lambdaBody fun
  where vtable' = foldr (uncurry ST.insertArrayLParam) vtable $
                  zip params $ map Just arrs
        params = lambdaParams fun
analyseExp vtable (LoopOp (Redomap _ outerfun innerfun acc arrs)) =
  Just $ analyseExpBody vtable' (lambdaBody innerfun) <>
         analyseExpBody vtable (lambdaBody outerfun)
  where vtable' = foldr (uncurry ST.insertArrayLParam) vtable $
                  zip arrparams $ map Just arrs
        arrparams = drop (length acc) $ lambdaParams innerfun
analyseExp vtable (If cond tbranch fbranch _) =
  Just $ analyseExpBody (ST.updateBounds True cond vtable) tbranch <>
         analyseExpBody (ST.updateBounds False cond vtable) fbranch
analyseExp _ _ = Nothing

analyseExpBody :: ST.SymbolTable Basic -> Body -> SCTable
analyseExpBody vtable = analyseBody vtable mempty

data Sufficiency = Sufficient
                 | Exact
                 | Both
                   deriving (Eq, Ord, Show)

data Env m = Env { envSimplifyEnv :: Simplify.Env (VariantM m)
                 , envSCTable :: SCTable
                 , envMaxLoops :: Int
                 , envGeneratingSuff :: Sufficiency
                 }

data Res = Res { resNeed :: Simplify.Need Invariance
               , resSufficiented :: Any
               , resInvariant :: All
               }

instance Monoid Res where
  Res n1 s1 i1 `mappend` Res n2 s2 i2 = Res (n1<>n2) (s1<>s2) (i1<>i2)
  mempty = Res mempty mempty mempty

forbiddenIn :: VName -> Context m -> Bool
forbiddenIn name (env, vtable) =
  maybe False bad $ ST.lookup name vtable
  where bad entry = (ST.loopVariable entry &&
                     ST.bindingDepth entry > envMaxLoops env) ||
                    maybe False (isForbidden . snd . bindingLore)
                    (ST.entryBinding entry)

newtype VariantM m a = VariantM (RWST
                                 (Env m)
                                 Res
                                 (Simplify.State (VariantM m))
                                 m
                                 a)
                      deriving (Applicative, Functor, Monad, Alternative,
                                MonadReader (Env m),
                                MonadState (Simplify.State (VariantM m)),
                                MonadWriter Res)

type Context m = (Env m, ST.SymbolTable Invariance)

getContext :: MonadFreshNames m => VariantM m (Context m)
getContext = do
  env <- ask
  vtable <- Simplify.getVtable
  return (env, vtable)

generating :: Monad m => Sufficiency -> VariantM m a -> VariantM m a
generating suff = local $ \env -> env { envGeneratingSuff = suff }

generatingSuff :: Monad m => VariantM m Bool
generatingSuff = do x <- asks envGeneratingSuff
                    case x of Exact -> return False
                              _     -> return True

collectInvariance :: Monad m => VariantM m a -> VariantM m (a, Bool)
collectInvariance m = pass $ do
  (x, res) <- listen m
  return ((x, getAll $ resInvariant res),
          const $ res { resInvariant = All True })

notInvariant :: Monad m => VariantM m ()
notInvariant = tell $ mempty { resInvariant = All False }

instance MonadFreshNames m => MonadFreshNames (VariantM m) where
  getNameSource = VariantM . lift $ getNameSource
  putNameSource = VariantM . lift . putNameSource

instance Monad m => HasTypeEnv (VariantM m) where

runVariantM :: (Functor m, Monad m) =>
               Env m -> VariantM m a -> m (a, Bool)
runVariantM env (VariantM m) =
  -- FIXME: We should also check resInvariant, but that's still
  -- hopelessly broken.
  second (getAny . resSufficiented) <$>
  evalRWST m env Simplify.emptyState

-- | We actually changed something to a sufficient condition.
sufficiented :: Monad m => VariantM m ()
sufficiented = tell $ mempty { resSufficiented = Any True }

instance MonadFreshNames m => MonadBinder (VariantM m) where
  type Lore (VariantM m) = Invariance
  mkLetM pat e = do
    context <- getContext
    let explore = if forbiddenExp context e
                  then TooVariant
                  else Invariant
        pat' = Pattern [ S.PatElem v BindVar Nothing | v <- patternIdents pat ]
    return $ mkWiseLetBinding pat' explore e

  mkBodyM bnds res =
    return $ mkWiseBody () bnds res

  addBinding      = Simplify.addBindingEngine
  collectBindings = Simplify.collectBindingsEngine

instance MonadFreshNames m =>
         Simplify.MonadEngine (VariantM m) where
  type InnerLore (VariantM m) = Invariance'
  askEngineEnv = asks envSimplifyEnv
  localEngineEnv f = local $ \env ->
    env { envSimplifyEnv = f $ envSimplifyEnv env }
  tellNeed need = tell $ Res need mempty mempty
  listenNeed = liftM (second resNeed) . listen
  getEngineState = get
  putEngineState = put

  passNeed m = pass $ do (x, f) <- m
                         return (x, \(Res need suff inv) -> Res (f need) suff inv)

  simplifyBody ds (Body _ [] res) = do
    res' <- Simplify.simplifyResult ds res
    suff <- generatingSuff
    if not suff
      then return res'
      else do
      let inspect se
            | True = liftM pure $ sufficientSubExp se
            | otherwise = do se' <- sufficientSubExp se
                             return [se, se']
      ses <- liftM concat $ mapM inspect $ resultSubExps res
      Simplify.simplifyResult ds res' { resultSubExps = ses }

  simplifyBody ds (Body bodylore (bnd:bnds) res) = do
    --trace ("body " ++ show (patternIdents $ bindingPattern bnd)) $
    generating Exact $ Simplify.simplifyBinding bnd
    Simplify.simplifyBody ds $ Body bodylore bnds res

  inspectBinding bnd@(Let pat lore e) = do
    -- If the binding has any boolean patElems, we need to try to
    -- compute a sufficient binding (if we are not already doing
    -- that).  XXX: this is pretty slow.
    suff <- generatingSuff
    if suff || Basic Bool `notElem` patternTypes pat || not (HS.null $ consumedInExp pat e) --  || any ("suff" `isInfixOf`) (map (textual . identName) $ patternIdents pat)
      then Simplify.defaultInspectBinding bnd
      else do
        vs <- mapM (newIdent' (<>"_suff")) $ patternIdents pat
        suffe <- generating Sufficient $
                 Simplify.simplifyExp =<< renameExp (removeExpWisdom e)
        let pat' = pat { patternElements =
                            zipWith tagPatElem (patternElements pat) $
                            map identName vs
                       }
            tagPatElem patElem v =
              patElem `setPatElemLore` (fst $ patElemLore patElem, Just v)
            suffpat = Pattern [ S.PatElem v BindVar Nothing | v <- vs ]
        makeSufficientBinding =<< mkLetM (addWisdomToPattern suffpat suffe) suffe
        Simplify.defaultInspectBinding $ Let pat' lore e

  simplifyLetBoundLore Nothing  = return Nothing
  simplifyLetBoundLore (Just v) = Just <$> Simplify.simplifyVName v

  simplifyFParamLore =
    return

  simplifyRetType    =
    liftM ExtRetType . mapM Simplify.simplifyExtType . retTypeValues

makeSufficientBinding :: MonadFreshNames m => S.Binding Invariance -> VariantM m ()
makeSufficientBinding bnd = do
  context <- getContext
  makeSufficientBinding' context bnd

makeSufficientBinding' :: MonadFreshNames m => Context m -> S.Binding Invariance -> VariantM m ()
makeSufficientBinding' context@(_,vtable) (Let pat _ e)
  | Just (Right se@(SE.RelExp SE.LTH0 ine)) <-
      simplify <$> SE.toScalExp (`suffScalExp` vtable) e,
    Int <- runReader (SE.scalExpType ine) types,
    Right suff <- AS.mkSuffConds se ranges types,
    x:xs <- filter (scalExpUsesNoForbidden context) $ map mkConj suff = do
  suffe <- SE.fromScalExp' $ foldl SE.SLogOr x xs
  letBind_ pat suffe
  where ranges = rangesRep vtable
        types = ST.typeEnv vtable
        simplify se = AS.simplify se ranges types
        mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs
makeSufficientBinding' _ (Let pat _ (PrimOp (BinOp LogAnd x y t))) = do
  x' <- sufficientSubExp x
  y' <- sufficientSubExp y
  letBind_ pat $ PrimOp $ BinOp LogAnd x' y' t
makeSufficientBinding' env (Let pat _ (If (Var v) tbranch fbranch [Basic Bool]))
  | v `forbiddenIn` env,
    -- FIXME: Check that tbranch and fbranch are safe.  We can do
    -- something smarter if 'v' actually comes from an 'or'.  Also,
    -- currently only handles case where pat is a singleton boolean.
    Body _ tbnds (Result [tres]) <- tbranch,
    Body _ fbnds (Result [fres]) <- fbranch,
    all safeBnd tbnds, all safeBnd fbnds = do
  mapM_ addBinding tbnds
  mapM_ addBinding fbnds
  letBind_ pat $ PrimOp $ BinOp LogAnd tres fres Bool
  where safeBnd = safeExp . bindingExp
makeSufficientBinding' _ bnd = Simplify.defaultInspectBinding bnd

-- | Like 'ST.lookupScalExp', but uses sufficient bindings if they exist.
suffScalExp :: VName -> ST.SymbolTable Invariance -> Maybe ScalExp
suffScalExp name vtable = asSuffScalExp =<< ST.lookup name vtable
  where asSuffScalExp entry
          | Just (_, Just suff) <- ST.entryLetBoundLore entry,
            Just se             <- suffScalExp suff vtable =
              Just se
          | otherwise = ST.asScalExp entry

sufficientSubExp :: MonadFreshNames m => SubExp -> VariantM m SubExp
sufficientSubExp se@(Constant {}) = return se
sufficientSubExp (Var v) =
  maybe (Var v) Var .
  (snd <=< ST.entryLetBoundLore <=< ST.lookup v) <$>
  Simplify.getVtable

scalExpUsesNoForbidden :: Context m -> ScalExp -> Bool
scalExpUsesNoForbidden context =
  not . any (`forbiddenIn` context) . freeIn

-- | The lore containing invariance information.
data Variance = Invariant
              | TooVariant
              deriving (Eq, Ord, Show)

instance FreeIn Variance where
  freeIn = const mempty
instance Rename Variance where
  rename = return
instance Substitute Variance where
  substituteNames = flip const

isForbidden :: Variance -> Bool
isForbidden Invariant = False
isForbidden TooVariant = True

data Invariance' = Invariance'
instance Lore.Lore Invariance' where
  type LetBound Invariance' = Maybe VName
  type Exp Invariance' = Variance
  representative = Invariance'
  loopResultContext _ = loopResultContext (representative :: Basic)
instance Ranged Invariance' where
instance PrettyLore Invariance' where
instance Substitutable Invariance' where
instance Renameable Invariance' where
instance Proper Invariance' where
instance Simplify.Simplifiable Invariance' where

type Invariance = Wise Invariance'

removeInvariance :: Rephraser Invariance' Basic
removeInvariance = Rephraser { rephraseExpLore = const ()
                             , rephraseLetBoundLore = const ()
                             , rephraseBodyLore = const ()
                             , rephraseFParamLore = const ()
                             , rephraseRetType = id
                             }

forbiddenExp :: Context m -> S.Exp Invariance -> Bool
forbiddenExp context = isNothing . walkExpM walk
  where walk = Walker { walkOnSubExp  = checkIf forbiddenSubExp
                      , walkOnBody    = checkIf forbiddenBody
                      , walkOnBinding = checkIf $ isForbidden . snd . bindingLore
                      , walkOnVName   = checkIf forbiddenVar
                      , walkOnLambda  = checkIf $ forbiddenBody . lambdaBody
                      , walkOnRetType = checkIf forbiddenRetType
                      , walkOnFParam  = checkIf forbiddenFParam
                      , walkOnCertificates = mapM_ $ checkIf forbiddenVar
                      }
        checkIf f x = if f x
                      then Nothing
                      else Just ()

        forbiddenVar = (`forbiddenIn` context)

        forbiddenSubExp (Var v) = v `forbiddenIn` context
        forbiddenSubExp (Constant {}) = False

        forbiddenBody = any (isForbidden . snd . bindingLore) . bodyBindings

        forbiddenRetType = any forbiddenVar . freeIn
        forbiddenFParam = any forbiddenVar . freeIn
