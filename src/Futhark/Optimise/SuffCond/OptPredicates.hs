{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Futhark.Optimise.SuffCond.OptPredicates
       (
         optimisePredicates
       )
       where

import Control.Applicative
import Control.Arrow (second)
import Data.Loc
import Data.List (isInfixOf)
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
import qualified Futhark.Optimise.Simplifier.Engine as Simplify
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Optimise.Simplifier (simplifyFun)
import Futhark.Substitute
import Futhark.Analysis.Rephrase
import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as S
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (Aliases, mkAliasedBody, mkAliasedLetBinding, removeExpAliases)

import Prelude hiding (any)

optimisePredicates :: MonadFreshNames m =>
                      RuleBook (VariantM m) -> Prog -> m Prog
optimisePredicates rules prog = do
  optimPreds <- mapM (maybeOptimiseFun rules) origfuns
  let newfuns = concat optimPreds
      subst = HM.fromList $
              zip (map funName origfuns) $
              map (map funName) optimPreds
  insertPredicateCalls subst $ Prog $ origfuns ++ newfuns
  where origfuns = progFunctions prog
        funName (fname,_,_,_,_) = fname

insertPredicateCalls :: MonadFreshNames m =>
                        HM.HashMap Name [Name] -> Prog -> m Prog
insertPredicateCalls subst prog =
  Prog <$> mapM treatFunction (progFunctions prog)
  where treatFunction (fname,rettype,params,fbody,loc) = do
          fbody' <- treatBody fbody
          return (fname,rettype,params,fbody',loc)
        treatBody (Body _ bnds res) = do
          bnds' <- mapM treatBinding bnds
          return $ mkBody (concat bnds') res
        treatLambda lam = do
          body <- treatBody $ lambdaBody lam
          return $ lam { lambdaBody = body }
        treatBinding (Let pat _ e) = do
          (e', bnds) <- treatExp e
          return $ bnds ++ [Let pat () e']
        treatExp e@(Apply predf predargs predt predloc)
          | Just preds <- HM.lookup predf subst =
            runBinder'' $ callPreds predt preds e $ \predf' ->
            Apply predf' predargs predt predloc
        treatExp e = do
          e' <- mapExpM mapper e
          return (e', [])
          where mapper = identityMapper { mapOnBody = treatBody
                                        , mapOnLambda = treatLambda
                                        }
        callPreds _ [] e _            = return e
        callPreds predt (f:fs) e call = do
          c <- letSubExp (nameToString f ++ "_result") $ call f
          let predloc = srclocOf c
          eIf (pure $ PrimOp $ SubExp c)
            (eBody [pure $ PrimOp $ SubExp $ constant True predloc])
            (eBody [callPreds predt fs e call])
            predt predloc

maybeOptimiseFun :: MonadFreshNames m =>
                    RuleBook (VariantM m) -> FunDec -> m [FunDec]
maybeOptimiseFun rules fundec@(_,[Basic Bool],_,body,_) = do
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
generateOptimisedPredicates' rules (fname, rettype, params, body, loc) suff sctable depth = do
  res <- runVariantM env $ Simplify.insertAllBindings $
         Simplify.simplifyBody (map diet rettype) $
         rephraseWithInvariance body
  case res of
    (body', _) -> do
      fundec <- simplifyFun (fname', rettype, params, removeBodyLore body', loc)
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
                              , rephraseBindeeLore = const Nothing
                              , rephraseBodyLore = const ()
                              }

data SCEntry = SufficientCond [[ScalExp]]
             deriving (Eq, Show)

type SCTable = HM.HashMap VName SCEntry

analyseBody :: ST.SymbolTable Basic -> SCTable -> Body -> SCTable
analyseBody _ sctable (Body _ [] _) =
  sctable

analyseBody vtable sctable (Body bodylore (bnd@(Let (Pattern [bindee]) _ e):bnds) res) =
  let vtable' = ST.insertBinding bnd vtable
      -- Construct a new sctable for recurrences.
      sctable' = case (analyseExp vtable e,
                       simplify <$> ST.lookupScalExp name vtable') of
        (Nothing, Just (Right se@(SE.RelExp SE.LTH0 ine)))
          | Int <- SE.scalExpType ine ->
          case AS.mkSuffConds se loc ranges of
            Left err  -> error $ show err -- Why can this even fail?
            Right ses -> HM.insert name (SufficientCond ses) sctable
        (Just eSCTable, _) -> sctable <> eSCTable
        _                  -> sctable
  in analyseBody vtable' sctable' $ Body bodylore bnds res
  where name = bindeeName bindee
        ranges = rangesRep vtable
        loc = srclocOf e
        simplify se = AS.simplify se loc ranges
analyseBody vtable sctable (Body bodylore (bnd:bnds) res) =
  analyseBody (ST.insertBinding bnd vtable) sctable $ Body bodylore bnds res

rangesRep :: ST.SymbolTable lore -> AS.RangesRep
rangesRep = HM.filter nonEmptyRange . HM.map toRep . ST.bindings
  where toRep entry =
          (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper

analyseExp :: ST.SymbolTable Basic -> Exp -> Maybe SCTable
analyseExp vtable (LoopOp (DoLoop _ _ i bound body _)) =
  Just $ analyseExpBody vtable' body
  where vtable' = clampLower $ clampUpper vtable
        clampUpper = ST.insertLoopVar (identName i) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampLower = case bound of Var v       -> identName v `ST.isAtLeast` 1
                                   Constant {} -> id
analyseExp vtable (LoopOp (Map _ fun arrs _)) =
  Just $ analyseExpBody vtable' $ lambdaBody fun
  where vtable' = foldr (uncurry ST.insertArrayParam) vtable $ zip params arrs
        params = lambdaParams fun
analyseExp vtable (LoopOp (Redomap _ outerfun innerfun acc arrs _)) =
  Just $ analyseExpBody vtable' (lambdaBody innerfun) <>
         analyseExpBody vtable (lambdaBody outerfun)
  where vtable' = foldr (uncurry ST.insertArrayParam) vtable $ zip arrparams arrs
        arrparams = drop (length acc) $ lambdaParams innerfun
analyseExp vtable (If cond tbranch fbranch _ _) =
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
                    maybe False (isForbidden . snd . bindingLore . snd) (ST.entryBinding entry)

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
      then mkBodyM [] res'
      else do
      let inspect se
            | True = liftM pure $ sufficientSubExp se
            | otherwise = do se' <- sufficientSubExp se
                             return [se, se']
      ses <- liftM concat $ mapM inspect $ resultSubExps res
      res'' <- Simplify.simplifyResult ds res' { resultSubExps = ses }
      mkBodyM [] res''

  simplifyBody ds (Body bodylore (bnd:bnds) res) = do
    --trace ("body " ++ show (patternIdents $ bindingPattern bnd)) $
    generating Exact $ Simplify.simplifyBinding bnd
    Simplify.simplifyBody ds $ Body bodylore bnds res

  inspectBinding bnd@(Let pat lore e) = do
    -- If the binding has any boolean bindees, we need to try to
    -- compute a sufficient binding (if we are not already doing
    -- that).  XXX: this is pretty slow.
    suff <- generatingSuff
    if suff || Basic Bool `notElem` patternTypes pat || not (HS.null $ consumedInExp e) --  || any ("suff" `isInfixOf`) (map (textual . identName) $ patternIdents pat)
      then Simplify.defaultInspectBinding bnd
      else do
        vs <- mapM (newIdent' (<>"_suff")) $ patternIdents pat
        let pat' = pat { patternBindees =
                            zipWith tagBindee (patternBindees pat) vs
                       }
            tagBindee bindee v =
              bindee { bindeeLore = (fst $ bindeeLore bindee, Just v) }
        suffe <- generating Sufficient $
                 Simplify.simplifyExp =<< renameExp (removeExpAliases e)
        makeSufficientBinding =<< mkLetM vs suffe
        Simplify.defaultInspectBinding $ Let pat' lore e

makeSufficientBinding :: MonadFreshNames m => S.Binding Invariance -> VariantM m ()
makeSufficientBinding bnd = do
  context <- getContext
  makeSufficientBinding' context bnd

makeSufficientBinding' :: MonadFreshNames m => Context m -> S.Binding Invariance -> VariantM m ()
makeSufficientBinding' context@(_,vtable) (Let pat _ e)
  | Just (Right se@(SE.RelExp SE.LTH0 ine)) <-
      simplify <$> SE.toScalExp (`suffScalExp` vtable) e,
    Int <- SE.scalExpType ine,
    Right suff <- AS.mkSuffConds se loc ranges,
    x:xs <- filter (scalExpUsesNoForbidden context) $ map mkConj suff = do
  suffe <- SE.fromScalExp' (srclocOf e) $ foldl SE.SLogOr x xs
  letBindPat pat suffe
  where ranges = rangesRep vtable
        simplify se = AS.simplify se loc ranges
        loc = srclocOf e
        mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs
makeSufficientBinding' _ (Let pat _ (PrimOp (BinOp LogAnd x y t loc))) = do
  x' <- sufficientSubExp x
  y' <- sufficientSubExp y
  letBindPat pat $ PrimOp $ BinOp LogAnd x' y' t loc
makeSufficientBinding' env (Let pat _ (If (Var v) tbranch fbranch _ loc))
  | identName v `forbiddenIn` env,
    -- FIXME: Check that tbranch and fbranch are safe.  We can do
    -- something smarter if 'v' actually comes from an 'or'.  Also,
    -- currently only handles case where pat is a singleton boolean.
    Body _ tbnds (Result _ [tres] _) <- tbranch,
    Body _ fbnds (Result _ [fres] _) <- fbranch,
    Basic Bool <- subExpType tres,
    Basic Bool <- subExpType fres,
    all safeBnd tbnds, all safeBnd fbnds = do
  mapM_ addBinding tbnds
  mapM_ addBinding fbnds
  letBindPat pat $ PrimOp $ BinOp LogAnd tres fres (Basic Bool) loc
  where safeBnd = safeExp . bindingExp
makeSufficientBinding' _ bnd = Simplify.defaultInspectBinding bnd

-- | Like 'ST.lookupScalExp', but uses sufficient bindings if they exist.
suffScalExp :: VName -> ST.SymbolTable Invariance -> Maybe ScalExp
suffScalExp name vtable = asSuffScalExp =<< ST.lookup name vtable
  where asSuffScalExp entry
          | Just ((_, Just suff), _) <- ST.entryBinding entry,
            Just se                  <- suffScalExp (identName suff) vtable =
              Just se
          | otherwise = ST.asScalExp entry

sufficientSubExp :: MonadFreshNames m => SubExp -> VariantM m SubExp
sufficientSubExp se@(Constant {}) = return se
sufficientSubExp (Var v) =
  maybe (Var v) Var .
  (snd . fst <=< ST.entryBinding <=< ST.lookup (identName v)) <$>
  Simplify.getVtable

scalExpUsesNoForbidden :: Context m -> ScalExp -> Bool
scalExpUsesNoForbidden context =
  not . any (`forbiddenIn` context) . HS.fromList . map identName . SE.getIds

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

data Invariance'
instance Lore.Lore Invariance' where
  type Binding Invariance' = Maybe Ident
  type Exp Invariance' = Variance
instance PrettyLore Invariance' where
instance Substitutable Invariance' where
instance Renameable Invariance' where
instance Proper Invariance' where

type Invariance = Aliases Invariance'

instance MonadFreshNames m => BindableM (VariantM m) where
  type Lore (VariantM m) = Invariance
  mkLetM pat e = do
    context <- getContext
    let explore = if forbiddenExp context e
                  then TooVariant
                  else Invariant
        pat' = Pattern $ map (`Bindee` Nothing) pat
    return $ mkAliasedLetBinding pat' explore e
  mkBodyM bnds res =
    return $ mkAliasedBody () bnds res

forbiddenExp :: Context m -> S.Exp Invariance -> Bool
forbiddenExp context = isNothing . walkExpM walk
  where walk = Walker { walkOnSubExp  = checkIf forbiddenSubExp
                      , walkOnType    = checkIf forbiddenType
                      , walkOnBody    = checkIf forbiddenBody
                      , walkOnBinding = checkIf $ isForbidden . snd . bindingLore
                      , walkOnIdent   = checkIf forbiddenIdent
                      , walkOnValue   = const $ return ()
                      , walkOnCertificates = mapM_ $ checkIf forbiddenIdent
                      , walkOnLambda  = checkIf $ forbiddenBody . lambdaBody
                      }
        checkIf f x = if f x
                      then Nothing
                      else Just ()

        forbiddenIdent = (`forbiddenIn` context) . identName

        forbiddenSubExp (Var v) = identName v `forbiddenIn` context
        forbiddenSubExp (Constant {}) = False

        forbiddenType t = any (`forbiddenIn` context) $ freeNamesIn t

        forbiddenBody = any (isForbidden . snd . bindingLore) . bodyBindings
