{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Futhark.Optimise.SuffCond.OptPredicates
       (
         optimisePredicates
       )
       where

import Control.Applicative
import Control.Arrow (second)
import Data.Loc
import Data.Foldable (any)
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.InternalRep
import Futhark.MonadFreshNames
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Analysis.ScalExp (ScalExp)
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Analysis.AlgSimplify as AS
import Futhark.Tools
import qualified Futhark.Optimise.Simplifier.Engine as Simplify
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Substitute

import Prelude hiding (any)

optimisePredicates :: MonadFreshNames m =>
                      RuleBook Forbidden -> Prog -> m Prog
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
        treatBody (Body bnds res) = do
          bnds' <- mapM treatBinding bnds
          return $ Body (concat bnds') res
        treatLambda lam = do
          body <- treatBody $ lambdaBody lam
          return $ lam { lambdaBody = body }
        treatBinding (Let pat e) = do
          (e', bnds) <- treatExp e
          return $ bnds ++ [Let pat e']
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
          eIf (pure $ SubExp c)
            (eBody [pure $ SubExp $ constant True predloc])
            (eBody [callPreds predt fs e call])
            predt predloc

maybeOptimiseFun :: MonadFreshNames m =>
                    RuleBook Forbidden -> FunDec -> m [FunDec]
maybeOptimiseFun rules fundec@(_,[Basic Bool],_,body,_) = do
  let sctable = analyseBody (ST.empty :: ST.SymbolTable ()) mempty body
  generateOptimisedPredicates rules fundec sctable
maybeOptimiseFun _ _ = return []

generateOptimisedPredicates :: MonadFreshNames m =>
                      RuleBook Forbidden -> FunDec -> SCTable -> m [FunDec]
generateOptimisedPredicates rules fundec sctable = do
  o1pred <- generateOptimisedPredicates' rules fundec "_0" sctable 0
  onpred <- generateOptimisedPredicates' rules fundec "_1" sctable 1
  return $ catMaybes [o1pred , onpred]

generateOptimisedPredicates' :: MonadFreshNames m =>
                       RuleBook Forbidden -> FunDec -> String
                    -> SCTable -> Int -> m (Maybe FunDec)
generateOptimisedPredicates' rules (fname, rettype, params, body, loc) suff sctable depth = do
  res <- runVariantM env $ Simplify.insertAllBindings $ Simplify.simplifyBody body
  case res of
    (body', True) -> return $ Just (fname', rettype, params, body', loc)
    _             -> return Nothing
  where fname' = fname <> nameFromString suff
        env = Env { envSimplifyEnv = Simplify.emptyEnv rules Nothing
                  , envSCTable = sctable
                  , envMaxLoops = depth
                  }

data SCEntry = SufficientCond [[ScalExp]] ScalExp
             deriving (Eq, Show)

type SCTable = HM.HashMap VName SCEntry

analyseBody :: ST.SymbolTable u -> SCTable -> Body -> SCTable
analyseBody _ sctable (Body [] _) =
  sctable

analyseBody vtable sctable (Body (bnd@(Let [v] e):bnds) res) =
  let vtable' = ST.insertBinding bnd vtable
      -- Construct a new sctable for recurrences.
      sctable' = case (analyseExp vtable e,
                       simplify <$> ST.lookupScalExp name vtable') of
        (Nothing, Just (Right se@(SE.RelExp SE.LTH0 ine)))
          | Int <- SE.scalExpType ine ->
          case AS.mkSuffConds se loc ranges of
            Left err  -> error $ show err -- Why can this even fail?
            Right ses -> HM.insert name (SufficientCond ses se) sctable
        (Just eSCTable, _) -> sctable <> eSCTable
        _                  -> sctable
  in analyseBody vtable' sctable' $ Body bnds res
  where name = identName v
        ranges = rangesRep vtable
        loc = srclocOf e
        simplify se = AS.simplify se loc ranges
analyseBody vtable sctable (Body (bnd:bnds) res) =
  analyseBody (ST.insertBinding bnd vtable) sctable $ Body bnds res

rangesRep :: ST.SymbolTable u -> AS.RangesRep
rangesRep = HM.filter nonEmptyRange . HM.map toRep . ST.bindings
  where toRep entry =
          (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper

analyseExp :: ST.SymbolTable u -> Exp -> Maybe SCTable
analyseExp vtable (DoLoop _ _ i bound body _) =
  Just $ analyseExpBody vtable' body
  where vtable' = clampLower $ clampUpper vtable
        clampUpper = ST.insertLoopVar (identName i) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampLower = case bound of Var v       -> identName v `ST.isAtLeast` 1
                                   Constant {} -> id
analyseExp vtable (Map _ fun arrs _) =
  Just $ analyseExpBody vtable' $ lambdaBody fun
  where vtable' = foldr (uncurry ST.insertArrayParam) vtable $ zip params arrs
        params = lambdaParams fun
analyseExp vtable (Redomap _ outerfun innerfun acc arrs _) =
  Just $ analyseExpBody vtable' (lambdaBody innerfun) <>
         analyseExpBody vtable (lambdaBody outerfun)
  where vtable' = foldr (uncurry ST.insertArrayParam) vtable $ zip arrparams arrs
        arrparams = drop (length acc) $ lambdaParams innerfun
analyseExp vtable (If cond tbranch fbranch _ _) =
  Just $ analyseExpBody (ST.updateBounds True cond vtable) tbranch <>
         analyseExpBody (ST.updateBounds False cond vtable) fbranch
analyseExp _ _ = Nothing

analyseExpBody :: ST.SymbolTable u -> Body -> SCTable
analyseExpBody vtable = analyseBody vtable mempty

newtype Forbidden = Forbidden { isForbidden :: Bool }
                    deriving (Eq, Ord, Show)

instance Substitute Forbidden where
  substituteNames _ x = x

instance ST.UserAnnotation Forbidden where
  annotationFor = const $ Forbidden False

data Env = Env { envSimplifyEnv :: Simplify.Env Forbidden
               , envSCTable :: SCTable
               , envMaxLoops :: Int
               }

data Res = Res { resNeed :: Simplify.Need Forbidden
               , resSufficiented :: Any
               , resInvariant :: All
               }

instance Monoid Res where
  Res n1 s1 i1 `mappend` Res n2 s2 i2 = Res (n1<>n2) (s1<>s2) (i1<>i2)
  mempty = Res mempty mempty mempty

forbiddenIn :: VName -> Env -> Bool
forbiddenIn name env =
  maybe False bad $ ST.lookup name $ Simplify.envVtable $ envSimplifyEnv env
  where bad entry = (ST.loopVariable entry &&
                     ST.bindingDepth entry > envMaxLoops env) ||
                    isForbidden (ST.userAnnotation entry)

newtype VariantM m a = VariantM (ReaderT Env
                                    (WriterT Res m)
                                   a)
                      deriving (Applicative, Functor, Monad, Alternative,
                                MonadReader Env,
                                MonadWriter Res)

collectInvariance :: Monad m => VariantM m a -> VariantM m (a, Bool)
collectInvariance m = pass $ do
  (x, res) <- listen m
  return ((x, getAll $ resInvariant res),
          const $ res { resInvariant = All True })

notInvariant :: Monad m => VariantM m ()
notInvariant = tell $ mempty { resInvariant = All False }

instance MonadFreshNames m => MonadFreshNames (VariantM m) where
  getNameSource = VariantM . lift . lift $ getNameSource
  putNameSource = VariantM . lift . lift . putNameSource

runVariantM :: Functor m => Env -> VariantM m a
             -> m (a, Bool)
runVariantM env (VariantM m) =
  -- FIXME: We should also check resInvariant, but that's still
  -- hopelessly broken.
  second (getAny . resSufficiented) <$>
  runWriterT (runReaderT m env)

-- | We actually changed something to a sufficient condition.
sufficiented :: Monad m => VariantM m ()
sufficiented = tell $ mempty { resSufficiented = Any True }

instance MonadFreshNames m =>
         Simplify.MonadEngine Forbidden (VariantM m) where
  askEngineEnv = asks envSimplifyEnv
  localEngineEnv f = local $ \env ->
    env { envSimplifyEnv = f $ envSimplifyEnv env }
  tellNeed need = tell $ Res need mempty mempty
  listenNeed = liftM (second resNeed) . listen

  passNeed m = pass $ do (x, f) <- m
                         return (x, \(Res need suff inv) -> Res (f need) suff inv)

  simplifyBody (Body [] res) = do
    env <- ask
    when (any (`forbiddenIn` env) names) notInvariant
    Body [] <$> Simplify.simplifyResult res
    where names = mapMaybe asName $ resultSubExps res
          asName (Var v)       = Just $ identName v
          asName (Constant {}) = Nothing

  simplifyBody (Body (bnd:bnds) res) = do
    ((hoisted, bnd'),invariant) <-
      collectInvariance $
      inspect =<< Simplify.simplifyBinding bnd
    Simplify.localVtable (ST.insertEntries hoisted) $
      case bnd' of
        Left newbnds ->
          Simplify.simplifyBody $ Body (newbnds++bnds) res
        Right (Let pat' e') ->
          Simplify.bindLetWith (const $ Forbidden $ not invariant) pat' e' $
          Simplify.simplifyBody $ Body bnds res
    where inspect (wrap, bnd') = do
            bnd'' <- either (return . Left) checkVariance bnd'
            return (wrap, bnd'')

checkVariance :: MonadFreshNames m => Binding -> VariantM m (Either [Binding] Binding)
checkVariance (Let pat e)
  | isLoop e = do
    maxLoops <- asks envMaxLoops
    depth <- Simplify.asksEngineEnv $ ST.depth . Simplify.envVtable
    when (depth >= maxLoops) notInvariant
    return $ Right $ Let pat e
  | otherwise = do
    let names = freeNamesInExp e
    env <- ask
    if any (`forbiddenIn` env) names then
      makeInvariant env $ Let pat e
    else return $ Right $ Let pat e
  where isLoop (Redomap {}) = True
        isLoop (Reduce {}) = True
        isLoop (DoLoop {}) = True
        isLoop (Scan {}) = True
        isLoop (Filter {}) = True
        isLoop (Map {}) = True
        isLoop (Apply {}) = True -- Treat funcalls as recurrences.
        isLoop _ = False

makeInvariant :: MonadFreshNames m =>
                 Env
              -> Binding -> VariantM m (Either [Binding] Binding)
makeInvariant env (Let [v] e)
  | Just (Right se@(SE.RelExp SE.LTH0 ine)) <- -- Why can this fail?
      simplify <$> SE.toScalExp (`ST.lookupScalExp` vtable) e,
    Int <- SE.scalExpType ine,
    Right suff <- AS.mkSuffConds se loc ranges,
    x:xs <- filter (scalExpUsesNoForbidden env) $ map mkConj suff = do
      (e', bnds) <- SE.fromScalExp loc $ foldl SE.SLogOr x xs
      sufficiented
      return $ Left $ bnds ++ [Let [v] e']
  where mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs
        loc = srclocOf e
        ranges = rangesRep vtable
        vtable = Simplify.envVtable $ envSimplifyEnv env
        simplify se = AS.simplify se loc ranges
makeInvariant env bnd@(Let pat (If (Var v) tbranch fbranch t loc)) = do
  let se = exactBinding (envSCTable env) v
  if scalExpUsesNoForbidden env se then
    case se of
      SE.Id v'
        | v' == v ->
          return $ Right $ Let pat $ If (Var v) tbranch fbranch t loc
      _ -> do (exbnds,v') <- scalExpToIdent v se
              return $ Left $ exbnds ++ [Let pat $ If (Var v') tbranch fbranch t loc]
    else
    -- FIXME: Check that tbranch and fbranch are safe.  We can do
    -- something smarter if 'v' actually comes from an 'or'.  Also,
    -- currently only handles case where pat is a singleton boolean.
    case (tbranch, fbranch) of
      (Body tbnds (Result _ [tres] _),
       Body fbnds (Result _ [fres] _))
        | Basic Bool <- subExpType tres,
          Basic Bool <- subExpType fres,
          all safeBnd tbnds, all safeBnd fbnds -> do
        sufficiented
        return $ Left $ tbnds ++ fbnds ++
                 [Let pat $ BinOp LogAnd tres fres (Basic Bool) loc]
      _ -> do notInvariant
              return $ Right bnd
  where safeBnd (Let _ e) = safeExp e
makeInvariant _ (Let pat e) = do
  notInvariant
  return $ Right $ Let pat e

exactBinding :: SCTable -> Ident -> ScalExp
exactBinding sctable v
  | Just (SufficientCond _ exact) <- HM.lookup (identName v) sctable =
    exact
  | otherwise =
    SE.Id v

scalExpToIdent :: MonadFreshNames m =>
                  Ident -> ScalExp -> m ([Binding], Ident)
scalExpToIdent v se = do
  (e', bnds) <- SE.fromScalExp (srclocOf v) se
  v' <- newIdent' (++"exact") v
  return (bnds ++ [Let [v'] e'], v')

scalExpUsesNoForbidden :: Env -> ScalExp -> Bool
scalExpUsesNoForbidden env =
  not . any (`forbiddenIn` env) . HS.fromList . map identName . SE.getIds
