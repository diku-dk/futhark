module Futhark.Optimise.SuffCond.OptPredicates
       (
         optimisePredicates
       )
       where

import Control.Applicative
import Control.Arrow (second)
import Data.Loc
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.InternalRep
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.SymbolTable as ST
import Futhark.Optimise.ScalExp (ScalExp)
import qualified Futhark.Optimise.ScalExp as SE
import qualified Futhark.Optimise.AlgSimplify as AS
import Futhark.Tools

optimisePredicates :: MonadFreshNames m => Prog -> m Prog
optimisePredicates prog = do
  optimPreds <- mapM maybeOptimiseFun origfuns
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

maybeOptimiseFun :: MonadFreshNames m => FunDec -> m [FunDec]
maybeOptimiseFun fundec@(_,[Basic Bool],_,body,_) = do
  let sctable = analyseBody ST.empty mempty body
  generatePredicates fundec sctable
maybeOptimiseFun _ = return []

generatePredicates :: MonadFreshNames m =>
                      FunDec -> SCTable -> m [FunDec]
generatePredicates fundec@(_,_,_,body,_) sctable = do
  o1pred <- generatePredicates' fundec "_0" sctable HS.empty
  onpred <- generatePredicates' fundec "_1" sctable $ allOutermostLoops body
  return $ catMaybes [o1pred , onpred]

generatePredicates' :: MonadFreshNames m =>
                       FunDec -> String
                    -> SCTable -> Loops -> m (Maybe FunDec)
generatePredicates' (fname, rettype, params, body, loc) suff sctable loops = do
  res <- runVariantM $ bodyVariantIn mempty sctable loops body
  case res of
    (Just body', True) -> return $ Just (fname', rettype, params, body', loc)
    _                  -> return Nothing
  where fname' = fname <> nameFromString suff

data SCEntry = SufficientCond [[ScalExp]]
             | SCTable SCTable
             deriving (Eq, Show)

type SCTable = HM.HashMap VName SCEntry

type Loops = Names

analyseBody :: ST.SymbolTable -> SCTable -> Body -> SCTable
analyseBody _ sctable (Body [] _) =
  sctable
analyseBody vtable sctable (Body (Let [v] e:bnds) res) =
  let vtable' = ST.insert name e vtable
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
  in analyseBody vtable' sctable' $ Body bnds res
  where name = identName v
        ranges = rangesRep vtable
        loc = srclocOf e
        simplify se = AS.simplify se loc True ranges
analyseBody vtable sctable (Body (Let _ _:bnds) res) =
  -- Not adding to symbol table here - this leaves a hole, but this
  -- looks like a weird binding anyway.
  analyseBody vtable sctable $ Body bnds res

rangesRep :: ST.SymbolTable -> AS.RangesRep
rangesRep = HM.filter nonEmptyRange . HM.map toRep . ST.bindings
  where toRep entry =
          (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper

analyseExp :: ST.SymbolTable -> Exp -> Maybe SCTable
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
analyseExp vtable (If _ tbranch fbranch _ _) =
  Just $ analyseExpBody vtable tbranch <>
         analyseExpBody vtable fbranch
analyseExp _ _ = Nothing

analyseExpBody :: ST.SymbolTable -> Body -> SCTable
analyseExpBody vtable = analyseBody vtable mempty

type VariantM m = MaybeT (WriterT Any m)

runVariantM :: Functor m => VariantM m a -> m (Maybe a, Bool)
runVariantM = fmap (second getAny) . runWriterT . runMaybeT

-- | We actually changed something to a sufficient condition.
sufficiented :: Monad m => VariantM m ()
sufficiented = tell $ Any True

type ForbiddenTable = Names

noneForbidden :: ForbiddenTable -> Names -> Bool
noneForbidden ftable = HS.null . HS.intersection ftable

forbidNames :: [VName] -> VName -> Loops -> ForbiddenTable -> ForbiddenTable
forbidNames names loop loops ftable
  | loop `HS.member` loops = ftable
  | otherwise              = foldr HS.insert ftable names

forbidParams :: [Param] -> VName -> Loops -> ForbiddenTable -> ForbiddenTable
forbidParams = forbidNames . map identName

bodyVariantIn :: MonadFreshNames m =>
                 ForbiddenTable -> SCTable -> Loops -> Body -> VariantM m Body
bodyVariantIn ftable sctable loops (Body bnds res) = do
  (ftable', bnds') <- foldM inspect (ftable,[]) bnds
  checkResult ftable' res
  return $ Body bnds' res
  where inspect (ftable', bnds') bnd@(Let pat _) = do
          optim <- bindingVariantIn ftable' sctable loops bnd
          case optim of
            Nothing ->
              return (ftable' `HS.union` HS.fromList (map identName pat),
                      bnds'++[bnd])
            Just newbnds ->
              return (ftable',
                      bnds'++newbnds)

checkResult :: Monad m => ForbiddenTable -> Result -> VariantM m ()
checkResult ftable (Result _ ses _)
  | noneForbidden ftable names = return ()
  | otherwise = fail "Result is not sufficiently invariant"
  where names = HS.fromList $ mapMaybe asName ses
        asName (Var v)       = Just $ identName v
        asName (Constant {}) = Nothing

bindingVariantIn :: MonadFreshNames m =>
                    ForbiddenTable -> SCTable -> Loops -> Binding -> VariantM m (Maybe [Binding])

-- We assume that a SOAC contributes only if it returns exactly a
-- single (boolean) value.
bindingVariantIn ftable sctable loops (Let [v] (Map cs fun args loc)) = do
  body <- bodyVariantIn (forbidParams (lambdaParams fun) name loops ftable)
          sctable loops $ lambdaBody fun
  return $ Just [Let [v] $ Map cs fun { lambdaBody = body } args loc]
  where name = identName v
bindingVariantIn ftable sctable loops (Let [v] (DoLoop res merge i bound body loc)) = do
  let names = identName i : map (identName . fst) merge
  body' <- bodyVariantIn (forbidNames names name loops ftable)
           sctable loops body
  return $ Just [Let [v] $ DoLoop res merge i bound body' loc]
  where name = identName v
bindingVariantIn ftable sctable loops (Let [v] (Redomap cs outerfun innerfun acc args loc)) = do
  outerbody <- bodyVariantIn ftable sctable loops $ lambdaBody outerfun
  let forbiddenParams = drop (length acc) $ lambdaParams innerfun
  innerbody <- bodyVariantIn (forbidParams forbiddenParams name loops ftable)
               sctable loops $ lambdaBody innerfun
  return $ Just [Let [v] $ Redomap cs
                 outerfun { lambdaBody = outerbody }
                 innerfun { lambdaBody = innerbody }
                 acc args loc]
  where name = identName v

bindingVariantIn ftable sctable loops (Let pat (If (Var v) tbranch fbranch t loc)) = do
  tbranch' <- bodyVariantIn ftable sctable loops tbranch
  fbranch' <- bodyVariantIn ftable sctable loops fbranch
  if noneForbidden ftable $ HS.singleton $ identName v then
    return $ Just [Let pat $ If (Var v) tbranch' fbranch' t loc]
    else
    -- FIXME: Check that tbranch and fbranch are safe.  We can do
    -- something smarter if 'v' actually comes from an 'or'.  Also,
    -- currently only handles case where pat is a singleton boolean.
    case (tbranch', fbranch') of
      (Body tbnds (Result _ [tres] _),
       Body fbnds (Result _ [fres] _))
        | Basic Bool <- subExpType tres,
          Basic Bool <- subExpType fres,
          all safeBnd tbnds, all safeBnd fbnds ->
        return $ Just $ tbnds ++ fbnds ++
                        [Let pat $ BinOp LogAnd tres fres (Basic Bool) loc]
      _ -> return Nothing
  where safeBnd (Let _ e) = safeExp e

bindingVariantIn ftable sctable _ (Let [v] e)
  | noneForbidden ftable $ freeNamesInExp e =
    return $ Just [Let [v] e]
  | Just (SufficientCond suff) <- HM.lookup (identName v) sctable =
    case filter (scalExpIsAtMostVariantIn ftable) $ map mkConj suff of
      []   -> return Nothing
      x:xs -> do (e', bnds) <- lift $ lift $ SE.fromScalExp loc $ foldl SE.SLogOr x xs
                 sufficiented
                 return $ Just $ bnds ++ [Let [v] e']
  where mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs
        loc = srclocOf e

-- Nothing we can do about this one, then.
bindingVariantIn _ _ _ _ = return Nothing

scalExpIsAtMostVariantIn :: ForbiddenTable -> ScalExp -> Bool
scalExpIsAtMostVariantIn ftable =
  noneForbidden ftable . HS.fromList . map identName . SE.getIds

allOutermostLoops :: Body -> Loops
allOutermostLoops (Body bnds _) =
  HS.fromList $ map identName $ mapMaybe loopIdentifier bnds
  where loopIdentifier (Let (v:_) e) =
          case e of DoLoop {}  -> Just v
                    Map {}     -> Just v
                    Redomap {} -> Just v
                    Reduce {}  -> Just v
                    Filter {}  -> Just v
                    Apply {}   -> Just v -- Treat funcalls as recurrences.
                    _          -> Nothing
        loopIdentifier (Let _ _) = Nothing
