module Futhark.SuffCond
       (
         optimiseProg
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
import qualified Futhark.EnablingOpts.SymbolTable as ST
import Futhark.EnablingOpts.ScalExp (ScalExp)
import qualified Futhark.EnablingOpts.ScalExp as SE
import qualified Futhark.EnablingOpts.AlgSimplify as AS
import Futhark.Tools

optimiseProg :: Prog -> Prog
optimiseProg prog =
  let m = do optimPreds <- mapM maybeOptimiseFun origfuns
             let newfuns = concat optimPreds
                 subst = HM.fromList $
                         zip (map funName origfuns) $
                         map (map funName) optimPreds
             insertPredicateCalls subst $ Prog $ origfuns ++ newfuns
  in evalState m $ newNameSourceForProg prog
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
          eIf (pure $ subExp c)
            (eBody $ pure $ subExp $ constant True predloc)
            (eBody $ callPreds predt fs e call)
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
analyseBody vtable sctable (Body [] res) =
  mconcat $ map tableForSubExp $ resultSubExps res
  where ranges = rangesRep vtable
        tableForSubExp (Var v)       = generateSCTable v vtable sctable ranges
        tableForSubExp (Constant {}) = mempty
analyseBody vtable sctable (Body (Let [v] e:bnds) res) =
  let vtable' = ST.insert name e vtable
      -- Construct a new sctable for recurrences.
      sctable' = case analyseExp vtable e of
        Nothing       -> sctable
        Just eSCTable -> HM.insert name (SCTable eSCTable) sctable
  in analyseBody vtable' sctable' $ Body bnds res
  where name = identName v
analyseBody vtable sctable (Body (Let _ _:bnds) res) =
  -- Not adding to symbol table here - this leaves a hole, but this
  -- looks like a weird binding anyway.
  analyseBody vtable sctable $ Body bnds res

generateSCTable :: Ident -> ST.SymbolTable -> SCTable -> AS.RangesRep -> SCTable
generateSCTable ident vtable sctable ranges =
  case (ST.lookupExp name vtable, simplify <$> ST.lookupScalExp name vtable) of
    (Just (BinOp LogAnd (Var x) (Var y) _ _), _) ->
      generateSCTable x vtable sctable ranges <>
      generateSCTable y vtable sctable ranges
    (_, Just (Right se@(SE.RelExp SE.LTH0 _))) ->
      case AS.mkSuffConds se loc ranges of
        Left err  -> error $ show err -- Why can this even fail?
        Right ses -> HM.singleton name $ SufficientCond ses
    (_, _) ->
      if name `HM.member` sctable then sctable
      else HM.singleton name $ SufficientCond []
  where loc = srclocOf ident
        name = identName ident
        simplify se = AS.simplify se loc True ranges

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
        clampLower = case bound of Var v -> identName v `ST.isAtLeast` 1
                                   _     -> id
analyseExp vtable (Map _ fun arrs _) =
  Just $ analyseExpBody vtable' $ lambdaBody fun
  where vtable' = foldr (uncurry ST.insertArrayParam) vtable $ zip params arrs
        params = lambdaParams fun
analyseExp vtable (Redomap _ outerfun innerfun acc arrs _) =
  Just $ analyseExpBody vtable' (lambdaBody innerfun) <>
         analyseExpBody vtable (lambdaBody outerfun)
  where vtable' = foldr (uncurry ST.insertArrayParam) vtable $ zip arrparams arrs
        arrparams = drop (length acc) $ lambdaParams innerfun
analyseExp _ _ = Nothing

analyseExpBody :: ST.SymbolTable -> Body -> SCTable
analyseExpBody vtable = analyseBody vtable mempty

type VariantM m = MaybeT (WriterT Any m)

runVariantM :: Functor m => VariantM m a -> m (Maybe a, Bool)
runVariantM = fmap (second getAny) . runWriterT . runMaybeT

-- | We actually changed something to a sufficient condition.
sufficiented :: Monad m => VariantM m ()
sufficiented = tell $ Any True

bodyVariantIn :: MonadFreshNames m =>
                 ForbiddenTable -> SCTable -> Loops -> Body -> VariantM m Body
bodyVariantIn ftable sctable loops (Body bnds res) = do
  bnds' <- mapM (bindingVariantIn ftable sctable loops) bnds
  return $ Body (concat bnds') res

type ForbiddenTable = Names

noneForbidden :: ForbiddenTable -> Names -> Bool
noneForbidden ftable = HS.null . HS.intersection ftable

forbidNames :: [VName] -> VName -> Loops -> ForbiddenTable -> ForbiddenTable
forbidNames names loop loops ftable
  | loop `HS.member` loops = ftable
  | otherwise              = foldr HS.insert ftable names

forbidParams :: [Param] -> VName -> Loops -> ForbiddenTable -> ForbiddenTable
forbidParams = forbidNames . map identName

bindingVariantIn :: MonadFreshNames m =>
                    ForbiddenTable -> SCTable -> Loops -> Binding -> VariantM m [Binding]

-- We assume that a SOAC contributes only if it returns exactly a
-- single (boolean) value.
bindingVariantIn ftable sctable loops (Let [v] e)
  | Just (SCTable eSCTable) <- HM.lookup (identName v) sctable =
    case e of Map cs fun args loc -> do
                body <- bodyVariantIn (forbidParams (lambdaParams fun) name loops ftable)
                        eSCTable loops $ lambdaBody fun
                return [Let [v] $ Map cs fun { lambdaBody = body } args loc]
              DoLoop res merge i bound body loc -> do
                let names = identName i : map (identName . fst) merge
                body' <- bodyVariantIn (forbidNames names name loops ftable)
                         eSCTable loops body
                return [Let [v] $ DoLoop res merge i bound body' loc]
              Redomap cs outerfun innerfun acc args loc -> do
                outerbody <- bodyVariantIn ftable eSCTable loops $ lambdaBody outerfun
                let forbiddenParams = drop (length acc) $ lambdaParams innerfun
                innerbody <- bodyVariantIn (forbidParams forbiddenParams name loops ftable)
                             eSCTable loops $ lambdaBody innerfun
                return [Let [v] $ Redomap cs
                        outerfun { lambdaBody = outerbody }
                        innerfun { lambdaBody = innerbody }
                        acc args loc]
              _ -> fail "Binding has own SCTable, but is not recognised as recurrence.  Bug?"
  where name = identName v

bindingVariantIn ftable sctable loops (Let pat (If (Var v) tbranch fbranch t loc)) = do
  tbranch' <- bodyVariantIn ftable sctable loops tbranch
  fbranch' <- bodyVariantIn ftable sctable loops fbranch
  if noneForbidden ftable $ HS.singleton $ identName v then
    return [Let pat $ If (Var v) tbranch' fbranch' t loc]
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
        return $ tbnds ++ fbnds ++
                 [Let pat $ BinOp LogAnd tres fres (Basic Bool) loc]
      _ -> fail "Branch is insufficiently invariant"
  where safeBnd (Let _ e) = safeExp e

bindingVariantIn ftable sctable _ (Let [v] e)
  | noneForbidden ftable $ freeNamesInExp e =
    return [Let [v] e]
  | Just (SufficientCond suff) <- HM.lookup (identName v) sctable =
    case filter (scalExpIsAtMostVariantIn ftable) $ map mkConj suff of
      []   -> fail "Cannot generate invariant sufficient condition"
      x:xs -> do (e', bnds) <- lift $ lift $ SE.fromScalExp loc $ foldl SE.SLogOr x xs
                 sufficiented
                 return $ bnds ++ [Let [v] e']
  where mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs
        loc = srclocOf e

-- Nothing we can do about this one, then.
bindingVariantIn _ _ _ _ = fail "Expression is too variant"

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
