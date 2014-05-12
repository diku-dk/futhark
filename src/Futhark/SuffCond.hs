module Futhark.SuffCond
       (
         optimiseProg
       )
       where

-- FIXME: We are not dealing properly with ranges (iota etc) yet.

import Control.Applicative
import Data.Loc
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Monoid
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.InternalRep
import Futhark.MonadFreshNames
import qualified Futhark.EnablingOpts.SymbolTable as ST
import Futhark.EnablingOpts.ScalExp (ScalExp)
import qualified Futhark.EnablingOpts.ScalExp as SE
import qualified Futhark.EnablingOpts.AlgSimplify as AS

optimiseProg :: Prog -> Prog
optimiseProg prog =
  let m = Prog <$> concat <$> mapM maybeOptimiseFun (progFunctions prog)
  in evalState m $ newNameSourceForProg prog

maybeOptimiseFun :: MonadFreshNames m => FunDec -> m [FunDec]
maybeOptimiseFun fundec@(_,[Basic Bool],_,body,_) = do
  let (sctable, _) = analyseBody ST.empty mempty body
  preds <- generatePredicates fundec sctable
  return $ fundec : preds
maybeOptimiseFun fundec = return [fundec]

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
    Just body' -> return $ Just (fname', rettype, params, body', loc)
    _          -> return Nothing
  where fname' = fname <> nameFromString suff

data SCEntry = SufficientCond [[ScalExp]]
             | SCTable SCTable
             deriving (Eq, Show)

type SCTable = HM.HashMap VName SCEntry

type VarianceTable = HM.HashMap VName Loops

type Loops = HS.HashSet VName

type AnalysisResult = (SCTable, VarianceTable)

variantIn :: VName -> VarianceTable -> Loops
variantIn name = fromMaybe HS.empty . HM.lookup name

analyseBody :: ST.SymbolTable -> AnalysisResult -> Body -> AnalysisResult
analyseBody vtable (sctable,vartable) (Body [] res) =
  (mconcat $ map tableForSubExp $ resultSubExps res,
   vartable)
  where ranges = rangesRep vtable
        tableForSubExp (Var v)       = generateSCTable v vtable sctable ranges
        tableForSubExp (Constant {}) = mempty
analyseBody vtable (sctable,vartable) (Body (Let [v] e:bnds) res) =
  let vtable' = ST.insert name e vtable
      -- Construct a new sctable for recurrences.
      (sctable',vartable') = case analyseExp (identName v) vtable e of
        Nothing ->
          (sctable,
           HM.insert name (freeVariant vartable e) vartable)
        Just (eSCTable,variance) ->
          (HM.insert name (SCTable eSCTable) sctable,
           HM.insert name variance vartable)
  in analyseBody vtable' (sctable',vartable') $ Body bnds res
  where name = identName v
analyseBody vtable sctable (Body (Let _ _:bnds) res) =
  -- Not adding to symbol table here - this leaves a hole, but this
  -- looks like a weird binding anyway.
  analyseBody vtable sctable $ Body bnds res

freeVariant :: VarianceTable -> Exp -> Loops
freeVariant vartable =
  mconcat . mapMaybe (`HM.lookup` vartable) . HS.toList . freeNamesInExp

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

analyseExp :: VName -> ST.SymbolTable -> Exp -> Maybe (SCTable, Loops)
analyseExp loop vtable (DoLoop _ merge i bound body _) =
  Just $ analyseExpBody vtable' vartable body
  where vtable' = clampLower $ clampUpper vtable
        vartable = boundnames `makeVariantIn` loop
        boundnames = identName i : map (identName . fst) merge
        clampUpper = ST.insertLoopVar (identName i) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampLower = case bound of Var v -> identName v `ST.isAtLeast` 1
                                   _     -> id
analyseExp loop vtable (Map _ fun _ _) =
  Just $ analyseExpBody vtable (paramsVariantIn fun loop) $ lambdaBody fun
analyseExp loop vtable (Redomap _ outerfun innerfun _ _ _) =
  Just $ analyseExpBody vtable innervartable (lambdaBody innerfun) <>
         analyseExpBody vtable outervartable (lambdaBody outerfun)
  where innervartable = innerfun `paramsVariantIn` loop
        outervartable = outerfun `paramsVariantIn` loop
analyseExp _ _ _ = Nothing

paramsVariantIn :: Lambda -> VName -> VarianceTable
paramsVariantIn fun name =
  map identName (lambdaParams fun) `makeVariantIn` name

makeVariantIn :: [VName] -> VName -> VarianceTable
makeVariantIn names loop =
  HM.fromList [ (name, HS.singleton loop) | name <- names ]

analyseExpBody :: ST.SymbolTable -> VarianceTable -> Body -> (SCTable, Loops)
analyseExpBody vtable vartable body =
  let (sctable, vartable') = analyseBody vtable (mempty,vartable) body
  in (sctable, varianceOfBodyResult vartable' body)

varianceOfBodyResult :: VarianceTable -> Body -> Loops
varianceOfBodyResult vartable =
  mconcat . map (varianceOfSubExp vartable) . resultSubExps . bodyResult

varianceOfSubExp :: VarianceTable -> SubExp -> Loops
varianceOfSubExp vartable (Var v)        = identName v `variantIn` vartable
varianceOfSubExp _        (Constant _ _) = mempty

type VariantM = MaybeT

runVariantM :: VariantM m a -> m (Maybe a)
runVariantM = runMaybeT

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

forbidParams :: Lambda -> VName -> Loops -> ForbiddenTable -> ForbiddenTable
forbidParams = forbidNames . map identName . lambdaParams

bindingVariantIn :: MonadFreshNames m =>
                    ForbiddenTable -> SCTable -> Loops -> Binding -> VariantM m [Binding]

-- We assume that a SOAC contributes only if it returns exactly a
-- single (boolean) value.
bindingVariantIn ftable sctable loops (Let [v] e)
  | Just (SCTable eSCTable) <- HM.lookup (identName v) sctable =
    case e of Map cs fun args loc -> do
                body <- bodyVariantIn (forbidParams fun name loops ftable)
                        eSCTable loops $ lambdaBody fun
                return [Let [v] $ Map cs fun { lambdaBody = body } args loc]
              DoLoop res merge i bound body loc -> do
                let names = identName i : map (identName . fst) merge
                body' <- bodyVariantIn (forbidNames names name loops ftable)
                         eSCTable loops body
                return [Let [v] $ DoLoop res merge i bound body' loc]
              Redomap cs outerfun innerfun acc args loc -> do
                outerbody <- bodyVariantIn (forbidParams outerfun name loops ftable)
                             eSCTable loops $ lambdaBody outerfun
                innerbody <- bodyVariantIn (forbidParams outerfun name loops ftable)
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
      x:xs -> do (e', bnds) <- lift $ SE.fromScalExp loc $ foldl SE.SLogOr x xs
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
