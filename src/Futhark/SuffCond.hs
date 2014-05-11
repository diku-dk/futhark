module Futhark.SuffCond
       (
         optimiseProg
       )
       where

-- FIXME: We are not dealing properly with ranges yet.

import Control.Applicative
import Data.Loc
import Data.Maybe
import Control.Monad.State
import Data.Monoid
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.InternalRep
import Futhark.MonadFreshNames
import qualified Futhark.EnablingOpts.SymbolTable as ST
import Futhark.EnablingOpts.ScalExp (ScalExp)
import qualified Futhark.EnablingOpts.ScalExp as SE
import qualified Futhark.EnablingOpts.AlgSimplify as AS

data SCEntry = SufficientCond [[ScalExp]]
             | SCTable SCTable

type SCTable = HM.HashMap VName SCEntry

optimiseProg :: Prog -> Prog
optimiseProg prog =
  let m = Prog <$> concat <$> mapM maybeOptimiseFun (progFunctions prog)
  in evalState m $ newNameSourceForProg prog

maybeOptimiseFun :: MonadFreshNames m => FunDec -> m [FunDec]
maybeOptimiseFun fundec@(_,[Basic Bool],_,body,_) = do
  preds <- generatePredicates fundec undefined (buildSCTable ST.empty HM.empty body)
  return $ fundec : preds
maybeOptimiseFun fundec = return [fundec]

buildSCTable :: ST.SymbolTable -> SCTable -> Body -> SCTable
buildSCTable vtable sctable (Body [] res) =
  let Var v = head $ resultSubExps res
  in generateSCTable vtable sctable (rangesRep vtable) v
buildSCTable vtable sctable (Body (Let [v] e:bnds) res) =
  let vtable' = ST.insert name e vtable
      -- Construct a new sctable for recurrences.
      sctable' = case buildSCTableForExp vtable e of
        Nothing       -> sctable
        Just eSCTable -> HM.insert name (SCTable eSCTable) sctable
  in buildSCTable vtable' sctable' $ Body bnds res
  where name = identName v
buildSCTable vtable sctable (Body (Let _ _:bnds) res) =
  -- Not adding to symbol table here - this leaves a hole, but this
  -- looks like a weird binding anyway.
  buildSCTable vtable sctable $ Body bnds res

generateSCTable :: ST.SymbolTable -> SCTable -> AS.RangesRep -> Ident -> SCTable
generateSCTable vtable sctable ranges ident =
  case (ST.lookupExp name vtable, ST.lookupScalExp name vtable) of
    (Just (BinOp LogAnd (Var x) (Var y) _ _), _) ->
      generateSCTable vtable sctable ranges x <>
      generateSCTable vtable sctable ranges y
    (_, Just se@(SE.RelExp SE.LTH0 _)) ->
      case AS.mkSuffConds se loc ranges of
        Left err  -> error $ show err -- Why can this even fail?
        Right ses -> HM.singleton name $ SufficientCond ses
    (_, _) ->
      case HM.lookup name sctable of
        Just (SCTable t) -> t
        Just _           -> error "Should never happen"
        Nothing          -> HM.singleton name $ SufficientCond []
  where name = identName ident
        loc = srclocOf ident

rangesRep :: ST.SymbolTable -> AS.RangesRep
rangesRep = HM.map toRep . ST.bindings
  where toRep entry =
          (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry

buildSCTableForExp :: ST.SymbolTable -> Exp -> Maybe SCTable
buildSCTableForExp vtable (DoLoop _ _ i bound body _) =
  Just $ buildSCTable (clampLower $ clampUpper vtable) HM.empty body
  where clampUpper = ST.insertLoopVar (identName i) bound
        -- If we enter the loop, then 'bound' is at least one.
        clampLower = case bound of Var v -> identName v `ST.isAtLeast` 1
                                   _     -> id
buildSCTableForExp vtable (Map _ fun _ _) =
  Just $ buildSCTable vtable HM.empty $ lambdaBody fun
buildSCTableForExp vtable (Redomap _ outerfun innerfun _ _ _) =
  Just $ buildSCTable vtable HM.empty (lambdaBody innerfun) <>
         buildSCTable vtable HM.empty (lambdaBody outerfun)
buildSCTableForExp _ _ = Nothing

type VarianceTable = HM.HashMap VName Loops

type Loops = HS.HashSet VName

variantIn :: VName -> VarianceTable -> Loops
variantIn name = fromMaybe HS.empty . HM.lookup name

atMostVariantIn :: VName -> Loops -> VarianceTable -> Bool
atMostVariantIn name loops vartable =
  HS.null $ (name `variantIn` vartable) `HS.difference` loops

generatePredicates :: MonadFreshNames m =>
                      FunDec -> VarianceTable -> SCTable -> m [FunDec]
generatePredicates fundec@(_,_,_,body,_) vartable sctable = do
  o1pred <- generatePredicates' fundec vartable sctable HS.empty
  onpred <- generatePredicates' fundec vartable sctable $ allOutermostLoops body
  return $ catMaybes [o1pred, onpred]

generatePredicates' :: MonadFreshNames m =>
                       FunDec -> VarianceTable -> SCTable -> Loops -> m (Maybe FunDec)
generatePredicates' (fname, rettype, params, body, loc) vartable sctable loops = do
  res <- bodyVariantIn vartable sctable loops body
  case res of
    Just body' -> return $ Just (fname', rettype, params, body', loc)
    _          -> return Nothing
  where fname' = fname <> nameFromString ("_" <> show (HS.size loops))

bodyVariantIn :: MonadFreshNames m =>
                 VarianceTable -> SCTable -> Loops -> Body -> m (Maybe Body)
bodyVariantIn vartable sctable loops (Body bnds res) = do
  resbnds <- mapM (bindingVariantIn vartable sctable loops) bnds
  case sequence resbnds of
    Nothing    -> return Nothing
    Just bnds' -> return $ Just $ Body (concat bnds') res

bindingVariantIn :: MonadFreshNames m =>
                    VarianceTable -> SCTable -> Loops -> Binding -> m (Maybe [Binding])

bindingVariantIn vartable sctable loops (Let [v] e)
  | atMostVariantIn (identName v) loops vartable =
    return $ Just [Let [v] e]
  | Just (SufficientCond suff) <- HM.lookup (identName v) sctable =
    case filter (scalExpIsAtMostVariantIn loops vartable) $ map mkConj suff of
      []   -> return Nothing
      x:xs -> do (e', bnds) <- SE.fromScalExp loc $ foldl SE.SLogOr x xs
                 return $ Just $ bnds ++ [Let [v] e']
  where mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs
        loc = srclocOf e

bindingVariantIn vartable sctable loops (Let pat (If (Var v) tbranch fbranch t loc)) = do
  tbranch' <- fromJust <$> bodyVariantIn vartable sctable loops tbranch
  fbranch' <- fromJust <$> bodyVariantIn vartable sctable loops fbranch
  if atMostVariantIn (identName v) loops vartable then
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

-- We assume that a SOAC contributes only if it returns exactly a
-- single (boolean) value.
bindingVariantIn vartable sctable loops (Let [v] e)
  | Just (SCTable eSCTable) <- HM.lookup (identName v) sctable =
    case e of Map cs fun args loc -> do
                body <- bodyVariantIn vartable eSCTable loops $ lambdaBody fun
                case body of
                  Just body' ->
                    return $ Just [Let [v] $ Map cs fun { lambdaBody = body' } args loc]
                  Nothing -> return Nothing
              DoLoop res merge i bound body loc -> do
                body' <- bodyVariantIn vartable eSCTable loops body
                case body' of
                  Just body'' ->
                    return $ Just [Let [v] $ DoLoop res merge i bound body'' loc]
                  Nothing -> return Nothing
              Redomap cs outerfun innerfun acc args loc -> do
                outerbody <- bodyVariantIn vartable eSCTable loops $ lambdaBody outerfun
                innerbody <- bodyVariantIn vartable eSCTable loops $ lambdaBody innerfun
                case (outerbody, innerbody) of
                  (Just outerbody', Just innerbody') ->
                    return $ Just [Let [v] $ Redomap cs
                                   outerfun { lambdaBody = outerbody' }
                                   innerfun { lambdaBody = innerbody' }
                                   acc args loc]
                  _ -> return Nothing
              _ -> return Nothing -- Should not happen.

-- Nothing we can do about this one, then.
bindingVariantIn _ _ _ _ = return Nothing

scalExpIsAtMostVariantIn :: Loops -> VarianceTable -> ScalExp -> Bool
scalExpIsAtMostVariantIn loops vartable = all (ok . identName) . SE.getIds
  where ok x = atMostVariantIn x loops vartable

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
