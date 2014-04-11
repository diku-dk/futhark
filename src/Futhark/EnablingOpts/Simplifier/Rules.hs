-- | This module implements simple simplification rules for bindings.
-- The intent is that you pass a symbol table and a binding, and is
-- given back a sequence of bindings, that are more efficient than the
-- original binding, yet compute the same result.
--
-- These rewrite rules are "local", in that they do not maintain any
-- state or look at the program as a whole.  Compare this to the
-- fusion algorithm in @Futhark.HOTrans.Fusion@, which must be implemented
-- as its own pass.
module Futhark.EnablingOpts.Simplifier.Rules
  ( topDownSimplifyBinding
  , bottomUpSimplifyBinding
  )

where

import Control.Applicative

import Data.Array
import Data.Bits
import Data.Either
import Data.Loc
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Set          as S

import Futhark.NeedNames
import qualified Futhark.EnablingOpts.SymbolTable as ST
import Futhark.EnablingOpts.ClosedForm
import qualified Futhark.EnablingOpts.AlgSimplify as AS
import qualified Futhark.EnablingOpts.ScalExp as SE
import Futhark.EnablingOpts.Simplifier.DataDependencies
import Futhark.InternalRep
import Futhark.MonadFreshNames

-- | @simplifyBinding lookup bnd@ performs simplification of the
-- binding @bnd@.  If simplification is possible, a replacement list
-- of bindings is returned, that bind at least the same banes as the
-- original binding (and possibly more, for intermediate results).
topDownSimplifyBinding :: MonadFreshNames m => ST.SymbolTable -> Binding -> m (Maybe [Binding])
topDownSimplifyBinding vtable bnd =
  provideNames $ applyRules topDownRules vtable bnd

-- | @simplifyBinding uses bnd@ performs simplification of the binding
-- @bnd@.  If simplification is possible, a replacement list of
-- bindings is returned, that bind at least the same banes as the
-- original binding (and possibly more, for intermediate results).
-- The first argument is the set of names used after this binding.
bottomUpSimplifyBinding :: MonadFreshNames m => HS.HashSet VName -> Binding -> m (Maybe [Binding])
bottomUpSimplifyBinding used bnd =
  provideNames $ applyRules bottomUpRules used bnd

applyRules :: [SimplificationRule a]
           -> a -> Binding -> NeedNames (Maybe [Binding])
applyRules []           _    _   = return Nothing
applyRules (rule:rules) context bnd = do
  res <- rule context bnd
  case res of Just bnds -> return $ Just bnds
              Nothing   -> applyRules rules context bnd

type SimplificationRule a = a -> Binding -> NeedNames (Maybe [Binding])

type TopDownRule = SimplificationRule ST.SymbolTable

type BottomUpRule = SimplificationRule (HS.HashSet VName)

topDownRules :: [TopDownRule]
topDownRules = [ liftIdentityMapping
               , removeReplicateMapping
               , hoistLoopInvariantMergeVariables
               , simplifyClosedFormRedomap
               , simplifyClosedFormReduce
               , simplifyScalarExp
               , letRule simplifyRearrange
               , letRule simplifyRotate
               , letRule simplifyBinOp
               , letRule simplifyNot
               , letRule simplifyNegate
               , letRule simplifyAssert
               , letRule simplifyConjoin
               , letRule simplifyIndexing
               , evaluateBranch
               , hoistBranchInvariant
               ]

bottomUpRules :: [BottomUpRule]
bottomUpRules = [ removeDeadMapping
                , removeUnusedLoopResult
                ]

liftIdentityMapping :: TopDownRule
liftIdentityMapping _ (Let pat (Map cs fun arrs loc)) =
  case foldr checkInvariance ([], [], []) $ zip3 pat ses rettype of
    ([], _, _) -> return Nothing
    (invariant, mapresult, rettype') ->
      let (pat', ses') = unzip mapresult
          lambdaRes = resultBody rescs ses' resloc
          fun' = fun { lambdaBody = lambdaRes `setBodyResult` lambdaBody fun
                     , lambdaReturnType = rettype'
                     }
      in return $ Just $ Let pat' (Map cs fun' arrs loc) : invariant
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        Result rescs ses resloc = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map subExpType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            (Let [outId] (subExp inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = (Let [outId] (Replicate outersize e loc) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = return Nothing

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping :: TopDownRule
removeReplicateMapping vtable (Let pat (Map cs fun arrs loc))
  | replicatesOrNot <- zipWith isReplicate (lambdaParams fun) arrs,
    any isRight replicatesOrNot =
  let (paramsAndArrs, parameterBnds) = partitionEithers replicatesOrNot
      (params, arrs') = unzip paramsAndArrs
      fun' = fun { lambdaParams = params }
      -- Empty maps are not permitted, so if that would be the result,
      -- turn the entire map into a replicate.
      n = arraysSize 0 $ map subExpType arrs
      Result _ ses resloc = bodyResult $ lambdaBody fun
      mapres = bodyBindings $ lambdaBody fun
      mapbnds = case arrs' of
                  [] -> mapres ++ [ Let [v] $ Replicate n e resloc
                                    | (v,e) <- zip pat ses ]
                  _  -> [Let pat $ Map cs fun' arrs' loc]
  in return $ Just $ parameterBnds ++ mapbnds
  where isReplicate p (Var v)
          | Just (Replicate _ e _) <- ST.lookupExp (identName v) vtable =
          Right (Let [fromParam p] $ subExp e)
        isReplicate p e =
          Left  (p, e)

        isRight (Right _) = True
        isRight (Left  _) = False
removeReplicateMapping _ _ = return Nothing

removeDeadMapping :: BottomUpRule
removeDeadMapping used (Let pat (Map cs fun arrs loc)) = return $
  let Result rcs ses resloc = bodyResult $ lambdaBody fun
      isUsed (v, _, _) = (`HS.member` used) $ identName v
      (pat',ses', ts') = unzip3 $ filter isUsed $ zip3 pat ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = resultBody rcs ses' resloc `setBodyResult`
                                lambdaBody fun
                 , lambdaReturnType = ts'
                 }
  in if pat /= pat'
     then Just [Let pat' $ Map cs fun' arrs loc]
     else Nothing
removeDeadMapping _ _ = return Nothing

-- This next one is tricky - it's easy enough to determine that some
-- loop variable is not used after the loop, but we must also make
-- sure that it has no influence on control flow in the loop, nor that
-- it affects any other values.  Fortunately, the latter restriction
-- is enough.  I do not claim that the current implementation of this
-- rule is perfect, but it should suffice for many cases.
--
-- FIXME: Also, we technically generate a bunch of undefined variables
-- this way... fortunately, the dead code eliminator will remove the
-- bindings containing them before we get to the code generator, but
-- it's still pretty ugly.
removeUnusedLoopResult :: BottomUpRule
removeUnusedLoopResult used (Let pat (DoLoop merge i bound body loc))
  | not $ all usedAfterwards pat = return $
  let Result cs es resloc = bodyResult body
      usedResults = map snd $ filter (usedAfterwards . fst) $ zip pat es
      necessary' = mconcat $ map dependencies usedResults
      resIsNecessary ((v,_), out, _) =
        identName v `HS.member` necessary' ||
        usedAfterwards out ||
        usedInPatType v
      (merge', pat', es') =
        unzip3 $ filter resIsNecessary $ zip3 merge pat es
      body' = resultBody cs es' resloc `setBodyResult` body
  in if pat == pat' || True
     then Nothing
     else Just [Let pat' $ DoLoop merge' i bound body' loc]
  where usedAfterwards = (`HS.member` used) . identName
        patDimNames = mconcat $ map freeNamesInExp $
                      concatMap (map subExp . arrayDims . identType) pat
        usedInPatType = (`HS.member` patDimNames) . identName

        allDependencies = dataDependencies body
        dependencies (Constant _ _) = HS.empty
        dependencies (Var v)        =
          fromMaybe HS.empty $ HM.lookup (identName v) allDependencies
removeUnusedLoopResult _ _ =
  return Nothing

hoistLoopInvariantMergeVariables :: TopDownRule
hoistLoopInvariantMergeVariables _ (Let pat (DoLoop merge idd n loopbody loc)) =
    -- Figure out which of the elemens of loopresult are loop-invariant,
  -- and hoist them out.
  case foldr checkInvariance ([], [], [], []) $ zip3 pat merge ses of
    ([], _, _, _) ->
      -- Nothing is invariant.
      return Nothing
    (invariant, pat', merge', ses') ->
      -- We have moved something invariant out of the loop.
      let loopbody' = resultBody cs ses' resloc `setBodyResult` loopbody
      in return $ Just $ invariant ++ [Let pat' $ DoLoop merge' idd n loopbody' loc]
  where Result cs ses resloc = bodyResult loopbody

        patDimNames = mconcat $ map freeNamesInExp $
                      concatMap (map subExp . arrayDims . identType) pat
        usedInPatType = (`HS.member` patDimNames) . identName

        checkInvariance (v0, (v1,initExp), Var v2) (invariant, pat', merge', resExps)
          | identName v1 == identName v2 && not (usedInPatType v0) =
          (Let [v0] (subExp initExp):Let [v1] (subExp initExp):invariant,
           pat', merge', resExps)
        checkInvariance (v0, (v1,initExp), resExp) (invariant, pat', merge', resExps) =
          (invariant, v0:pat', (v1,initExp):merge', resExp:resExps)
hoistLoopInvariantMergeVariables _ _ = return Nothing

-- | A function that, given a variable name, returns its definition.
type VarLookup = VName -> Maybe Exp

type LetTopDownRule = VarLookup -> Exp -> Maybe Exp

letRule :: LetTopDownRule -> TopDownRule
letRule rule vtable (Let pat e) =
  return $ (:[]) . Let pat <$> rule look e
  where look = (`ST.lookupExp` vtable)

simplifyClosedFormRedomap :: TopDownRule
simplifyClosedFormRedomap vtable (Let pat (Redomap _ _ innerfun acc arr _)) =
  foldClosedForm (`ST.lookupExp` vtable) pat innerfun acc arr
simplifyClosedFormRedomap _ _ = return Nothing

simplifyClosedFormReduce :: TopDownRule
simplifyClosedFormReduce vtable (Let pat (Reduce _ fun args _)) =
  foldClosedForm (`ST.lookupExp` vtable) pat fun acc arr
  where (acc, arr) = unzip args
simplifyClosedFormReduce _ _ = return Nothing

simplifyScalarExp :: TopDownRule
simplifyScalarExp vtable (Let [v] e)
  | Just se <- SE.toScalExp (`ST.lookupScalExp` vtable) e,
    Right se' <- AS.simplify se loc True (rangesRep vtable), -- Cheap?  What?
    se /= se' = do -- Only perform simplification if something
                   -- actually changed - if 'simplify' is not
                   -- idempotent, this is going to cause an infinite
                   -- loop in the simplifier.
  (e',bnds) <- SE.fromScalExp loc se'
  return $ Just $ bnds ++ [Let [v] e']
  where loc = srclocOf e

simplifyScalarExp _ _ = return Nothing

rangesRep :: ST.SymbolTable -> AS.RangesRep
rangesRep = HM.map toRep . ST.bindings
  where toRep entry =
          (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry

simplifyRearrange :: LetTopDownRule

-- Handle identity permutation.
simplifyRearrange _ (Rearrange _ perm e _)
  | perm == [0..arrayRank (subExpType e) - 1] = Just $ subExp e

-- If asked to rotate a constant, just do it.
simplifyRearrange _ (Rearrange _ perm (Constant val _) loc) =
  Just $ subExp $ Constant (permuteArray perm val) loc

simplifyRearrange look (Rearrange cs perm (Var v) loc) =
  case look $ identName v of
    Just (Rearrange cs2 perm2 e _) ->
      -- Rearranging a rearranging: compose the permutations.
      Just $ Rearrange (cs++cs2) (perm `permuteCompose` perm2) e loc
    _ -> Nothing

simplifyRearrange _ _ = Nothing

simplifyRotate :: LetTopDownRule
-- A zero-rotation is identity.
simplifyRotate _ (Rotate _ 0 e _) =
  Just $ subExp e

-- If asked to rotate a constant, just do it.
simplifyRotate _ (Rotate _ i (Constant val _) loc) =
  Just $ subExp $ Constant (rotateArray i val) loc

simplifyRotate look (Rotate _ _ (Var v) _) = do
  bnd <- look $ identName v
  case bnd of
    -- Rotating a replicate is identity.
    Replicate {} ->
      Just $ subExp $ Var v
    _ ->
      Nothing

simplifyRotate _ _ = Nothing

simplifyBinOp :: LetTopDownRule

simplifyBinOp _ (BinOp Plus e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e2
  | isCt0 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _,  Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1+v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1+v2
      _ -> Nothing

simplifyBinOp _ (BinOp Minus e1 e2 _ pos)
  | isCt0 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1-v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1-v2
      _ -> Nothing

simplifyBinOp _ (BinOp Times e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e1
  | isCt0 e2 = Just $ subExp e2
  | isCt1 e1 = Just $ subExp e2
  | isCt1 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1*v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1*v2
      _ -> Nothing

simplifyBinOp _ (BinOp Divide e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e1
  | isCt1 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `div` v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1 / v2
      _ -> Nothing

simplifyBinOp _ (BinOp Mod e1 e2 _ pos) =
  case (e1, e2) of
    (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
      binOpRes pos $ IntVal $ v1 `mod` v2
    _ -> Nothing

simplifyBinOp _ (BinOp Pow e1 e2 _ pos)
  | isCt0 e2 =
    case subExpType e1 of
      Basic Int  -> binOpRes pos $ IntVal 1
      Basic Real -> binOpRes pos $ RealVal 1.0
      _          -> Nothing
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 ^ v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1**v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftL e1 e2 _ pos)
  | isCt0 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftL` v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftR e1 e2 _ pos)
  | isCt0 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftR` v2
      _ -> Nothing

simplifyBinOp _ (BinOp Band e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e1
  | isCt0 e2 = Just $ subExp e2
  | isCt1 e1 = Just $ subExp e2
  | isCt1 e2 = Just $ subExp e1
  | e1 == e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .&. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Bor e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e2
  | isCt0 e2 = Just $ subExp e1
  | isCt1 e1 = Just $ subExp e1
  | isCt1 e2 = Just $ subExp e2
  | e1 == e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .|. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Xor e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e2
  | isCt0 e2 = Just $ subExp e1
  | e1 == e2 = binOpRes pos $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `xor` v2
      _ -> Nothing

simplifyBinOp _ (BinOp LogAnd e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e1
  | isCt0 e2 = Just $ subExp e2
  | isCt1 e1 = Just $ subExp e2
  | isCt1 e2 = Just $ subExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 && v2
      _ -> Nothing

simplifyBinOp _ (BinOp LogOr e1 e2 _ pos)
  | isCt0 e1 = Just $ subExp e2
  | isCt0 e2 = Just $ subExp e1
  | isCt1 e1 = Just $ subExp e1
  | isCt1 e2 = Just $ subExp e2
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (LogVal v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 || v2
      _ -> Nothing

simplifyBinOp _ (BinOp Equal e1 e2 _ pos)
  | e1 == e2 = binOpRes pos $ LogVal True
  | otherwise =
    case (e1, e2) of
      -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      _ -> Nothing

simplifyBinOp _ (BinOp Less e1 e2 _ pos)
  | e1 == e2 = binOpRes pos $ LogVal False
  | otherwise =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    _ -> Nothing

simplifyBinOp _ (BinOp Leq e1 e2 _ pos)
  | e1 == e2 = binOpRes pos $ LogVal True
  | otherwise =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (BasicVal (IntVal  v1)) _, Constant (BasicVal (IntVal  v2)) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal  v2)) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2 )) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    _ -> Nothing

simplifyBinOp _ _ = Nothing

binOpRes :: SrcLoc -> BasicValue -> Maybe Exp
binOpRes loc v = Just $ subExp $ Constant (BasicVal v) loc

simplifyNot :: LetTopDownRule
simplifyNot _ (Not (Constant (BasicVal (LogVal v)) _) loc) =
  Just $ subExp $ constant (not v) loc
simplifyNot _ _ = Nothing

simplifyNegate :: LetTopDownRule
simplifyNegate _ (Negate (Constant (BasicVal (IntVal  v)) _) pos) =
  Just $ subExp $ constant (negate v) pos
simplifyNegate _ (Negate (Constant (BasicVal (RealVal  v)) _) pos) =
  Just $ subExp $ constant (negate v) pos
simplifyNegate _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: LetTopDownRule
simplifyAssert _ (Assert (Constant (BasicVal (LogVal True)) _) loc) =
  Just $ subExp $ Constant (BasicVal Checked) loc
simplifyAssert _ _ =
  Nothing

simplifyConjoin :: LetTopDownRule
simplifyConjoin look (Conjoin es loc) =
  -- Remove trivial certificates.
  let check seen (Constant (BasicVal Checked) _) = seen
      check seen (Var idd) =
        case look $ identName idd of
          Just (Conjoin es2 _) -> seen `S.union` S.fromList es2
          _                    -> Var idd `S.insert` seen
      check seen e = e `S.insert` seen
      origset = S.fromList es
      newset = foldl check S.empty es
      es' = S.toList newset
  in case es' of
       []                    -> Just $ subExp $ Constant (BasicVal Checked) loc
       [c]                   -> Just $ subExp c
       _ | origset /= newset -> Just $ Conjoin es' loc
         | otherwise         -> Nothing
simplifyConjoin _ _ = Nothing

simplifyIndexing :: LetTopDownRule
simplifyIndexing look (Index cs idd inds loc) =
  case look $ identName idd of
    Nothing -> Nothing

    Just (SubExps [Constant v _] _)
      | Just iis <- ctIndex inds,
        length iis == length (valueShape v),
        Just el <- arrValInd v iis -> Just $ subExp $ Constant el loc

    Just (Iota _ _)
      | [ii] <- inds -> Just $ subExp ii

    Just (Index cs2 aa ais _) ->
      Just $ Index (cs++cs2) aa (ais ++ inds) loc

    Just (e@ArrayLit {})
       | Just iis <- ctIndex inds,
         Just el <- arrLitInd e iis -> Just el

    Just (Replicate _ (Var vv) _)
      | [_]   <- inds -> Just $ subExp $ Var vv
      | _:is' <- inds -> Just $ Index cs vv is' loc

    Just (Replicate _ (Constant arr@(ArrayVal _ _) _) _)
       | _:is' <- inds,
         Just iis <- ctIndex is',
         Just el <- arrValInd arr iis ->
           Just $ subExp $ Constant el loc

    Just (Replicate _ val@(Constant _ _) _)
      | [_] <- inds -> Just $ subExp val

    Just (Rearrange cs2 perm (Var src) _)
       | permuteReach perm < length inds ->
         let inds' = permuteShape (take (length inds) perm) inds
         in Just $ Index (cs++cs2) src inds' loc

    _ -> Nothing

simplifyIndexing _ _ = Nothing

evaluateBranch :: TopDownRule
evaluateBranch _ (Let pat (If e1 tb fb _ _))
  | Just branch <- checkBranch =
  let Result _ ses resloc = bodyResult branch
  in return $ Just $ bodyBindings branch ++ [Let pat $ SubExps ses resloc]
  where checkBranch
          | isCt1 e1  = Just tb
          | isCt0 e1  = Just fb
          | otherwise = Nothing
evaluateBranch _ _ = return Nothing

hoistBranchInvariant :: TopDownRule
hoistBranchInvariant _ (Let pat (If e1 tb fb ts loc)) = return $
  let Result tcs tses tresloc = bodyResult tb
      Result fcs fses fresloc = bodyResult fb
      (pat', res, invariant) =
        foldl branchInvariant ([], [], []) $
        zip pat (zip3 tses fses ts)
      (tses', fses', ts') = unzip3 res
      tb' = resultBody tcs tses' tresloc `setBodyResult` tb
      fb' = resultBody fcs fses' fresloc `setBodyResult` fb
  in if null invariant
     then Nothing
     else Just $ invariant ++ [Let pat' $ If e1 tb' fb' ts' loc]
  where branchInvariant (pat', res, invariant) (v, (tse, fse, t))
          | tse == fse = (pat', res, Let [v] (subExp tse) : invariant)
          | otherwise  = (v:pat', (tse,fse,t):res, invariant)
hoistBranchInvariant _ _ = return Nothing

-- Some helper functions

isCt1 :: SubExp -> Bool
isCt1 (Constant (BasicVal (IntVal x))  _) = x == 1
isCt1 (Constant (BasicVal (RealVal x)) _) = x == 1
isCt1 (Constant (BasicVal (LogVal x))  _) = x
isCt1 _                                   = False

isCt0 :: SubExp -> Bool
isCt0 (Constant (BasicVal (IntVal x))  _) = x == 0
isCt0 (Constant (BasicVal (RealVal x)) _) = x == 0
isCt0 (Constant (BasicVal (LogVal x))  _) = not x
isCt0 _                                   = False

ctIndex :: [SubExp] -> Maybe [Int]
ctIndex [] = Just []
ctIndex (Constant (BasicVal (IntVal ii)) _:is) =
  case ctIndex is of
    Nothing -> Nothing
    Just y  -> Just (ii:y)
ctIndex _ = Nothing

arrValInd :: Value -> [Int] -> Maybe Value
arrValInd v [] = Just v
arrValInd (ArrayVal arr _) (i:is) = arrValInd (arr ! i) is
arrValInd _ _ = Nothing

arrLitInd :: Exp -> [Int] -> Maybe Exp
arrLitInd e [] = Just e
arrLitInd (ArrayLit els _ _) (i:is) = arrLitInd (subExp $ els !! i) is
arrLitInd (SubExps [Constant arr@(ArrayVal _ _) loc] _) (i:is) =
  case arrValInd arr (i:is) of
    Nothing -> Nothing
    Just v  -> Just $ subExp $ Constant v loc
arrLitInd _ _ = Nothing
