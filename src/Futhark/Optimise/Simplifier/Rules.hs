-- | This module defines a collection of simplification rules, as per
-- "Futhark.Optimise.Simplifier.Rule".  They are used in the
-- simplifier.
module Futhark.Optimise.Simplifier.Rules
  ( standardRules
  )

where

import Control.Applicative

import Data.Array
import Data.Bits
import Data.Either
import Data.List
import Data.Loc
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Set          as S

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.DataDependencies
import Futhark.Optimise.Simplifier.ClosedForm
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Simplify
import qualified Futhark.Analysis.AlgSimplify as AS
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.InternalRep
import Futhark.Tools

topDownRules :: TopDownRules
topDownRules = [ liftIdentityMapping
               , removeReplicateMapping
               , hoistLoopInvariantMergeVariables
               , simplifyClosedFormRedomap
               , simplifyClosedFormReduce
               , simplifyClosedFormLoop
               , letRule simplifyRearrange
               , letRule simplifyRotate
               , letRule simplifyBinOp
               , letRule simplifyNot
               , letRule simplifyNegate
               , letRule simplifyAssert
               , letRule simplifyConjoin
               , letRule simplifyIndexing
               , evaluateBranch
               , simplifyBoolBranch
               , hoistBranchInvariant
               , simplifyScalExp
               ]

bottomUpRules :: BottomUpRules
bottomUpRules = [ removeDeadMapping
                , removeUnusedLoopResult
                , removeRedundantMergeVariables
                , removeDeadBranchResult
                , removeUnnecessaryCopy
                ]

standardRules :: RuleBook
standardRules = (topDownRules, bottomUpRules)

liftIdentityMapping :: TopDownRule
liftIdentityMapping _ (Let pat (Map cs fun arrs loc)) =
  case foldr checkInvariance ([], [], []) $ zip3 pat ses rettype of
    ([], _, _) -> cannotSimplify
    (invariant, mapresult, rettype') ->
      let (pat', ses') = unzip mapresult
          lambdaRes = resultBody rescs ses' resloc
          fun' = fun { lambdaBody = lambdaRes `setBodyResult` lambdaBody fun
                     , lambdaReturnType = rettype'
                     }
      in return $ Let pat' (Map cs fun' arrs loc) : invariant
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        Result rescs ses resloc = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map subExpType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            (Let [outId] (SubExp inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = (Let [outId] (Replicate outersize e loc) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = cannotSimplify

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping :: TopDownRule
removeReplicateMapping vtable (Let pat (Map cs fun arrs loc))
  | not $ null parameterBnds =
  let (params, arrs') = unzip paramsAndArrs
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
  in return $ parameterBnds ++ mapbnds
  where (paramsAndArrs, parameterBnds) =
          partitionEithers $ zipWith isReplicate (lambdaParams fun) arrs

        isReplicate p (Var v)
          | Just (Replicate _ e _) <- ST.lookupExp (identName v) vtable =
          Right (Let [fromParam p] $ SubExp e)
        isReplicate p e =
          Left  (p, e)

removeReplicateMapping _ _ = cannotSimplify

removeDeadMapping :: BottomUpRule
removeDeadMapping (_, used) (Let pat (Map cs fun arrs loc)) =
  let Result rcs ses resloc = bodyResult $ lambdaBody fun
      isUsed (v, _, _) = (`UT.used` used) $ identName v
      (pat',ses', ts') = unzip3 $ filter isUsed $ zip3 pat ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = resultBody rcs ses' resloc `setBodyResult`
                                lambdaBody fun
                 , lambdaReturnType = ts'
                 }
  in if pat /= pat'
     then return [Let pat' $ Map cs fun' arrs loc]
     else cannotSimplify
removeDeadMapping _ _ = cannotSimplify

-- After removing a result, we may also have to remove some of the
-- shape bindings.
removeUnusedLoopResult :: BottomUpRule
removeUnusedLoopResult (_, used) (Let pat (DoLoop respat merge i bound body loc))
  | explpat' <- filter (usedAfterwards . fst) explpat,
    explpat' /= explpat =
  let shapes = concatMap (arrayDims . identType) $ map snd explpat'
      implpat' = filter ((`elem` shapes) . Var . snd) implpat
      pat' = map fst $ implpat'++explpat'
      respat' = map snd explpat'
  in return [Let pat' $ DoLoop respat' merge i bound body loc]
  where -- | Check whether the variable binding is used afterwards.
        -- But also, check whether one of the shapes is used!  FIXME:
        -- This is in fact too conservative, as we only need to
        -- preserve one binding with this shape.
        usedAfterwards v =
          identName v `UT.used` used  ||
          any isUsedImplRes (concatMap varDim $ arrayDims $ identType v)
        isUsedImplRes v =
          v `UT.used` used && v `elem` map identName pat
        varDim (Var v)       = [identName v]
        varDim (Constant {}) = []
        taggedpat = zip pat $ loopResult respat $ map fst merge
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat
removeUnusedLoopResult _ _ = cannotSimplify

-- This next one is tricky - it's easy enough to determine that some
-- loop result is not used after the loop (as in
-- 'removeUnusedLoopResult'), but here, we must also make sure that it
-- does not affect any other values.
--
-- I do not claim that the current implementation of this rule is
-- perfect, but it should suffice for many cases, and should never
-- generate wrong code.
removeRedundantMergeVariables :: BottomUpRule
removeRedundantMergeVariables _ (Let pat (DoLoop respat merge i bound body loc))
  | not $ all (usedInResult . fst) merge =
  let Result cs es resloc = bodyResult body
      usedResults = map snd $ filter (usedInResult . fst) $ zip mergepat es
      necessary' = mconcat $ map dependencies usedResults
      resIsNecessary ((v,_), _) =
        usedInResult v ||
        identName v `HS.member` necessary' ||
        usedInPatType v
      (keep, discard) = partition resIsNecessary $ zip merge es
      (merge', es') = unzip keep
      body' = resultBody cs es' resloc `setBodyResult` body
      -- We can't just remove the bindings in 'discard', since the loop
      -- body may still use their names in (now-dead) expressions.
      -- Hence, we add them inside the loop, fully aware that dead-code
      -- removal will eventually get rid of them.  Some care is
      -- necessary to handle unique bindings.
      body'' = insertBindings (dummyBindings discard) body'
  in if merge == merge'
     then cannotSimplify
     else return [Let pat $ DoLoop respat merge' i bound body'' loc]
  where (mergepat, _) = unzip merge
        usedInResult = (`elem` respat)
        patDimNames = mconcat $ map freeNamesInExp $
                      concatMap (map SubExp . arrayDims . identType) mergepat
        usedInPatType = (`HS.member` patDimNames) . identName

        dummyBindings = map dummyBinding
        dummyBinding ((v,e), _)
          | unique (identType v) = Let [v] $ Copy e $ srclocOf v
          | otherwise            = Let [v] $ SubExp e

        allDependencies = dataDependencies body
        dependencies (Constant _ _) = HS.empty
        dependencies (Var v)        =
          fromMaybe HS.empty $ HM.lookup (identName v) allDependencies
removeRedundantMergeVariables _ _ =
  cannotSimplify

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantMergeVariables :: TopDownRule
hoistLoopInvariantMergeVariables _ (Let pat (DoLoop respat merge idd n loopbody loc)) =
    -- Figure out which of the elements of loopresult are
    -- loop-invariant, and hoist them out.
  case foldr checkInvariance ([], explpat, [], []) $ zip merge ses of
    ([], _, _, _) ->
      -- Nothing is invariant.
      cannotSimplify
    (invariant, explpat', merge', ses') ->
      -- We have moved something invariant out of the loop.
      let loopbody' = resultBody cs ses' resloc `setBodyResult` loopbody
          invariantShape (_, shapemerge) = shapemerge `elem` map fst merge'
          (implpat',implinvariant) = partition invariantShape implpat
          implinvariant' = [ (p, Var v) | (p,v) <- implinvariant ]
          pat' = map fst $ implpat'++explpat'
          respat' = map snd explpat'
      in return $ map letPat (invariant ++ implinvariant') ++
         [Let pat' $ DoLoop respat' merge' idd n loopbody' loc]
  where Result cs ses resloc = bodyResult loopbody
        letPat (v1, v2) = Let [v1] $ SubExp v2
        taggedpat = zip pat $ loopResult respat $ map fst merge
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat

        removeFromResult (v,initExp) explpat' =
          case partition ((==v) . snd) explpat' of
            ([(resv,_)], rest) -> (Just (resv, initExp), rest)
            (_,      _)        -> (Nothing,              explpat')

        checkInvariance ((v1,initExp), resExp) (invariant, explpat', merge', resExps)
          | theSame v1 initExp resExp =
          let (bnd, explpat'') = removeFromResult (v1,initExp) explpat'
          in (maybe id (:) bnd $ (v1, initExp) : invariant,
              explpat'', merge', resExps)
        checkInvariance ((v1,initExp), resExp) (invariant, explpat', merge', resExps) =
          (invariant, explpat', (v1,initExp):merge', resExp:resExps)

        theSame v1 _       (Var v2) = identName v1 == identName v2
        theSame _  initExp resExp   = initExp      == resExp
hoistLoopInvariantMergeVariables _ _ = cannotSimplify

-- | A function that, given a variable name, returns its definition.
type VarLookup = VName -> Maybe Exp

type LetTopDownRule = VarLookup -> Exp -> Maybe Exp

letRule :: LetTopDownRule -> TopDownRule
letRule rule vtable (Let pat e) =
  liftMaybe $ (:[]) . Let pat <$> rule look e
  where look = (`ST.lookupExp` vtable)

simplifyClosedFormRedomap :: TopDownRule
simplifyClosedFormRedomap vtable (Let pat (Redomap _ _ innerfun acc arr _)) =
  foldClosedForm (`ST.lookupExp` vtable) pat innerfun acc arr
simplifyClosedFormRedomap _ _ = cannotSimplify

simplifyClosedFormReduce :: TopDownRule
simplifyClosedFormReduce vtable (Let pat (Reduce _ fun args _)) =
  foldClosedForm (`ST.lookupExp` vtable) pat fun acc arr
  where (acc, arr) = unzip args
simplifyClosedFormReduce _ _ = cannotSimplify

simplifyClosedFormLoop :: TopDownRule
simplifyClosedFormLoop _ (Let pat (DoLoop respat merge _ bound body _)) =
  loopClosedForm pat respat merge bound body
simplifyClosedFormLoop _ _ = cannotSimplify

simplifyRearrange :: LetTopDownRule

-- Handle identity permutation.
simplifyRearrange _ (Rearrange _ perm e _)
  | perm == [0..arrayRank (subExpType e) - 1] = Just $ SubExp e

-- If asked to rotate a constant, just do it.
simplifyRearrange _ (Rearrange _ perm (Constant val _) loc) =
  Just $ SubExp $ Constant (permuteArray perm val) loc

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
  Just $ SubExp e

-- If asked to rotate a constant, just do it.
simplifyRotate _ (Rotate _ i (Constant val _) loc) =
  Just $ SubExp $ Constant (rotateArray i val) loc

simplifyRotate look (Rotate _ _ (Var v) _) = do
  bnd <- look $ identName v
  case bnd of
    -- Rotating a replicate is identity.
    Replicate {} ->
      Just $ SubExp $ Var v
    _ ->
      Nothing

simplifyRotate _ _ = Nothing

simplifyBinOp :: LetTopDownRule

simplifyBinOp _ (BinOp Plus e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _,  Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1+v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1+v2
      _ -> Nothing

simplifyBinOp _ (BinOp Minus e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1-v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1-v2
      _ -> Nothing

simplifyBinOp _ (BinOp Times e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1*v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1*v2
      _ -> Nothing

simplifyBinOp _ (BinOp Divide e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e1
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
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 ^ v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1**v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftL e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftL` v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftR e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftR` v2
      _ -> Nothing

simplifyBinOp _ (BinOp Band e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | e1 == e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .&. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Bor e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e2
  | e1 == e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .|. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Xor e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | e1 == e2 = binOpRes pos $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `xor` v2
      _ -> Nothing

simplifyBinOp look (BinOp LogAnd e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | Var v <- e1,
    Just (Not e1' _) <- look $ identName v,
    e1' == e2 = binOpRes pos $ LogVal False
  | Var v <- e2,
    Just (Not e2' _) <- look $ identName v,
    e2' == e1 = binOpRes pos $ LogVal False
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 && v2
      _ -> Nothing

simplifyBinOp look (BinOp LogOr e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e2
  | Var v <- e1,
    Just (Not e1' _) <- look $ identName v,
    e1' == e2 = binOpRes pos $ LogVal True
  | Var v <- e2,
    Just (Not e2' _) <- look $ identName v,
    e2' == e1 = binOpRes pos $ LogVal True
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
binOpRes loc v = Just $ SubExp $ Constant (BasicVal v) loc

simplifyNot :: LetTopDownRule
simplifyNot _ (Not (Constant (BasicVal (LogVal v)) _) loc) =
  Just $ SubExp $ constant (not v) loc
simplifyNot _ _ = Nothing

simplifyNegate :: LetTopDownRule
simplifyNegate _ (Negate (Constant (BasicVal (IntVal  v)) _) pos) =
  Just $ SubExp $ constant (negate v) pos
simplifyNegate _ (Negate (Constant (BasicVal (RealVal  v)) _) pos) =
  Just $ SubExp $ constant (negate v) pos
simplifyNegate _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: LetTopDownRule
simplifyAssert _ (Assert (Constant (BasicVal (LogVal True)) _) loc) =
  Just $ SubExp $ Constant (BasicVal Checked) loc
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
       []                    -> Just $ SubExp $ Constant (BasicVal Checked) loc
       [c]                   -> Just $ SubExp c
       _ | origset /= newset -> Just $ Conjoin es' loc
         | otherwise         -> Nothing
simplifyConjoin _ _ = Nothing

simplifyIndexing :: LetTopDownRule
simplifyIndexing look (Index cs idd inds loc) =
  case look $ identName idd of
    Nothing -> Nothing

    Just (SubExp (Var v)) ->
      return $ Index cs v inds loc

    Just (SubExp (Constant v _))
      | Just iis <- ctIndex inds,
        length iis == length (valueShape v),
        Just el <- arrValInd v iis -> Just $ SubExp $ Constant el loc

    Just (Iota _ _)
      | [ii] <- inds -> Just $ SubExp ii

    Just (Index cs2 aa ais _) ->
      Just $ Index (cs++cs2) aa (ais ++ inds) loc

    Just (e@ArrayLit {})
       | Just iis <- ctIndex inds,
         Just el <- arrLitInd e iis -> Just el

    Just (Replicate _ (Var vv) _)
      | [_]   <- inds -> Just $ SubExp $ Var vv
      | _:is' <- inds -> Just $ Index cs vv is' loc

    Just (Replicate _ (Constant arr@(ArrayVal _ _) _) _)
       | _:is' <- inds,
         Just iis <- ctIndex is',
         Just el <- arrValInd arr iis ->
           Just $ SubExp $ Constant el loc

    Just (Replicate _ val@(Constant _ _) _)
      | [_] <- inds -> Just $ SubExp val

    Just (Rearrange cs2 perm (Var src) _)
       | permuteReach perm < length inds ->
         let inds' = permuteShape (take (length inds) perm) inds
         in Just $ Index (cs++cs2) src inds' loc

    _ -> Nothing

simplifyIndexing _ _ = Nothing

evaluateBranch :: TopDownRule
evaluateBranch _ (Let pat (If e1 tb fb t _))
  | Just branch <- checkBranch =
  let ses = resultSubExps $ bodyResult branch
      ses' = subExpShapeContext t ses ++ ses
  in return $ bodyBindings branch ++ [Let [p] $ SubExp se
                                     | (p,se) <- zip pat ses']
  where checkBranch
          | isCt1 e1  = Just tb
          | isCt0 e1  = Just fb
          | otherwise = Nothing
evaluateBranch _ _ = cannotSimplify

-- IMPROVE: This rule can be generalised to work in more cases,
-- especially when the branches have bindings, or return more than one
-- value.
simplifyBoolBranch :: TopDownRule
simplifyBoolBranch _ (Let [v] (If cond tb fb ts loc))
  | Body [] (Result [] [tres] _) <- tb,
    Body [] (Result [] [fres] _) <- fb,
    all (==Basic Bool) ts,
    False = do
  (e, bnds) <-
    runBinder'' (eBinOp LogOr (pure $ BinOp LogAnd cond tres (Basic Bool) loc)
                              (eBinOp LogAnd (pure $ Not cond loc)
                                             (pure $ SubExp fres) (Basic Bool) loc)
                              (Basic Bool) loc)
  return $ bnds ++ [Let [v] e]
simplifyBoolBranch _ _ = cannotSimplify

hoistBranchInvariant :: TopDownRule
hoistBranchInvariant _ (Let pat (If e1 tb fb ts loc)) =
  let Result tcs tses tresloc = bodyResult tb
      Result fcs fses fresloc = bodyResult fb
      (pat', res, invariant) =
        foldl branchInvariant ([], [], []) $
        zip pat (zip3 tses fses ts)
      (tses', fses', ts') = unzip3 res
      tb' = resultBody tcs tses' tresloc `setBodyResult` tb
      fb' = resultBody fcs fses' fresloc `setBodyResult` fb
  in if null invariant
     then cannotSimplify
     else return $ invariant ++ [Let pat' $ If e1 tb' fb' ts' loc]
  where branchInvariant (pat', res, invariant) (v, (tse, fse, t))
          | tse == fse = (pat', res, Let [v] (SubExp tse) : invariant)
          | otherwise  = (v:pat', (tse,fse,t):res, invariant)
hoistBranchInvariant _ _ = cannotSimplify

simplifyScalExp :: TopDownRule
simplifyScalExp vtable (Let pat e)
  | Just orig <- SE.toScalExp (`ST.lookupScalExp` vtable) e,
    Right new@(SE.Val _) <- AS.simplify orig loc ranges,
    orig /= new = do
      (e', bnds) <- SE.fromScalExp loc new
      return $ bnds ++ [Let pat e']
  where loc = srclocOf e
        ranges = HM.filter nonEmptyRange $ HM.map toRep $ ST.bindings vtable
        toRep entry = (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper
simplifyScalExp _ _ = cannotSimplify

removeUnnecessaryCopy :: BottomUpRule
removeUnnecessaryCopy (_,used) (Let [v] (Copy se _))
  | not $ any (`UT.isConsumed` used) $
    identName v : HS.toList (aliases $ subExpType se) =
    return [Let [v] $ SubExp se]
removeUnnecessaryCopy _ _ = cannotSimplify

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: BottomUpRule
removeDeadBranchResult (_, used) (Let pat (If e1 tb fb ts loc))
  | -- Figure out which of the names in 'pat' are used...
    patused <- map ((`UT.used` used) . identName) pat,
    -- If they are not all used, then this rule applies.
    not (and patused) =
  -- Remove the parts of the branch-results that correspond to dead
  -- return value bindings.  Note that this leaves dead code in the
  -- branch bodies, but that will be removed later.
  let Result tcs tses tresloc = bodyResult tb
      Result fcs fses fresloc = bodyResult fb
      pick = map snd . filter fst . zip patused
      tb' = resultBody tcs (pick tses) tresloc `setBodyResult` tb
      fb' = resultBody fcs (pick fses) fresloc `setBodyResult` fb
      ts' = pick ts
      pat' = pick pat
  in return [Let pat' $ If e1 tb' fb' ts' loc]
removeDeadBranchResult _ _ = cannotSimplify

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
arrValInd v@(ArrayVal arr _) (i:is)
  | i >= 0, i < valueSize v = arrValInd (arr ! i) is
arrValInd _ _ = Nothing

arrLitInd :: Exp -> [Int] -> Maybe Exp
arrLitInd e [] = Just e
arrLitInd (ArrayLit els _ _) (i:is)
  | i >= 0, i < length els = arrLitInd (SubExp $ els !! i) is
arrLitInd (SubExp (Constant arr@(ArrayVal _ _) loc)) (i:is) =
  case arrValInd arr (i:is) of
    Nothing -> Nothing
    Just v  -> Just $ SubExp $ Constant v loc
arrLitInd _ _ = Nothing
