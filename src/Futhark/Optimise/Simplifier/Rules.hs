{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module defines a collection of simplification rules, as per
-- "Futhark.Optimise.Simplifier.Rule".  They are used in the
-- simplifier.
module Futhark.Optimise.Simplifier.Rules
  ( standardRules
  , basicRules
  )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Either
import Data.Foldable (any, all)
import Data.List hiding (any, all)
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.DataDependencies
import Futhark.Optimise.Simplifier.ClosedForm
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.RuleM
import qualified Futhark.Analysis.AlgSimplify as AS
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Tools

import Prelude hiding (any, all)

topDownRules :: MonadBinder m => TopDownRules m
topDownRules = [ liftIdentityMapping
               , removeReplicateMapping
               , removeUnusedMapInput
               , hoistLoopInvariantMergeVariables
               , simplifyClosedFormRedomap
               , simplifyClosedFormReduce
               , simplifyClosedFormLoop
               , letRule simplifyRearrange
               , letRule simplifyBinOp
               , letRule simplifyNot
               , letRule simplifyComplement
               , letRule simplifyNegate
               , letRule simplifyAssert
               , letRule simplifyIndexing
               , simplifyIndexIntoReshape
               , simplifyIndexIntoSplit
               , removeEmptySplits
               , removeSingletonSplits
               , evaluateBranch
               , simplifyBoolBranch
               , hoistBranchInvariant
               , simplifyScalExp
               , letRule simplifyIdentityReshape
               , letRule simplifyReshapeReshape
               , removeScratchValue
               , hackilySimplifyBranch
               , removeIdentityInPlace
               , simplifyBranchContext
               ]

bottomUpRules :: MonadBinder m => BottomUpRules m
bottomUpRules = [ removeDeadMapping
                , removeUnusedLoopResult
                , removeRedundantMergeVariables
                , removeDeadBranchResult
                , removeUnnecessaryCopy
                , simplifyEqualBranchResult
                ]

standardRules :: MonadBinder m => RuleBook m
standardRules = (topDownRules, bottomUpRules)

-- | Rules that only work on 'Basic' lores or similar.  Includes 'standardRules'.
basicRules :: MonadBinder m => RuleBook m
basicRules = (topDownRules, removeUnnecessaryCopy : bottomUpRules)

liftIdentityMapping :: MonadBinder m => TopDownRule m
liftIdentityMapping _ (Let pat _ (LoopOp (Map cs fun arrs))) = do
  outersize <- arraysSize 0 <$> mapM lookupType arrs
  case foldr (checkInvariance outersize) ([], [], []) $
       zip3 (patternElements pat) ses rettype of
    ([], _, _) -> cannotSimplify
    (invariant, mapresult, rettype') -> do
      let (pat', ses') = unzip mapresult
          fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                     , lambdaReturnType = rettype'
                     }
      mapM_ (uncurry letBind) invariant
      letBindNames'_ (map patElemName pat') $ LoopOp $ Map cs fun' arrs
  where inputMap = HM.fromList $ zip (map paramName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        ses = bodyResult $ lambdaBody fun

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance :: SubExp
                        -> (PatElem lore, SubExp, Type)
                        -> ([(Pattern lore, Exp lore)],
                            [(PatElem lore, SubExp)],
                            [Type])
                        -> ([(Pattern lore, Exp lore)],
                            [(PatElem lore, SubExp)],
                            [Type])
        checkInvariance _ (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup v inputMap =
            ((Pattern [] [outId], PrimOp $ SubExp $ Var inp) : invariant,
             mapresult,
             rettype')
        checkInvariance outersize (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = ((Pattern [] [outId], PrimOp $ Replicate outersize e) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = cannotSimplify

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping :: MonadBinder m => TopDownRule m
removeReplicateMapping vtable (Let pat _ (LoopOp (Map cs fun arrs)))
  | not $ null parameterBnds = do
  n <- arraysSize 0 <$> mapM lookupType arrs
  let (params, arrs') = unzip paramsAndArrs
      fun' = fun { lambdaParams = params }
      -- Empty maps are not permitted, so if that would be the result,
      -- turn the entire map into a replicate.
      ses = bodyResult $ lambdaBody fun
      mapres = bodyBindings $ lambdaBody fun
  mapM_ (uncurry letBindNames') parameterBnds
  case arrs' of
    [] -> do mapM_ addBinding mapres
             sequence_ [ letBind (Pattern [] [p]) $ PrimOp $ Replicate n e
                       | (p,e) <- zip (patternValueElements pat) ses ]
    _  -> letBind_ pat $ LoopOp $ Map cs fun' arrs'
  where (paramsAndArrs, parameterBnds) =
          partitionEithers $ zipWith isReplicate (lambdaParams fun) arrs

        isReplicate p v
          | Just (Replicate _ e) <-
            asPrimOp =<< ST.lookupExp v vtable =
              Right ([paramName p], PrimOp $ SubExp e)
          | otherwise =
              Left (p, v)

removeReplicateMapping _ _ = cannotSimplify

-- | Remove inputs that are not used inside the @map@.
removeUnusedMapInput :: MonadBinder m => TopDownRule m
removeUnusedMapInput _ (Let pat _ (LoopOp (Map cs fun arrs)))
  | (used,unused) <- partition usedInput params_and_arrs,
    not (null unused) =
      -- Empty maps are not permitted, so if that would be the result,
      -- turn the entire map into a replicate.
      case used of
        [] -> do
          mapM_ addBinding $ bodyBindings $ lambdaBody fun
          forM_ (zip (patternNames pat) $
                bodyResult $ lambdaBody fun) $ \(name, res_se) ->
            letBindNames'_ [name] $ PrimOp $ Replicate width res_se
        _ -> do
          let (used_params, used_arrs) = unzip used
              fun' = fun { lambdaParams = used_params }
          letBind_ pat $ LoopOp $ Map cs fun' used_arrs
  where params_and_arrs = zip (lambdaParams fun) arrs
        width = arraysSize 0 $ patternTypes pat
        used_in_body = freeInBody $ lambdaBody fun
        usedInput (param, _) = paramName param `HS.member` used_in_body
removeUnusedMapInput _ _ = cannotSimplify

removeDeadMapping :: MonadBinder m => BottomUpRule m
removeDeadMapping (_, used) (Let pat _ (LoopOp (Map cs fun arrs))) =
  let ses = bodyResult $ lambdaBody fun
      isUsed (bindee, _, _) = (`UT.used` used) $ patElemName bindee
      (pat',ses', ts') = unzip3 $ filter isUsed $
                         zip3 (patternElements pat) ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                 , lambdaReturnType = ts'
                 }
  in if pat /= Pattern [] pat'
     then letBind_ (Pattern [] pat') $ LoopOp $ Map cs fun' arrs
     else cannotSimplify
removeDeadMapping _ _ = cannotSimplify

-- After removing a result, we may also have to remove some existential bindings.
removeUnusedLoopResult :: forall m.MonadBinder m => BottomUpRule m
removeUnusedLoopResult (_, used) (Let pat _ (LoopOp (DoLoop respat merge form body)))
  | explpat' <- filter (keep . fst) explpat,
    explpat' /= explpat =
  let ctxrefs = concatMap (references . snd) explpat'
      patctxrefs = mconcat $ map (freeIn . fst) explpat'
      bindeeUsed = (`HS.member` patctxrefs) . patElemName
      mergeParamUsed = (`elem` ctxrefs)
      keepImpl (bindee,ident) = bindeeUsed bindee || mergeParamUsed ident
      implpat' = filter keepImpl implpat
      implpat'' = map fst implpat'
      explpat'' = map fst explpat'
      respat' = map snd explpat'
  in letBind_ (Pattern implpat'' explpat'') $ LoopOp $ DoLoop respat' merge form body
  where -- | Check whether the variable binding is used afterwards OR
        -- is responsible for some used existential part.
        keep bindee =
          patElemName bindee `elem` nonremovablePatternNames
        patNames = patternNames pat
        nonremovablePatternNames =
          filter (`UT.used` used) patNames <>
          map patElemName (filter interestingBindee $ patternElements pat)
        interestingBindee bindee =
          any (`elem` patNames) $
          freeIn (patElemLore bindee) <> freeIn (patElemType bindee)
        taggedpat = zip (patternElements pat) $
                    loopResultContext (representative :: Lore m) respat (map fst merge) ++
                    respat
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat
        references name = maybe [] (HS.toList . freeIn . paramLore) $
                          find ((name==) . paramName) $
                          map fst merge
removeUnusedLoopResult _ _ = cannotSimplify

-- This next one is tricky - it's easy enough to determine that some
-- loop result is not used after the loop (as in
-- 'removeUnusedLoopResult'), but here, we must also make sure that it
-- does not affect any other values.
--
-- I do not claim that the current implementation of this rule is
-- perfect, but it should suffice for many cases, and should never
-- generate wrong code.
removeRedundantMergeVariables :: MonadBinder m => BottomUpRule m
removeRedundantMergeVariables _ (Let pat _ (LoopOp (DoLoop respat merge form body)))
  | not $ all (explicitlyReturned . fst) merge =
  let es = bodyResult body
      returnedResultSubExps = map snd $ filter (explicitlyReturned . fst) $ zip mergepat es
      necessaryForReturned = mconcat $ map dependencies returnedResultSubExps
      resIsNecessary ((v,_), _) =
        explicitlyReturned v ||
        paramName v `HS.member` necessaryForReturned ||
        referencedInPat v ||
        referencedInForm v
      (keep, discard) = partition resIsNecessary $ zip merge es
      (merge', es') = unzip keep
      body' = body { bodyResult = es' }
  in if merge == merge'
     then cannotSimplify
     else do
       -- We can't just remove the bindings in 'discard', since the loop
       -- body may still use their names in (now-dead) expressions.
       -- Hence, we add them inside the loop, fully aware that dead-code
       -- removal will eventually get rid of them.  Some care is
       -- necessary to handle unique bindings.
       body'' <- insertBindingsM $ do
         mapM_ (uncurry letBindNames') $ dummyBindings discard
         return body'
       letBind_ pat $ LoopOp $ DoLoop respat merge' form body''
  where (mergepat, _) = unzip merge
        explicitlyReturned = (`elem` respat) . paramName
        patAnnotNames = mconcat [ freeIn (paramType bindee) <>
                                  freeIn (paramLore bindee)
                                | bindee <- mergepat ]
        referencedInPat = (`HS.member` patAnnotNames) . paramName
        referencedInForm = (`HS.member` freeIn form) . paramName

        dummyBindings = map dummyBinding
        dummyBinding ((p,e), _)
          | unique (paramType p),
            Var v <- e            = ([paramName p], PrimOp $ Copy v)
          | otherwise             = ([paramName p], PrimOp $ SubExp e)

        allDependencies = dataDependencies body
        dependencies (Constant _) = HS.empty
        dependencies (Var v)        =
          fromMaybe HS.empty $ HM.lookup v allDependencies
removeRedundantMergeVariables _ _ =
  cannotSimplify

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantMergeVariables :: forall m.MonadBinder m => TopDownRule m
hoistLoopInvariantMergeVariables _ (Let pat _ (LoopOp (DoLoop respat merge form loopbody))) =
    -- Figure out which of the elements of loopresult are
    -- loop-invariant, and hoist them out.
  case foldr checkInvariance ([], explpat, [], []) $
       zip merge ses of
    ([], _, _, _) ->
      -- Nothing is invariant.
      cannotSimplify
    (invariant, explpat', merge', ses') -> do
      -- We have moved something invariant out of the loop.
      let loopbody' = loopbody { bodyResult = ses' }
          invariantShape :: (a, VName) -> Bool
          invariantShape (_, shapemerge) = shapemerge `elem`
                                           map (paramName . fst) merge'
          (implpat',implinvariant) = partition invariantShape implpat
          implinvariant' = [ (patElemIdent p, Var v) | (p,v) <- implinvariant ]
          implpat'' = map fst implpat'
          explpat'' = map fst explpat'
          respat' = map snd explpat'
      forM_ (invariant ++ implinvariant') $ \(v1,v2) ->
        letBindNames'_ [identName v1] $ PrimOp $ SubExp v2
      letBind_ (Pattern implpat'' explpat'') $
        LoopOp $ DoLoop respat' merge' form loopbody'
  where ses = bodyResult loopbody
        taggedpat = zip (patternElements pat) $
                    loopResultContext (representative :: Lore m)
                    respat (map fst merge) ++ respat
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat

        namesOfMergeParams = HS.fromList $ map (paramName . fst) merge

        removeFromResult (mergeParam,mergeInit) explpat' =
          case partition ((==paramName mergeParam) . snd) explpat' of
            ([(patelem,_)], rest) ->
              (Just (patElemIdent patelem, mergeInit), rest)
            (_,      _) ->
              (Nothing, explpat')

        checkInvariance :: ((FParam (Lore m), SubExp), SubExp)
                        -> ([(Ident, SubExp)], [(PatElem (Lore m), VName)],
                            [(FParam (Lore m), SubExp)], [SubExp])
                        -> ([(Ident, SubExp)], [(PatElem (Lore m), VName)],
                            [(FParam (Lore m), SubExp)], [SubExp])
        checkInvariance
          ((mergeParam,mergeInit), resExp)
          (invariant, explpat', merge', resExps)
          | not (unique (paramType mergeParam)),
            isInvariant resExp =
          let (bnd, explpat'') =
                removeFromResult (mergeParam,mergeInit) explpat'
          in (maybe id (:) bnd $ (paramIdent mergeParam, mergeInit) : invariant,
              explpat'', merge', resExps)
          where
            -- A non-unique merge variable is invariant if the corresponding
            -- subexp in the result is EITHER:
            --
            --  (0) a variable of the same name as the parameter, where
            --  all existential parameters are already known to be
            --  invariant
            isInvariant (Var v2)
              | paramName mergeParam == v2 =
                allExistentialInvariant
                (HS.fromList $ map (identName . fst) invariant) mergeParam
            --  (1) or identical to the initial value of the parameter.
            isInvariant _ = mergeInit == resExp

        checkInvariance ((mergeParam,mergeInit), resExp) (invariant, explpat', merge', resExps) =
          (invariant, explpat', (mergeParam,mergeInit):merge', resExp:resExps)

        allExistentialInvariant namesOfInvariant mergeParam =
          all (invariantOrNotMergeParam namesOfInvariant)
          (paramName mergeParam `HS.delete` freeIn mergeParam)
        invariantOrNotMergeParam namesOfInvariant name =
          not (name `HS.member` namesOfMergeParams) ||
          name `HS.member` namesOfInvariant

hoistLoopInvariantMergeVariables _ _ = cannotSimplify

-- | A function that, given a variable name, returns its definition.
type VarLookup lore = VName -> Maybe (Exp lore)

-- | A function that, given a subexpression, returns its type.
type TypeLookup = SubExp -> Maybe Type

type LetTopDownRule lore u = VarLookup lore -> TypeLookup
                             -> PrimOp lore -> Maybe (PrimOp lore)

letRule :: MonadBinder m => LetTopDownRule (Lore m) u -> TopDownRule m
letRule rule vtable (Let pat _ (PrimOp op)) =
  letBind_ pat =<< liftMaybe (PrimOp <$> rule defOf typeOf op)
  where defOf = (`ST.lookupExp` vtable)
        typeOf (Var v) = ST.lookupType v vtable
        typeOf (Constant v) = Just $ Basic $ basicValueType v
letRule _ _ _ =
  cannotSimplify

simplifyClosedFormRedomap :: MonadBinder m => TopDownRule m
simplifyClosedFormRedomap vtable (Let pat _ (LoopOp (Redomap _ _ innerfun acc arr))) =
  foldClosedForm (`ST.lookupExp` vtable) pat innerfun acc arr
simplifyClosedFormRedomap _ _ = cannotSimplify

simplifyClosedFormReduce :: MonadBinder m => TopDownRule m
simplifyClosedFormReduce vtable (Let pat _ (LoopOp (Reduce _ fun args))) =
  foldClosedForm (`ST.lookupExp` vtable) pat fun acc arr
  where (acc, arr) = unzip args
simplifyClosedFormReduce _ _ = cannotSimplify

simplifyClosedFormLoop :: MonadBinder m => TopDownRule m
simplifyClosedFormLoop _ (Let pat _ (LoopOp (DoLoop respat merge (ForLoop i bound) body))) =
  loopClosedForm pat respat merge (HS.singleton i) bound body
simplifyClosedFormLoop _ _ = cannotSimplify

simplifyRearrange :: LetTopDownRule lore u

-- Handle identity permutation.
simplifyRearrange _ typeOf (Rearrange _ perm e)
  | Just t <- typeOf $ Var e,
    perm == [0..arrayRank t - 1] = Just $ SubExp $ Var e

simplifyRearrange defOf _ (Rearrange cs perm v) =
  case asPrimOp =<< defOf v of
    Just (Rearrange cs2 perm2 e) ->
      -- Rearranging a rearranging: compose the permutations.
      Just $ Rearrange (cs++cs2) (perm `permuteCompose` perm2) e
    _ -> Nothing

simplifyRearrange _ _ _ = Nothing

simplifyBinOp :: LetTopDownRule lore u

simplifyBinOp _ _ (BinOp Plus e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | otherwise = SubExp <$> numBinOp op e1 e2
    where op x y = Just $ x + y

simplifyBinOp _ _ (BinOp Minus e1 e2 _)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise = SubExp <$> numBinOp op e1 e2
    where op x y = Just $ x - y

simplifyBinOp _ _ (BinOp Times e1 e2 _)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise = SubExp <$> numBinOp op e1 e2
    where op x y = Just $ x * y

simplifyBinOp _ _ (BinOp Divide e1 e2 _)
  | isCt0 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e1
  | isCt0 e2 = Nothing
  | otherwise = SubExp <$> intFloatBinOp intop floatop e1 e2
  where intop x y = return $ x `div` y
        floatop x y = return $ x / y

simplifyBinOp _ _ (BinOp Mod e1 e2 _)
  | isCt0 e2 = Nothing
  | otherwise = SubExp <$> intBinOp op e1 e2
  where op x y = Just $ x `mod` y

simplifyBinOp _ _ (BinOp IntDivide e1 e2 _)
  | isCt0 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e1
  | isCt0 e2 = Nothing
  | otherwise = SubExp <$> intBinOp op e1 e2
  where op x y = return $ x `div` y

simplifyBinOp _ typeOf (BinOp Pow e1 e2 _)
  | isCt0 e2 =
    case typeOf e1 of
      Just (Basic Int)     -> binOpRes $ IntVal 1
      Just (Basic Float32) -> binOpRes $ Float32Val 1.0
      Just (Basic Float64) -> binOpRes $ Float64Val 1.0
      _                    -> Nothing
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = Just $ SubExp e1
  | otherwise = SubExp <$> intFloatBinOp intop floatop e1 e2
  where intop x y = return $ x ^ y
        floatop x y = return $ x ** y

simplifyBinOp _ _ (BinOp ShiftL e1 e2 _)
  | isCt0 e2 = Just $ SubExp e1
  | isCt0 e1 = Just $ SubExp $ Constant $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `shiftL` fromIntegral v2
      _ -> Nothing

simplifyBinOp _ _ (BinOp ShiftR e1 e2 _)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `shiftR` fromIntegral v2
      _ -> Nothing

simplifyBinOp _ _ (BinOp Band e1 e2 _)
  | isCt0 e1 = Just $ SubExp $ Constant $ IntVal 0
  | isCt0 e2 = Just $ SubExp $ Constant $ IntVal 0
  | e1 == e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 .&. v2
      _ -> Nothing

simplifyBinOp _ _ (BinOp Bor e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | e1 == e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 .|. v2
      _ -> Nothing

simplifyBinOp _ _ (BinOp Xor e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | e1 == e2 = binOpRes $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `xor` v2
      _ -> Nothing

simplifyBinOp defOf _ (BinOp LogAnd e1 e2 _)
  | isCt0 e1 = Just $ SubExp $ Constant $ LogVal False
  | isCt0 e2 = Just $ SubExp $ Constant $ LogVal False
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | Var v <- e1,
    Just (Not e1') <- asPrimOp =<< defOf v,
    e1' == e2 = binOpRes $ LogVal False
  | Var v <- e2,
    Just (Not e2') <- asPrimOp =<< defOf v,
    e2' == e1 = binOpRes $ LogVal False
  | otherwise =
    case (e1, e2) of
      (Constant (LogVal  v1), Constant (LogVal v2)) ->
        binOpRes $ LogVal $ v1 && v2
      _ -> Nothing

simplifyBinOp defOf _ (BinOp LogOr e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp $ Constant $ LogVal True
  | isCt1 e2 = Just $ SubExp $ Constant $ LogVal True
  | Var v <- e1,
    Just (Not e1') <- asPrimOp =<< defOf v,
    e1' == e2 = binOpRes $ LogVal True
  | Var v <- e2,
    Just (Not e2') <- asPrimOp =<< defOf v,
    e2' == e1 = binOpRes $ LogVal True
  | otherwise =
    case (e1, e2) of
      (Constant (LogVal v1), Constant (LogVal v2)) ->
        binOpRes $ LogVal $ v1 || v2
      _ -> Nothing

simplifyBinOp _ _ (BinOp Equal e1 e2 _)
  | e1 == e2 = binOpRes $ LogVal True
  | otherwise = SubExp <$> ordBinOp op e1 e2
  where op x y = return $ x == y

simplifyBinOp _ _ (BinOp Less e1 e2 _)
  | e1 == e2 = binOpRes $ LogVal False
  | otherwise = SubExp <$> ordBinOp op e1 e2
  where op x y = return $ x < y

simplifyBinOp _ _ (BinOp Leq e1 e2 _)
  | e1 == e2 = binOpRes $ LogVal True
  | otherwise = SubExp <$> ordBinOp op e1 e2
  where op x y = return $ x <= y

simplifyBinOp _ _ _ = Nothing

binOpRes :: BasicValue -> Maybe (PrimOp lore)
binOpRes = Just . SubExp . Constant

simplifyNot :: LetTopDownRule lore u
simplifyNot _ _ (Not (Constant (LogVal v))) =
  Just $ SubExp $ constant (not v)
simplifyNot _ _ _ = Nothing

simplifyComplement :: LetTopDownRule lore u
simplifyComplement _ _ (Complement (Constant (IntVal v))) =
  Just $ SubExp $ constant $ complement v
simplifyComplement _ _ _ = Nothing

simplifyNegate :: LetTopDownRule lore u
simplifyNegate _ _ (Negate (Constant (IntVal v))) =
  Just $ SubExp $ constant $ negate v
simplifyNegate _ _ (Negate (Constant (Float32Val v))) =
  Just $ SubExp $ constant $ negate v
simplifyNegate _ _ (Negate (Constant (Float64Val v))) =
  Just $ SubExp $ constant $ negate v
simplifyNegate _ _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: LetTopDownRule lore u
simplifyAssert _ _ (Assert (Constant (LogVal True)) _) =
  Just $ SubExp $ Constant Checked
simplifyAssert _ _ _ =
  Nothing

simplifyIndexing :: LetTopDownRule lore u
simplifyIndexing defOf typeOf (Index cs idd inds) =
  case asPrimOp =<< defOf idd of
    Nothing -> Nothing

    Just (SubExp (Var v)) ->
      return $ Index cs v inds

    Just (Iota _)
      | [ii] <- inds -> Just $ SubExp ii

    Just (Index cs2 aa ais) ->
      Just $ Index (cs++cs2) aa (ais ++ inds)

    Just (e@ArrayLit {})
       | Just iis <- ctIndex inds,
         Just el <- arrLitInd e iis -> Just el

    Just (Replicate _ (Var vv))
      | [_]   <- inds -> Just $ SubExp $ Var vv
      | _:is' <- inds -> Just $ Index cs vv is'

    Just (Replicate _ val@(Constant _))
      | [_] <- inds -> Just $ SubExp val

    Just (Rearrange cs2 perm src)
       | permuteReach perm <= length inds ->
         let inds' = permuteShape (take (length inds) perm) inds
         in Just $ Index (cs++cs2) src inds'

    Just (Reshape cs2 [_] v2)
      | Just [_] <- arrayDims <$> typeOf (Var v2) ->
        Just $ Index (cs++cs2) v2 inds

    _ -> Nothing

simplifyIndexing _ _ _ = Nothing

simplifyIndexIntoReshape :: MonadBinder m => TopDownRule m
simplifyIndexIntoReshape vtable (Let pat _ (PrimOp (Index cs idd inds)))
  | Just (Reshape cs2 newshape idd2) <- asPrimOp =<< ST.lookupExp idd vtable,
    length newshape == length inds = do
      -- Linearise indices and map to old index space.
      oldshape <- arrayDims <$> lookupType idd2
      let mult x y = eBinOp Times x y Int
          plus x y = eBinOp Plus x y Int
          eproduct [] = return $ PrimOp $ SubExp $ Constant $ IntVal 1
          eproduct (x:xs) = foldl mult x xs
          esum [] = return $ PrimOp $ SubExp $ Constant $ IntVal 0
          esum (x:xs) = foldl plus x xs

          sliceSizes [] =
            return [PrimOp $ SubExp $ Constant $ IntVal 1]
          sliceSizes (n:ns) =
            (:) <$> eproduct (map eSubExp $ n:ns) <*> sliceSizes ns

          computeNewIndices :: MonadBinder m => [SubExp] -> SubExp -> m [SubExp]
          computeNewIndices [] _ = return []
          computeNewIndices (size:slices) i = do
            i' <- letSubExp "i" $
                  PrimOp $ BinOp IntDivide i size Int
            i_remnant <- letSubExp "i_remnant" =<<
                         eBinOp Minus (eSubExp i)
                         (eBinOp Times
                          (eSubExp i')
                          (eSubExp size) Int) Int
            is <- computeNewIndices slices i_remnant
            return $ i' : is
      new_slice_sizes <- mapM (letSubExp "new_slice_size") =<< drop 1 <$> sliceSizes newshape
      old_slice_sizes <- mapM (letSubExp "old_slice_size") =<< drop 1 <$> sliceSizes oldshape
      ind_sizes <- zipWithM mult (map eSubExp inds) (map eSubExp new_slice_sizes)
      flat_index <- letSubExp "flat_index" =<< esum (map return ind_sizes)
      new_indices <- computeNewIndices old_slice_sizes flat_index
      letBind_ pat $ PrimOp $ Index (cs++cs2) idd2 new_indices
simplifyIndexIntoReshape _ _ =
  cannotSimplify

simplifyIndexIntoSplit :: MonadBinder m => TopDownRule m
simplifyIndexIntoSplit vtable (Let pat _ (PrimOp (Index cs idd inds)))
  | Just (Let split_pat _ (PrimOp (Split cs2 ns idd2))) <-
      ST.entryBinding =<< ST.lookup idd vtable,
    first_index : rest_indices <- inds = do
      -- Figure out the extra offset that we should add to the first index.
      let plus x y = eBinOp Plus x y Int
          esum [] = return $ PrimOp $ SubExp $ Constant $ IntVal 0
          esum (x:xs) = foldl plus x xs

      patElem_and_offset <-
        zip (patternValueElements split_pat) <$>
        mapM esum (inits $ map eSubExp ns)
      case find ((==idd) . patElemName . fst) patElem_and_offset of
        Nothing ->
          cannotSimplify -- Probably should not happen.
        Just (_, offset_e) -> do
          offset <- letSubExp "offset" offset_e
          offset_index <- letSubExp "offset_index" $
                          PrimOp $ BinOp Plus first_index offset Int
          letBind_ pat $ PrimOp $ Index (cs++cs2) idd2 (offset_index:rest_indices)
simplifyIndexIntoSplit _ _ =
  cannotSimplify


removeEmptySplits :: MonadBinder m => TopDownRule m
removeEmptySplits _ (Let pat _ (PrimOp (Split cs ns arr)))
  | (pointless,sane) <- partition (isCt0 . snd) $ zip (patternValueElements pat) ns,
    not (null pointless) = do
      rt <- rowType <$> lookupType arr
      letBind_ (Pattern [] $ map fst sane) $
        PrimOp $ Split cs (map snd sane) arr
      forM_ pointless $ \(patElem,_) ->
        letBindNames' [patElemName patElem] $
        PrimOp $ ArrayLit [] rt
removeEmptySplits _ _ =
  cannotSimplify

removeSingletonSplits :: MonadBinder m => TopDownRule m
removeSingletonSplits _ (Let pat _ (PrimOp (Split _ [n] arr))) = do
  size <- arraySize 0 <$> lookupType arr
  if size == n then
    letBind_ pat $ PrimOp $ SubExp $ Var arr
    else cannotSimplify
removeSingletonSplits _ _ =
  cannotSimplify

evaluateBranch :: MonadBinder m => TopDownRule m
evaluateBranch _ (Let pat _ (If e1 tb fb t))
  | Just branch <- checkBranch = do
  let ses = bodyResult branch
  mapM_ addBinding $ bodyBindings branch
  ctx <- subExpShapeContext t ses
  let ses' = ctx ++ ses
  sequence_ [ letBind (Pattern [] [p]) $ PrimOp $ SubExp se
            | (p,se) <- zip (patternElements pat) ses']
  where checkBranch
          | isCt1 e1  = Just tb
          | isCt0 e1  = Just fb
          | otherwise = Nothing
evaluateBranch _ _ = cannotSimplify

-- IMPROVE: This rule can be generalised to work in more cases,
-- especially when the branches have bindings, or return more than one
-- value.
simplifyBoolBranch :: MonadBinder m => TopDownRule m
-- if c then True else False == c
simplifyBoolBranch _
  (Let pat _
   (If cond
    (Body _ [] [Constant (LogVal True)])
    (Body _ [] [Constant (LogVal False)])
    _)) =
  letBind_ pat $ PrimOp $ SubExp cond
-- When typeOf(x)==bool, if c then x else y == (c && x) || (!c && y)
simplifyBoolBranch _ (Let pat _ (If cond tb fb ts))
  | Body _ [] [tres] <- tb,
    Body _ [] [fres] <- fb,
    patternSize pat == length ts,
    all (==Basic Bool) ts,
    False = do -- FIXME: disable because algebraic optimiser cannot handle it.
  e <- eBinOp LogOr (pure $ PrimOp $ BinOp LogAnd cond tres Bool)
                    (eBinOp LogAnd (pure $ PrimOp $ Not cond)
                     (pure $ PrimOp $ SubExp fres) Bool)
       Bool
  letBind_ pat e
simplifyBoolBranch _ _ = cannotSimplify

-- XXX: this is a nasty ad-hoc rule for handling a pattern that occurs
-- due to limitations in shape analysis.  A better way would be proper
-- control flow analysis.
--
-- XXX: another hack is due to missing CSE.
hackilySimplifyBranch :: MonadBinder m => TopDownRule m
hackilySimplifyBranch vtable
  (Let pat _
   (If (Var cond_a)
    (Body _ [] [se1_a])
    (Body _ [] [Var v])
    _))
  | Just (If (Var cond_b)
           (Body _ [] [se1_b])
           (Body _ [] [_])
           _) <- ST.lookupExp v vtable,
    let cond_a_e = ST.lookupExp cond_a vtable,
    let cond_b_e = ST.lookupExp cond_b vtable,
    se1_a == se1_b,
    cond_a == cond_b ||
    (isJust cond_a_e && cond_a_e == cond_b_e) =
      letBind_ pat $ PrimOp $ SubExp $ Var v
hackilySimplifyBranch _ _ =
  cannotSimplify

hoistBranchInvariant :: MonadBinder m => TopDownRule m
hoistBranchInvariant _ (Let pat _ (If e1 tb fb ret))
  | patternSize pat == length ret = do
  let tses = bodyResult tb
      fses = bodyResult fb
  (pat', res, invariant) <-
    foldM branchInvariant ([], [], False) $
    zip (patternElements pat) (zip tses fses)
  let (tses', fses') = unzip res
      tb' = tb { bodyResult = tses' }
      fb' = fb { bodyResult = fses' }
  if invariant -- Was something hoisted?
     then letBind_ (Pattern [] pat') =<<
          eIf (eSubExp e1) (pure tb') (pure fb')
     else cannotSimplify
  where branchInvariant (pat', res, invariant) (v, (tse, fse))
          | tse == fse = do
            letBind_ (Pattern [] [v]) $ PrimOp $ SubExp tse
            return (pat', res, True)
          | otherwise  =
            return (v:pat', (tse,fse):res, invariant)
hoistBranchInvariant _ _ = cannotSimplify

-- | Non-existentialise the parts of the context that are the same in
-- both branches.
simplifyBranchContext :: MonadBinder m => TopDownRule m
simplifyBranchContext _ bnd@(Let pat _ e@(If cond tbranch fbranch _))
  | not $ null $ patternContextElements pat = do
      ctx_res <- expContext pat e
      let old_ctx =
            patternContextElements pat
          (free_ctx, new_ctx) =
            partitionEithers $
            zipWith ctxPatElemIsKnown old_ctx ctx_res
      if null free_ctx then
        cannotSimplify
        else do let ret' = existentialiseExtTypes
                           (HS.fromList $ map patElemName new_ctx) $
                           staticShapes $ patternValueTypes pat
                forM_ free_ctx $ \(name, se) ->
                  letBind_ (Pattern [] [name]) $ PrimOp $ SubExp se
                letBind_ pat { patternContextElements = new_ctx } $
                  If cond tbranch fbranch ret'
  where ctxPatElemIsKnown patElem (Just se) =
          Left (patElem, se)
        ctxPatElemIsKnown patElem _ =
          Right patElem
simplifyBranchContext _ _ =
  cannotSimplify

simplifyScalExp :: MonadBinder m => TopDownRule m
simplifyScalExp vtable (Let pat _ e) = do
  res <- SE.toScalExp (`ST.lookupScalExp` vtable) e
  case res of
    -- If the sufficient condition is 'True', then it statically succeeds.
    Just se@(SE.RelExp SE.LTH0 _)
      | Right (SE.Val (LogVal True)) <- mkDisj <$> AS.mkSuffConds se ranges ->
        letBind_ pat $ PrimOp $ SubExp $ Constant $ LogVal True
    Just se
      | Right new <- AS.simplify se ranges,
        SE.Val val <- new,
        se /= new ->
           letBind_ pat $ PrimOp $ SubExp $ Constant val
    _ -> cannotSimplify
  where ranges = ST.rangesRep vtable
        mkDisj []     = SE.Val $ LogVal False
        mkDisj (x:xs) = foldl SE.SLogOr (mkConj x) $ map mkConj xs
        mkConj []     = SE.Val $ LogVal True
        mkConj (x:xs) = foldl SE.SLogAnd x xs

simplifyIdentityReshape :: LetTopDownRule lore u
simplifyIdentityReshape _ typeOf (Reshape _ newshape v)
  | Just t <- typeOf $ Var v,
    newshape == arrayDims t = -- No-op reshape.
    Just $ SubExp $ Var v
simplifyIdentityReshape _ _ _ = Nothing

simplifyReshapeReshape :: LetTopDownRule lore u
simplifyReshapeReshape defOf _ (Reshape cs newshape v)
  | Just (Reshape cs2 _ v2) <- asPrimOp =<< defOf v =
    Just $ Reshape (cs++cs2) newshape v2
simplifyReshapeReshape _ _ _ = Nothing

removeUnnecessaryCopy :: MonadBinder m => BottomUpRule m
removeUnnecessaryCopy (_,used) (Let (Pattern [] [d]) _ (PrimOp (Copy v))) = do
  t <- lookupType v
  let originalNotUsedAnymore =
        unique t && not (any (`UT.used` used) $ vnameAliases v)
  if basicType t || originalNotUsedAnymore
    then letBind_ (Pattern [] [d]) $ PrimOp $ SubExp $ Var v
    else cannotSimplify
removeUnnecessaryCopy _ _ = cannotSimplify

removeIdentityInPlace :: MonadBinder m => TopDownRule m
removeIdentityInPlace vtable (Let (Pattern [] [d]) _ e)
  | BindInPlace _ dest destis <- patElemBindage d,
    arrayFrom e dest destis =
    letBind_ (Pattern [] [d { patElemBindage = BindVar}]) $ PrimOp $ SubExp $ Var dest
  where arrayFrom (PrimOp (Copy v)) dest destis
          | Just e' <- ST.lookupExp v vtable =
              arrayFrom e' dest destis
        arrayFrom (PrimOp (Index _ src srcis)) dest destis =
          src == dest && destis == srcis
        arrayFrom _ _ _ =
          False
removeIdentityInPlace _ _ =
  cannotSimplify

removeScratchValue :: MonadBinder m => TopDownRule m
removeScratchValue _ (Let
                      (Pattern [] [PatElem v (BindInPlace _ src _) _])
                      _
                      (PrimOp (Scratch {}))) =
    letBindNames'_ [identName v] $ PrimOp $ SubExp $ Var src
removeScratchValue _ _ =
  cannotSimplify

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: MonadBinder m => BottomUpRule m
removeDeadBranchResult (_, used) (Let pat _ (If e1 tb fb rettype))
  | -- Only if there is no existential context...
    patternSize pat == length rettype,
    -- Figure out which of the names in 'pat' are used...
    patused <- map (`UT.used` used) $ patternNames pat,
    -- If they are not all used, then this rule applies.
    not (and patused) =
  -- Remove the parts of the branch-results that correspond to dead
  -- return value bindings.  Note that this leaves dead code in the
  -- branch bodies, but that will be removed later.
  let tses = bodyResult tb
      fses = bodyResult fb
      pick = map snd . filter fst . zip patused
      tb' = tb { bodyResult = pick tses }
      fb' = fb { bodyResult = pick fses }
      pat' = pick $ patternElements pat
  in letBind_ (Pattern [] pat') =<<
     eIf (eSubExp e1) (pure tb') (pure fb')
removeDeadBranchResult _ _ = cannotSimplify

-- | Simplify return values of a branch if it is later asserted that
-- they have some specific value.  FIXME: this is not entiiiiirely
-- sound, as in practice we just end up removing the eventual
-- assertion.  This is really just about eliminating shape computation
-- branches.  Maybe there is a better way.
simplifyEqualBranchResult :: MonadBinder m => BottomUpRule m
simplifyEqualBranchResult (_, used) (Let pat _ (If e1 tb fb rettype))
  | -- Only if there is no existential context...
    patternSize pat == length rettype,
    let (simplified,orig) = partitionEithers $ map isActually $
                            zip4 (patternElements pat) tses fses rettype,
    not (null simplified) = do
      let mkSimplified (bindee, se) =
            letBind_ (Pattern [] [bindee]) $ PrimOp $ SubExp se
      mapM_ mkSimplified simplified
      let (bindees,tses',fses',rettype') = unzip4 orig
          pat' = Pattern [] bindees
          tb' = tb { bodyResult = tses' }
          fb' = fb { bodyResult = fses' }
      letBind_ pat' $ If e1 tb' fb' rettype'
  where tses = bodyResult tb
        fses = bodyResult fb
        isActually (bindee, se1, se2, t)
          | UT.isEqualTo se1 name used =
              Left (bindee, se1)
          | UT.isEqualTo se2 name used =
              Left (bindee, se2)
          | otherwise =
              Right (bindee, se1, se2, t)
          where name = patElemName bindee
simplifyEqualBranchResult _ _ = cannotSimplify

-- Some helper functions

isCt1 :: SubExp -> Bool
isCt1 (Constant (IntVal x))     = x == 1
isCt1 (Constant (Float32Val x)) = x == 1
isCt1 (Constant (Float64Val x)) = x == 1
isCt1 (Constant (LogVal x))     = x
isCt1 _                         = False

isCt0 :: SubExp -> Bool
isCt0 (Constant (IntVal x))     = x == 0
isCt0 (Constant (Float32Val x)) = x == 0
isCt0 (Constant (Float64Val x)) = x == 0
isCt0 (Constant (LogVal x))     = not x
isCt0 _                         = False

ctIndex :: [SubExp] -> Maybe [Int32]
ctIndex [] = Just []
ctIndex (Constant (IntVal ii):is) =
  case ctIndex is of
    Nothing -> Nothing
    Just y  -> Just (ii:y)
ctIndex _ = Nothing

arrLitInd :: PrimOp lore -> [Int32] -> Maybe (PrimOp lore)
arrLitInd e [] = Just e
arrLitInd (ArrayLit els _) (i:is)
  | i >= 0, i < genericLength els = arrLitInd (SubExp $ els !! fromIntegral i) is
arrLitInd _ _ = Nothing

ordBinOp :: (Functor m, Monad m) =>
            (forall a. Ord a => a -> a -> m Bool)
         -> SubExp -> SubExp -> m SubExp
ordBinOp op (Constant (IntVal x)) (Constant (IntVal y)) =
  Constant <$> LogVal <$> x `op` y
ordBinOp op (Constant (CharVal x)) (Constant (CharVal y)) =
  Constant <$> LogVal <$> x `op` y
ordBinOp op (Constant (Float32Val x)) (Constant (Float32Val y)) =
  Constant <$> LogVal <$> x `op` y
ordBinOp op (Constant (Float64Val x)) (Constant (Float64Val y)) =
  Constant <$> LogVal <$> x `op` y
ordBinOp op (Constant (LogVal x)) (Constant (LogVal y)) =
  Constant <$> LogVal <$> x `op` y
ordBinOp _ _ _ =
  fail "ordBinOp: operands not of appropriate type."

numBinOp :: (Functor m, Monad m) =>
            (forall num. Num num => num -> num -> m num)
         -> SubExp -> SubExp -> m SubExp
numBinOp op (Constant (IntVal x)) (Constant (IntVal y)) =
  Constant <$> IntVal <$> x `op` y
numBinOp op (Constant (Float32Val x)) (Constant (Float32Val y)) =
  Constant <$> Float32Val <$> x `op` y
numBinOp op (Constant (Float64Val x)) (Constant (Float64Val y)) =
  Constant <$> Float64Val <$> x `op` y
numBinOp _ _ _ =
  fail "numBinOp: operands not of appropriate type."

intBinOp :: (Functor m, Monad m) =>
            (forall int. Integral int => int -> int -> m int)
         -> SubExp -> SubExp -> m SubExp
intBinOp op (Constant (IntVal x)) (Constant (IntVal y)) =
  Constant <$> IntVal <$> x `op` y
intBinOp _ _ _ =
  fail "intBinOp: operands not of appropriate type."

intFloatBinOp :: (Functor m, Monad m) =>
                 (forall int. Integral int => int -> int -> m int)
              -> (forall float. Floating float => float -> float -> m float)
              -> SubExp -> SubExp -> m SubExp
intFloatBinOp intop _ (Constant (IntVal x)) (Constant (IntVal y)) =
  Constant <$> IntVal <$> x `intop` y
intFloatBinOp _ floatop (Constant (Float32Val x)) (Constant (Float32Val y)) =
  Constant <$> Float32Val <$> x `floatop` y
intFloatBinOp _ floatop (Constant (Float64Val x)) (Constant (Float64Val y)) =
  Constant <$> Float64Val <$> x `floatop` y
intFloatBinOp _ _ _ _ =
  fail "intFloatBinOp: operands not of appropriate type."
