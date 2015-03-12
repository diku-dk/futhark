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
import Futhark.Optimise.Simplifier.Simplify
import qualified Futhark.Analysis.AlgSimplify as AS
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Tools

import Prelude hiding (any, all)

topDownRules :: MonadBinder m => TopDownRules m
topDownRules = [ liftIdentityMapping
               , removeReplicateMapping
               , hoistLoopInvariantMergeVariables
               , simplifyClosedFormRedomap
               , simplifyClosedFormReduce
               , simplifyClosedFormLoop
               , letRule simplifyRearrange
               , letRule simplifyBinOp
               , letRule simplifyNot
               , letRule simplifyNegate
               , letRule simplifyAssert
               , letRule simplifyIndexing
               , evaluateBranch
               , simplifyBoolBranch
               , hoistBranchInvariant
               , simplifyScalExp
               , letRule simplifyIdentityReshape
               , letRule simplifyReshapeReshape
               , removeScratchValue
               , hackilySimplifyBranch
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
liftIdentityMapping _ (Let pat _ (LoopOp (Map cs fun arrs))) =
  case foldr checkInvariance ([], [], []) $ zip3 (patternElements pat) ses rettype of
    ([], _, _) -> cannotSimplify
    (invariant, mapresult, rettype') -> do
      let (pat', ses') = unzip mapresult
          lambdaRes = Result ses'
          fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = lambdaRes }
                     , lambdaReturnType = rettype'
                     }
      mapM_ (uncurry letBind) invariant
      letBindNames'_ (map patElemName pat') $ LoopOp $ Map cs fun' arrs
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        Result ses = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map identType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance :: (PatElem lore, SubExp, Type)
                        -> ([(Pattern lore, Exp lore)],
                            [(PatElem lore, SubExp)],
                            [Type])
                        -> ([(Pattern lore, Exp lore)],
                            [(PatElem lore, SubExp)],
                            [Type])
        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            ((Pattern [outId], PrimOp $ SubExp $ Var inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = ((Pattern [outId], PrimOp $ Replicate outersize e) : invariant,
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
  let (params, arrs') = unzip paramsAndArrs
      fun' = fun { lambdaParams = params }
      -- Empty maps are not permitted, so if that would be the result,
      -- turn the entire map into a replicate.
      n = arraysSize 0 $ map identType arrs
      Result ses = bodyResult $ lambdaBody fun
      mapres = bodyBindings $ lambdaBody fun
  mapM_ (uncurry letBindNames') parameterBnds
  case arrs' of
    [] -> do mapM_ addBinding mapres
             sequence_ [ letBind p $ PrimOp $ Replicate n e
                       | (p,e) <- zip (splitPattern pat) ses ]
    _  -> letBind_ pat $ LoopOp $ Map cs fun' arrs'
  where (paramsAndArrs, parameterBnds) =
          partitionEithers $ zipWith isReplicate (lambdaParams fun) arrs

        isReplicate p v
          | Just (Replicate _ e) <-
            asPrimOp =<< ST.lookupExp (identName v) vtable =
              Right ([identName p], PrimOp $ SubExp e)
          | otherwise =
              Left (p, v)

removeReplicateMapping _ _ = cannotSimplify

removeDeadMapping :: MonadBinder m => BottomUpRule m
removeDeadMapping (_, used) (Let pat _ (LoopOp (Map cs fun arrs))) =
  let Result ses = bodyResult $ lambdaBody fun
      isUsed (bindee, _, _) = (`UT.used` used) $ patElemName bindee
      (pat',ses', ts') = unzip3 $ filter isUsed $
                         zip3 (patternElements pat) ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = Result ses' }
                 , lambdaReturnType = ts'
                 }
  in if pat /= Pattern pat'
     then letBind_ (Pattern pat') $ LoopOp $ Map cs fun' arrs
     else cannotSimplify
removeDeadMapping _ _ = cannotSimplify

-- After removing a result, we may also have to remove some existential bindings.
removeUnusedLoopResult :: forall m.MonadBinder m => BottomUpRule m
removeUnusedLoopResult (_, used) (Let pat _ (LoopOp (DoLoop respat merge form body)))
  | explpat' <- filter (keep . fst) explpat,
    explpat' /= explpat =
  let ctxrefs = concatMap (references . snd) explpat'
      patctxrefs = mconcat $ map (freeNamesIn . fst) explpat'
      bindeeUsed = (`HS.member` patctxrefs) . patElemName
      mergeParamUsed = (`elem` ctxrefs) . identName
      keepImpl (bindee,ident) = bindeeUsed bindee || mergeParamUsed ident
      implpat' = filter keepImpl implpat
      pat' = map fst $ implpat'++explpat'
      respat' = map snd explpat'
  in letBind_ (Pattern pat') $ LoopOp $ DoLoop respat' merge form body
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
          freeNamesIn (patElemLore bindee) <> freeNamesIn (patElemType bindee)
        taggedpat = zip (patternElements pat) $
                    loopResultContext (representative :: Lore m) respat (map fst merge) ++
                    respat
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat
        references ident = maybe [] (HS.toList . freeNamesIn . fparamLore) $
                           find ((identName ident==) . fparamName) $
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
  let Result es = bodyResult body
      returnedResultSubExps = map snd $ filter (explicitlyReturned . fst) $ zip mergepat es
      necessaryForReturned = mconcat $ map dependencies returnedResultSubExps
      resIsNecessary ((v,_), _) =
        explicitlyReturned v ||
        fparamName v `HS.member` necessaryForReturned ||
        referencedInPat v ||
        referencedInForm v
      (keep, discard) = partition resIsNecessary $ zip merge es
      (merge', es') = unzip keep
      body' = body { bodyResult = Result es' }
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
        explicitlyReturned = (`elem` respat) . fparamIdent
        patAnnotNames = mconcat [ freeNamesIn (fparamType bindee) <>
                                  freeNamesIn (fparamLore bindee)
                                | bindee <- mergepat ]
        referencedInPat = (`HS.member` patAnnotNames) . fparamName
        referencedInForm = (`HS.member` freeNamesIn form) . fparamName

        dummyBindings = map dummyBinding
        dummyBinding ((v,e), _)
          | unique (fparamType v) = ([fparamName v], PrimOp $ Copy e)
          | otherwise             = ([fparamName v], PrimOp $ SubExp e)

        allDependencies = dataDependencies body
        dependencies (Constant _) = HS.empty
        dependencies (Var v)        =
          fromMaybe HS.empty $ HM.lookup (identName v) allDependencies
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
    (invariant, explpat', merge', ses') ->
      -- We have moved something invariant out of the loop.
      let loopbody' = loopbody { bodyResult = Result ses' }
          invariantShape :: (a, Ident) -> Bool
          invariantShape (_, shapemerge) = shapemerge `elem`
                                           map (fparamIdent . fst) merge'
          (implpat',implinvariant) = partition invariantShape implpat
          implinvariant' = [ (patElemIdent p, Var v) | (p,v) <- implinvariant ]
          pat' = map fst $ implpat'++explpat'
          respat' = map snd explpat'
      in do forM_ (invariant ++ implinvariant') $ \(v1,v2) ->
              letBindNames'_ [identName v1] $ PrimOp $ SubExp v2
            letBind_ (Pattern pat') $
              LoopOp $ DoLoop respat' merge' form loopbody'
  where Result ses = bodyResult loopbody
        taggedpat = zip (patternElements pat) $
                    loopResultContext (representative :: Lore m)
                    respat (map fst merge) ++ respat
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat

        namesOfMergeParams = HS.fromList $ map (fparamName . fst) merge

        removeFromResult (mergeParam,mergeInit) explpat' =
          case partition ((==fparamIdent mergeParam) . snd) explpat' of
            ([(patelem,_)], rest) ->
              (Just (patElemIdent patelem, mergeInit), rest)
            (_,      _) ->
              (Nothing, explpat')

        checkInvariance :: ((FParam (Lore m), SubExp), SubExp)
                        -> ([(Ident, SubExp)], [(PatElem (Lore m), Ident)],
                            [(FParam (Lore m), SubExp)], [SubExp])
                        -> ([(Ident, SubExp)], [(PatElem (Lore m), Ident)],
                            [(FParam (Lore m), SubExp)], [SubExp])
        checkInvariance
          ((mergeParam,mergeInit), resExp)
          (invariant, explpat', merge', resExps)
          | not (unique (fparamType mergeParam)),
            isInvariant resExp =
          let (bnd, explpat'') =
                removeFromResult (mergeParam,mergeInit) explpat'
          in (maybe id (:) bnd $ (fparamIdent mergeParam, mergeInit) : invariant,
              explpat'', merge', resExps)
          where
            -- A non-unique merge variable is invariant if the corresponding
            -- subexp in the result is EITHER:
            --
            --  (0) a variable of the same name as the parameter, where
            --  all existential parameters are already known to be
            --  invariant
            isInvariant (Var v2)
              | fparamName mergeParam == identName v2 =
                allExistentialInvariant
                (HS.fromList $ map (identName . fst) invariant) mergeParam
            --  (1) or identical to the initial value of the parameter.
            isInvariant _ = mergeInit == resExp

        checkInvariance ((mergeParam,mergeInit), resExp) (invariant, explpat', merge', resExps) =
          (invariant, explpat', (mergeParam,mergeInit):merge', resExp:resExps)

        allExistentialInvariant namesOfInvariant mergeParam =
          all (invariantOrNotMergeParam namesOfInvariant)
          (fparamName mergeParam `HS.delete` freeNamesIn mergeParam)
        invariantOrNotMergeParam namesOfInvariant name =
          not (name `HS.member` namesOfMergeParams) ||
          name `HS.member` namesOfInvariant

hoistLoopInvariantMergeVariables _ _ = cannotSimplify

-- | A function that, given a variable name, returns its definition.
type VarLookup lore = VName -> Maybe (Exp lore)

type LetTopDownRule lore u = VarLookup lore -> PrimOp lore -> Maybe (PrimOp lore)

letRule :: MonadBinder m => LetTopDownRule (Lore m) u -> TopDownRule m
letRule rule vtable (Let pat _ (PrimOp op)) =
  letBind_ pat =<< liftMaybe (PrimOp <$> rule look op)
  where look = (`ST.lookupExp` vtable)
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
simplifyClosedFormLoop _ (Let pat _ (LoopOp (DoLoop respat merge (ForLoop _ bound) body))) =
  loopClosedForm pat respat merge bound body
simplifyClosedFormLoop _ _ = cannotSimplify

simplifyRearrange :: LetTopDownRule lore u

-- Handle identity permutation.
simplifyRearrange _ (Rearrange _ perm e)
  | perm == [0..arrayRank (identType e) - 1] = Just $ SubExp $ Var e

simplifyRearrange look (Rearrange cs perm v) =
  case asPrimOp =<< look (identName v) of
    Just (Rearrange cs2 perm2 e) ->
      -- Rearranging a rearranging: compose the permutations.
      Just $ Rearrange (cs++cs2) (perm `permuteCompose` perm2) e
    _ -> Nothing

simplifyRearrange _ _ = Nothing

simplifyBinOp :: LetTopDownRule lore u

simplifyBinOp _ (BinOp Plus e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1),  Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1+v2
      (Constant (RealVal v1), Constant (RealVal v2)) ->
        binOpRes $ RealVal $ v1+v2
      _ -> Nothing

simplifyBinOp _ (BinOp Minus e1 e2 _)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1-v2
      (Constant (RealVal v1), Constant (RealVal v2)) ->
        binOpRes $ RealVal $ v1-v2
      _ -> Nothing

simplifyBinOp _ (BinOp Times e1 e2 _)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1*v2
      (Constant (RealVal v1), Constant (RealVal v2)) ->
        binOpRes $ RealVal $ v1*v2
      _ -> Nothing

simplifyBinOp _ (BinOp Divide e1 e2 _)
  | isCt0 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `div` v2
      (Constant (RealVal v1), Constant (RealVal v2)) ->
        binOpRes $ RealVal $ v1 / v2
      _ -> Nothing

simplifyBinOp _ (BinOp Mod e1 e2 _) =
  case (e1, e2) of
    (Constant (IntVal v1), Constant (IntVal v2)) ->
      binOpRes $ IntVal $ v1 `mod` v2
    _ -> Nothing

simplifyBinOp _ (BinOp Pow e1 e2 _)
  | isCt0 e2 =
    case subExpType e1 of
      Basic Int  -> binOpRes $ IntVal 1
      Basic Real -> binOpRes $ RealVal 1.0
      _          -> Nothing
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 ^ v2
      (Constant (RealVal v1), Constant (RealVal v2)) ->
        binOpRes $ RealVal $ v1**v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftL e1 e2 _)
  | isCt0 e2 = Just $ SubExp e1
  | isCt0 e1 = Just $ SubExp $ Constant $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `shiftL` v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftR e1 e2 _)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `shiftR` v2
      _ -> Nothing

simplifyBinOp _ (BinOp Band e1 e2 _)
  | isCt0 e1 = Just $ SubExp $ Constant $ IntVal 0
  | isCt0 e2 = Just $ SubExp $ Constant $ IntVal 0
  | e1 == e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 .&. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Bor e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | e1 == e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 .|. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Xor e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | e1 == e2 = binOpRes $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ IntVal $ v1 `xor` v2
      _ -> Nothing

simplifyBinOp look (BinOp LogAnd e1 e2 _)
  | isCt0 e1 = Just $ SubExp $ Constant $ LogVal False
  | isCt0 e2 = Just $ SubExp $ Constant $ LogVal False
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | Var v <- e1,
    Just (Not e1') <- asPrimOp =<< look (identName v),
    e1' == e2 = binOpRes $ LogVal False
  | Var v <- e2,
    Just (Not e2') <- asPrimOp =<< look (identName v),
    e2' == e1 = binOpRes $ LogVal False
  | otherwise =
    case (e1, e2) of
      (Constant (LogVal  v1), Constant (LogVal v2)) ->
        binOpRes $ LogVal $ v1 && v2
      _ -> Nothing

simplifyBinOp look (BinOp LogOr e1 e2 _)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp $ Constant $ LogVal True
  | isCt1 e2 = Just $ SubExp $ Constant $ LogVal True
  | Var v <- e1,
    Just (Not e1') <- asPrimOp =<< look (identName v),
    e1' == e2 = binOpRes $ LogVal True
  | Var v <- e2,
    Just (Not e2') <- asPrimOp =<< look (identName v),
    e2' == e1 = binOpRes $ LogVal True
  | otherwise =
    case (e1, e2) of
      (Constant (LogVal v1), Constant (LogVal v2)) ->
        binOpRes $ LogVal $ v1 || v2
      _ -> Nothing

simplifyBinOp _ (BinOp Equal e1 e2 _)
  | e1 == e2 = binOpRes $ LogVal True
  | otherwise =
    case (e1, e2) of
      -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
      (Constant (IntVal v1), Constant (IntVal v2)) ->
        binOpRes $ LogVal $ v1==v2
      (Constant (RealVal v1), Constant (RealVal v2)) ->
        binOpRes $ LogVal $ v1==v2
      (Constant (LogVal  v1), Constant (LogVal v2)) ->
        binOpRes $ LogVal $ v1==v2
      (Constant (CharVal v1), Constant (CharVal v2)) ->
        binOpRes $ LogVal $ v1==v2
      _ -> Nothing

simplifyBinOp _ (BinOp Less e1 e2 _)
  | e1 == e2 = binOpRes $ LogVal False
  | otherwise =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (IntVal v1), Constant (IntVal v2)) ->
      binOpRes $ LogVal $ v1<v2
    (Constant (RealVal v1), Constant (RealVal v2)) ->
      binOpRes $ LogVal $ v1<v2
    (Constant (LogVal  v1), Constant (LogVal v2)) ->
      binOpRes $ LogVal $ v1<v2
    (Constant (CharVal v1), Constant (CharVal v2)) ->
      binOpRes $ LogVal $ v1<v2
    _ -> Nothing

simplifyBinOp _ (BinOp Leq e1 e2 _)
  | e1 == e2 = binOpRes $ LogVal True
  | otherwise =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (IntVal  v1), Constant (IntVal  v2)) ->
      binOpRes $ LogVal $ v1<=v2
    (Constant (RealVal v1), Constant (RealVal v2)) ->
      binOpRes $ LogVal $ v1<=v2
    (Constant (LogVal  v1), Constant (LogVal  v2)) ->
      binOpRes $ LogVal $ v1<=v2
    (Constant (CharVal v1), Constant (CharVal v2 )) ->
      binOpRes $ LogVal $ v1<=v2
    _ -> Nothing

simplifyBinOp _ _ = Nothing

binOpRes :: BasicValue -> Maybe (PrimOp lore)
binOpRes = Just . SubExp . Constant

simplifyNot :: LetTopDownRule lore u
simplifyNot _ (Not (Constant (LogVal v))) =
  Just $ SubExp $ constant (not v)
simplifyNot _ _ = Nothing

simplifyNegate :: LetTopDownRule lore u
simplifyNegate _ (Negate (Constant (IntVal  v))) =
  Just $ SubExp $ constant $ negate v
simplifyNegate _ (Negate (Constant (RealVal  v))) =
  Just $ SubExp $ constant $ negate v
simplifyNegate _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: LetTopDownRule lore u
simplifyAssert _ (Assert (Constant (LogVal True)) _) =
  Just $ SubExp $ Constant Checked
simplifyAssert _ _ =
  Nothing

simplifyIndexing :: LetTopDownRule lore u
simplifyIndexing look (Index cs idd inds) =
  case asPrimOp =<< look (identName idd) of
    Nothing -> Nothing

    Just (SubExp (Var v)) ->
      return $ Index cs (setIdentUniqueness v u) inds

    Just (Iota _)
      | [ii] <- inds -> Just $ SubExp ii

    Just (Index cs2 aa ais) ->
      Just $ Index (cs++cs2) (setIdentUniqueness aa u) (ais ++ inds)

    Just (e@ArrayLit {})
       | Just iis <- ctIndex inds,
         Just el <- arrLitInd e iis -> Just el

    Just (Replicate _ (Var vv))
      | [_]   <- inds -> Just $ SubExp $ Var $ setIdentUniqueness vv u
      | _:is' <- inds -> Just $ Index cs (setIdentUniqueness vv u) is'

    Just (Replicate _ val@(Constant _))
      | [_] <- inds -> Just $ SubExp val

    Just (Rearrange cs2 perm src)
       | permuteReach perm <= length inds ->
         let inds' = permuteShape (take (length inds) perm) inds
         in Just $ Index (cs++cs2) (setIdentUniqueness src u) inds'

    Just (Reshape cs2 [_] v2) ->
      Just $ Index (cs++cs2) (setIdentUniqueness v2 u) inds

    _ -> Nothing
  where u = uniqueness $ identType idd

simplifyIndexing _ _ = Nothing

evaluateBranch :: MonadBinder m => TopDownRule m
evaluateBranch _ (Let pat _ (If e1 tb fb t))
  | Just branch <- checkBranch =
  let ses = resultSubExps $ bodyResult branch
      ses' = subExpShapeContext t ses ++ ses
  in do mapM_ addBinding $ bodyBindings branch
        sequence_ [ letBind (Pattern [p]) $ PrimOp $ SubExp se
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
    (Body _ [] (Result [Constant (LogVal True)]))
    (Body _ [] (Result [Constant (LogVal False)]))
    _)) =
  letBind_ pat $ PrimOp $ SubExp cond
-- When typeOf(x)==bool, if c then x else y == (c && x) || (!c && y)
simplifyBoolBranch _ (Let pat _ (If cond tb fb ts))
  | Body _ [] (Result [tres]) <- tb,
    Body _ [] (Result [fres]) <- fb,
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
    (Body _ [] (Result [se1_a]))
    (Body _ [] (Result [Var v]))
    _))
  | Just (If (Var cond_b)
           (Body _ [] (Result [se1_b]))
           (Body _ [] (Result [_]))
           _) <- ST.lookupExp (identName v) vtable,
    let cond_a_e = ST.lookupExp (identName cond_a) vtable,
    let cond_b_e = ST.lookupExp (identName cond_b) vtable,
    se1_a == se1_b,
    cond_a == cond_b ||
    (isJust cond_a_e && cond_a_e == cond_b_e) =
      letBind_ pat $ PrimOp $ SubExp $ Var v
hackilySimplifyBranch _ _ =
  cannotSimplify

hoistBranchInvariant :: MonadBinder m => TopDownRule m
hoistBranchInvariant _ (Let pat _ (If e1 tb fb ret))
  | patternSize pat == length ret = do
  let Result tses = bodyResult tb
      Result fses = bodyResult fb
  (pat', res, invariant) <-
    foldM branchInvariant ([], [], False) $
    zip (patternElements pat) (zip tses fses)
  let (tses', fses') = unzip res
      tb' = tb { bodyResult = Result tses' }
      fb' = fb { bodyResult = Result fses' }
  if invariant -- Was something hoisted?
     then letBind_ (Pattern pat') =<<
          eIf (eSubExp e1) (pure tb') (pure fb')
     else cannotSimplify
  where branchInvariant (pat', res, invariant) (v, (tse, fse))
          | tse == fse = do
            letBind_ (Pattern [v]) $ PrimOp $ SubExp tse
            return (pat', res, True)
          | otherwise  =
            return (v:pat', (tse,fse):res, invariant)
hoistBranchInvariant _ _ = cannotSimplify

simplifyScalExp :: MonadBinder m => TopDownRule m
simplifyScalExp vtable (Let pat _ e)
  | Just orig <- SE.toScalExp (`ST.lookupScalExp` vtable) e,
    Right new@(SE.Val _) <- AS.simplify orig ranges,
    orig /= new = do
      e' <- SE.fromScalExp' new
      letBind_ pat e'
  where ranges = HM.filter nonEmptyRange $ HM.map toRep $ ST.bindings vtable
        toRep entry = (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper
simplifyScalExp _ _ = cannotSimplify

simplifyIdentityReshape :: LetTopDownRule lore u
simplifyIdentityReshape _ (Reshape _ newshape v)
  | newshape == arrayDims (identType v) = -- No-op reshape.
    Just $ SubExp $ Var v
simplifyIdentityReshape _ _ = Nothing

simplifyReshapeReshape :: LetTopDownRule lore u
simplifyReshapeReshape look (Reshape cs newshape v)
  | Just (Reshape cs2 _ v2) <- asPrimOp =<< look (identName v) =
    Just $ Reshape (cs++cs2) newshape v2
simplifyReshapeReshape _ _ = Nothing

removeUnnecessaryCopy :: MonadBinder m => BottomUpRule m
removeUnnecessaryCopy _ (Let (Pattern [v]) _ (PrimOp (Copy se)))
  | basicType $ subExpType se =
    letBind_ (Pattern [v]) $ PrimOp $ SubExp se

removeUnnecessaryCopy (_,used) (Let (Pattern [v]) _ (PrimOp (Copy (Var v2))))
  | unique $ identType v2,
    not $ any (`UT.used` used) $ identAliases v2 =
      letBind_ (Pattern [v]) $ PrimOp $ SubExp $ Var v2
removeUnnecessaryCopy _ _ = cannotSimplify

removeScratchValue :: MonadBinder m => TopDownRule m
removeScratchValue _ (Let
                      (Pattern [PatElem v (BindInPlace _ src _) _])
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
  let Result tses = bodyResult tb
      Result fses = bodyResult fb
      pick = map snd . filter fst . zip patused
      tb' = tb { bodyResult = Result (pick tses) }
      fb' = fb { bodyResult = Result (pick fses) }
      pat' = pick $ patternElements pat
  in letBind_ (Pattern pat') =<<
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
            letBind_ (Pattern [bindee]) $ PrimOp $ SubExp se
      mapM_ mkSimplified simplified
      let (bindees,tses',fses',rettype') = unzip4 orig
          pat' = Pattern bindees
          tb' = tb { bodyResult = Result tses' }
          fb' = fb { bodyResult = Result fses' }
      letBind_ pat' $ If e1 tb' fb' rettype'
  where tses = resultSubExps $ bodyResult tb
        fses = resultSubExps $ bodyResult fb
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
isCt1 (Constant (IntVal x))  = x == 1
isCt1 (Constant (RealVal x)) = x == 1
isCt1 (Constant (LogVal x))  = x
isCt1 _                      = False

isCt0 :: SubExp -> Bool
isCt0 (Constant (IntVal x))  = x == 0
isCt0 (Constant (RealVal x)) = x == 0
isCt0 (Constant (LogVal x))  = not x
isCt0 _                      = False

ctIndex :: [SubExp] -> Maybe [Int]
ctIndex [] = Just []
ctIndex (Constant (IntVal ii):is) =
  case ctIndex is of
    Nothing -> Nothing
    Just y  -> Just (ii:y)
ctIndex _ = Nothing

arrLitInd :: PrimOp lore -> [Int] -> Maybe (PrimOp lore)
arrLitInd e [] = Just e
arrLitInd (ArrayLit els _) (i:is)
  | i >= 0, i < length els = arrLitInd (SubExp $ els !! i) is
arrLitInd _ _ = Nothing
