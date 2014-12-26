{-# LANGUAGE ScopedTypeVariables #-}
-- | This module defines a collection of simplification rules, as per
-- "Futhark.Optimise.Simplifier.Rule".  They are used in the
-- simplifier.
module Futhark.Optimise.Simplifier.Rules
  ( standardRules
  )

where

import Control.Applicative
import Control.Monad

import Data.Bits
import Data.Either
import Data.Foldable (any, all)
import Data.List hiding (any, all)
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

bottomUpRules :: MonadBinder m => BottomUpRules m
bottomUpRules = [ removeDeadMapping
                , removeUnusedLoopResult
                , removeRedundantMergeVariables
                , removeDeadBranchResult
                , removeUnnecessaryCopy
                ]

standardRules :: MonadBinder m => RuleBook m
standardRules = (topDownRules, bottomUpRules)

liftIdentityMapping :: MonadBinder m => TopDownRule m
liftIdentityMapping _ (Let pat _ (LoopOp (Map cs fun arrs loc))) =
  case foldr checkInvariance ([], [], []) $ zip3 (patternBindees pat) ses rettype of
    ([], _, _) -> cannotSimplify
    (invariant, mapresult, rettype') -> do
      let (pat', ses') = unzip mapresult
          lambdaRes = Result rescs ses' resloc
          fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = lambdaRes }
                     , lambdaReturnType = rettype'
                     }
      mapM_ (uncurry letBind) invariant
      letBindNames_ (map bindeeName pat') $ LoopOp $ Map cs fun' arrs loc
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        Result rescs ses resloc = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map subExpType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance :: (PatBindee lore, SubExp, Type)
                        -> ([(Pattern lore, Exp lore)],
                            [(PatBindee lore, SubExp)],
                            [Type])
                        -> ([(Pattern lore, Exp lore)],
                            [(PatBindee lore, SubExp)],
                            [Type])
        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            ((Pattern [outId], PrimOp $ SubExp inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = ((Pattern [outId], PrimOp $ Replicate outersize e loc) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = cannotSimplify

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping :: MonadBinder m => TopDownRule m
removeReplicateMapping vtable (Let pat _ (LoopOp (Map cs fun arrs loc)))
  | not $ null parameterBnds = do
  let (params, arrs') = unzip paramsAndArrs
      fun' = fun { lambdaParams = params }
      -- Empty maps are not permitted, so if that would be the result,
      -- turn the entire map into a replicate.
      n = arraysSize 0 $ map subExpType arrs
      Result _ ses resloc = bodyResult $ lambdaBody fun
      mapres = bodyBindings $ lambdaBody fun
  mapM_ (uncurry letBindNames) parameterBnds
  case arrs' of
    [] -> do mapM_ addBinding mapres
             sequence_ [ letBind p $ PrimOp $ Replicate n e resloc
                       | (p,e) <- zip (splitPattern pat) ses ]
    _  -> letBind_ pat $ LoopOp $ Map cs fun' arrs' loc
  where (paramsAndArrs, parameterBnds) =
          partitionEithers $ zipWith isReplicate (lambdaParams fun) arrs

        isReplicate p (Var v)
          | Just (Replicate _ e _) <- asPrimOp =<< ST.lookupExp (identName v) vtable =
          Right ([identName p], PrimOp $ SubExp e)
        isReplicate p e =
          Left  (p, e)

removeReplicateMapping _ _ = cannotSimplify

removeDeadMapping :: MonadBinder m => BottomUpRule m
removeDeadMapping (_, used) (Let pat _ (LoopOp (Map cs fun arrs loc))) =
  let Result rcs ses resloc = bodyResult $ lambdaBody fun
      isUsed (bindee, _, _) = (`UT.used` used) $ bindeeName bindee
      (pat',ses', ts') = unzip3 $ filter isUsed $
                         zip3 (patternBindees pat) ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = Result rcs ses' resloc }
                 , lambdaReturnType = ts'
                 }
  in if pat /= Pattern pat'
     then letBind_ (Pattern pat') $ LoopOp $ Map cs fun' arrs loc
     else cannotSimplify
removeDeadMapping _ _ = cannotSimplify

-- After removing a result, we may also have to remove some existential bindings.
removeUnusedLoopResult :: forall m.MonadBinder m => BottomUpRule m
removeUnusedLoopResult (_, used) (Let pat _ (LoopOp (DoLoop respat merge i bound body loc)))
  | explpat' <- filter (keep . fst) explpat,
    explpat' /= explpat =
  let ctxrefs = concatMap references $ map snd explpat'
      implpat' = filter ((`elem` ctxrefs) . identName . snd) implpat
      pat' = map fst $ implpat'++explpat'
      respat' = map snd explpat'
  in letBind_ (Pattern pat') $ LoopOp $ DoLoop respat' merge i bound body loc
  where -- | Check whether the variable binding is used afterwards OR
        -- is responsible for some used existential part.
        keep bindee =
          bindeeName bindee `elem` nonremovablePatternNames
        patNames = patternNames pat
        nonremovablePatternNames =
          filter (`UT.used` used) patNames <>
          map bindeeName (filter interestingBindee $ patternBindees pat)
        interestingBindee bindee =
          any (`elem` patNames) $
          freeNamesIn (bindeeLore bindee) <> freeNamesIn (bindeeType bindee)
        taggedpat = zip (patternBindees pat) $
                    loopResultContext (representative :: Lore m) respat (map fst merge) ++
                    respat
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat
        references ident = maybe [] (HS.toList . freeNamesIn . bindeeLore) $
                           find ((identName ident==) . bindeeName) $
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
removeRedundantMergeVariables _ (Let pat _ (LoopOp (DoLoop respat merge i bound body loc)))
  | not $ all (explicitlyReturned . fst) merge =
  let Result cs es resloc = bodyResult body
      returnedResultSubExps = map snd $ filter (explicitlyReturned . fst) $ zip mergepat es
      necessaryForReturned = mconcat $ map dependencies returnedResultSubExps
      resIsNecessary ((v,_), _) =
        explicitlyReturned v ||
        bindeeName v `HS.member` necessaryForReturned ||
        referencedInPat v
      (keep, discard) = partition resIsNecessary $ zip merge es
      (merge', es') = unzip keep
      body' = body { bodyResult = Result cs es' resloc }
  in if merge == merge'
     then cannotSimplify
     else do
       -- We can't just remove the bindings in 'discard', since the loop
       -- body may still use their names in (now-dead) expressions.
       -- Hence, we add them inside the loop, fully aware that dead-code
       -- removal will eventually get rid of them.  Some care is
       -- necessary to handle unique bindings.
       body'' <- insertBindingsM $ do
         mapM_ (uncurry letBindNames) $ dummyBindings discard
         return body'
       letBind_ pat $ LoopOp $ DoLoop respat merge' i bound body'' loc
  where (mergepat, _) = unzip merge
        explicitlyReturned = (`elem` respat) . bindeeIdent
        patAnnotNames = mconcat [ freeNamesIn (bindeeType bindee) <>
                                  freeNamesIn (bindeeLore bindee)
                                | bindee <- mergepat ]
        referencedInPat = (`HS.member` patAnnotNames) . bindeeName

        dummyBindings = map dummyBinding
        dummyBinding ((v,e), _)
          | unique (bindeeType v) = ([bindeeName v], PrimOp $ Copy e $ srclocOf v)
          | otherwise             = ([bindeeName v], PrimOp $ SubExp e)

        allDependencies = dataDependencies body
        dependencies (Constant _ _) = HS.empty
        dependencies (Var v)        =
          fromMaybe HS.empty $ HM.lookup (identName v) allDependencies
removeRedundantMergeVariables _ _ =
  cannotSimplify

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantMergeVariables :: forall m.MonadBinder m => TopDownRule m
hoistLoopInvariantMergeVariables _ (Let pat _ (LoopOp (DoLoop respat merge idd n loopbody loc))) =
    -- Figure out which of the elements of loopresult are
    -- loop-invariant, and hoist them out.
  case foldr checkInvariance ([], explpat, [], []) $
       zip merge ses of
    ([], _, _, _) ->
      -- Nothing is invariant.
      cannotSimplify
    (invariant, explpat', merge', ses') ->
      -- We have moved something invariant out of the loop.
      let loopbody' = loopbody { bodyResult = Result cs ses' resloc }
          invariantShape :: (a, Ident) -> Bool
          invariantShape (_, shapemerge) = shapemerge `elem`
                                           map (bindeeIdent . fst) merge'
          (implpat',implinvariant) = partition invariantShape implpat
          implinvariant' = [ (bindeeIdent p, Var v) | (p,v) <- implinvariant ]
          pat' = map fst $ implpat'++explpat'
          respat' = map snd explpat'
      in do forM_ (invariant ++ implinvariant') $ \(v1,v2) ->
              letBindNames_ [identName v1] $ PrimOp $ SubExp v2
            letBind_ (Pattern pat') $
              LoopOp $ DoLoop respat' merge' idd n loopbody' loc
  where Result cs ses resloc = bodyResult loopbody
        taggedpat = zip (patternBindees pat) $
                    loopResultContext (representative :: Lore m)
                    respat (map fst merge) ++ respat
        (implpat, explpat) = splitAt (length taggedpat - length respat) taggedpat

        namesOfMergeParams = HS.fromList $ map (bindeeName . fst) merge

        removeFromResult (mergeParam,mergeInit) explpat' =
          case partition ((==bindeeIdent mergeParam) . snd) explpat' of
            ([(Bindee resv _,_)], rest) ->
              (Just (resv, mergeInit), rest)
            (_,      _) ->
              (Nothing, explpat')

        checkInvariance :: ((FParam (Lore m), SubExp), SubExp)
                        -> ([(Ident, SubExp)], [(PatBindee (Lore m), Ident)],
                            [(FParam (Lore m), SubExp)], [SubExp])
                        -> ([(Ident, SubExp)], [(PatBindee (Lore m), Ident)],
                            [(FParam (Lore m), SubExp)], [SubExp])
        checkInvariance
          ((mergeParam,mergeInit), resExp)
          (invariant, explpat', merge', resExps)
          | isInvariant resExp =
          let (bnd, explpat'') =
                removeFromResult (mergeParam,mergeInit) explpat'
          in (maybe id (:) bnd $ (bindeeIdent mergeParam, mergeInit) : invariant,
              explpat'', merge', resExps)
          where
            -- A merge variable is invariant if the corresponding
            -- subexp in the result is EITHER:
            --
            --  (0) a variable of the same name as the parameter, where
            --  all existential parameters are already known to be
            --  invariant
            isInvariant (Var v2)
              | bindeeName mergeParam == identName v2 =
                allExistentialInvariant
                (HS.fromList $ map (identName . fst) invariant) mergeParam
            --  (1) or identical to the initial value of the parameter.
            isInvariant _ = mergeInit == resExp

        checkInvariance ((mergeParam,mergeInit), resExp) (invariant, explpat', merge', resExps) =
          (invariant, explpat', (mergeParam,mergeInit):merge', resExp:resExps)

        allExistentialInvariant namesOfInvariant mergeParam =
          all (invariantOrNotMergeParam namesOfInvariant)
          (bindeeName mergeParam `HS.delete` freeNamesIn mergeParam)
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
simplifyClosedFormRedomap vtable (Let pat _ (LoopOp (Redomap _ _ innerfun acc arr _))) =
  foldClosedForm (`ST.lookupExp` vtable) pat innerfun acc arr
simplifyClosedFormRedomap _ _ = cannotSimplify

simplifyClosedFormReduce :: MonadBinder m => TopDownRule m
simplifyClosedFormReduce vtable (Let pat _ (LoopOp (Reduce _ fun args _))) =
  foldClosedForm (`ST.lookupExp` vtable) pat fun acc arr
  where (acc, arr) = unzip args
simplifyClosedFormReduce _ _ = cannotSimplify

simplifyClosedFormLoop :: MonadBinder m => TopDownRule m
simplifyClosedFormLoop _ (Let pat _ (LoopOp (DoLoop respat merge _ bound body _))) =
  loopClosedForm pat respat merge bound body
simplifyClosedFormLoop _ _ = cannotSimplify

simplifyRearrange :: LetTopDownRule lore u

-- Handle identity permutation.
simplifyRearrange _ (Rearrange _ perm e _)
  | perm == [0..arrayRank (subExpType e) - 1] = Just $ SubExp e

simplifyRearrange look (Rearrange cs perm (Var v) loc) =
  case asPrimOp =<< look (identName v) of
    Just (Rearrange cs2 perm2 e _) ->
      -- Rearranging a rearranging: compose the permutations.
      Just $ Rearrange (cs++cs2) (perm `permuteCompose` perm2) e loc
    _ -> Nothing

simplifyRearrange _ _ = Nothing

simplifyRotate :: LetTopDownRule lore u
-- A zero-rotation is identity.
simplifyRotate _ (Rotate _ 0 e _) =
  Just $ SubExp e

simplifyRotate look (Rotate _ _ (Var v) _) = do
  bnd <- asPrimOp =<< look (identName v)
  case bnd of
    -- Rotating a replicate is identity.
    Replicate {} ->
      Just $ SubExp $ Var v
    _ ->
      Nothing

simplifyRotate _ _ = Nothing

simplifyBinOp :: LetTopDownRule lore u

simplifyBinOp _ (BinOp Plus e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _,  Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1+v2
      (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
        binOpRes pos $ RealVal $ v1+v2
      _ -> Nothing

simplifyBinOp _ (BinOp Minus e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1-v2
      (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
        binOpRes pos $ RealVal $ v1-v2
      _ -> Nothing

simplifyBinOp _ (BinOp Times e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1*v2
      (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
        binOpRes pos $ RealVal $ v1*v2
      _ -> Nothing

simplifyBinOp _ (BinOp Divide e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1 `div` v2
      (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
        binOpRes pos $ RealVal $ v1 / v2
      _ -> Nothing

simplifyBinOp _ (BinOp Mod e1 e2 _ pos) =
  case (e1, e2) of
    (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
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
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1 ^ v2
      (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
        binOpRes pos $ RealVal $ v1**v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftL e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1 `shiftL` v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftR e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
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
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
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
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1 .|. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Xor e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | e1 == e2 = binOpRes pos $ IntVal 0
  | otherwise =
    case (e1, e2) of
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ IntVal $ v1 `xor` v2
      _ -> Nothing

simplifyBinOp look (BinOp LogAnd e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | Var v <- e1,
    Just (Not e1' _) <- asPrimOp =<< look (identName v),
    e1' == e2 = binOpRes pos $ LogVal False
  | Var v <- e2,
    Just (Not e2' _) <- asPrimOp =<< look (identName v),
    e2' == e1 = binOpRes pos $ LogVal False
  | otherwise =
    case (e1, e2) of
      (Constant (LogVal  v1) _, Constant (LogVal v2) _) ->
        binOpRes pos $ LogVal $ v1 && v2
      _ -> Nothing

simplifyBinOp look (BinOp LogOr e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e2
  | Var v <- e1,
    Just (Not e1' _) <- asPrimOp =<< look (identName v),
    e1' == e2 = binOpRes pos $ LogVal True
  | Var v <- e2,
    Just (Not e2' _) <- asPrimOp =<< look (identName v),
    e2' == e1 = binOpRes pos $ LogVal True
  | otherwise =
    case (e1, e2) of
      (Constant (LogVal v1) _, Constant (LogVal v2) _) ->
        binOpRes pos $ LogVal $ v1 || v2
      _ -> Nothing

simplifyBinOp _ (BinOp Equal e1 e2 _ pos)
  | e1 == e2 = binOpRes pos $ LogVal True
  | otherwise =
    case (e1, e2) of
      -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
      (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (LogVal  v1) _, Constant (LogVal v2) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (CharVal v1) _, Constant (CharVal v2) _) ->
        binOpRes pos $ LogVal $ v1==v2
      _ -> Nothing

simplifyBinOp _ (BinOp Less e1 e2 _ pos)
  | e1 == e2 = binOpRes pos $ LogVal False
  | otherwise =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (IntVal v1) _, Constant (IntVal v2) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (LogVal  v1) _, Constant (LogVal v2) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (CharVal v1) _, Constant (CharVal v2) _) ->
      binOpRes pos $ LogVal $ v1<v2
    _ -> Nothing

simplifyBinOp _ (BinOp Leq e1 e2 _ pos)
  | e1 == e2 = binOpRes pos $ LogVal True
  | otherwise =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (IntVal  v1) _, Constant (IntVal  v2) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (RealVal v1) _, Constant (RealVal v2) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (LogVal  v1) _, Constant (LogVal  v2) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (CharVal v1) _, Constant (CharVal v2 ) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    _ -> Nothing

simplifyBinOp _ _ = Nothing

binOpRes :: SrcLoc -> BasicValue -> Maybe (PrimOp lore)
binOpRes loc v = Just $ SubExp $ Constant v loc

simplifyNot :: LetTopDownRule lore u
simplifyNot _ (Not (Constant (LogVal v) _) loc) =
  Just $ SubExp $ constant (not v) loc
simplifyNot _ _ = Nothing

simplifyNegate :: LetTopDownRule lore u
simplifyNegate _ (Negate (Constant (IntVal  v) _) pos) =
  Just $ SubExp $ constant (negate v) pos
simplifyNegate _ (Negate (Constant (RealVal  v) _) pos) =
  Just $ SubExp $ constant (negate v) pos
simplifyNegate _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: LetTopDownRule lore u
simplifyAssert _ (Assert (Constant (LogVal True) _) loc) =
  Just $ SubExp $ Constant Checked loc
simplifyAssert _ _ =
  Nothing

simplifyConjoin :: LetTopDownRule lore u
simplifyConjoin look (Conjoin es loc) =
  -- Remove trivial certificates.
  let check seen (Constant Checked _) = seen
      check seen (Var idd) =
        case asPrimOp =<< look (identName idd) of
          Just (Conjoin es2 _) -> seen `S.union` S.fromList es2
          _                    -> Var idd `S.insert` seen
      check seen e = e `S.insert` seen
      origset = S.fromList es
      newset = foldl check S.empty es
      es' = S.toList newset
  in case es' of
       []                    -> Just $ SubExp $ Constant Checked loc
       [c]                   -> Just $ SubExp c
       _ | origset /= newset -> Just $ Conjoin es' loc
         | otherwise         -> Nothing
simplifyConjoin _ _ = Nothing

simplifyIndexing :: LetTopDownRule lore u
simplifyIndexing look (Index cs idd inds loc) =
  case asPrimOp =<< look (identName idd) of
    Nothing -> Nothing

    Just (SubExp (Var v)) ->
      return $ Index cs v inds loc

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

    Just (Replicate _ val@(Constant _ _) _)
      | [_] <- inds -> Just $ SubExp val

    Just (Rearrange cs2 perm (Var src) _)
       | permuteReach perm <= length inds ->
         let inds' = permuteShape (take (length inds) perm) inds
         in Just $ Index (cs++cs2) src inds' loc

    _ -> Nothing

simplifyIndexing _ _ = Nothing

evaluateBranch :: MonadBinder m => TopDownRule m
evaluateBranch _ (Let pat _ (If e1 tb fb t _))
  | Just branch <- checkBranch =
  let ses = resultSubExps $ bodyResult branch
      ses' = subExpShapeContext (resTypeValues t) ses ++ ses
  in do mapM_ addBinding $ bodyBindings branch
        sequence_ [ letBind (Pattern [p]) $ PrimOp $ SubExp se
                  | (p,se) <- zip (patternBindees pat) ses']
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
    (Body _ [] (Result _ [Constant (LogVal True) _] _))
    (Body _ [] (Result _ [Constant (LogVal False) _] _))
    _ _)) =
  letBind_ pat $ PrimOp $ SubExp cond
-- When typeOf(x)==bool, if c then x else y == (c && x) || (!c && y)
simplifyBoolBranch _ (Let pat _ (If cond tb fb ts loc))
  | Body _ [] (Result [] [tres] _) <- tb,
    Body _ [] (Result [] [fres] _) <- fb,
    Just ts' <- simpleType ts,
    all (==Basic Bool) ts',
    False = do -- FIXME: disable because algebraic optimiser cannot handle it.
  e <- eBinOp LogOr (pure $ PrimOp $ BinOp LogAnd cond tres (Basic Bool) loc)
                    (eBinOp LogAnd (pure $ PrimOp $ Not cond loc)
                     (pure $ PrimOp $ SubExp fres) (Basic Bool) loc)
       (Basic Bool) loc
  letBind_ pat e
simplifyBoolBranch _ _ = cannotSimplify

hoistBranchInvariant :: MonadBinder m => TopDownRule m
hoistBranchInvariant _ (Let pat _ (If e1 tb fb ts loc)) = do
  let Result tcs tses tresloc = bodyResult tb
      Result fcs fses fresloc = bodyResult fb
  (pat', res, invariant) <-
    foldM branchInvariant ([], [], False) $
    zip (patternBindees pat) (zip3 tses fses $ resTypeElems ts)
  let (tses', fses', ts') = unzip3 res
      tb' = tb { bodyResult = Result tcs tses' tresloc }
      fb' = fb { bodyResult = Result fcs fses' fresloc }
  if invariant -- Was something hoisted?
     then letBind_ (Pattern pat') $ If e1 tb' fb' (ResType ts') loc
     else cannotSimplify
  where branchInvariant (pat', res, invariant) (v, (tse, fse, t))
          | tse == fse = do
            letBind_ (Pattern [v]) $ PrimOp $ SubExp tse
            return (pat', res, True)
          | otherwise  =
            return (v:pat', (tse,fse,t):res, invariant)
hoistBranchInvariant _ _ = cannotSimplify

simplifyScalExp :: MonadBinder m => TopDownRule m
simplifyScalExp vtable (Let pat _ e)
  | Just orig <- SE.toScalExp (`ST.lookupScalExp` vtable) e,
    Right new@(SE.Val _) <- AS.simplify orig loc ranges,
    orig /= new = do
      e' <- SE.fromScalExp' loc new
      letBind_ pat e'
  where loc = srclocOf e
        ranges = HM.filter nonEmptyRange $ HM.map toRep $ ST.bindings vtable
        toRep entry = (ST.bindingDepth entry, lower, upper)
          where (lower, upper) = ST.valueRange entry
        nonEmptyRange (_, lower, upper) = isJust lower || isJust upper
simplifyScalExp _ _ = cannotSimplify

removeUnnecessaryCopy :: MonadBinder m => BottomUpRule m
removeUnnecessaryCopy (_,used) (Let (Pattern [v]) _ (PrimOp (Copy se _)))
  | not $ any (`UT.isConsumed` used) $
    bindeeName v : HS.toList (subExpAliases se),
    -- FIXME: This needs to be fixed, but it is trickier than one
    -- might think...
    False =
    letBindNames_ [bindeeName v] $ PrimOp $ SubExp se
removeUnnecessaryCopy _ _ = cannotSimplify

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: MonadBinder m => BottomUpRule m
removeDeadBranchResult (_, used) (Let pat _ (If e1 tb fb ts loc))
  | -- Figure out which of the names in 'pat' are used...
    patused <- map (`UT.used` used) $ patternNames pat,
    -- If they are not all used, then this rule applies.
    not (and patused) =
  -- Remove the parts of the branch-results that correspond to dead
  -- return value bindings.  Note that this leaves dead code in the
  -- branch bodies, but that will be removed later.
  let Result tcs tses tresloc = bodyResult tb
      Result fcs fses fresloc = bodyResult fb
      pick = map snd . filter fst . zip patused
      tb' = tb { bodyResult = Result tcs (pick tses) tresloc }
      fb' = fb { bodyResult = Result fcs (pick fses) fresloc }
      ts' = pick $ resTypeElems ts
      pat' = pick $ patternBindees pat
  in letBind_ (Pattern pat') $ If e1 tb' fb' (ResType ts') loc
removeDeadBranchResult _ _ = cannotSimplify

-- Some helper functions

isCt1 :: SubExp -> Bool
isCt1 (Constant (IntVal x)  _) = x == 1
isCt1 (Constant (RealVal x) _) = x == 1
isCt1 (Constant (LogVal x)  _) = x
isCt1 _                                   = False

isCt0 :: SubExp -> Bool
isCt0 (Constant (IntVal x)  _) = x == 0
isCt0 (Constant (RealVal x) _) = x == 0
isCt0 (Constant (LogVal x)  _) = not x
isCt0 _                                   = False

ctIndex :: [SubExp] -> Maybe [Int]
ctIndex [] = Just []
ctIndex (Constant (IntVal ii) _:is) =
  case ctIndex is of
    Nothing -> Nothing
    Just y  -> Just (ii:y)
ctIndex _ = Nothing

arrLitInd :: PrimOp lore -> [Int] -> Maybe (PrimOp lore)
arrLitInd e [] = Just e
arrLitInd (ArrayLit els _ _) (i:is)
  | i >= 0, i < length els = arrLitInd (SubExp $ els !! i) is
arrLitInd _ _ = Nothing
