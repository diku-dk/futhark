{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Representation.SOACS.Simplify
       ( simplifySOACS
       , simplifyLambda
       , simplifyStms

       , simpleSOACS
       )
where

import Control.Monad
import Data.Foldable
import Data.Either
import Data.List
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set      as S

import Futhark.Representation.SOACS
import qualified Futhark.Representation.AST as AST
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rules
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.ClosedForm
import Futhark.Optimise.Simplify.Lore
import Futhark.Tools
import Futhark.Pass
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Transform.Rename

simpleSOACS :: Simplify.SimpleOps SOACS
simpleSOACS = Simplify.bindableSimpleOps simplifySOAC

simplifySOACS :: Prog -> PassM Prog
simplifySOACS = Simplify.simplifyProg simpleSOACS soacRules blockers
  where blockers = Engine.noExtraHoistBlockers { Engine.getArraySizes = getShapeNames }

-- | Getting the roots of what to hoist, for now only variable
-- names that represent shapes/sizes.
getShapeNames :: (Attributes lore, LetAttr lore ~ (VarWisdom, Type)) =>
                 AST.Stm lore -> Names
getShapeNames bnd =
  let tps1 = map patElemType $ patternElements $ stmPattern bnd
      tps2 = map (snd . patElemAttr) $ patternElements $ stmPattern bnd
  in  S.fromList $ subExpVars $ concatMap arrayDims (tps1 ++ tps2)

simplifyLambda :: (HasScope SOACS m, MonadFreshNames m) =>
                  Lambda -> [Maybe VName] -> m Lambda
simplifyLambda =
  Simplify.simplifyLambda simpleSOACS soacRules Engine.noExtraHoistBlockers

simplifyStms :: (HasScope SOACS m, MonadFreshNames m) =>
                Stms SOACS -> m (Stms SOACS)
simplifyStms =
  Simplify.simplifyStms simpleSOACS soacRules Engine.noExtraHoistBlockers

simplifySOAC :: Simplify.SimplifyOp SOACS
simplifySOAC (Stream outerdim form lam arr) = do
  outerdim' <- Engine.simplify outerdim
  (form', form_hoisted) <- simplifyStreamForm form
  arr' <- mapM Engine.simplify arr
  (lam', lam_hoisted) <- Engine.simplifyLambda lam (map Just arr)
  return (Stream outerdim' form' lam' arr', form_hoisted <> lam_hoisted)
  where simplifyStreamForm (Parallel o comm lam0 acc) = do
          acc'  <- mapM Engine.simplify acc
          (lam0', hoisted) <- Engine.simplifyLambda lam0 $
                              replicate (length $ lambdaParams lam0) Nothing
          return (Parallel o comm lam0' acc', hoisted)
        simplifyStreamForm (Sequential acc) = do
          acc' <- mapM Engine.simplify acc
          return (Sequential acc', mempty)

simplifySOAC (Map w fun arrs) = do
  w' <- Engine.simplify w
  arrs' <- mapM Engine.simplify arrs
  (fun', hoisted) <- Engine.simplifyLambda fun $ map Just arrs'
  return (Map w' fun' arrs', hoisted)

simplifySOAC (Reduce w comm fun input) = do
  (fun', hoisted) <- Engine.simplifyLambda fun (map (const Nothing) arrs)
  (,) <$> (Reduce <$> Engine.simplify w <*> pure comm <*> pure fun' <*>
           (zip <$> mapM Engine.simplify acc <*> mapM Engine.simplify arrs))
      <*> pure hoisted
  where (acc, arrs) = unzip input

simplifySOAC (Scan w fun input) = do
  (fun', hoisted) <- Engine.simplifyLambda fun (map (const Nothing) arrs)
  (,) <$> (Scan <$> Engine.simplify w <*> pure fun' <*>
           (zip <$> mapM Engine.simplify acc <*> mapM Engine.simplify arrs))
      <*> pure hoisted
  where (acc, arrs) = unzip input

simplifySOAC (Redomap w comm outerfun innerfun acc arrs) = do
  (outerfun', outerfun_hoisted) <-
    Engine.simplifyLambda outerfun (map (const Nothing) arrs)
  (innerfun', innerfun_hoisted) <-
    Engine.simplifyLambda innerfun (map Just arrs)
  (,) <$> (Redomap <$> Engine.simplify w <*> pure comm <*>
           pure outerfun' <*> pure innerfun' <*>
           mapM Engine.simplify acc <*> mapM Engine.simplify arrs)
      <*> pure (outerfun_hoisted <> innerfun_hoisted)

simplifySOAC (Scanomap w outerfun innerfun acc arrs) = do
  (outerfun', outerfun_hoisted) <-
    Engine.simplifyLambda outerfun (map (const Nothing) arrs)
  (innerfun', innerfun_hoisted) <-
    Engine.simplifyLambda innerfun (map Just arrs)
  (,) <$> (Scanomap <$> Engine.simplify w <*>
           pure outerfun' <*> pure innerfun' <*>
           mapM Engine.simplify acc <*> mapM Engine.simplify arrs)
      <*> pure (outerfun_hoisted <> innerfun_hoisted)

simplifySOAC (Scatter len lam ivs as) = do
  len' <- Engine.simplify len
  (lam', hoisted) <- Engine.simplifyLambda lam $ map Just ivs
  ivs' <- mapM Engine.simplify ivs
  as' <- mapM Engine.simplify as
  return (Scatter len' lam' ivs' as', hoisted)

instance BinderOps (Wise SOACS) where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

soacRules :: RuleBook (Wise SOACS)
soacRules = standardRules <> ruleBook topDownRules bottomUpRules

topDownRules :: [TopDownRule (Wise SOACS)]
topDownRules = [RuleOp removeReplicateMapping,
                RuleOp removeReplicateRedomap,
                RuleOp removeReplicateWrite,
                RuleOp removeUnusedMapInput,
                RuleOp simplifyClosedFormRedomap,
                RuleOp simplifyClosedFormReduce,
                RuleOp simplifyKnownIterationSOAC,
                RuleOp fuseConcatScatter
               ]

bottomUpRules :: [BottomUpRule (Wise SOACS)]
bottomUpRules = [RuleOp removeDeadMapping,
                 RuleOp removeDeadWrite,
                 RuleBasicOp removeUnnecessaryCopy,
                 RuleOp liftIdentityMapping,
                 RuleOp removeDuplicateMapOutput
                ]

liftIdentityMapping :: BottomUpRuleOp (Wise SOACS)
liftIdentityMapping (_, usages) pat _ (Map outersize fun arrs) =
  case foldr checkInvariance ([], [], []) $
       zip3 (patternElements pat) ses rettype of
    ([], _, _) -> cannotSimplify
    (invariant, mapresult, rettype') -> do
      let (pat', ses') = unzip mapresult
          fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                     , lambdaReturnType = rettype'
                     }
      mapM_ (uncurry letBind) invariant
      letBindNames_ (map patElemName pat') $ Op $ Map outersize fun' arrs
  where inputMap = M.fromList $ zip (map paramName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        ses = bodyResult $ lambdaBody fun

        freeOrConst (Var v)    = v `S.member` free
        freeOrConst Constant{} = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- M.lookup v inputMap =
              let e | patElemName outId `UT.isConsumed` usages = Copy inp
                    | otherwise                                = SubExp $ Var inp
              in ((Pattern [] [outId], BasicOp e) : invariant,
                  mapresult,
                  rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = ((Pattern [] [outId], BasicOp $ Replicate (Shape [outersize]) e) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ _ _ = cannotSimplify

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping :: TopDownRuleOp (Wise SOACS)
removeReplicateMapping vtable pat _ (Map outersize fun arrs)
  | Just (bnds, fun', arrs') <- removeReplicateInput vtable fun arrs = do
      forM_ bnds $ \(vs,cs,e) -> certifying cs $ letBindNames vs e
      letBind_ pat $ Op $ Map outersize fun' arrs'
removeReplicateMapping _ _ _ _ = cannotSimplify

-- | Like 'removeReplicateMapping', but for 'Redomap'.
removeReplicateRedomap :: TopDownRuleOp (Wise SOACS)
removeReplicateRedomap vtable pat _ (Redomap w comm redfun foldfun nes arrs)
  | Just (bnds, foldfun', arrs') <- removeReplicateInput vtable foldfun arrs = do
      forM_ bnds $ \(vs,cs,e) -> certifying cs $ letBindNames vs e
      letBind_ pat $ Op $ Redomap w comm redfun foldfun' nes arrs'
removeReplicateRedomap _ _ _ _ = cannotSimplify

-- | Like 'removeReplicateMapping', but for 'Scatter'.
removeReplicateWrite :: TopDownRuleOp (Wise SOACS)
removeReplicateWrite vtable pat _ (Scatter len lam ivs as)
  | Just (bnds, lam', ivs') <- removeReplicateInput vtable lam ivs = do
      forM_ bnds $ \(vs,cs,e) -> certifying cs $ letBindNames vs e
      letBind_ pat $ Op $ Scatter len lam' ivs' as
removeReplicateWrite _ _ _ _ = cannotSimplify

removeReplicateInput :: Aliased lore =>
                        ST.SymbolTable lore
                     -> AST.Lambda lore -> [VName]
                     -> Maybe ([([VName], Certificates, AST.Exp lore)],
                                AST.Lambda lore, [VName])
removeReplicateInput vtable fun arrs
  | not $ null parameterBnds = do
  let (arr_params', arrs') = unzip params_and_arrs
      fun' = fun { lambdaParams = acc_params <> arr_params' }
  return (parameterBnds, fun', arrs')
  | otherwise = Nothing

  where params = lambdaParams fun
        (acc_params, arr_params) =
          splitAt (length params - length arrs) params
        (params_and_arrs, parameterBnds) =
          partitionEithers $ zipWith isReplicateAndNotConsumed arr_params arrs

        isReplicateAndNotConsumed p v
          | Just (BasicOp (Replicate (Shape (_:ds)) e), v_cs) <-
              ST.lookupExp v vtable,
            not $ paramName p `S.member` consumedByLambda fun =
              Right ([paramName p],
                     v_cs,
                     case ds of
                       [] -> BasicOp $ SubExp e
                       _  -> BasicOp $ Replicate (Shape ds) e)
          | otherwise =
              Left (p, v)

-- | Remove inputs that are not used inside the @map@.
removeUnusedMapInput :: TopDownRuleOp (Wise SOACS)
removeUnusedMapInput _ pat _ (Map width fun arrs)
  | (used,unused) <- partition usedInput params_and_arrs,
    not (null unused) = do
      let (used_params, used_arrs) = unzip used
          fun' = fun { lambdaParams = used_params }
      letBind_ pat $ Op $ Map width fun' used_arrs
  where params_and_arrs = zip (lambdaParams fun) arrs
        used_in_body = freeInBody $ lambdaBody fun
        usedInput (param, _) = paramName param `S.member` used_in_body
removeUnusedMapInput _ _ _ _ = cannotSimplify

removeDeadMapping :: BottomUpRuleOp (Wise SOACS)
removeDeadMapping (_, used) pat _ (Map width fun arrs) =
  let ses = bodyResult $ lambdaBody fun
      isUsed (bindee, _, _) = (`UT.used` used) $ patElemName bindee
      (pat',ses', ts') = unzip3 $ filter isUsed $
                         zip3 (patternElements pat) ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                 , lambdaReturnType = ts'
                 }
  in if pat /= Pattern [] pat'
     then letBind_ (Pattern [] pat') $ Op $ Map width fun' arrs
     else cannotSimplify
removeDeadMapping _ _ _ _ = cannotSimplify

removeDuplicateMapOutput :: BottomUpRuleOp (Wise SOACS)
removeDuplicateMapOutput (_, used) pat _ (Map width fun arrs) =
  let ses = bodyResult $ lambdaBody fun
      ts = lambdaReturnType fun
      pes = patternValueElements pat
      ses_ts_pes = zip3 ses ts pes
      (ses_ts_pes', copies) =
        foldl checkForDuplicates (mempty,mempty) ses_ts_pes
  in if null copies then cannotSimplify
     else do
       let (ses', ts', pes') = unzip3 ses_ts_pes'
           pat' = Pattern [] pes'
           fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                      , lambdaReturnType = ts' }
       letBind_ pat' $ Op $ Map width fun' arrs
       forM_ copies $ \(from,to) ->
         if UT.isConsumed (patElemName to) used then
           letBind_ (Pattern [] [to]) $ BasicOp $ Copy $ patElemName from
         else
           letBind_ (Pattern [] [to]) $ BasicOp $ SubExp $ Var $ patElemName from
  where checkForDuplicates (ses_ts_pes',copies) (se,t,pe)
          | Just (_,_,pe') <- find (\(x,_,_) -> x == se) ses_ts_pes' =
              -- This subexp has been returned before, producing the
              -- array pe'.
              (ses_ts_pes', (pe', pe) : copies)
          | otherwise = (ses_ts_pes' ++ [(se,t,pe)], copies)
removeDuplicateMapOutput _ _ _ _ = cannotSimplify

-- | If we are writing to an array that is never used, get rid of it.
removeDeadWrite :: BottomUpRuleOp (Wise SOACS)
removeDeadWrite (_, used) pat _ (Scatter w fun arrs dests) =
  let (i_ses, v_ses) = splitAt (length dests) $ bodyResult $ lambdaBody fun
      (i_ts, v_ts) = splitAt (length dests) $ lambdaReturnType fun
      isUsed (bindee, _, _, _, _, _) = (`UT.used` used) $ patElemName bindee
      (pat', i_ses', v_ses', i_ts', v_ts', dests') =
        unzip6 $ filter isUsed $
        zip6 (patternElements pat) i_ses v_ses i_ts v_ts dests
      fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = i_ses' ++ v_ses' }
                 , lambdaReturnType = i_ts' ++ v_ts'
                 }
  in if pat /= Pattern [] pat'
     then letBind_ (Pattern [] pat') $ Op $ Scatter w fun' arrs dests'
     else cannotSimplify
removeDeadWrite _ _ _ _ = cannotSimplify

-- Future improvements: handle concatenations of more than two arrays.
fuseConcatScatter :: TopDownRuleOp (Wise SOACS)
fuseConcatScatter vtable pat _ (Scatter _ fun arrs dests)
  | Just (ws@(w':_), xs, ys, css) <- unzip4 <$> mapM isConcat arrs,
    all (w'==) ws = do
      fun2 <- renameLambda fun
      let fun_n = length $ lambdaReturnType fun
          (fun_is, fun_vs) = splitAt (fun_n `div` 2) $ bodyResult (lambdaBody fun)
          (fun2_is, fun2_vs) = splitAt (fun_n `div` 2) $ bodyResult (lambdaBody fun2)
          (its, vts) = splitAt (fun_n `div` 2) $ lambdaReturnType fun
          (its2, vts2) = splitAt (fun_n `div` 2) $ lambdaReturnType fun
      let fun' = Lambda
                 { lambdaParams = lambdaParams fun <> lambdaParams fun2
                 , lambdaBody = mkBody (bodyStms (lambdaBody fun) <>
                                        bodyStms (lambdaBody fun2)) $
                                mix [fun_is, fun2_is] <> mix [fun_vs, fun2_vs]
                 , lambdaReturnType = mix [its, its2] <> mix [vts, vts2]
                 }
      certifying (mconcat css) $
        letBind_ pat $ Op $ Scatter w' fun' (xs++ys) $ map incWrites dests
  where sizeOf :: VName -> Maybe SubExp
        sizeOf x = arraySize 0 . ST.entryType <$> ST.lookup x vtable
        mix = concat . transpose
        incWrites (w, n, a) = (w, n+1, a)
        isConcat v = case ST.lookupExp v vtable of
          Just (BasicOp (Concat 0 x [y] _), cs) -> do
            x_w <- sizeOf x
            y_w <- sizeOf y
            guard $ x_w == y_w
            return (x_w, x, y, cs)
          _ -> Nothing
fuseConcatScatter _ _ _ _ = cannotSimplify

simplifyClosedFormRedomap :: TopDownRuleOp (Wise SOACS)
simplifyClosedFormRedomap vtable pat _ (Redomap _ _ _ innerfun acc arr) =
  foldClosedForm (`ST.lookupExp` vtable) pat innerfun acc arr
simplifyClosedFormRedomap _ _ _ _ = cannotSimplify

simplifyClosedFormReduce :: TopDownRuleOp (Wise SOACS)
simplifyClosedFormReduce vtable pat _ (Reduce _ _ fun args) =
  foldClosedForm (`ST.lookupExp` vtable) pat fun acc arr
  where (acc, arr) = unzip args
simplifyClosedFormReduce _ _ _ _ = cannotSimplify

-- For now we just remove singleton maps.
simplifyKnownIterationSOAC :: (BinderOps lore, Op lore ~ SOAC lore) =>
                              TopDownRuleOp lore
simplifyKnownIterationSOAC _ pat _ (Map (Constant k) fun arrs)
  | oneIsh k = do
      zipWithM_ bindParam (lambdaParams fun) arrs
      ses <- bodyBind $ lambdaBody fun
      zipWithM_ bindResult (patternValueElements pat) ses
        where bindParam p a = do
                a_t <- lookupType a
                letBindNames_ [paramName p] $
                  BasicOp $ Index a $ fullSlice a_t [DimFix $ constant (0::Int32)]
              bindResult pe se =
                letBindNames_ [patElemName pe] $
                BasicOp $ ArrayLit [se] $ rowType $ patElemType pe
simplifyKnownIterationSOAC _ _ _ _ = cannotSimplify
