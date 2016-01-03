{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Representation.SOACS.Simplify
       ( simplifySOACS
       , simplifyFun
       , simplifyLambda
       )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.List hiding (any, all)
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import Prelude hiding (any, all)

import Futhark.Representation.SOACS
import qualified Futhark.Representation.AST as AST
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.MonadFreshNames
import Futhark.Binder.Class
import Futhark.Optimise.Simplifier (simplifyProgWithRules, noExtraHoistBlockers)
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.RuleM
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.ClosedForm
import Futhark.Transform.Substitute
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Analysis.ScalExp as SE

simplifySOACS :: MonadFreshNames m => Prog -> m Prog
simplifySOACS =
  simplifyProgWithRules bindableSimpleOps soacRules noExtraHoistBlockers

simplifyFun :: MonadFreshNames m => FunDec -> m FunDec
simplifyFun =
  Simplifier.simplifyFunWithRules bindableSimpleOps soacRules Engine.noExtraHoistBlockers

simplifyLambda :: (HasScope SOACS m, MonadFreshNames m) =>
                  Lambda -> SubExp -> [Maybe VName] -> m Lambda
simplifyLambda =
  Simplifier.simplifyLambdaWithRules bindableSimpleOps soacRules Engine.noExtraHoistBlockers

instance Engine.SimplifiableOp SOACS (SOAC SOACS) where
  simplifyOp (Stream cs outerdim form lam arr ii) = do
    cs' <- Engine.simplify cs
    outerdim' <- Engine.simplify outerdim
    form' <- simplifyStreamForm outerdim' form
    arr' <- mapM Engine.simplify arr
    vtable <- Engine.getVtable
    let (chunk:_) = extLambdaParams lam
        se_outer = case outerdim of
                      Var idd    -> fromMaybe (SE.Id idd Int) (ST.lookupScalExp idd vtable)
                      Constant c -> SE.Val c
        se_1 = SE.Val $ IntVal 1
        -- extension: one may similarly treat iota stream-array case,
        -- by setting the bounds to [0, se_outer-1]
        parbnds  = [ (chunk, se_1, se_outer) ]
    lam' <- Engine.simplifyExtLambda lam outerdim' parbnds
    return $ Stream cs' outerdim' form' lam' arr' ii
    where simplifyStreamForm _ (MapLike o) =
            return $ MapLike o
          simplifyStreamForm outerdim' (RedLike o comm lam0 acc) = do
              acc'  <- mapM Engine.simplify acc
              lam0' <- Engine.simplifyLambda lam0 outerdim' $
                       replicate (length $ lambdaParams lam0) Nothing
              return $ RedLike o comm lam0' acc'
          simplifyStreamForm _ (Sequential acc) = do
              acc'  <- mapM Engine.simplify acc
              return $ Sequential acc'

  simplifyOp (Map cs w fun arrs) = do
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    arrs' <- mapM Engine.simplify arrs
    fun' <- Engine.simplifyLambda fun w $ map Just arrs'
    return $ Map cs' w' fun' arrs'

  simplifyOp (ConcatMap cs w fun arrs) = do
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    arrs' <- mapM (mapM Engine.simplify) arrs
    fun' <- Engine.simplifyLambda fun w $ map (const Nothing) $ lambdaParams fun
    return $ ConcatMap cs' w' fun' arrs'

  simplifyOp (Reduce cs w comm fun input) = do
    let (acc, arrs) = unzip input
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    acc' <- mapM Engine.simplify acc
    arrs' <- mapM Engine.simplify arrs
    fun' <- Engine.simplifyLambda fun w $ map Just arrs'
    return $ Reduce cs' w' comm fun' (zip acc' arrs')

  simplifyOp (Scan cs w fun input) = do
    let (acc, arrs) = unzip input
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    acc' <- mapM Engine.simplify acc
    arrs' <- mapM Engine.simplify arrs
    fun' <- Engine.simplifyLambda fun w $ map Just arrs'
    return $ Scan cs' w' fun' (zip acc' arrs')

  simplifyOp (Redomap cs w comm outerfun innerfun acc arrs) = do
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    acc' <- mapM Engine.simplify acc
    arrs' <- mapM Engine.simplify arrs
    outerfun' <- Engine.simplifyLambda outerfun w $
                 replicate (length $ lambdaParams outerfun) Nothing
    (innerfun', used) <- Engine.tapUsage $ Engine.simplifyLambda innerfun w $ map Just arrs
    (innerfun'', arrs'') <- removeUnusedParams used innerfun' arrs'
    return $ Redomap cs' w' comm outerfun' innerfun'' acc' arrs''
    where removeUnusedParams used lam arrinps
            | (accparams, arrparams) <- splitAt (length acc) $ lambdaParams lam =
                let (arrparams', arrinps') =
                      unzip $ filter ((`UT.used` used) . paramName . fst) $
                      zip arrparams arrinps
                in return (lam { lambdaParams = accparams ++ arrparams' },
                           arrinps')
            | otherwise = return (lam, arrinps)

soacRules :: (MonadBinder m,
              LocalScope (Lore m) m,
              Op (Lore m) ~ SOAC (Lore m)) => RuleBook m
soacRules = (std_td_rules <> topDownRules,
             std_bu_rules <> bottomUpRules)
  where (std_td_rules, std_bu_rules) = standardRules

topDownRules :: (MonadBinder m,
                 LocalScope (Lore m) m,
                 Op (Lore m) ~ SOAC (Lore m)) => TopDownRules m
topDownRules = [liftIdentityMapping,
                removeReplicateMapping,
                removeReplicateRedomap,
                removeIotaMapping,
                removeIotaRedomap,
                removeUnusedMapInput,
                simplifyClosedFormRedomap,
                simplifyClosedFormReduce,
                simplifyStream
               ]

bottomUpRules :: (MonadBinder m,
                  LocalScope (Lore m) m,
                  Op (Lore m) ~ SOAC (Lore m)) => BottomUpRules m
bottomUpRules = [removeDeadMapping
                ]

liftIdentityMapping :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) =>
                       TopDownRule m
liftIdentityMapping _ (Let pat _ (Op (Map cs outersize fun arrs))) =
  case foldr checkInvariance ([], [], []) $
       zip3 (patternElements pat) ses rettype of
    ([], _, _) -> cannotSimplify
    (invariant, mapresult, rettype') -> do
      let (pat', ses') = unzip mapresult
          fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                     , lambdaReturnType = rettype'
                     }
      mapM_ (uncurry letBind) invariant
      letBindNames'_ (map patElemName pat') $ Op $ Map cs outersize fun' arrs
  where inputMap = HM.fromList $ zip (map paramName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        ses = bodyResult $ lambdaBody fun

        freeOrConst (Var v)    = v `HS.member` free
        freeOrConst Constant{} = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup v inputMap =
            ((Pattern [] [outId], PrimOp $ SubExp $ Var inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = ((Pattern [] [outId], PrimOp $ Replicate outersize e) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = cannotSimplify

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
removeReplicateMapping vtable (Let pat _ (Op (Map cs outersize fun arrs)))
  | Just (bnds, fun', arrs') <- removeReplicateInput vtable fun arrs = do
      mapM_ (uncurry letBindNames') bnds
      letBind_ pat $ Op $ Map cs outersize fun' arrs'

removeReplicateMapping _ _ = cannotSimplify

-- | Remove all arguments to the map that are iotas.
-- These can be turned into references to the index variable instead.
removeIotaMapping :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
removeIotaMapping vtable (Let pat _ (Op (Map cs outersize fun arrs)))
  | Just (fun', arrs') <- removeIotaInput vtable fun arrs =
      letBind_ pat $ Op $ Map cs outersize fun' arrs'
removeIotaMapping _ _ = cannotSimplify

-- | Like 'removeIotaMapping', but for 'Redomap'.
removeIotaRedomap :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
removeIotaRedomap vtable (Let pat _ (Op (Redomap cs w comm redfun foldfun nes arrs)))
  | Just (foldfun', arrs') <- removeIotaInput vtable foldfun arrs =
      letBind_ pat $ Op $ Redomap cs w comm redfun foldfun' nes arrs'
removeIotaRedomap _ _ = cannotSimplify

-- | Like 'removeReplicateMapping', but for 'Redomap'.
removeReplicateRedomap :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
removeReplicateRedomap vtable (Let pat _ (Op (Redomap cs w comm redfun foldfun nes arrs)))
  | Just (bnds, foldfun', arrs') <- removeReplicateInput vtable foldfun arrs = do
      mapM_ (uncurry letBindNames') bnds
      letBind_ pat $ Op $ Redomap cs w comm redfun foldfun' nes arrs'
removeReplicateRedomap _ _ = cannotSimplify

removeIotaInput :: Attributes lore =>
                   ST.SymbolTable lore
                -> AST.Lambda lore -> [VName] -> Maybe (AST.Lambda lore, [VName])
removeIotaInput vtable fun arrs
  | not $ null iotaParams =
    let substs = HM.fromList $ zip iotaParams $ repeat $ lambdaIndex fun
        (arr_params', arrs') = unzip params_and_arrs
        fun' = substituteNames substs fun { lambdaParams = acc_params <> arr_params' }
    in Just (fun', arrs')
  | otherwise = Nothing
  where params = lambdaParams fun
        (acc_params, arr_params) =
          splitAt (length params - length arrs) params
        (params_and_arrs, iotaParams) =
          partitionEithers $ zipWith isIota arr_params arrs

        isIota p v
          | Just (Iota _) <- asPrimOp =<< ST.lookupExp v vtable =
              Right $ paramName p
          | otherwise =
              Left (p, v)

removeReplicateInput :: Attributes lore =>
                        ST.SymbolTable lore
                        -> AST.Lambda lore -> [VName]
                     -> Maybe ([([VName], AST.Exp lore)],
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
          partitionEithers $ zipWith isReplicate arr_params arrs

        isReplicate p v
          | Just (Replicate _ e) <-
            asPrimOp =<< ST.lookupExp v vtable =
              Right ([paramName p], PrimOp $ SubExp e)
          | otherwise =
              Left (p, v)

-- | Remove inputs that are not used inside the @map@.
removeUnusedMapInput :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
removeUnusedMapInput _ (Let pat _ (Op (Map cs width fun arrs)))
  | (used,unused) <- partition usedInput params_and_arrs,
    not (null unused) = do
      let (used_params, used_arrs) = unzip used
          fun' = fun { lambdaParams = used_params }
      letBind_ pat $ Op $ Map cs width fun' used_arrs
  where params_and_arrs = zip (lambdaParams fun) arrs
        used_in_body = freeInBody $ lambdaBody fun
        usedInput (param, _) = paramName param `HS.member` used_in_body
removeUnusedMapInput _ _ = cannotSimplify

removeDeadMapping :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => BottomUpRule m
removeDeadMapping (_, used) (Let pat _ (Op (Map cs width fun arrs))) =
  let ses = bodyResult $ lambdaBody fun
      isUsed (bindee, _, _) = (`UT.used` used) $ patElemName bindee
      (pat',ses', ts') = unzip3 $ filter isUsed $
                         zip3 (patternElements pat) ses $ lambdaReturnType fun
      fun' = fun { lambdaBody = (lambdaBody fun) { bodyResult = ses' }
                 , lambdaReturnType = ts'
                 }
  in if pat /= Pattern [] pat'
     then letBind_ (Pattern [] pat') $ Op $ Map cs width fun' arrs
     else cannotSimplify
removeDeadMapping _ _ = cannotSimplify

simplifyClosedFormRedomap :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
simplifyClosedFormRedomap vtable (Let pat _ (Op (Redomap _ _ _ _ innerfun acc arr))) =
  foldClosedForm (`ST.lookupExp` vtable) pat innerfun acc arr
simplifyClosedFormRedomap _ _ = cannotSimplify

simplifyClosedFormReduce :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m)) => TopDownRule m
simplifyClosedFormReduce vtable (Let pat _ (Op (Reduce _ _ _ fun args))) =
  foldClosedForm (`ST.lookupExp` vtable) pat fun acc arr
  where (acc, arr) = unzip args
simplifyClosedFormReduce _ _ = cannotSimplify

-- The simplifyStream stuff is something that Cosmin left lodged in
-- the simplification engine itself at some point.  I moved it here
-- and turned it into a rule, but I don't really understand what's
-- going on.

simplifyStream :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m),
                   LocalScope (Lore m) m) => TopDownRule m
simplifyStream vtable (Let pat _ lss@(Op (Stream cs outerdim form lam arr ii))) = do
  lss' <- frobStream vtable cs outerdim form lam arr ii
  rtp <- expExtType lss
  rtp' <- expExtType lss'
  if rtp == rtp' then cannotSimplify
    else do
    let patels      = patternElements pat
        argpattps   = map patElemType $ drop (length patels - length rtp) patels
    (newpats,newsubexps) <- unzip <$> reverse <$>
                            foldM gatherPat [] (zip3 rtp rtp' argpattps)
    let newexps' = map (PrimOp . SubExp) newsubexps
        rmvdpatels = concatMap patternElements newpats
        patels' = concatMap (\p-> if p `elem` rmvdpatels then [] else [p]) patels
        (ctx,vals) = splitAt (length patels' - length rtp') patels'
        pat' = Pattern ctx vals
        newpatexps' = zip newpats newexps' ++ [(pat',lss')]
        newpats' = newpats ++ [pat']
        (_,newexps'') = unzip newpatexps'
        newpatexps''= zip newpats' newexps''
    forM_ newpatexps'' $ \(p,e) -> addBinding =<< mkLetM p e
      where gatherPat acc (_, Basic _, _) = return acc
            gatherPat acc (_, Mem {}, _) = return acc
            gatherPat acc (Array _ shp _, Array _ shp' _, Array _ pshp _) =
              foldM gatherShape acc (zip3 (extShapeDims shp) (extShapeDims shp') (shapeDims pshp))
            gatherPat _ _ =
              fail $ "In simplifyBinding \"let pat = stream()\": "++
                     " reached unreachable case!"
            gatherShape acc (Ext i, Free se', Var pid) = do
              let patind  = elemIndex pid $
                            map patElemName $ patternElements pat
              case patind of
                Just k -> return $ (Pattern [] [patternElements pat !! k], se') : acc
                Nothing-> fail $ "In simplifyBinding \"let pat = stream()\": pat "++
                                 "element of known dim not found: "++pretty pid++" "++show i++" "++pretty se'++"."
            gatherShape _ (Free se, Ext i', _) =
              fail $ "In simplifyBinding \"let pat = stream()\": "++
                     " previous known dimension: " ++ pretty se ++
                     " becomes existential: ?" ++ show i' ++ "!"
            gatherShape acc _ = return acc
simplifyStream _ _ = cannotSimplify

frobStream :: (MonadBinder m, Op (Lore m) ~ SOAC (Lore m),
               LocalScope (Lore m) m) =>
              ST.SymbolTable (Lore m)
           -> Certificates -> SubExp -> StreamForm (Lore m)
           -> AST.ExtLambda (Lore m) -> [VName] -> ChunkIntent
           -> m (AST.Exp (Lore m))
frobStream vtab cs outerdim form lam arr ii = do
  lam' <- frobExtLambda vtab lam
  return $ Op $ Stream cs outerdim form lam' arr ii

frobExtLambda :: (MonadBinder m, LocalScope (Lore m) m) =>
                 ST.SymbolTable (Lore m)
              -> AST.ExtLambda (Lore m)
              -> m (AST.ExtLambda (Lore m))
frobExtLambda vtable (ExtLambda index params body rettype) = do
  let bodyres = bodyResult body
      bodyenv = scopeOf $ bodyBindings body
      vtable' = foldr ST.insertLParam vtable params
  rettype' <- zipWithM (refineArrType vtable' bodyenv params) bodyres rettype
  return $ ExtLambda index params body rettype'
    where refineArrType :: (MonadBinder m, LocalScope (Lore m) m) =>
                           ST.SymbolTable (Lore m)
                        -> Scope (Lore m)
                        -> [AST.LParam (Lore m)] -> SubExp -> ExtType
                        -> m ExtType
          refineArrType vtable' bodyenv pars x (Array btp shp u) = do
            let vtab = ST.bindings vtable'
            dsx <- localScope bodyenv $
                   shapeDims <$> arrayShape <$> subExpType x
            let parnms = map paramName pars
                dsrtpx = extShapeDims shp
                (resdims,_) =
                    foldl (\ (lst,i) el ->
                            case el of
                              (Free (Constant c), _) -> (lst++[Free (Constant c)], i)
                              ( _,      Constant c ) -> (lst++[Free (Constant c)], i)
                              (Free (Var tid), Var pid) ->
                                if not (HM.member tid vtab) &&
                                        HM.member pid vtab
                                then (lst++[Free (Var pid)], i)
                                else (lst++[Free (Var tid)], i)
                              (Ext _, Var pid) ->
                                if HM.member pid vtab ||
                                   pid `elem` parnms
                                then (lst ++ [Free (Var pid)], i)
                                else (lst ++ [Ext i],        i+1)
                          ) ([],0) (zip dsrtpx dsx)
            return $ Array btp (ExtShape resdims) u
          refineArrType _ _ _ _ tp = return tp
