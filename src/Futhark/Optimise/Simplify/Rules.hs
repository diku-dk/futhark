{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a collection of simplification rules, as per
-- "Futhark.Optimise.Simplify.Rule".  They are used in the
-- simplifier.
--
-- For performance reasons, many sufficiently simple logically
-- separate rules are merged into single "super-rules", like ruleIf
-- and ruleBasicOp.  This is because it is relatively expensive to
-- activate a rule just to determine that it does not apply.  Thus, it
-- is more efficient to have a few very fat rules than a lot of small
-- rules.  This does not affect the compiler result in any way; it is
-- purely an optimisation to speed up compilation.
module Futhark.Optimise.Simplify.Rules
  ( standardRules,
    removeUnnecessaryCopy,
  )
where

import Control.Monad
import Data.Either
import Data.List (find, unzip4, zip4)
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.BasicOp
import Futhark.Optimise.Simplify.Rules.Index
import Futhark.Optimise.Simplify.Rules.Loop
import Futhark.Util

topDownRules :: BuilderOps rep => [TopDownRule rep]
topDownRules =
  [ RuleGeneric constantFoldPrimFun,
    RuleIf ruleIf,
    RuleIf hoistBranchInvariant,
    RuleGeneric withAccTopDown
  ]

bottomUpRules :: BuilderOps rep => [BottomUpRule rep]
bottomUpRules =
  [ RuleIf removeDeadBranchResult,
    RuleGeneric withAccBottomUp,
    RuleBasicOp simplifyIndex
  ]

-- | A set of standard simplification rules.  These assume pure
-- functional semantics, and so probably should not be applied after
-- memory block merging.
standardRules :: (BuilderOps rep, Aliased rep) => RuleBook rep
standardRules = ruleBook topDownRules bottomUpRules <> loopRules <> basicOpRules

-- | Turn @copy(x)@ into @x@ iff @x@ is not used after this copy
-- statement and it can be consumed.
--
-- This simplistic rule is only valid before we introduce memory.
removeUnnecessaryCopy :: (BuilderOps rep, Aliased rep) => BottomUpRuleBasicOp rep
removeUnnecessaryCopy (vtable, used) (Pattern [] [d]) _ (Copy v)
  | not (v `UT.isConsumed` used),
    (not (v `UT.used` used) && consumable) || not (patElemName d `UT.isConsumed` used) =
    Simplify $ letBindNames [patElemName d] $ BasicOp $ SubExp $ Var v
  where
    -- We need to make sure we can even consume the original.  The big
    -- missing piece here is that we cannot do copy removal inside of
    -- 'map' and other SOACs, but that is handled by SOAC-specific rules.
    consumable = fromMaybe False $ do
      e <- ST.lookup v vtable
      guard $ ST.entryDepth e == ST.loopDepth vtable
      consumableStm e `mplus` consumableFParam e
    consumableFParam =
      Just . maybe False (unique . declTypeOf) . ST.entryFParam
    consumableStm e = do
      pat <- stmPattern <$> ST.entryStm e
      pe <- find ((== v) . patElemName) (patternElements pat)
      guard $ aliasesOf pe == mempty
      pure True
removeUnnecessaryCopy _ _ _ _ = Skip

constantFoldPrimFun :: BuilderOps rep => TopDownRuleGeneric rep
constantFoldPrimFun _ (Let pat (StmAux cs attrs _) (Apply fname args _ _))
  | Just args' <- mapM (isConst . fst) args,
    Just (_, _, fun) <- M.lookup (nameToString fname) primFuns,
    Just result <- fun args' =
    Simplify $
      certifying cs $
        attributing attrs $
          letBind pat $ BasicOp $ SubExp $ Constant result
  where
    isConst (Constant v) = Just v
    isConst _ = Nothing
constantFoldPrimFun _ _ = Skip

simplifyIndex :: BuilderOps rep => BottomUpRuleBasicOp rep
simplifyIndex (vtable, used) pat@(Pattern [] [pe]) (StmAux cs attrs _) (Index idd inds)
  | Just m <- simplifyIndexing vtable seType idd inds consumed = Simplify $ do
    res <- m
    attributing attrs $ case res of
      SubExpResult cs' se ->
        certifying (cs <> cs') $
          letBindNames (patternNames pat) $ BasicOp $ SubExp se
      IndexResult extra_cs idd' inds' ->
        certifying (cs <> extra_cs) $
          letBindNames (patternNames pat) $ BasicOp $ Index idd' inds'
  where
    consumed = patElemName pe `UT.isConsumed` used
    seType (Var v) = ST.lookupType v vtable
    seType (Constant v) = Just $ Prim $ primValueType v
simplifyIndex _ _ _ _ = Skip

ruleIf :: BuilderOps rep => TopDownRuleIf rep
ruleIf _ pat _ (e1, tb, fb, IfDec _ ifsort)
  | Just branch <- checkBranch,
    ifsort /= IfFallback || isCt1 e1 = Simplify $ do
    let ses = bodyResult branch
    addStms $ bodyStms branch
    sequence_
      [ letBindNames [patElemName p] $ BasicOp $ SubExp se
        | (p, se) <- zip (patternElements pat) ses
      ]
  where
    checkBranch
      | isCt1 e1 = Just tb
      | isCt0 e1 = Just fb
      | otherwise = Nothing

-- IMPROVE: the following two rules can be generalised to work in more
-- cases, especially when the branches have bindings, or return more
-- than one value.
--
-- if c then True else v == c || v
ruleIf
  _
  pat
  _
  ( cond,
    Body _ tstms [Constant (BoolValue True)],
    Body _ fstms [se],
    IfDec ts _
    )
    | null tstms,
      null fstms,
      [Prim Bool] <- map extTypeOf ts =
      Simplify $ letBind pat $ BasicOp $ BinOp LogOr cond se
-- When type(x)==bool, if c then x else y == (c && x) || (!c && y)
ruleIf _ pat _ (cond, tb, fb, IfDec ts _)
  | Body _ tstms [tres] <- tb,
    Body _ fstms [fres] <- fb,
    all (safeExp . stmExp) $ tstms <> fstms,
    all ((== Prim Bool) . extTypeOf) ts = Simplify $ do
    addStms tstms
    addStms fstms
    e <-
      eBinOp
        LogOr
        (pure $ BasicOp $ BinOp LogAnd cond tres)
        ( eBinOp
            LogAnd
            (pure $ BasicOp $ UnOp Not cond)
            (pure $ BasicOp $ SubExp fres)
        )
    letBind pat e
ruleIf _ pat _ (_, tbranch, _, IfDec _ IfFallback)
  | null $ patternContextNames pat,
    all (safeExp . stmExp) $ bodyStms tbranch = Simplify $ do
    let ses = bodyResult tbranch
    addStms $ bodyStms tbranch
    sequence_
      [ letBindNames [patElemName p] $ BasicOp $ SubExp se
        | (p, se) <- zip (patternElements pat) ses
      ]
ruleIf _ pat _ (cond, tb, fb, _)
  | Body _ _ [Constant (IntValue t)] <- tb,
    Body _ _ [Constant (IntValue f)] <- fb =
    if oneIshInt t && zeroIshInt f
      then
        Simplify $
          letBind pat $ BasicOp $ ConvOp (BToI (intValueType t)) cond
      else
        if zeroIshInt t && oneIshInt f
          then Simplify $ do
            cond_neg <- letSubExp "cond_neg" $ BasicOp $ UnOp Not cond
            letBind pat $ BasicOp $ ConvOp (BToI (intValueType t)) cond_neg
          else Skip
ruleIf _ _ _ _ = Skip

-- | Move out results of a conditional expression whose computation is
-- either invariant to the branches (only done for results in the
-- context), or the same in both branches.
hoistBranchInvariant :: BuilderOps rep => TopDownRuleIf rep
hoistBranchInvariant _ pat _ (cond, tb, fb, IfDec ret ifsort) = Simplify $ do
  let tses = bodyResult tb
      fses = bodyResult fb
  (hoistings, (pes, ts, res)) <-
    fmap (fmap unzip3 . partitionEithers) $
      mapM branchInvariant $
        zip3
          (patternElements pat)
          (map Left [0 .. num_ctx -1] ++ map Right ret)
          (zip tses fses)
  let ctx_fixes = catMaybes hoistings
      (tses', fses') = unzip res
      tb' = tb {bodyResult = tses'}
      fb' = fb {bodyResult = fses'}
      ret' = foldr (uncurry fixExt) (rights ts) ctx_fixes
      (ctx_pes, val_pes) = splitFromEnd (length ret') pes
  if not $ null hoistings -- Was something hoisted?
    then do
      -- We may have to add some reshapes if we made the type
      -- less existential.
      tb'' <- reshapeBodyResults tb' $ map extTypeOf ret'
      fb'' <- reshapeBodyResults fb' $ map extTypeOf ret'
      letBind (Pattern ctx_pes val_pes) $
        If cond tb'' fb'' (IfDec ret' ifsort)
    else cannotSimplify
  where
    num_ctx = length $ patternContextElements pat
    bound_in_branches =
      namesFromList $
        concatMap (patternNames . stmPattern) $
          bodyStms tb <> bodyStms fb
    mem_sizes = freeIn $ filter (isMem . patElemType) $ patternElements pat
    invariant Constant {} = True
    invariant (Var v) = not $ v `nameIn` bound_in_branches

    isMem Mem {} = True
    isMem _ = False
    sizeOfMem v = v `nameIn` mem_sizes

    branchInvariant (pe, t, (tse, fse))
      -- Do both branches return the same value?
      | tse == fse = do
        letBindNames [patElemName pe] $ BasicOp $ SubExp tse
        hoisted pe t

      -- Do both branches return values that are free in the
      -- branch, and are we not the only pattern element?  The
      -- latter is to avoid infinite application of this rule.
      | invariant tse,
        invariant fse,
        patternSize pat > 1,
        Prim _ <- patElemType pe,
        not $ sizeOfMem $ patElemName pe = do
        bt <- expTypesFromPattern $ Pattern [] [pe]
        letBindNames [patElemName pe]
          =<< ( If cond <$> resultBodyM [tse]
                  <*> resultBodyM [fse]
                  <*> pure (IfDec bt ifsort)
              )
        hoisted pe t
      | otherwise =
        return $ Right (pe, t, (tse, fse))

    hoisted pe (Left i) = return $ Left $ Just (i, Var $ patElemName pe)
    hoisted _ Right {} = return $ Left Nothing

    reshapeBodyResults body rets = buildBody_ $ do
      ses <- bodyBind body
      let (ctx_ses, val_ses) = splitFromEnd (length rets) ses
      (ctx_ses ++) <$> zipWithM reshapeResult val_ses rets
    reshapeResult (Var v) t@Array {} = do
      v_t <- lookupType v
      let newshape = arrayDims $ removeExistentials t v_t
      if newshape /= arrayDims v_t
        then letSubExp "branch_ctx_reshaped" $ shapeCoerce newshape v
        else return $ Var v
    reshapeResult se _ =
      return se

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: BuilderOps rep => BottomUpRuleIf rep
removeDeadBranchResult (_, used) pat _ (e1, tb, fb, IfDec rettype ifsort)
  | -- Only if there is no existential context...
    patternSize pat == length rettype,
    -- Figure out which of the names in 'pat' are used...
    patused <- map (`UT.isUsedDirectly` used) $ patternNames pat,
    -- If they are not all used, then this rule applies.
    not (and patused) =
    -- Remove the parts of the branch-results that correspond to dead
    -- return value bindings.  Note that this leaves dead code in the
    -- branch bodies, but that will be removed later.
    let tses = bodyResult tb
        fses = bodyResult fb
        pick :: [a] -> [a]
        pick = map snd . filter fst . zip patused
        tb' = tb {bodyResult = pick tses}
        fb' = fb {bodyResult = pick fses}
        pat' = pick $ patternElements pat
        rettype' = pick rettype
     in Simplify $ letBind (Pattern [] pat') $ If e1 tb' fb' $ IfDec rettype' ifsort
  | otherwise = Skip

withAccTopDown :: BuilderOps rep => TopDownRuleGeneric rep
-- A WithAcc with no accumulators is sent to Valhalla.
withAccTopDown _ (Let pat aux (WithAcc [] lam)) = Simplify . auxing aux $ do
  lam_res <- bodyBind $ lambdaBody lam
  forM_ (zip (patternNames pat) lam_res) $ \(v, se) ->
    letBindNames [v] $ BasicOp $ SubExp se
-- Identify those results in 'lam' that are free and move them out.
withAccTopDown vtable (Let pat aux (WithAcc inputs lam)) = Simplify . auxing aux $ do
  let (cert_params, acc_params) =
        splitAt (length inputs) $ lambdaParams lam
      (acc_res, nonacc_res) =
        splitFromEnd num_nonaccs $ bodyResult $ lambdaBody lam
      (acc_pes, nonacc_pes) =
        splitFromEnd num_nonaccs $ patternElements pat

  -- Look at accumulator results.
  (acc_pes', inputs', params', acc_res') <-
    fmap (unzip4 . catMaybes) . mapM tryMoveAcc $
      zip4
        (chunks (map inputArrs inputs) acc_pes)
        inputs
        (zip cert_params acc_params)
        acc_res
  let (cert_params', acc_params') = unzip params'

  -- Look at non-accumulator results.
  (nonacc_pes', nonacc_res') <-
    unzip . catMaybes <$> mapM tryMoveNonAcc (zip nonacc_pes nonacc_res)

  when (concat acc_pes' == acc_pes && nonacc_pes' == nonacc_pes) cannotSimplify

  lam' <-
    mkLambda (cert_params' ++ acc_params') $
      bodyBind $ (lambdaBody lam) {bodyResult = acc_res' <> nonacc_res'}

  letBind (Pattern [] (concat acc_pes' <> nonacc_pes')) $ WithAcc inputs' lam'
  where
    num_nonaccs = length (lambdaReturnType lam) - length inputs
    inputArrs (_, arrs, _) = length arrs

    tryMoveAcc (pes, (_, arrs, _), (_, acc_p), Var v)
      | paramName acc_p == v = do
        forM_ (zip pes arrs) $ \(pe, arr) ->
          letBindNames [patElemName pe] $ BasicOp $ SubExp $ Var arr
        pure Nothing
    tryMoveAcc x =
      pure $ Just x

    tryMoveNonAcc (pe, Var v)
      | v `ST.elem` vtable = do
        letBindNames [patElemName pe] $ BasicOp $ SubExp $ Var v
        pure Nothing
    tryMoveNonAcc (pe, Constant v) = do
      letBindNames [patElemName pe] $ BasicOp $ SubExp $ Constant v
      pure Nothing
    tryMoveNonAcc x =
      pure $ Just x
withAccTopDown _ _ = Skip

withAccBottomUp :: BuilderOps rep => BottomUpRuleGeneric rep
-- Eliminate dead results.
withAccBottomUp (_, utable) (Let pat aux (WithAcc inputs lam))
  | not $ all (`UT.used` utable) $ patternNames pat = Simplify $ do
    let (acc_res, nonacc_res) =
          splitFromEnd num_nonaccs $ bodyResult $ lambdaBody lam
        (acc_pes, nonacc_pes) =
          splitFromEnd num_nonaccs $ patternElements pat
        (cert_params, acc_params) =
          splitAt (length inputs) $ lambdaParams lam

    -- Eliminate unused accumulator results
    let (acc_pes', inputs', param_pairs, acc_res') =
          unzip4 . filter keepAccRes $
            zip4
              (chunks (map inputArrs inputs) acc_pes)
              inputs
              (zip cert_params acc_params)
              acc_res
        (cert_params', acc_params') = unzip param_pairs

    -- Eliminate unused non-accumulator results
    let (nonacc_pes', nonacc_res') =
          unzip $ filter keepNonAccRes $ zip nonacc_pes nonacc_res

    when (concat acc_pes' == acc_pes && nonacc_pes' == nonacc_pes) cannotSimplify

    let pes' = concat acc_pes' ++ nonacc_pes'

    lam' <- mkLambda (cert_params' ++ acc_params') $ do
      void $ bodyBind $ lambdaBody lam
      pure $ acc_res' ++ nonacc_res'

    auxing aux $ letBind (Pattern [] pes') $ WithAcc inputs' lam'
  where
    num_nonaccs = length (lambdaReturnType lam) - length inputs
    inputArrs (_, arrs, _) = length arrs
    keepAccRes (pes, _, _, _) = any ((`UT.used` utable) . patElemName) pes
    keepNonAccRes (pe, _) = patElemName pe `UT.used` utable
withAccBottomUp _ _ = Skip

-- Some helper functions

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False
