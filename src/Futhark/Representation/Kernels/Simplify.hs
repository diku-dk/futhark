{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyLambda

       -- * Building blocks
       , simplifyKernelOp
       )
where

import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Futhark.Representation.Kernels
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rules
import Futhark.Optimise.Simplify.Lore
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.SOACS.Simplify (simplifySOAC)
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Util (chunks)
import qualified Futhark.Transform.FirstOrderTransform as FOT

simpleKernels :: Simplify.SimpleOps Kernels
simpleKernels = Simplify.bindableSimpleOps $ simplifyKernelOp simplifySOAC

simplifyKernels :: Prog Kernels -> PassM (Prog Kernels)
simplifyKernels =
  Simplify.simplifyProg simpleKernels kernelRules Simplify.noExtraHoistBlockers

simplifyLambda :: (HasScope Kernels m, MonadFreshNames m) =>
                  Lambda Kernels -> [Maybe VName] -> m (Lambda Kernels)
simplifyLambda =
  Simplify.simplifyLambda simpleKernels kernelRules Engine.noExtraHoistBlockers

simplifyKernelOp :: (Engine.SimplifiableLore lore,
                     BodyAttr lore ~ ()) =>
                    Simplify.SimplifyOp lore op
                 -> HostOp lore op
                 -> Engine.SimpleM lore (HostOp (Wise lore) (OpWithWisdom op), Stms (Wise lore))

simplifyKernelOp f (OtherOp op) = do
  (op', stms) <- f op
  return (OtherOp op', stms)

simplifyKernelOp _ (SegOp (SegMap lvl space ts kbody)) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (kbody', body_hoisted) <- simplifyKernelBody space kbody
  return (SegOp $ SegMap lvl' space' ts' kbody',
          body_hoisted)

simplifyKernelOp _ (SegOp (SegRed lvl space reds ts kbody)) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)
  (reds', reds_hoisted) <- fmap unzip $ forM reds $ \(SegRedOp comm lam nes shape) -> do
    (lam', hoisted) <-
      Engine.localVtable (<>scope_vtable) $
      Engine.simplifyLambda lam $ replicate (length nes * 2) Nothing
    shape' <- Engine.simplify shape
    nes' <- mapM Engine.simplify nes
    return (SegRedOp comm lam' nes' shape', hoisted)

  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return (SegOp $ SegRed lvl' space' reds' ts' kbody',
          mconcat reds_hoisted <> body_hoisted)
  where scope = scopeOfSegSpace space
        scope_vtable = ST.fromScope scope

simplifyKernelOp _ (SegOp (SegScan lvl space scan_op nes ts kbody)) = do
  lvl' <- Engine.simplify lvl
  (space', scan_op', nes', ts', kbody', hoisted) <-
    simplifyRedOrScan space scan_op nes ts kbody

  return (SegOp $ SegScan lvl' space' scan_op' nes' ts' kbody',
          hoisted)

simplifyKernelOp _ (SegOp (SegHist lvl space ops ts kbody)) = do
  (lvl', space', ts') <- Engine.simplify (lvl, space, ts)

  (ops', ops_hoisted) <- fmap unzip $ forM ops $
    \(HistOp w rf arrs nes dims lam) -> do
      w' <- Engine.simplify w
      rf' <- Engine.simplify rf
      arrs' <- Engine.simplify arrs
      nes' <- Engine.simplify nes
      dims' <- Engine.simplify dims
      (lam', op_hoisted) <-
        Engine.localVtable (<>scope_vtable) $
        Engine.simplifyLambda lam $
        replicate (length nes * 2) Nothing
      return (HistOp w' rf' arrs' nes' dims' lam',
              op_hoisted)

  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return (SegOp $ SegHist lvl' space' ops' ts' kbody',
          mconcat ops_hoisted <> body_hoisted)

  where scope = scopeOfSegSpace space
        scope_vtable = ST.fromScope scope

simplifyKernelOp _ (SizeOp (SplitSpace o w i elems_per_thread)) =
  (,) <$> (SizeOp <$>
           (SplitSpace <$> Engine.simplify o <*> Engine.simplify w
            <*> Engine.simplify i <*> Engine.simplify elems_per_thread))
      <*> pure mempty
simplifyKernelOp _ (SizeOp (GetSize key size_class)) =
  return (SizeOp $ GetSize key size_class, mempty)
simplifyKernelOp _ (SizeOp (GetSizeMax size_class)) =
  return (SizeOp $ GetSizeMax size_class, mempty)
simplifyKernelOp _ (SizeOp (CmpSizeLe key size_class x)) = do
  x' <- Engine.simplify x
  return (SizeOp $ CmpSizeLe key size_class x', mempty)
simplifyKernelOp _ (SizeOp (CalcNumGroups w max_num_groups group_size)) = do
  w' <- Engine.simplify w
  return (SizeOp $ CalcNumGroups w' max_num_groups group_size, mempty)

simplifyRedOrScan :: (Engine.SimplifiableLore lore, BodyAttr lore ~ ()) =>
                     SegSpace
                  -> Lambda lore -> [SubExp] -> [Type]
                  -> KernelBody lore
                  -> Simplify.SimpleM lore
                  (SegSpace, Lambda (Wise lore), [SubExp], [Type], KernelBody (Wise lore),
                   Stms (Wise lore))
simplifyRedOrScan space scan_op nes ts kbody = do
  space' <- Engine.simplify space
  nes' <- mapM Engine.simplify nes
  ts' <- mapM Engine.simplify ts

  (scan_op', scan_op_hoisted) <-
    Engine.localVtable (<>scope_vtable) $
    Engine.simplifyLambda scan_op $ replicate (length nes * 2) Nothing

  (kbody', body_hoisted) <- simplifyKernelBody space kbody

  return (space', scan_op', nes', ts', kbody',
          scan_op_hoisted <> body_hoisted)

  where scope = scopeOfSegSpace space
        scope_vtable = ST.fromScope scope

simplifyKernelBody :: (Engine.SimplifiableLore lore, BodyAttr lore ~ ()) =>
                      SegSpace -> KernelBody lore
                   -> Engine.SimpleM lore (KernelBody (Wise lore), Stms (Wise lore))
simplifyKernelBody space (KernelBody _ stms res) = do
  par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers

  ((body_stms, body_res), hoisted) <-
    Engine.localVtable (<>scope_vtable) $
    Engine.localVtable (\vtable -> vtable { ST.simplifyMemory = True }) $
    Engine.blockIf (Engine.hasFree bound_here
                    `Engine.orIf` Engine.isOp
                    `Engine.orIf` par_blocker
                    `Engine.orIf` Engine.isConsumed) $
    Engine.simplifyStms stms $ do
    res' <- Engine.localVtable (ST.hideCertified $ namesFromList $ M.keys $ scopeOf stms) $
            mapM Engine.simplify res
    return ((res', UT.usages $ freeIn res'), mempty)

  return (mkWiseKernelBody () body_stms body_res,
          hoisted)

  where scope_vtable = ST.fromScope $ scopeOfSegSpace space
        bound_here = namesFromList $ M.keys $ scopeOfSegSpace space

mkWiseKernelBody :: (Attributes lore, CanBeWise (Op lore)) =>
                    BodyAttr lore -> Stms (Wise lore) -> [KernelResult] -> KernelBody (Wise lore)
mkWiseKernelBody attr bnds res =
  let Body attr' _ _ = mkWiseBody attr bnds res_vs
  in KernelBody attr' bnds res
  where res_vs = map kernelResultSubExp res

instance Engine.Simplifiable SplitOrdering where
  simplify SplitContiguous =
    return SplitContiguous
  simplify (SplitStrided stride) =
    SplitStrided <$> Engine.simplify stride

instance Engine.Simplifiable SegLevel where
  simplify (SegThread num_groups group_size virt) =
    SegThread <$> traverse Engine.simplify num_groups <*>
    traverse Engine.simplify group_size <*> pure virt
  simplify (SegGroup num_groups group_size virt) =
    SegGroup <$> traverse Engine.simplify num_groups <*>
    traverse Engine.simplify group_size <*> pure virt
  simplify (SegThreadScalar num_groups group_size virt) =
    SegThreadScalar <$> traverse Engine.simplify num_groups <*>
    traverse Engine.simplify group_size <*> pure virt

instance Engine.Simplifiable SegSpace where
  simplify (SegSpace phys dims) =
    SegSpace phys <$> mapM (traverse Engine.simplify) dims

instance Engine.Simplifiable KernelResult where
  simplify (Returns what) =
    Returns <$> Engine.simplify what
  simplify (WriteReturns ws a res) =
    WriteReturns <$> Engine.simplify ws <*> Engine.simplify a <*> Engine.simplify res
  simplify (ConcatReturns o w pte what) =
    ConcatReturns
    <$> Engine.simplify o
    <*> Engine.simplify w
    <*> Engine.simplify pte
    <*> Engine.simplify what
  simplify (TileReturns dims what) =
    TileReturns <$> Engine.simplify dims <*> Engine.simplify what

instance BinderOps (Wise Kernels) where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

kernelRules :: RuleBook (Wise Kernels)
kernelRules = standardRules <>
              ruleBook [ RuleOp removeInvariantKernelResults
                       , RuleOp mergeSegRedOps
                       , RuleOp redomapIotaToLoop ]
                       [ RuleOp distributeKernelResults
                       , RuleBasicOp removeUnnecessaryCopy]

-- If a kernel produces something invariant to the kernel, turn it
-- into a replicate.
removeInvariantKernelResults :: TopDownRuleOp (Wise Kernels)
removeInvariantKernelResults vtable (Pattern [] kpes) attr
                             (SegOp (SegMap lvl space ts (KernelBody _ kstms kres))) = Simplify $ do

  case lvl of
    SegThreadScalar{} -> cannotSimplify
    _ -> return ()

  (ts', kpes', kres') <-
    unzip3 <$> filterM checkForInvarianceResult (zip3 ts kpes kres)

  -- Check if we did anything at all.
  when (kres == kres')
    cannotSimplify

  addStm $ Let (Pattern [] kpes') attr $ Op $ SegOp $ SegMap lvl space ts' $
    mkWiseKernelBody () kstms kres'
  where isInvariant Constant{} = True
        isInvariant (Var v) = isJust $ ST.lookup v vtable

        checkForInvarianceResult (_, pe, Returns se)
          | isInvariant se = do
              letBindNames_ [patElemName pe] $
                BasicOp $ Replicate (Shape $ segSpaceDims space) se
              return False
        checkForInvarianceResult _ =
          return True
removeInvariantKernelResults _ _ _ _ = Skip

-- Some kernel results can be moved outside the kernel, which can
-- simplify further analysis.
distributeKernelResults :: BottomUpRuleOp (Wise Kernels)
distributeKernelResults (vtable, used)
  (Pattern [] kpes) attr (SegOp (SegMap lvl space kts (KernelBody _ kstms kres))) = Simplify $ do
  -- Iterate through the bindings.  For each, we check whether it is
  -- in kres and can be moved outside.  If so, we remove it from kres
  -- and kpes and make it a binding outside.
  (kpes', kts', kres', kstms_rev) <- localScope (scopeOfSegSpace space) $
    foldM distribute (kpes, kts, kres, []) kstms

  when (kpes' == kpes)
    cannotSimplify

  addStm $ Let (Pattern [] kpes') attr $ Op $ SegOp $
    SegMap lvl space kts' $ mkWiseKernelBody () (stmsFromList $ reverse kstms_rev) kres'
  where
    free_in_kstms = fold $ fmap freeIn kstms

    distribute (kpes', kts', kres', kstms_rev) bnd
      | Let (Pattern [] [pe]) _ (BasicOp (Index arr slice)) <- bnd,
        space_slice <- map (DimFix . Var . fst) $ unSegSpace space,
        space_slice `isPrefixOf` slice,
        remaining_slice <- drop (length space_slice) slice,
        all (isJust . flip ST.lookup vtable) $ namesToList $
          freeIn arr <> freeIn remaining_slice,
        Just (kpe, kpes'', kts'', kres'') <- isResult kpes' kts' kres' pe = do
          let outer_slice = map (\d -> DimSlice
                                       (constant (0::Int32))
                                       d
                                       (constant (1::Int32))) $
                            segSpaceDims space
              index kpe' = letBind_ (Pattern [] [kpe']) $ BasicOp $ Index arr $
                           outer_slice <> remaining_slice
          if patElemName kpe `UT.isConsumed` used
            then do precopy <- newVName $ baseString (patElemName kpe) <> "_precopy"
                    index kpe { patElemName = precopy }
                    letBind_ (Pattern [] [kpe]) $ BasicOp $ Copy precopy
            else index kpe
          return (kpes'', kts'', kres'',
                  if patElemName pe `nameIn` free_in_kstms
                  then bnd : kstms_rev
                  else kstms_rev)

    distribute (kpes', kts', kres', kstms_rev) bnd =
      return (kpes', kts', kres', bnd : kstms_rev)

    isResult kpes' kts' kres' pe =
      case partition matches $ zip3 kpes' kts' kres' of
        ([(kpe,_,_)], kpes_and_kres)
          | (kpes'', kts'', kres'') <- unzip3 kpes_and_kres ->
              Just (kpe, kpes'', kts'', kres'')
        _ -> Nothing
      where matches (_, _, kre) = kre == Returns (Var $ patElemName pe)
distributeKernelResults _ _ _ _ = Skip

-- If a SegRed contains two reduction operations that have the same
-- vector shape, merge them together.  This saves on communication
-- overhead, but can in principle lead to more local memory usage.
mergeSegRedOps :: TopDownRuleOp (Wise Kernels)
mergeSegRedOps _ (Pattern [] pes) _ (SegOp (SegRed lvl space ops ts kbody))
  | length ops > 1,
    op_groupings <- groupBy sameShape $ zip ops $ chunks (map (length . segRedNeutral) ops) $
                    zip3 red_pes red_ts red_res,
    any ((>1) . length) op_groupings = Simplify $ do
      let (ops', aux) = unzip $ mapMaybe combineOps op_groupings
          (red_pes', red_ts', red_res') = unzip3 $ concat aux
          pes' = red_pes' ++ map_pes
          ts' = red_ts' ++ map_ts
          kbody' = kbody { kernelBodyResult = red_res' ++ map_res }
      letBind_ (Pattern [] pes') $ Op $ SegOp $ SegRed lvl space ops' ts' kbody'
  where (red_pes, map_pes) = splitAt (segRedResults ops) pes
        (red_ts, map_ts) = splitAt (segRedResults ops) ts
        (red_res, map_res) = splitAt (segRedResults ops) $ kernelBodyResult kbody

        sameShape (op1, _) (op2, _) = segRedShape op1 == segRedShape op2

        combineOps :: [(SegRedOp (Wise Kernels), [a])]
                   -> Maybe (SegRedOp (Wise Kernels), [a])
        combineOps [] = Nothing
        combineOps (x:xs) = Just $ foldl' combine x xs

        combine (op1, op1_aux) (op2, op2_aux) =
          let lam1 = segRedLambda op1
              lam2 = segRedLambda op2
              (op1_xparams, op1_yparams) =
                splitAt (length (segRedNeutral op1)) $ lambdaParams lam1
              (op2_xparams, op2_yparams) =
                splitAt (length (segRedNeutral op2)) $ lambdaParams lam2
              lam = Lambda { lambdaParams = op1_xparams ++ op2_xparams ++
                                            op1_yparams ++ op2_yparams
                           , lambdaReturnType = lambdaReturnType lam1 ++ lambdaReturnType lam2
                           , lambdaBody =
                               mkBody (bodyStms (lambdaBody lam1) <> bodyStms (lambdaBody lam2)) $
                               bodyResult (lambdaBody lam1) <> bodyResult (lambdaBody lam2)
                           }
          in (SegRedOp { segRedComm = segRedComm op1 <> segRedComm op2
                       , segRedLambda = lam
                       , segRedNeutral = segRedNeutral op1 ++ segRedNeutral op2
                       , segRedShape = segRedShape op1 -- Same as shape of op2 due to the grouping.
                       },
               op1_aux ++ op2_aux)
mergeSegRedOps _ _ _ _ = Skip

-- We turn reductions over (solely) iotas into do-loops, because there
-- is no useful structure here anyway.  This is mostly a hack to work
-- around the fact that loop tiling would otherwise pointlessly tile
-- them.
redomapIotaToLoop :: TopDownRuleOp (Wise Kernels)
redomapIotaToLoop vtable pat aux (OtherOp soac@(Screma _ form [arr]))
  | Just _ <- isRedomapSOAC form,
    Just (Iota{}, _) <- ST.lookupBasicOp arr vtable =
      Simplify $ certifying (stmAuxCerts aux) $ FOT.transformSOAC pat soac
redomapIotaToLoop _ _ _ _ =
  Skip
