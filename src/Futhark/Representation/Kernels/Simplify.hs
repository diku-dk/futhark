{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyLambda

       , Kernels

       -- * Building blocks
       , simplifyKernelOp
       )
where

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
                     BodyDec lore ~ ()) =>
                    Simplify.SimplifyOp lore op
                 -> HostOp lore op
                 -> Engine.SimpleM lore (HostOp (Wise lore) (OpWithWisdom op), Stms (Wise lore))

simplifyKernelOp f (OtherOp op) = do
  (op', stms) <- f op
  return (OtherOp op', stms)

simplifyKernelOp _ (SegOp op) = do
  (op', hoisted) <- simplifySegOp op
  return (SegOp op', hoisted)

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

instance BinderOps (Wise Kernels) where
  mkExpDecB = bindableMkExpDecB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance HasSegOp (Wise Kernels) where
  type SegOpLevel (Wise Kernels) = SegLevel
  asSegOp (SegOp op) = Just op
  asSegOp _ = Nothing
  segOp = SegOp

kernelRules :: RuleBook (Wise Kernels)
kernelRules = standardRules <> segOpRules <>
              ruleBook
              [ RuleOp redomapIotaToLoop ]
              [ RuleBasicOp removeUnnecessaryCopy ]

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
