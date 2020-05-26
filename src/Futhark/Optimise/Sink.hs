{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | "Sinking" is conceptually the opposite of hoisting.  The idea is
-- to take code that looks like this:
--
-- @
-- x = xs[i]
-- y = ys[i]
-- if x != 0 then {
--   y
-- } else {
--   0
-- }
-- @
--
-- and turn it into
--
-- @
-- x = xs[i]
-- if x != 0 then {
--   y = ys[i]
--   y
-- } else {
--   0
-- }
-- @
--
-- The idea is to delay loads from memory until (if) they are actually
-- needed.  Code patterns like the above is particularly common in
-- code that makes use of pattern matching on sum types.
--
-- We are currently quite conservative about when we do this.  In
-- particular, if any consumption is going on in a body, we don't do
-- anything.  This is far too conservative.  Also, we are careful
-- never to duplicate work.
--
-- This pass redundantly computes free-variable information a lot.  If
-- you ever see this pass as being a compilation speed bottleneck,
-- start by caching that a bit.
--
-- This pass is defined on the Kernels representation.  This is not
-- because we do anything kernel-specific here, but simply because
-- more explicit indexing is going on after SOACs are gone.

module Futhark.Optimise.Sink (sinkKernels, sinkMC) where

import Control.Monad.State
import Data.Bifunctor
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.Range as Range
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.IR.Aliases
import Futhark.IR.Ranges
import Futhark.IR.Kernels
import Futhark.IR.MC

import Futhark.Pass

-- We do not care about ranges, but in order to use ST.SymbolTable
-- (which is a convenient way to handle aliases), we need range information.
type SymbolTable lore = ST.SymbolTable lore
type Sinking lore = M.Map VName (Stm lore)
type Sunk = S.Set VName
type Sinker lore a = SymbolTable lore -> Sinking lore -> a -> (a, Sunk)
type Constraints lore = (ASTLore lore,
                         Aliased lore,
                         ST.IndexOp (Op lore),
                         RangesOf (BodyDec lore),
                         RangeOf (LetDec lore),
                         RangedOp (Op lore))

-- | Given a statement, compute how often each of its free variables
-- are used.  Not accurate: what we care about are only 1, and greater
-- than 1.
multiplicity :: Constraints lore => Stm lore -> M.Map VName Int
multiplicity stm =
  case stmExp stm of
    If cond tbranch fbranch _ ->
      free cond 1 <> M.unionWith (+) (free tbranch 1) (free fbranch 1)
    Op{} -> free stm 2
    DoLoop{} -> free stm 2
    _ -> free stm 1
  where free x k = M.fromList $ zip (namesToList $ freeIn x) $ repeat k

optimiseBranch :: Constraints lore =>
                  Sinker lore (Op lore)
               -> Sinker lore (Body lore)
optimiseBranch onOp vtable sinking (Body attr stms res) =
  let (stms', stms_sunk) = optimiseStms onOp vtable sinking' stms $ freeIn res
  in (Body attr (sunk_stms <> stms') res,
      sunk <> stms_sunk)
  where free_in_stms = freeIn stms <> freeIn res
        (sinking_here, sinking') = M.partitionWithKey sunkHere sinking
        sunk_stms = stmsFromList $ M.elems sinking_here
        sunkHere v stm =
          v `nameIn` free_in_stms &&
          all (`ST.available` vtable) (namesToList (freeIn stm))
        sunk = S.fromList $ concatMap (patternNames . stmPattern) sunk_stms

optimiseStms :: Constraints lore =>
                Sinker lore (Op lore)
             -> SymbolTable lore -> Sinking lore
             -> Stms lore -> Names
             -> (Stms lore, Sunk)
optimiseStms onOp init_vtable init_sinking all_stms free_in_res =
  let (all_stms', sunk) =
        optimiseStms' init_vtable init_sinking $ stmsToList all_stms
  in (stmsFromList all_stms', sunk)
  where
    multiplicities = foldl' (M.unionWith (+))
                     (M.fromList (zip (namesToList free_in_res) [1..]))
                     (map multiplicity $ stmsToList all_stms)

    optimiseStms' _ _ [] = ([], mempty)

    optimiseStms' vtable sinking (stm : stms)
      | BasicOp Index{} <- stmExp stm,
        [pe] <- patternElements (stmPattern stm),
        primType $ patElemType pe,
        maybe True (==1) $ M.lookup (patElemName pe) multiplicities =
          let (stms', sunk) =
                optimiseStms' vtable' (M.insert (patElemName pe) stm sinking) stms
          in if patElemName pe `S.member` sunk
             then (stms', sunk)
             else (stm : stms', sunk)

      | If cond tbranch fbranch ret <- stmExp stm =
          let (tbranch', tsunk) = optimiseBranch onOp vtable sinking tbranch
              (fbranch', fsunk) = optimiseBranch onOp vtable sinking fbranch
              (stms', sunk) = optimiseStms' vtable' sinking stms
          in (stm { stmExp = If cond tbranch' fbranch' ret } : stms',
              tsunk <> fsunk <> sunk)

      | Op op <- stmExp stm =
          let (op', op_sunk) = onOp vtable sinking op
              (stms', stms_sunk) = optimiseStms' vtable' sinking stms
          in (stm { stmExp = Op op' } : stms',
              stms_sunk <> op_sunk)

      | otherwise =
          let (stms', stms_sunk) = optimiseStms' vtable' sinking stms
              (e', stm_sunk) = runState (mapExpM mapper (stmExp stm)) mempty
          in (stm { stmExp = e' } : stms',
              stm_sunk <> stms_sunk)

      where vtable' = ST.insertStm stm vtable
            mapper =
              identityMapper
              { mapOnBody = \scope body -> do
                  let (body', sunk) =
                        optimiseBody onOp (ST.fromScope scope <> vtable)
                        sinking body
                  modify (<>sunk)
                  return body'
              }

optimiseBody :: Constraints lore =>
                Sinker lore (Op lore)
             -> Sinker lore (Body lore)
optimiseBody onOp vtable sinking (Body attr stms res) =
  let (stms', sunk) = optimiseStms onOp vtable sinking stms $ freeIn res
  in (Body attr stms' res, sunk)

optimiseKernelBody :: Constraints lore =>
                      Sinker lore (Op lore)
                   -> Sinker lore (KernelBody lore)
optimiseKernelBody onOp vtable sinking (KernelBody attr stms res) =
  let (stms', sunk) = optimiseStms onOp vtable sinking stms $ freeIn res
  in (KernelBody attr stms' res, sunk)

optimiseSegOp :: Constraints lore =>
                 Sinker lore (Op lore) -> Sinker lore (SegOp lvl lore)
optimiseSegOp onOp vtable sinking op =
  let scope = scopeOfSegSpace $ segSpace op
  in runState (mapSegOpM (opMapper scope) op) mempty
  where opMapper scope =
          identitySegOpMapper
          { mapOnSegOpLambda = \lam -> do
              let (body, sunk) =
                    optimiseBody onOp op_vtable sinking $
                    lambdaBody lam
              modify (<>sunk)
              return lam { lambdaBody = body }

          , mapOnSegOpBody = \body -> do
              let (body', sunk) =
                    optimiseKernelBody onOp op_vtable sinking body
              modify (<>sunk)
              return body'
          }
          where op_vtable = ST.fromScope scope <> vtable

type SinkLore lore = Ranges (Aliases lore)

sink :: (ASTLore lore,
         CanBeAliased (Op lore),
         CanBeRanged (OpWithAliases (Op lore)),
         AliasedOp (OpWithRanges (OpWithAliases (Op lore))),
         ST.IndexOp (OpWithRanges (OpWithAliases (Op lore)))) =>
        Sinker (SinkLore lore) (Op (SinkLore lore)) -> Pass lore lore
sink onOp =
  Pass "sink" "move memory loads closer to their uses" $
  fmap (removeProgAliases . removeProgRanges) .
  intraproceduralTransformationWithConsts onConsts onFun .
  Range.rangeAnalysis . Alias.aliasAnalysis
  where onFun _ fd = do
          let vtable = ST.insertFParams (funDefParams fd) mempty
              (body, _) = optimiseBody onOp vtable mempty $ funDefBody fd
          return fd { funDefBody = body }

        onConsts consts =
          pure $ fst $ optimiseStms onOp mempty mempty consts $
          namesFromList $ M.keys $ scopeOf consts

-- | Sinking in GPU kernels.
sinkKernels :: Pass Kernels Kernels
sinkKernels = sink onHostOp
  where onHostOp :: Sinker (SinkLore Kernels) (Op (SinkLore Kernels))
        onHostOp vtable sinking (SegOp op) =
          first SegOp $ optimiseSegOp onHostOp vtable sinking op
        onHostOp _ _ op = (op, mempty)

-- | Sinking for multicore.
sinkMC :: Pass MC MC
sinkMC = sink onSegOp
  where onSegOp :: Sinker (SinkLore MC) (Op (SinkLore MC))
        onSegOp = optimiseSegOp onSegOp
