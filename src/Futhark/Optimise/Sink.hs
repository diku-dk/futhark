{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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

module Futhark.Optimise.Sink (sink) where

import Control.Monad.State
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.Range as Range
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.MonadFreshNames
import Futhark.Representation.Aliases
import Futhark.Representation.Ranges
import Futhark.Representation.Kernels
import Futhark.Pass

-- We do not care about ranges, but in order to use ST.SymbolTable
-- (which is a convenient way to handle aliases), we need range information.
type SinkLore = Ranges (Aliases Kernels)
type SymbolTable = ST.SymbolTable SinkLore
type Sinking = M.Map VName (Stm SinkLore)
type Sunk = S.Set VName

-- | Given a statement, compute how often each of its free variables
-- are used.  Not accurate: what we care about are only 1, and greater
-- than 1.
multiplicity :: Stm SinkLore -> M.Map VName Int
multiplicity stm =
  case stmExp stm of
    If cond tbranch fbranch _ ->
      free cond 1 <> M.unionWith (+) (free tbranch 1) (free fbranch 1)
    Op{} -> free stm 2
    DoLoop{} -> free stm 2
    _ -> free stm 1
  where free x k = M.fromList $ zip (namesToList $ freeIn x) $ repeat k

optimiseBranch :: SymbolTable -> Sinking -> Body SinkLore
               -> (Body SinkLore, Sunk)
optimiseBranch vtable sinking (Body attr stms res) =
  let (stms', stms_sunk) = optimiseStms vtable sinking' stms $ freeIn res
  in (Body attr (sunk_stms <> stms') res,
      sunk <> stms_sunk)
  where free_in_stms = freeIn stms <> freeIn res
        (sinking_here, sinking') = M.partitionWithKey sunkHere sinking
        sunk_stms = stmsFromList $ M.elems sinking_here
        sunkHere v stm =
          v `nameIn` free_in_stms &&
          all (`ST.available` vtable) (namesToList (freeIn stm))
        sunk = S.fromList $ concatMap (patternNames . stmPattern) sunk_stms

optimiseStms :: SymbolTable -> Sinking -> Stms SinkLore -> Names
             -> (Stms SinkLore, Sunk)
optimiseStms init_vtable init_sinking all_stms free_in_res =
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
          let (tbranch', tsunk) = optimiseBranch vtable sinking tbranch
              (fbranch', fsunk) = optimiseBranch vtable sinking fbranch
              (stms', sunk) = optimiseStms' vtable' sinking stms
          in (stm { stmExp = If cond tbranch' fbranch' ret } : stms',
              tsunk <> fsunk <> sunk)

      | Op (SegOp op) <- stmExp stm =
          let scope = scopeOfSegSpace $ segSpace op
              (stms', stms_sunk) = optimiseStms' vtable' sinking stms
              (op', op_sunk) = runState (mapSegOpM (opMapper scope) op) mempty
          in (stm { stmExp = Op (SegOp op') } : stms',
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
                        optimiseBody (ST.fromScope scope <> vtable) sinking body
                  modify (<>sunk)
                  return body'
              }

            opMapper scope =
              identitySegOpMapper
              { mapOnSegOpLambda = \lam -> do
                  let (body, sunk) =
                        optimiseBody op_vtable sinking $
                        lambdaBody lam
                  modify (<>sunk)
                  return lam { lambdaBody = body }

              , mapOnSegOpBody = \body -> do
                  let (body', sunk) =
                        optimiseKernelBody op_vtable sinking body
                  modify (<>sunk)
                  return body'
              }
              where op_vtable = ST.fromScope scope <> vtable

optimiseBody :: SymbolTable -> Sinking -> Body SinkLore
             -> (Body SinkLore, Sunk)
optimiseBody vtable sinking (Body attr stms res) =
  let (stms', sunk) = optimiseStms vtable sinking stms $ freeIn res
  in (Body attr stms' res, sunk)

optimiseKernelBody :: SymbolTable -> Sinking -> KernelBody SinkLore
                   -> (KernelBody SinkLore, Sunk)
optimiseKernelBody vtable sinking (KernelBody attr stms res) =
  let (stms', sunk) = optimiseStms vtable sinking stms $ freeIn res
  in (KernelBody attr stms' res, sunk)

optimiseFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
optimiseFunDef fundef = do
  let fundef' = Range.analyseFun $ Alias.analyseFun fundef
      vtable = ST.insertFParams (funDefParams fundef') mempty
      (body, _) = optimiseBody vtable mempty $ funDefBody fundef'
  return fundef { funDefBody = removeBodyAliases $ removeBodyRanges body }

sink :: Pass Kernels Kernels
sink = Pass "sink" "move memory loads closer to their uses" $
       intraproceduralTransformation optimiseFunDef
