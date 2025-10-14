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
-- This pass is defined on post-SOACS representations.  This is not
-- because we do anything GPU-specific here, but simply because more
-- explicit indexing is going on after SOACs are gone.
module Futhark.Optimise.Sink (sinkGPU, sinkMC) where

import Control.Monad.State
import Data.Bifunctor
import Data.List (foldl')
import Data.Map qualified as M
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Builder.Class
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.MC
import Futhark.Pass

type SymbolTable rep = ST.SymbolTable rep

type Sinking rep = M.Map VName (Stm rep)

type Sunk = Names

type Sinker rep a = SymbolTable rep -> Sinking rep -> a -> (a, Sunk)

type Constraints rep =
  ( ASTRep rep,
    Aliased rep,
    Buildable rep,
    ST.IndexOp (Op rep)
  )

-- | Given a statement, compute how often each of its free variables
-- are used.  Not accurate: what we care about are only 1, and greater
-- than 1.
multiplicity :: (Constraints rep) => Stm rep -> M.Map VName Int
multiplicity stm =
  case stmExp stm of
    Match cond cases defbody _ ->
      foldl' comb mempty $
        free 1 cond : free 1 defbody : map (free 1 . caseBody) cases
    Op {} -> free 2 stm
    Loop {} -> free 2 stm
    WithAcc {} -> free 2 stm
    _ -> free 1 stm
  where
    free k x = M.fromList $ map (,k) $ namesToList $ freeIn x
    comb = M.unionWith (+)

optimiseBranch ::
  (Constraints rep) =>
  Sinker rep (Op rep) ->
  Sinker rep (Body rep)
optimiseBranch onOp vtable sinking (Body dec stms res) =
  let (stms', stms_sunk) = optimiseStms onOp vtable sinking' (sunk_stms <> stms) $ freeIn res
   in ( Body dec stms' res,
        sunk <> stms_sunk
      )
  where
    free_in_stms = freeIn stms <> freeIn res
    (sinking_here, sinking') = M.partitionWithKey sunkHere sinking
    sunk_stms = stmsFromList $ M.elems sinking_here
    sunkHere v stm =
      v
        `nameIn` free_in_stms
        && all (`ST.available` vtable) (namesToList (freeIn stm))
    sunk = namesFromList $ foldMap (patNames . stmPat) sunk_stms

optimiseLoop ::
  (Constraints rep) =>
  Sinker rep (Op rep) ->
  Sinker rep ([(FParam rep, SubExp)], LoopForm, Body rep)
optimiseLoop onOp vtable sinking (merge, form, body0) =
  let (body1, sunk) = optimiseBody onOp vtable' sinking body0
   in ((merge, form, body1), sunk)
  where
    (params, _) = unzip merge
    scope = case form of
      WhileLoop {} -> scopeOfFParams params
      ForLoop i it _ -> M.insert i (IndexName it) $ scopeOfFParams params
    vtable' = ST.insertScope scope vtable

optimiseStms ::
  (Constraints rep) =>
  Sinker rep (Op rep) ->
  SymbolTable rep ->
  Sinking rep ->
  Stms rep ->
  Names ->
  (Stms rep, Sunk)
optimiseStms onOp init_vtable init_sinking all_stms free_in_res =
  let (all_stms', sunk) =
        optimiseStms' init_vtable init_sinking $ stmsToList all_stms
   in (stmsFromList all_stms', sunk)
  where
    multiplicities =
      foldl'
        (M.unionWith (+))
        (M.fromList (map (,1) (namesToList free_in_res)))
        (map multiplicity $ stmsToList all_stms)

    optimiseStms' _ _ [] = ([], mempty)
    optimiseStms' vtable sinking (stm : stms)
      | BasicOp Index {} <- stmExp stm,
        [pe] <- patElems (stmPat stm),
        primType $ patElemType pe,
        maybe True (== 1) $ M.lookup (patElemName pe) multiplicities =
          let (stms', sunk) =
                optimiseStms' vtable' (M.insert (patElemName pe) stm sinking) stms
           in if patElemName pe `nameIn` sunk
                then (stms', sunk)
                else (stm : stms', sunk)
      | Match cond cases defbody ret <- stmExp stm =
          let onCase (Case vs body) =
                let (body', body_sunk) = optimiseBranch onOp vtable sinking body
                 in (Case vs body', body_sunk)
              (cases', cases_sunk) = unzip $ map onCase cases
              (defbody', defbody_sunk) = optimiseBranch onOp vtable sinking defbody
              (stms', sunk) = optimiseStms' vtable' sinking stms
           in ( stm {stmExp = Match cond cases' defbody' ret} : stms',
                mconcat cases_sunk <> defbody_sunk <> sunk
              )
      | Loop merge lform body <- stmExp stm =
          let comps = (merge, lform, body)
              (comps', loop_sunk) = optimiseLoop onOp vtable sinking comps
              (merge', _, body') = comps'

              (stms', stms_sunk) = optimiseStms' vtable' sinking stms
           in ( stm {stmExp = Loop merge' lform body'} : stms',
                stms_sunk <> loop_sunk
              )
      | Op op <- stmExp stm =
          let (op', op_sunk) = onOp vtable sinking op
              (stms', stms_sunk) = optimiseStms' vtable' sinking stms
           in ( stm {stmExp = Op op'} : stms',
                stms_sunk <> op_sunk
              )
      | otherwise =
          let (stms', stms_sunk) = optimiseStms' vtable' sinking stms
              (e', stm_sunk) = runState (mapExpM mapper (stmExp stm)) mempty
           in ( stm {stmExp = e'} : stms',
                stm_sunk <> stms_sunk
              )
      where
        vtable' = ST.insertStm stm vtable
        mapper =
          identityMapper
            { mapOnBody = \scope body -> do
                let (body', sunk) =
                      optimiseBody
                        onOp
                        (ST.insertScope scope vtable)
                        sinking
                        body
                modify (<> sunk)
                pure body'
            }

optimiseBody ::
  (Constraints rep) =>
  Sinker rep (Op rep) ->
  Sinker rep (Body rep)
optimiseBody onOp vtable sinking (Body attr stms res) =
  let (stms', sunk) = optimiseStms onOp vtable sinking stms $ freeIn res
   in (Body attr stms' res, sunk)

optimiseKernelBody ::
  (Constraints rep) =>
  Sinker rep (Op rep) ->
  Sinker rep (KernelBody rep)
optimiseKernelBody onOp vtable sinking (Body attr stms res) =
  let (stms', sunk) = optimiseStms onOp vtable sinking stms $ freeIn res
   in (Body attr stms' res, sunk)

optimiseSegOp ::
  (Constraints rep) =>
  Sinker rep (Op rep) ->
  Sinker rep (SegOp lvl rep)
optimiseSegOp onOp vtable sinking op =
  let scope = scopeOfSegSpace $ segSpace op
   in runState (mapSegOpM (opMapper scope) op) mempty
  where
    opMapper scope =
      identitySegOpMapper
        { mapOnSegBinOpLambda = \lam -> do
            let (body, sunk) =
                  optimiseBody onOp op_vtable sinking $
                    lambdaBody lam
            modify (<> sunk)
            pure lam {lambdaBody = body},
          mapOnSegPostOpLambda = \lam -> do
            let (body, sunk) =
                  optimiseBody onOp op_vtable sinking $
                    lambdaBody lam
            modify (<> sunk)
            pure lam {lambdaBody = body},
          mapOnSegOpBody = \body -> do
            let (body', sunk) =
                  optimiseKernelBody onOp op_vtable sinking body
            modify (<> sunk)
            pure body'
        }
      where
        op_vtable = ST.insertScope scope vtable

type SinkRep rep = Aliases rep

sink ::
  ( Buildable rep,
    AliasableRep rep,
    ST.IndexOp (Op (Aliases rep))
  ) =>
  Sinker (SinkRep rep) (Op (SinkRep rep)) ->
  Pass rep rep
sink onOp =
  Pass "sink" "move memory loads closer to their uses" $
    fmap removeProgAliases
      . intraproceduralTransformationWithConsts onConsts onFun
      . Alias.aliasAnalysis
  where
    onFun _ fd = do
      let vtable = ST.insertFParams (funDefParams fd) ST.empty
          (body, _) = optimiseBody onOp vtable mempty $ funDefBody fd
      pure fd {funDefBody = body}

    onConsts consts =
      pure $
        fst $
          optimiseStms onOp ST.empty mempty consts $
            namesFromList $
              M.keys $
                scopeOf consts

-- | Sinking in GPU kernels.
sinkGPU :: Pass GPU GPU
sinkGPU = sink onHostOp
  where
    onHostOp :: Sinker (SinkRep GPU) (Op (SinkRep GPU))
    onHostOp vtable sinking (SegOp op) =
      first SegOp $ optimiseSegOp onHostOp vtable sinking op
    onHostOp vtable sinking (GPUBody types body) =
      first (GPUBody types) $ optimiseBody onHostOp vtable sinking body
    onHostOp _ _ op = (op, mempty)

-- | Sinking for multicore.
sinkMC :: Pass MC MC
sinkMC = sink onHostOp
  where
    onHostOp :: Sinker (SinkRep MC) (Op (SinkRep MC))
    onHostOp vtable sinking (ParOp par_op op) =
      let (par_op', par_sunk) =
            maybe
              (Nothing, mempty)
              (first Just . optimiseSegOp onHostOp vtable sinking)
              par_op
          (op', sunk) = optimiseSegOp onHostOp vtable sinking op
       in (ParOp par_op' op', par_sunk <> sunk)
    onHostOp _ _ op = (op, mempty)
