-- | Differentiation related to accumulators in the input program.
module Futhark.AD.Rev.Acc
  ( diffWithAcc,
    diffUpdateAcc,
  )
where

-- Note [Adjoints of accumulators]
--
-- The general case of taking adjoints of WithAcc is tricky.  We make
-- some assumptions and lay down a basic design.
--
-- First, we assume that any WithAccs that occur in the program are
-- come from one of these sources:
--
-- - A previous instance of VJP, which means we can rely on the operator having
--   a constant adjoint (it's addition as appropriate to the type).
--
-- - A scatter, meaning there is no operator.
--
-- (These can actually be distinguished by the presence of an operator, although
-- we do not currently bother.)
--
-- Second, the adjoint of an accumulator is an array of the same type
-- as the underlying array.  For example, the adjoint type of the
-- primal type 'acc(c, [n], {f64})' is '[n]f64'.  In principle the
-- adjoint of 'acc(c, [n], {f64,f32})' should be two arrays of type
-- '[]f64', '[]f32'.  Our current design assumes that adjoints are
-- single variables.  This is fixable.
--
-- In the return sweep, when inserting the with_acc, we still compute the
-- "original" accumulator result, but modified such that its initial value is
-- the adjoint of the result of the accumulator. We also modify the update_accs
-- of these accumulators to be with zero values. This means that the array that
-- is produced will be equal to the adjoint of the result, except for those
-- places that have been updated, where it will be zero. This is intuitively
-- sensible - values that have been overwritten (and so do not contribute to the
-- result) should obviously have zero sensitivity.
--
-- # Adjoint of UpdateAcc
--
-- Consider primal code
--
--     update_acc(acc, i, v)
--
-- Interpreted as an imperative statement, this means
--
--     acc[i] ⊕= v
--
-- for some '⊕'.  Normally all the compiler knows of '⊕' is that it
-- is associative and commutative, but because we assume that all
-- accumulators are the result of previous AD transformations, we
-- can assume that '⊕' actually behaves like addition - that is, has
-- unit partial derivatives.  So the return sweep is
--
--     v_adj += acc_adj[i]
--
-- Further, we modify the primal code so that it becomes
--
--     update_acc(acc, i, 0)
--
-- for some appropriate notion of zero.
--
-- # Adjoint of Map
--
-- Suppose we have primal code
--
--   let acc' =
--     map (...) acc
--
-- where "acc : acc(c, [n], {f64})" and the width of the Map is "w".
-- Our normal transformation for Map input arrays is to similarly map
-- their adjoint, but clearly this doesn't work here because the
-- semantics of mapping an adjoint is an "implicit replicate".  So
-- when generating the return sweep we actually perform that
-- replication:
--
--   map (...) (replicate w acc_adj)
--
-- But what about the contributions to "acc'"?  Those we also have to
-- take special care of.  The result of the map itself is actually a
-- multidimensional array:
--
--   let acc_contribs =
--     map (...) (replicate w acc'_adj)
--
-- which we must then sum to add to the contribution.
--
--   acc_adj += sum(acc_contribs)
--
-- I'm slightly worried about the asymptotics of this, since my
-- intuition of this is that the contributions might be rather sparse.
-- (Maybe completely zero?  If so it will be simplified away
-- entirely.)  Perhaps a better solution is to treat
-- accumulator-inputs in the primal code as we do free variables, and
-- create accumulators for them in the return sweep.
--
-- # Vectorised WithAcc
--
-- When WithAcc occurs in vectorised AD, the accumulator element types gain
-- extra leading "vectorised" dimensions corresponding to the enclosing vector
-- shape. For example, if the primal type inside a map of width @w@ is @acc(c,
-- [n], {f64})@, the adjoint type is @[w][n]f64@ -- but the internal accumulator
-- layout expects shape @[n][w]f64@ (the accumulator shape comes first, then the
-- vectorised dimensions, then element dimensions).
--
-- This means we must transpose accumulator adjoints when entering and
-- leaving the return-sweep WithAcc:
--
--  * On entry: transpose result adjoints from @[vec...][shape...]elem@ to
--    @[shape...][vec...]elem@ so they can serve as initial values for the
--    accumulators.
--
--  * On exit: transpose the produced arrays back from @[shape...][vec...]elem@
--    to @[vec...][shape...]elem@ to match the expected adjoint layout.
--
-- This is actually quite similar to how other SOACs must be handled.
--
-- Additionally, the accumulator parameter types in the lambda (and any
-- Acc-typed pattern elements or inner lambda parameters referring to the same
-- certs) must be updated to reflect the vectorised element types *before*
-- differentiation. This ensures that 'lookupAdj' on accumulator variables
-- inside the lambda produces adjoints with the correct vectorised type.
--
-- The UpdateAcc case is simpler under vectorisation: because the accumulator
-- adjoint already has the vectorised dimensions folded into its element type, a
-- plain index into the adjoint at the update indices directly yields the
-- correctly-shaped contribution.
--
-- # Consumption
--
-- A minor problem is that our usual way of handling consumption (Note
-- [Consumption]) is not viable, because accumulators are not
-- copyable.  Fortunately, while the accumulators that are consumed in
-- the forward sweep will also be present in the return sweep given
-- our current translation rules, they will be dead code.  As long as
-- we are careful to run dead code elimination after revVJP, we should
-- be good.

-- Note [Array Adjoints of Match]
--
-- Some unusual, but sadly not completely contrived, contain Match
-- expressions that return multiple arrays, and there the arrays
-- returned by one branch have overlapping aliases with another
-- branch, although in different places. As an example consider this:
--
--   let (X,Y) = if c
--               then (A, B)
--               else (B, A)
--
-- Because our aliasing representation cannot express mutually
-- exclusive aliases, we will consider X and Y to be aliased to each
-- other. In practice, this means it is unlikely for X or Y to be
-- consumed, because it would also consume the other (although it's
-- possible for carefully written code).
--
-- When producing adjoints for this, it will be something like
--
--   let (X_adj,Y_adj) = if c
--                       then (A_adj, B_adj)
--                       else (B_adj, A_adj)
--
-- which completely reflects the primal code. However, while it is
-- unlikely that any consumption takes place for the original primal
-- variables, it is almost guaranteed that X_adj and Y_adj will be
-- consumed (that is the main way we use adjoints after all), and due
-- to the conservative aliasing, when one is consumed, so is the
-- other! To avoid this tragic fate, we are forced to copy any
-- array-typed adjoints returned by a Match. This can be quite costly.
-- However:
--
-- 1) Futhark has pretty OK copy removal, so maybe it can get rid of
--    these by using information not available to the AD pass.
--
-- 2) In many cases, arrays will have accumulator adjoints, which are
--    not subject to this problem.
--
-- Issue #2228 was caused by neglecting to do this.

import Control.Monad
import Control.Monad.Identity
import Data.List ((\\))
import Futhark.AD.Rev.Monad
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util (chunks, takeLast)

-- | Transform updates on accumulators matching the given certificates into
-- updates that write provided zero values.
zeroOutUpdates :: [(VName, [SubExp])] -> Lambda SOACS -> Lambda SOACS
zeroOutUpdates certs_to_zeroes lam = lam {lambdaBody = onBody $ lambdaBody lam}
  where
    onExp = runIdentity . mapExpM mapper
      where
        mapper =
          (identityMapper :: (Monad m) => Mapper SOACS SOACS m)
            { mapOnOp = traverseSOACStms (\_ stms -> pure $ onStms stms),
              mapOnBody = \_ body -> pure $ onBody body
            }
    onStms = fmap onStm
    onStm (Let (Pat [pe]) aux (BasicOp (UpdateAcc safety acc is _)))
      | Acc c _ _ _ <- patElemType pe,
        Just zero <- lookup c certs_to_zeroes =
          Let (Pat [pe]) aux (BasicOp (UpdateAcc safety acc is zero))
    onStm (Let pat aux e) = Let pat aux $ onExp e

    onBody body = body {bodyStms = onStms $ bodyStms body}

-- Update accumulator parameter types in the lambda to include vectorised
-- element types. Also updates all Acc-typed pattern elements and inner
-- lambda parameters that reference the same accumulator certs.
updateAccParamTypes :: Int -> Shape -> Lambda SOACS -> Lambda SOACS
updateAccParamTypes n_inputs adj_sh lam
  | adj_sh == mempty = lam
  | otherwise =
      let (cert_ps, rest_ps) = splitAt n_inputs (lambdaParams lam)
          (acc_ps, other_ps) = splitAt n_inputs rest_ps
          acc_ps' = map (updateParam cert_names) acc_ps
          cert_names = map paramName cert_ps
          body' = updateBody cert_names (lambdaBody lam)
          ret' = map (updateAccType cert_names) (lambdaReturnType lam)
       in lam
            { lambdaParams = cert_ps ++ acc_ps' ++ other_ps,
              lambdaReturnType = ret',
              lambdaBody = body'
            }
  where
    updateParam :: [VName] -> Param Type -> Param Type
    updateParam certs p =
      p {paramDec = updateAccType certs (paramDec p)}

    updateAccType :: [VName] -> Type -> Type
    updateAccType certs (Acc cert acc_shape ts u)
      | cert `elem` certs =
          Acc cert acc_shape (map (`arrayOfShape` adj_sh) ts) u
    updateAccType _ t = t

    updateBody :: [VName] -> Body SOACS -> Body SOACS
    updateBody certs body =
      body {bodyStms = fmap (updateStm certs) (bodyStms body)}

    updateStm :: [VName] -> Stm SOACS -> Stm SOACS
    updateStm certs (Let pat aux e) =
      Let (updatePat certs pat) aux (updateExp certs e)

    updatePat :: [VName] -> Pat Type -> Pat Type
    updatePat certs (Pat pes) =
      Pat $ map (\pe -> pe {patElemDec = updateAccType certs (patElemDec pe)}) pes

    updateExp :: [VName] -> Exp SOACS -> Exp SOACS
    updateExp certs = runIdentity . mapExpM mapper
      where
        mapper =
          (identityMapper :: (Monad m) => Mapper SOACS SOACS m)
            { mapOnBody = \_ b -> pure $ updateBody certs b,
              mapOnOp = pure . updateSOAC certs
            }

    updateSOAC :: [VName] -> SOAC SOACS -> SOAC SOACS
    updateSOAC certs = runIdentity . mapSOACM mapper
      where
        mapper =
          identitySOACMapper
            { mapOnSOACLambda = pure . updateLambda certs
            }

    updateLambda :: [VName] -> Lambda SOACS -> Lambda SOACS
    updateLambda certs l =
      l
        { lambdaParams = map (updateParam certs) (lambdaParams l),
          lambdaReturnType = map (updateAccType certs) (lambdaReturnType l),
          lambdaBody = updateBody certs (lambdaBody l)
        }

diffWithAcc ::
  VjpOps ->
  Pat Type ->
  StmAux () ->
  [(Shape, [VName], Maybe (Lambda SOACS, [SubExp]))] ->
  Lambda SOACS ->
  ADM () ->
  ADM ()
diffWithAcc ops pat aux inputs lam m = do
  addStm $ Let pat aux $ WithAcc inputs lam
  m
  returnSweepCode $ do
    adj_shape <- askShape
    adjs <- mapM lookupAdj $ patNames pat
    -- Transpose the accumulator result adjoints from [vec...][shape...]elem
    -- to [shape...][vec...]elem, matching the internal accumulator layout.
    adjs' <- transposeAdjs adj_shape adjs
    lam' <- renameLambda lam
    -- Update the lambda's accumulator parameter types to reflect vectorised
    -- element types BEFORE differentiation, so that lookupAdj on Acc variables
    -- inside the lambda gives the correct vectorised adjoint type.
    let lam'_vec = updateAccParamTypes n_inputs adj_shape lam'
    free_vars <- filterM isActive $ namesToList $ freeIn lam'_vec
    free_accs <- filterM (fmap isAcc . lookupType) free_vars
    let free_vars' = free_vars \\ free_accs
    lam'' <- diffLambda' adjs' free_vars' lam'_vec
    (inputs_zeroes, inputs') <-
      unzip <$> zipWithM (renameInputLambda adj_shape) (chunks lengths adjs) inputs
    let certs = map paramName $ take n_inputs $ lambdaParams lam''
    raw_adjs <-
      letTupExp "with_acc_contrib" . WithAcc inputs' $
        zeroOutUpdates (zip certs inputs_zeroes) lam''
    -- The accumulator results have shape [shape...][vec...]elem. Transpose
    -- back to [vec...][shape...]elem for the adjoint.
    let n_arrs = sum lengths
        (arr_adjs, free_adjs) = splitAt n_arrs raw_adjs
    arr_adjs' <- zipWithM (transposeAccResult adj_shape) (map (\(s, _, _) -> s) inputs) arr_adjs
    zipWithM_ insAdj arrs arr_adjs'
    zipWithM_ insAdj free_vars' free_adjs
  where
    n_inputs = length inputs
    lengths = map (\(_, as, _) -> length as) inputs
    arrs = concatMap (\(_, as, _) -> as) inputs

    -- Transpose the accumulator-related adjoints from [vec...][shape...]elem
    -- to [shape...][vec...]elem. Non-accumulator adjs are left unchanged.
    transposeAdjs :: Shape -> [Adj] -> ADM [Adj]
    transposeAdjs adj_sh adjs
      | adj_sh == mempty = pure adjs
      | otherwise = do
          let n_arrs = sum lengths
              (acc_adjs, other_adjs) = splitAt n_arrs adjs
          acc_adjs' <- mapM transposeAdj acc_adjs
          pure $ acc_adjs' ++ other_adjs

    transposeAdj :: Adj -> ADM Adj
    transposeAdj adj = do
      v <- adjVal adj
      v' <- vecToInner v
      pure $ AdjVal $ Var v'

    -- Transpose [shape...][vec...][elem...] to [vec...][shape...][elem...]
    transposeAccResult :: Shape -> Shape -> VName -> ADM VName
    transposeAccResult adj_sh shape v
      | adj_sh == mempty = pure v
      | otherwise = do
          v_t <- lookupType v
          let r = shapeRank adj_sh
              s = shapeRank shape
              total = arrayRank v_t
              perm = [s .. s + r - 1] ++ [0 .. s - 1] ++ [s + r .. total - 1]
          letExp (baseName v <> "_tr") $ BasicOp $ Rearrange v perm

    renameInputLambda adj_sh as_adj (shape, as, _) = do
      -- Compute element types with vectorised dimensions included.
      orig_nes_ts <- mapM (fmap (stripArray (shapeRank shape)) . lookupType) as
      let vec_nes_ts = map (`arrayOfShape` adj_sh) orig_nes_ts
      zeroes <- mapM (zeroArray mempty) vec_nes_ts
      -- Transpose adjoints from [vec...][shape...]elem to [shape...][vec...]elem
      -- so they match the accumulator layout.
      as' <- mapM adjVal as_adj
      as'' <- mapM vecToInner as'
      pure (map Var zeroes, (shape, as'', Nothing))

    diffLambda' res_adjs get_adjs_for (Lambda params ts body) = do
      localScope (scopeOfLParams params) $ do
        Body () stms res <- vjpBody ops res_adjs get_adjs_for body
        let body' = Body () stms $ take n_inputs res <> takeLast (length get_adjs_for) res
        ts' <- mapM lookupType get_adjs_for
        pure $ Lambda params (take n_inputs ts <> ts') body'

diffUpdateAcc ::
  Pat Type ->
  StmAux () ->
  Safety ->
  VName ->
  [SubExp] ->
  [SubExp] ->
  ADM () ->
  ADM ()
diffUpdateAcc pat aux safety acc is vs m = do
  addStm $ Let pat aux $ BasicOp $ UpdateAcc safety acc is vs
  m
  pat_adjs <- mapM lookupAdjVal (patNames pat)
  returnSweepCode $ forM_ (zip pat_adjs vs) $ \(adj, v) -> do
    adj_t <- lookupType adj
    let index_adj = pure $ BasicOp $ Index adj $ fullSlice adj_t $ map DimFix is
    adj_i <-
      letExp "updateacc_val_adj" =<< case safety of
        Unsafe ->
          index_adj
        Safe ->
          -- The primal UpdateAcc may be out-of-bounds, in which case
          -- indexing the adjoint is dangerous.
          eIf
            (eShapeInBounds (arrayShape adj_t) (map eSubExp is))
            (eBody [index_adj])
            (eBody [pure $ zeroExp $ stripArray (length is) adj_t])
    updateSubExpAdj v adj_i
