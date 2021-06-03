{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | It is well known that fully parallel loops can always be
-- interchanged inwards with a sequential loop.  This module
-- implements that transformation.
--
-- This is also where we implement loop-switching (for branches),
-- which is semantically similar to interchange.
module Futhark.Pass.ExtractKernels.Interchange
  ( SeqLoop (..),
    interchangeLoops,
    Branch (..),
    interchangeBranch,
    WithAccStm (..),
    interchangeWithAcc,
  )
where

import Control.Monad.Identity
import Data.List (find)
import Data.Maybe
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.Distribution
  ( KernelNest,
    LoopNesting (..),
    kernelNestLoops,
  )
import Futhark.Tools
import Futhark.Transform.Rename

-- | An encoding of a sequential do-loop with no existential context,
-- alongside its result pattern.
data SeqLoop = SeqLoop [Int] Pattern [(FParam, SubExp)] (LoopForm SOACS) Body

seqLoopStm :: SeqLoop -> Stm
seqLoopStm (SeqLoop _ pat merge form body) =
  Let pat (defAux ()) $ DoLoop [] merge form body

interchangeLoop ::
  (MonadBinder m, LocalScope SOACS m) =>
  (VName -> Maybe VName) ->
  SeqLoop ->
  LoopNesting ->
  m SeqLoop
interchangeLoop
  isMapParameter
  (SeqLoop perm loop_pat merge form body)
  (MapNesting pat aux w params_and_arrs) = do
    merge_expanded <-
      localScope (scopeOfLParams $ map fst params_and_arrs) $
        mapM expand merge

    let loop_pat_expanded =
          Pattern [] $ map expandPatElem $ patternElements loop_pat
        new_params =
          [ Param pname $ fromDecl ptype
            | (Param pname ptype, _) <- merge
          ]
        new_arrs = map (paramName . fst) merge_expanded
        rettype = map rowType $ patternTypes loop_pat_expanded

    -- If the map consumes something that is bound outside the loop
    -- (i.e. is not a merge parameter), we have to copy() it.  As a
    -- small simplification, we just remove the parameter outright if
    -- it is not used anymore.  This might happen if the parameter was
    -- used just as the inital value of a merge parameter.
    ((params', arrs'), pre_copy_bnds) <-
      runBinder $
        localScope (scopeOfLParams new_params) $
          unzip . catMaybes <$> mapM copyOrRemoveParam params_and_arrs

    let lam = Lambda (params' <> new_params) body rettype
        map_bnd =
          Let loop_pat_expanded aux $
            Op $ Screma w (arrs' <> new_arrs) (mapSOAC lam)
        res = map Var $ patternNames loop_pat_expanded
        pat' = Pattern [] $ rearrangeShape perm $ patternValueElements pat

    return $
      SeqLoop perm pat' merge_expanded form $
        mkBody (pre_copy_bnds <> oneStm map_bnd) res
    where
      free_in_body = freeIn body

      copyOrRemoveParam (param, arr)
        | not (paramName param `nameIn` free_in_body) =
          return Nothing
        | otherwise =
          return $ Just (param, arr)

      expandedInit _ (Var v)
        | Just arr <- isMapParameter v =
          return $ Var arr
      expandedInit param_name se =
        letSubExp (param_name <> "_expanded_init") $
          BasicOp $ Replicate (Shape [w]) se

      expand (merge_param, merge_init) = do
        expanded_param <-
          newParam (param_name <> "_expanded") $
            arrayOf (paramDeclType merge_param) (Shape [w]) $
              uniqueness $ declTypeOf merge_param
        expanded_init <- expandedInit param_name merge_init
        return (expanded_param, expanded_init)
        where
          param_name = baseString $ paramName merge_param

      expandPatElem (PatElem name t) =
        PatElem name $ arrayOfRow t w

-- | Given a (parallel) map nesting and an inner sequential loop, move
-- the maps inside the sequential loop.  The result is several
-- statements - one of these will be the loop, which will then contain
-- statements with @map@ expressions.
interchangeLoops ::
  (MonadFreshNames m, HasScope SOACS m) =>
  KernelNest ->
  SeqLoop ->
  m (Stms SOACS)
interchangeLoops nest loop = do
  (loop', bnds) <-
    runBinder $
      foldM (interchangeLoop isMapParameter) loop $
        reverse $ kernelNestLoops nest
  return $ bnds <> oneStm (seqLoopStm loop')
  where
    isMapParameter v =
      fmap snd $
        find ((== v) . paramName . fst) $
          concatMap loopNestingParamsAndArrs $ kernelNestLoops nest

data Branch = Branch [Int] Pattern SubExp Body Body (IfDec (BranchType SOACS))

branchStm :: Branch -> Stm
branchStm (Branch _ pat cond tbranch fbranch ret) =
  Let pat (defAux ()) $ If cond tbranch fbranch ret

interchangeBranch1 ::
  (MonadBinder m) =>
  Branch ->
  LoopNesting ->
  m Branch
interchangeBranch1
  (Branch perm branch_pat cond tbranch fbranch (IfDec ret if_sort))
  (MapNesting pat aux w params_and_arrs) = do
    let ret' = map (`arrayOfRow` Free w) ret
        pat' = Pattern [] $ rearrangeShape perm $ patternValueElements pat

        (params, arrs) = unzip params_and_arrs
        lam_ret = rearrangeShape perm $ map rowType $ patternTypes pat

        branch_pat' =
          Pattern [] $ map (fmap (`arrayOfRow` w)) $ patternElements branch_pat

        mkBranch branch = (renameBody =<<) $ do
          let lam = Lambda params branch lam_ret
              res = map Var $ patternNames branch_pat'
              map_bnd = Let branch_pat' aux $ Op $ Screma w arrs $ mapSOAC lam
          return $ mkBody (oneStm map_bnd) res

    tbranch' <- mkBranch tbranch
    fbranch' <- mkBranch fbranch
    return $
      Branch [0 .. patternSize pat -1] pat' cond tbranch' fbranch' $
        IfDec ret' if_sort

interchangeBranch ::
  (MonadFreshNames m, HasScope SOACS m) =>
  KernelNest ->
  Branch ->
  m (Stms SOACS)
interchangeBranch nest loop = do
  (loop', bnds) <-
    runBinder $ foldM interchangeBranch1 loop $ reverse $ kernelNestLoops nest
  return $ bnds <> oneStm (branchStm loop')

data WithAccStm
  = WithAccStm [Int] Pattern [(Shape, [VName], Maybe (Lambda, [SubExp]))] Lambda

withAccStm :: WithAccStm -> Stm
withAccStm (WithAccStm _ pat inputs lam) =
  Let pat (defAux ()) $ WithAcc inputs lam

interchangeWithAcc1 ::
  (MonadBinder m, Lore m ~ SOACS) =>
  WithAccStm ->
  LoopNesting ->
  m WithAccStm
interchangeWithAcc1
  (WithAccStm perm _withacc_pat inputs acc_lam)
  (MapNesting map_pat map_aux w params_and_arrs) = do
    inputs' <- mapM onInput inputs
    lam_params' <- newAccLamParams $ lambdaParams acc_lam
    iota_p <- newParam "iota_p" $ Prim int64
    acc_lam' <- trLam (Var (paramName iota_p)) <=< mkLambda lam_params' $ do
      let acc_params = drop (length inputs) lam_params'
          orig_acc_params = drop (length inputs) $ lambdaParams acc_lam
      iota_w <-
        letExp "acc_inter_iota" . BasicOp $
          Iota w (intConst Int64 0) (intConst Int64 1) Int64
      let (params, arrs) = unzip params_and_arrs
          maplam_ret = lambdaReturnType acc_lam
          maplam = Lambda (iota_p : orig_acc_params ++ params) (lambdaBody acc_lam) maplam_ret
      auxing map_aux . letTupExp' "withacc_inter" $
        Op $ Screma w (iota_w : map paramName acc_params ++ arrs) (mapSOAC maplam)
    let pat = Pattern [] $ rearrangeShape perm $ patternValueElements map_pat
        perm' = [0 .. patternSize pat -1]
    pure $ WithAccStm perm' pat inputs' acc_lam'
    where
      newAccLamParams ps = do
        let (cert_ps, acc_ps) = splitAt (length ps `div` 2) ps
        -- Should not rename the certificates.
        acc_ps' <- forM acc_ps $ \(Param v t) ->
          newParam (baseString v) t
        pure $ cert_ps <> acc_ps'

      num_accs = length inputs
      acc_certs = map paramName $ take num_accs $ lambdaParams acc_lam
      onArr v =
        pure . maybe v snd $
          find ((== v) . paramName . fst) params_and_arrs
      onInput (shape, arrs, op) =
        (Shape [w] <> shape,,) <$> mapM onArr arrs <*> traverse onOp op

      onOp (op_lam, nes) = do
        -- We need to add an additional index parameter because we are
        -- extending the index space of the accumulator.
        idx_p <- newParam "idx" $ Prim int64
        pure (op_lam {lambdaParams = idx_p : lambdaParams op_lam}, nes)

      trType :: TypeBase shape u -> TypeBase shape u
      trType (Acc acc ispace ts u)
        | acc `elem` acc_certs =
          Acc acc (Shape [w] <> ispace) ts u
      trType t = t

      trParam :: Param (TypeBase shape u) -> Param (TypeBase shape u)
      trParam = fmap trType

      trLam i (Lambda params body ret) =
        localScope (scopeOfLParams params) $
          Lambda (map trParam params) <$> trBody i body <*> pure (map trType ret)

      trBody i (Body dec stms res) =
        inScopeOf stms $ Body dec <$> traverse (trStm i) stms <*> pure res

      trStm i (Let pat aux e) =
        Let (fmap trType pat) aux <$> trExp i e

      trSOAC i = mapSOACM mapper
        where
          mapper =
            identitySOACMapper {mapOnSOACLambda = trLam i}

      trExp i (WithAcc acc_inputs lam) =
        WithAcc acc_inputs <$> trLam i lam
      trExp i (BasicOp (UpdateAcc acc is ses)) = do
        acc_t <- lookupType acc
        pure $ case acc_t of
          Acc cert _ _ _
            | cert `elem` acc_certs ->
              BasicOp $ UpdateAcc acc (i : is) ses
          _ ->
            BasicOp $ UpdateAcc acc is ses
      trExp i e = mapExpM mapper e
        where
          mapper =
            identityMapper
              { mapOnBody = \scope -> localScope scope . trBody i,
                mapOnRetType = pure . trType,
                mapOnBranchType = pure . trType,
                mapOnFParam = pure . trParam,
                mapOnLParam = pure . trParam,
                mapOnOp = trSOAC i
              }

interchangeWithAcc ::
  (MonadFreshNames m, HasScope SOACS m) =>
  KernelNest ->
  WithAccStm ->
  m (Stms SOACS)
interchangeWithAcc nest withacc = do
  (withacc', stms) <-
    runBinder $ foldM interchangeWithAcc1 withacc $ reverse $ kernelNestLoops nest
  return $ stms <> oneStm (withAccStm withacc')
