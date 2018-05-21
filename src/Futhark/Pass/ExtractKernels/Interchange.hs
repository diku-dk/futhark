{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | It is well known that fully parallel loops can always be
-- interchanged inwards with a sequential loop.  This module
-- implements that transformation.
--
-- This is also where we implement loop-switching (for branches),
-- which is semantically similar to interchange.
module Futhark.Pass.ExtractKernels.Interchange
       (
         SeqLoop (..)
       , interchangeLoops
       , Branch (..)
       , interchangeBranch
       ) where

import Control.Monad.RWS.Strict
import qualified Data.Set as S
import Data.Maybe
import Data.List

import Futhark.Pass.ExtractKernels.Distribution
  (LoopNesting(..), KernelNest, kernelNestLoops)
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Tools

-- | An encoding of a sequential do-loop with no existential context,
-- alongside its result pattern.
data SeqLoop = SeqLoop [Int] Pattern [(FParam, SubExp)] (LoopForm SOACS) Body

seqLoopStm :: SeqLoop -> Stm
seqLoopStm (SeqLoop _ pat merge form body) =
  Let pat (defAux ()) $ DoLoop [] merge form body

interchangeLoop :: (MonadBinder m, LocalScope SOACS m) =>
                   SeqLoop -> LoopNesting
                -> m SeqLoop
interchangeLoop
  (SeqLoop perm loop_pat merge form body)
  (MapNesting pat cs w params_and_arrs) = do
    merge_expanded <-
      localScope (scopeOfLParams $ map fst params_and_arrs) $
      mapM expand merge

    let loop_pat_expanded =
          Pattern [] $ map expandPatElem $ patternElements loop_pat
        new_params = [ Param pname $ fromDecl ptype
                     | (Param pname ptype, _) <- merge ]
        new_arrs = map (paramName . fst) merge_expanded
        rettype = map rowType $ patternTypes loop_pat_expanded

    -- If the map consumes something that is bound outside the loop
    -- (i.e. is not a merge parameter), we have to copy() it.  As a
    -- small simplification, we just remove the parameter outright if
    -- it is not used anymore.  This might happen if the parameter was
    -- used just as the inital value of a merge parameter.
    ((params', arrs'), pre_copy_bnds) <-
      runBinder $ localScope (scopeOfLParams new_params) $
      unzip . catMaybes <$> mapM copyOrRemoveParam params_and_arrs

    body' <- mkDummyStms (params'<>new_params) body

    let lam = Lambda (params'<>new_params) body' rettype
        map_bnd = Let loop_pat_expanded (StmAux cs ()) $
                  Op $ Screma w (mapSOAC lam) $ arrs' <> new_arrs
        res = map Var $ patternNames loop_pat_expanded
        pat' = Pattern [] $ rearrangeShape perm $ patternValueElements pat

    return $
      SeqLoop [0..patternSize pat-1] pat' merge_expanded form $
      mkBody (pre_copy_bnds<>oneStm map_bnd) res
  where free_in_body = freeInBody body

        copyOrRemoveParam (param, arr)
          | not (paramName param `S.member` free_in_body) =
            return Nothing
          | otherwise =
            return $ Just (param, arr)

        expandedInit _ (Var v)
          | Just (_, arr) <-
              find ((==v).paramName.fst) params_and_arrs =
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
            where param_name = baseString $ paramName merge_param

        expandPatElem (PatElem name t) =
          PatElem name $ arrayOfRow t w

        -- | The kernel extractor cannot handle identity mappings, so
        -- insert dummy statements for body results that are just a
        -- lambda parameter.
        mkDummyStms params (Body () stms res) = do
          (res', extra_stms) <- unzip <$> mapM dummyStm res
          return $ Body () (stms<>mconcat extra_stms) res'
          where dummyStm (Var v)
                  | Just p <- find ((==v) . paramName) params = do
                      dummy <- newVName (baseString v ++ "_dummy")
                      return (Var dummy,
                              oneStm $
                                Let (Pattern [] [PatElem dummy $ paramType p])
                                    (defAux ()) $
                                     BasicOp $ SubExp $ Var $ paramName p)
                dummyStm se = return (se, mempty)

-- | Given a (parallel) map nesting and an inner sequential loop, move
-- the maps inside the sequential loop.  The result is several
-- statements - one of these will be the loop, which will then contain
-- statements with 'Map' expressions.
interchangeLoops :: (MonadFreshNames m, HasScope SOACS m) =>
                    KernelNest -> SeqLoop
                 -> m (Stms SOACS)
interchangeLoops nest loop = do
  (loop', bnds) <-
    runBinder $ foldM interchangeLoop loop $ reverse $ kernelNestLoops nest
  return $ bnds <> oneStm (seqLoopStm loop')

data Branch = Branch [Int] Pattern SubExp Body Body (IfAttr (BranchType SOACS))

branchStm :: Branch -> Stm
branchStm (Branch _ pat cond tbranch fbranch ret) =
  Let pat (defAux ()) $ If cond tbranch fbranch ret

interchangeBranch1 :: (MonadBinder m, LocalScope SOACS m) =>
                      Branch -> LoopNesting -> m Branch
interchangeBranch1
  (Branch perm branch_pat cond tbranch fbranch (IfAttr ret if_sort))
  (MapNesting pat cs w params_and_arrs) = do
    let ret' = map (`arrayOfRow` Free w) ret
        pat' = Pattern [] $ rearrangeShape perm $ patternValueElements pat

        (params, arrs) = unzip params_and_arrs
        lam_ret = map rowType $ patternTypes pat

        branch_pat' =
          Pattern [] $ map (fmap (`arrayOfRow` w)) $ patternElements branch_pat

        mkBranch branch = (renameBody=<<) $ do
          branch' <- if null $ bodyStms branch
                     then runBodyBinder $
                          -- XXX: We need a temporary dummy binding to
                          -- prevent an empty map body.  The kernel
                          -- extractor does not like empty map bodies.
                          resultBody <$> mapM dummyBind (bodyResult branch)
                     else return branch
          let lam = Lambda params branch' lam_ret
              res = map Var $ patternNames branch_pat'
              map_bnd = Let branch_pat' (StmAux cs ()) $ Op $ Screma w (mapSOAC lam) arrs
          return $ mkBody (oneStm map_bnd) res

    tbranch' <- mkBranch tbranch
    fbranch' <- mkBranch fbranch
    return $ Branch [0..patternSize pat-1] pat' cond tbranch' fbranch' $
      IfAttr ret' if_sort
  where dummyBind se = do
          dummy <- newVName "dummy"
          letBindNames_ [dummy] (BasicOp $ SubExp se)
          return $ Var dummy

interchangeBranch :: (MonadFreshNames m, HasScope SOACS m) =>
                     KernelNest -> Branch -> m (Stms SOACS)
interchangeBranch nest loop = do
  (loop', bnds) <-
    runBinder $ foldM interchangeBranch1 loop $ reverse $ kernelNestLoops nest
  return $ bnds <> oneStm (branchStm loop')
