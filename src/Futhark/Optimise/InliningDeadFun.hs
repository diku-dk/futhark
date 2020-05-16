{-# LANGUAGE FlexibleContexts #-}
-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineFunctions
  , removeDeadFunctions
  )
  where

import Control.Monad.Identity
import Control.Monad.State
import Data.List (partition)
import Data.Loc
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Parallel.Strategies

import Futhark.Representation.SOACS
import Futhark.Representation.SOACS.Simplify
  (simpleSOACS, simplifyFun, simplifyConsts)
import Futhark.Optimise.CSE
import Futhark.Optimise.Simplify.Lore (addScopeWisdom)
import Futhark.Transform.CopyPropagate
  (copyPropagateInProg, copyPropagateInFun)
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Transform.Rename
import Futhark.Analysis.CallGraph
import Futhark.Binder
import Futhark.Pass

parMapM :: MonadFreshNames m => (a -> State VNameSource b) -> [a] -> m [b]
parMapM f as =
  modifyNameSource $ \src ->
  let f' a = runState (f a) src
      (bs, srcs) = unzip $ parMap rpar f' as
  in (bs, mconcat srcs)

aggInlineFunctions :: MonadFreshNames m =>
                      CallGraph
                   -> (Stms SOACS, [FunDef SOACS])
                   -> m (Stms SOACS, [FunDef SOACS])
aggInlineFunctions cg =
  fmap (fmap (filter keep)) . recurse 0 . addVtable
  where fdmap fds =
          M.fromList $ zip (map funDefName fds) fds

        addVtable (consts, funs) =
          (ST.fromScope (addScopeWisdom (scopeOf consts)),
           consts, funs)

        noCallsTo which fundec =
          not $ any (`S.member` which) $ allCalledBy (funDefName fundec) cg

        -- The inverse rate at which we perform full simplification
        -- after inlining.  For the other steps we just do copy
        -- propagation.  The rate here has been determined
        -- heuristically and is probably not optimal for any given
        -- program.
        simplifyRate :: Int
        simplifyRate = 4

        -- We apply simplification after every round of inlining,
        -- because it is more efficient to shrink the program as soon
        -- as possible, rather than wait until it has balooned after
        -- full inlining.
        recurse i (vtable, consts, funs) = do
          let remaining = S.fromList $ map funDefName funs
              (to_be_inlined, maybe_inline_in) =
                partition (noCallsTo remaining) funs
              (not_to_inline_in, to_inline_in) =
                partition (noCallsTo
                           (S.fromList $ map funDefName to_be_inlined))
                maybe_inline_in
              (not_actually_inlined, to_be_inlined') =
                partition keep to_be_inlined
          if null to_be_inlined
            then return (consts, funs)
            else do

            (vtable', consts') <-
              if any ((`calledByConsts` cg) . funDefName) to_be_inlined'
              then simplifyConsts . performCSEOnStms True =<<
                   inlineInStms (fdmap to_be_inlined') consts
              else pure (vtable, consts)

            let simplifyFun' fd
                  | i `rem` simplifyRate == 0 =
                      copyPropagateInFun simpleSOACS vtable' .
                      performCSEOnFunDef True =<<
                      simplifyFun vtable' fd
                  | otherwise =
                      copyPropagateInFun simpleSOACS vtable' fd

            let onFun = simplifyFun' <=<
                        inlineInFunDef (fdmap to_be_inlined')
            to_inline_in' <- parMapM onFun to_inline_in
            fmap (not_actually_inlined<>) <$>
              recurse (i+1)
              (vtable', consts', not_to_inline_in <> to_inline_in')

        keep fd =
          isJust (funDefEntryPoint fd) || callsRecursive fd

        callsRecursive fd = any recursive $ allCalledBy (funDefName fd) cg
        recursive fname = calls fname fname cg

-- | @inlineInFunDef constf fdmap caller@ inlines in @calleer@ the
-- functions in @fdmap@ that are called as @constf@. At this point the
-- preconditions are that if @fdmap@ is not empty, and, more
-- importantly, the functions in @fdmap@ do not call any other
-- functions.
inlineInFunDef :: MonadFreshNames m =>
                  M.Map Name (FunDef SOACS) -> FunDef SOACS
               -> m (FunDef SOACS)
inlineInFunDef fdmap (FunDef entry name rtp args body) =
  FunDef entry name rtp args <$> inlineInBody fdmap body

inlineFunction :: MonadFreshNames m =>
                  Pattern
               -> StmAux attr
               -> [(SubExp, Diet)]
               -> (Safety, SrcLoc, [SrcLoc])
               -> FunDef SOACS
               -> m [Stm]
inlineFunction pat aux args (safety,loc,locs) fun = do
  Body _ stms res <-
    renameBody $ mkBody
    (stmsFromList param_stms <> stmsFromList body_stms)
    (bodyResult (funDefBody fun))
  let res_stms =
        certify (stmAuxCerts aux) <$>
        zipWith (reshapeIfNecessary (patternNames pat))
        (patternIdents pat) res
  pure $ stmsToList stms <> res_stms
  where param_names =
          map paramName $ funDefParams fun

        param_stms =
          zipWith (reshapeIfNecessary param_names)
          (map paramIdent $ funDefParams fun) (map fst args)

        body_stms =
          stmsToList $
          addLocations safety (filter notNoLoc (loc:locs)) $
          bodyStms $ funDefBody fun

        reshapeIfNecessary dim_names ident se
          | t@Array{} <- identType ident,
            any (`elem` dim_names) (subExpVars $ arrayDims t),
            Var v <- se =
              mkLet [] [ident] $ shapeCoerce (arrayDims t) v
          | otherwise =
              mkLet [] [ident] $ BasicOp $ SubExp se

        notNoLoc = (/=NoLoc) . locOf

inlineInStms :: MonadFreshNames m =>
                M.Map Name (FunDef SOACS) -> Stms SOACS -> m (Stms SOACS)
inlineInStms fdmap stms =
  bodyStms <$> inlineInBody fdmap (mkBody stms [])

inlineInBody :: MonadFreshNames m =>
                M.Map Name (FunDef SOACS) -> Body -> m Body
inlineInBody fdmap = onBody
  where inline (Let pat aux (Apply fname args _ what) : rest)
          | Just fd <- M.lookup fname fdmap =
              (<>) <$> inlineFunction pat aux args what fd <*> inline rest

        inline (stm : rest) =
          (:) <$> onStm stm <*> inline rest
        inline [] =
          pure mempty

        onBody (Body attr stms res) =
          Body attr . stmsFromList <$> inline (stmsToList stms) <*> pure res

        onStm (Let pat aux e) =
          Let pat aux <$> mapExpM inliner e

        inliner =
          identityMapper { mapOnBody = const onBody
                         , mapOnOp = onSOAC
                         }

        onSOAC =
          mapSOACM identitySOACMapper
          { mapOnSOACLambda = onLambda }

        onLambda (Lambda params body ret) =
          Lambda params <$> onBody body <*> pure ret

addLocations :: Safety -> [SrcLoc] -> Stms SOACS -> Stms SOACS
addLocations caller_safety more_locs = fmap onStm
  where onStm stm = stm { stmExp = onExp $ stmExp stm }
        onExp (Apply fname args t (safety, loc,locs)) =
          Apply fname args t (min caller_safety safety, loc,locs++more_locs)
        onExp (BasicOp (Assert cond desc (loc,locs))) =
          case caller_safety of
            Safe -> BasicOp $ Assert cond desc (loc,locs++more_locs)
            Unsafe -> BasicOp $ SubExp $ Constant Checked
        onExp (Op soac) = Op $ runIdentity $ mapSOACM
                          identitySOACMapper { mapOnSOACLambda = return . onLambda
                                             } soac
        onExp e = mapExp identityMapper { mapOnBody = const $ return . onBody
                                        } e
        onBody body =
          body { bodyStms = addLocations caller_safety more_locs $ bodyStms body }
        onLambda :: Lambda -> Lambda
        onLambda lam = lam { lambdaBody = onBody $ lambdaBody lam }

-- | Inline all functions and remove the resulting dead functions.
inlineFunctions :: Pass SOACS SOACS
inlineFunctions =
  Pass { passName = "Inline functions"
       , passDescription = "Inline and remove resulting dead functions."
       , passFunction = pass
       }
  where pass prog@(Prog consts funs) = do
          let cg = buildCallGraph prog
          (consts', funs') <- aggInlineFunctions cg (consts, funs)
          copyPropagateInProg simpleSOACS $ Prog consts' funs'

-- | @removeDeadFunctions prog@ removes the functions that are unreachable from
-- the main function from the program.
removeDeadFunctions :: Pass SOACS SOACS
removeDeadFunctions =
  Pass { passName = "Remove dead functions"
       , passDescription = "Remove the functions that are unreachable from entry points"
       , passFunction = return . pass
       }
  where pass prog =
          let cg        = buildCallGraph prog
              live_funs = filter ((`isFunInCallGraph` cg) . funDefName) $
                          progFuns prog
          in prog { progFuns = live_funs }
