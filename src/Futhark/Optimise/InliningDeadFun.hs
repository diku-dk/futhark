{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Parallel.Strategies

import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify
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

aggInlineFunctions :: MonadFreshNames m => Prog SOACS -> m (Prog SOACS)
aggInlineFunctions prog =
  let Prog consts funs = prog
  in uncurry Prog . fmap (filter keep) <$>
     recurse 0 (ST.fromScope (addScopeWisdom (scopeOf consts)), consts, funs)
  where fdmap fds =
          M.fromList $ zip (map funDefName fds) fds

        cg = buildCallGraph prog
        noninlined = findNoninlined prog

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
              keep_although_inlined = filter keep to_be_inlined
          if null to_be_inlined
            then return (consts, funs)
            else do

            (vtable', consts') <-
              if any ((`calledByConsts` cg) . funDefName) to_be_inlined
              then simplifyConsts . performCSEOnStms True =<<
                   inlineInStms (fdmap to_be_inlined) consts
              else pure (vtable, consts)

            let simplifyFun' fd
                  | i `rem` simplifyRate == 0 =
                      copyPropagateInFun simpleSOACS vtable' .
                      performCSEOnFunDef True =<<
                      simplifyFun vtable' fd
                  | otherwise =
                      copyPropagateInFun simpleSOACS vtable' fd

            let onFun = simplifyFun' <=<
                        inlineInFunDef (fdmap to_be_inlined)
            to_inline_in' <- parMapM onFun to_inline_in
            fmap (keep_although_inlined<>) <$>
              recurse (i+1)
              (vtable', consts', not_to_inline_in <> to_inline_in')

        keep fd =
          isJust (funDefEntryPoint fd) ||
          funDefName fd `S.member` noninlined

-- | @inlineInFunDef constf fdmap caller@ inlines in @calleer@ the
-- functions in @fdmap@ that are called as @constf@. At this point the
-- preconditions are that if @fdmap@ is not empty, and, more
-- importantly, the functions in @fdmap@ do not call any other
-- functions.
inlineInFunDef :: MonadFreshNames m =>
                  M.Map Name (FunDef SOACS) -> FunDef SOACS
               -> m (FunDef SOACS)
inlineInFunDef fdmap (FunDef entry attrs name rtp args body) =
  FunDef entry attrs name rtp args <$> inlineInBody fdmap body

inlineFunction :: MonadFreshNames m =>
                  Pattern
               -> StmAux dec
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
        zipWith bindSubExp (patternIdents pat) res
  pure $ stmsToList stms <> res_stms
  where param_stms =
          zipWith bindSubExp
          (map paramIdent $ funDefParams fun) (map fst args)

        body_stms =
          stmsToList $
          addLocations (stmAuxAttrs aux) safety (filter notmempty (loc:locs)) $
          bodyStms $ funDefBody fun

        -- Note that the sizes of arrays may not be correct at this
        -- point - it is crucial that we run copy propagation before
        -- the type checker sees this!
        bindSubExp ident se =
          mkLet [] [ident] $ BasicOp $ SubExp se

        notmempty = (/=mempty) . locOf

inlineInStms :: MonadFreshNames m =>
                M.Map Name (FunDef SOACS) -> Stms SOACS -> m (Stms SOACS)
inlineInStms fdmap stms =
  bodyStms <$> inlineInBody fdmap (mkBody stms [])

inlineInBody :: MonadFreshNames m =>
                M.Map Name (FunDef SOACS) -> Body -> m Body
inlineInBody fdmap = onBody
  where inline (Let pat aux (Apply fname args _ what) : rest)
          | Just fd <- M.lookup fname fdmap,
            not $ "noinline" `inAttrs` funDefAttrs fd,
            not $ "noinline" `inAttrs` stmAuxAttrs aux =
              (<>) <$> inlineFunction pat aux args what fd <*> inline rest

        inline (stm : rest) =
          (:) <$> onStm stm <*> inline rest
        inline [] =
          pure mempty

        onBody (Body dec stms res) =
          Body dec . stmsFromList <$> inline (stmsToList stms) <*> pure res

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

-- Propagate source locations and attributes to the inlined
-- statements.  Attributes are propagated only when applicable (this
-- probably means that every supported attribute needs to be handled
-- specially here).
addLocations :: Attrs -> Safety -> [SrcLoc] -> Stms SOACS -> Stms SOACS
addLocations attrs caller_safety more_locs = fmap onStm
  where onStm (Let pat aux (Apply fname args t (safety, loc,locs))) =
          Let pat aux' $
          Apply fname args t (min caller_safety safety, loc,locs++more_locs)
          where aux' = aux { stmAuxAttrs = attrs <> stmAuxAttrs aux }
        onStm (Let pat aux (BasicOp (Assert cond desc (loc,locs)))) =
          Let pat (withAttrs (attrsForAssert attrs) aux) $
          case caller_safety of
            Safe -> BasicOp $ Assert cond desc (loc,locs++more_locs)
            Unsafe -> BasicOp $ SubExp $ Constant Checked
        onStm (Let pat aux (Op soac)) =
          Let pat (withAttrs attrs' aux) $ Op $ runIdentity $ mapSOACM
          identitySOACMapper { mapOnSOACLambda = return . onLambda
                             } soac
          where attrs' = attrs `withoutAttrs` for_assert
                for_assert = attrsForAssert attrs
                onLambda lam =
                  lam { lambdaBody = onBody for_assert $ lambdaBody lam }
        onStm (Let pat aux e) =
          Let pat aux $ onExp e

        onExp = mapExp identityMapper
                { mapOnBody = const $ return . onBody attrs }

        withAttrs attrs' aux = aux { stmAuxAttrs = attrs' <> stmAuxAttrs aux }

        onBody attrs' body =
          body { bodyStms = addLocations attrs' caller_safety more_locs $
                            bodyStms body }

-- | Inline all functions and remove the resulting dead functions.
inlineFunctions :: Pass SOACS SOACS
inlineFunctions =
  Pass { passName = "Inline functions"
       , passDescription = "Inline and remove resulting dead functions."
       , passFunction = copyPropagateInProg simpleSOACS <=< aggInlineFunctions
       }

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
