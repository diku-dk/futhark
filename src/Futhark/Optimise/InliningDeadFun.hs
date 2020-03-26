{-# LANGUAGE FlexibleContexts #-}
-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineFunctions
  , inlineConstants
  , removeDeadFunctions
  )
  where

import Control.Monad.Identity
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Futhark.Representation.SOACS
import Futhark.Representation.SOACS.Simplify (simpleSOACS, simplifyFun)
import Futhark.Optimise.CSE
import Futhark.Transform.CopyPropagate (copyPropagateInFun)
import Futhark.Transform.Rename
import Futhark.Analysis.CallGraph
import Futhark.Binder
import Futhark.Pass

aggInlineFunctions :: MonadFreshNames m =>
                      CallGraph -> [FunDef SOACS] -> m [FunDef SOACS]
aggInlineFunctions cg =
  fmap (filter (keep mempty)) . recurse 0 . filter isFunInCallGraph
  where isFunInCallGraph fundec =
          isJust $ M.lookup (funDefName fundec) cg

        constfuns =
          S.fromList $ M.keys $ M.filter (==ConstFun) $ mconcat $ M.elems cg

        fdmap fds =
          M.fromList $ zip (map funDefName fds) fds

        noCallsTo :: (Name -> Bool) -> FunDef SOACS -> Bool
        noCallsTo interesting fundec =
          case M.lookup (funDefName fundec) cg of
            Just calls -> not $ any interesting (M.keys calls)
            _ -> False

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
        recurse i funs = do
          let remaining = S.fromList $ map funDefName funs
              (to_be_inlined, maybe_inline_in) =
                partition (noCallsTo (`S.member` remaining)) funs
              (not_to_inline_in, to_inline_in) =
                partition (noCallsTo
                           (`elem` map funDefName to_be_inlined))
                maybe_inline_in
              keep_around =
                S.fromList $ map funDefName $
                filter expensiveConstant to_be_inlined
              not_actually_inlined =
                filter (keep keep_around) to_be_inlined
          if null to_be_inlined
            then return funs
            else do let simplify
                          | i `rem` simplifyRate == 0 =
                              copyPropagateInFun simpleSOACS <=<
                              pure . performCSEOnFunDef True <=<
                              simplifyFun
                          | otherwise = copyPropagateInFun simpleSOACS

                    let onFun = simplify <=< renameFun .
                                doInlineInCaller (fdmap to_be_inlined) False
                    to_inline_in' <- mapM onFun to_inline_in
                    (not_actually_inlined<>) <$>
                      recurse (i+1) (not_to_inline_in <> to_inline_in')

        keep keep_around fd =
          isJust (funDefEntryPoint fd)
          || callsRecursive fd
          || funDefName fd `S.member` keep_around

        expensiveConstant fd =
          funDefName fd `S.member` constfuns &&
          not (null (bodyStms (funDefBody fd)))

        callsRecursive fd = maybe False (any recursive . M.keys) $
                            M.lookup (funDefName fd) cg

        recursive fname = case M.lookup fname cg of
                            Just calls -> fname `M.member` calls
                            Nothing -> False

-- | @doInlineInCaller constf fdmap caller@ inlines in @calleer@
-- the functions in @fdmap@ that are called as @constf@. At this
-- point the preconditions are that if @fdmap@ is not empty, and,
-- more importantly, the functions in @fdmap@ do not call any
-- other functions. Further extensions that transform a tail-recursive
-- function to a do or while loop, should do the transformation first
-- and then do the inlining.
doInlineInCaller :: M.Map Name (FunDef SOACS) -> Bool -> FunDef SOACS
                 -> FunDef SOACS
doInlineInCaller fdmap always_reshape (FunDef entry name rtp args body) =
  let body' = inlineInBody fdmap always_reshape body
  in FunDef entry name rtp args body'

inlineFunction :: Bool
               -> Pattern
               -> StmAux attr
               -> [(SubExp, Diet)]
               -> (ConstFun, Safety, SrcLoc, [SrcLoc])
               -> FunDef SOACS
               -> [Stm]
inlineFunction always_reshape pat aux args (_,safety,loc,locs) fun =
  param_stms <> body_stms <> res_stms
  where param_names =
          map paramName $ funDefParams fun

        param_stms =
          zipWith (reshapeIfNecessary param_names)
          (map paramIdent $ funDefParams fun) (map fst args)

        body_stms =
          stmsToList $
          addLocations safety (filter notNoLoc (loc:locs)) $
          bodyStms $ funDefBody fun

        res_stms =
          certify (stmAuxCerts aux) <$>
          zipWith (reshapeIfNecessary (patternNames pat))
          (patternIdents pat) (bodyResult $ funDefBody fun)

        reshapeIfNecessary dim_names ident se
          | t@Array{} <- identType ident,
            always_reshape || any (`elem` dim_names) (subExpVars $ arrayDims t),
            Var v <- se =
              mkLet [] [ident] $ shapeCoerce (arrayDims t) v
          | otherwise =
              mkLet [] [ident] $ BasicOp $ SubExp se

        notNoLoc = (/=NoLoc) . locOf

inlineInBody :: M.Map Name (FunDef SOACS) -> Bool -> Body -> Body
inlineInBody fdmap always_reshape = onBody
  where inline (Let pat aux (Apply fname args _ what) : rest)
          | Just fd <- M.lookup fname fdmap =
              inlineFunction always_reshape pat aux args what fd
              <> inline rest
        inline (stm : rest) =
          onStm stm : inline rest
        inline [] = mempty

        onBody (Body attr stms res) =
          Body attr (stmsFromList $ inline (stmsToList stms)) res

        onStm (Let pat aux e) =
          Let pat aux $ mapExp inliner e

        inliner =
          identityMapper { mapOnBody = const $ return . onBody
                         , mapOnOp = return . onSOAC
                         }

        onSOAC =
          runIdentity . mapSOACM identitySOACMapper
          { mapOnSOACLambda = return . onLambda }

        onLambda (Lambda params body ret) =
          Lambda params (onBody body) ret

addLocations :: Safety -> [SrcLoc] -> Stms SOACS -> Stms SOACS
addLocations caller_safety more_locs = fmap onStm
  where onStm stm = stm { stmExp = onExp $ stmExp stm }
        onExp (Apply fname args t (constf, safety, loc,locs)) =
          Apply fname args t (constf, min caller_safety safety, loc,locs++more_locs)
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

-- | Inline 'NotConstFun' functions and remove the resulting dead functions.
inlineFunctions :: Pass SOACS SOACS
inlineFunctions =
  Pass { passName = "Inline functions"
       , passDescription = "Inline and remove resulting dead functions."
       , passFunction = pass
       }
  where pass prog = do
          let cg = buildCallGraph prog
          Prog <$> aggInlineFunctions cg (progFuns prog)

aggInlineConstants :: [FunDef SOACS] -> [FunDef SOACS]
aggInlineConstants orig_fds =
  map inlineInEntry $ filter (isJust . funDefEntryPoint) orig_fds
  where fdmap = M.fromList $ zip (map funDefName orig_fds) orig_fds

        inlineInEntry fd =
          fd { funDefBody = constsInBody mempty $ funDefBody fd }

        constsInBody prev body =
          body { bodyStms = constsInStms prev (bodyStms body) }

        constsInStms prev stms =
          case stmsHead stms of
            Nothing -> mempty

            Just (Let pat aux (Apply fname args _ prop),
                  stms')
              | Just ses <- M.lookup fname prev ->
                  stmsFromList
                  (zipWith reshapeResult (patternIdents pat) ses)
                  <> constsInStms prev stms'

              | Just fd <- M.lookup fname fdmap ->
                  let stm_stms =
                        inlineFunction True pat aux args prop fd
                      prev' =
                        M.insert fname (map Var $ patternNames pat) prev
                  in constsInStms prev' $ stmsFromList stm_stms <> stms'

            Just (stm, stms') ->
              oneStm stm <> constsInStms prev stms'

        reshapeResult ident se
          | t@Array{} <- identType ident,
            Var v <- se =
              mkLet [] [ident] $ shapeCoerce (arrayDims t) v
          | otherwise =
              mkLet [] [ident] $ BasicOp $ SubExp se

-- | Inline 'ConstFun' functions and remove the resulting dead functions.
inlineConstants :: Pass SOACS SOACS
inlineConstants =
  Pass { passName = "Inline constants"
       , passDescription = "Inline and remove dead constants."
       , passFunction = pass
       }
  where pass prog = return $ Prog $ aggInlineConstants $ progFuns prog

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
              live_funs = filter (isFunInCallGraph cg) (progFuns prog)
          in Prog live_funs
        isFunInCallGraph cg fundec = isJust $ M.lookup (funDefName fundec) cg
