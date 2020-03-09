{-# LANGUAGE FlexibleContexts #-}
-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineAndRemoveDeadFunctions
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
import Futhark.Transform.CopyPropagate (copyPropagateInFun)
import Futhark.Transform.Rename
import Futhark.Analysis.CallGraph
import Futhark.Binder
import Futhark.Pass

aggInlining :: MonadFreshNames m => CallGraph -> [FunDef SOACS] -> m [FunDef SOACS]
aggInlining cg = fmap (filter keep) .
                 recurse 0 .
                 filter isFunInCallGraph
  where isFunInCallGraph fundec =
          isJust $ M.lookup (funDefName fundec) cg

        noCallsTo :: (Name -> Bool) -> FunDef SOACS -> Bool
        noCallsTo interesting fundec =
          case M.lookup (funDefName fundec) cg of
            Just calls | not $ any interesting calls -> True
            _                                        -> False

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
              inlined_but_entry_points =
                filter (isJust . funDefEntryPoint) to_be_inlined
          if null to_be_inlined
            then return funs
            else do let simplify
                          | i `rem` simplifyRate == 0 = simplifyFun
                          | otherwise = copyPropagateInFun simpleSOACS

                    let onFun = simplify <=< renameFun .
                                (`doInlineInCaller` to_be_inlined)
                    to_inline_in' <- recurse (i+1) . (not_to_inline_in++) =<<
                                     mapM onFun to_inline_in
                    return $ inlined_but_entry_points ++ to_inline_in'

        keep fundec = isJust (funDefEntryPoint fundec) || callsRecursive fundec

        callsRecursive fundec = maybe False (any recursive) $
                                M.lookup (funDefName fundec) cg

        recursive fname = case M.lookup fname cg of
                            Just calls -> fname `elem` calls
                            Nothing -> False

-- | @doInlineInCaller caller inlcallees@ inlines in @calleer@ the functions
-- in @inlcallees@. At this point the preconditions are that if @inlcallees@
-- is not empty, and, more importantly, the functions in @inlcallees@ do
-- not call any other functions. Further extensions that transform a
-- tail-recursive function to a do or while loop, should do the transformation
-- first and then do the inlining.
doInlineInCaller :: FunDef SOACS ->  [FunDef SOACS] -> FunDef SOACS
doInlineInCaller (FunDef entry name rtp args body) inlcallees =
  let body' = inlineInBody inlcallees body
  in FunDef entry name rtp args body'

inlineInBody :: [FunDef SOACS] -> Body -> Body
inlineInBody inlcallees (Body attr stms res) =
  Body attr (stmsFromList $ inline (stmsToList stms)) res
  where inline (Let pat aux (Apply fname args _ (safety,loc,locs)) : rest)
          | fun:_ <- filter ((== fname) . funDefName) inlcallees =
              let param_names =
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
              in param_stms <> body_stms <> res_stms <> inline rest
        inline (stm : rest) =
          inlineInStm inlcallees stm : inline rest
        inline [] = mempty

        reshapeIfNecessary dim_names ident se
          | t@Array{} <- identType ident,
            any (`elem` dim_names) $ subExpVars $ arrayDims t,
            Var v <- se =
              mkLet [] [ident] $ shapeCoerce (arrayDims t) v
          | otherwise =
              mkLet [] [ident] $ BasicOp $ SubExp se

notNoLoc :: SrcLoc -> Bool
notNoLoc = (/=NoLoc) . locOf

inliner :: Monad m => [FunDef SOACS] -> Mapper SOACS SOACS m
inliner funs = identityMapper { mapOnBody = const $ return . inlineInBody funs
                              , mapOnOp = return . inlineInSOAC funs
                              }

inlineInSOAC :: [FunDef SOACS] -> SOAC SOACS -> SOAC SOACS
inlineInSOAC inlcallees = runIdentity . mapSOACM identitySOACMapper
                          { mapOnSOACLambda = return . inlineInLambda inlcallees
                          }

inlineInStm :: [FunDef SOACS] -> Stm -> Stm
inlineInStm inlcallees (Let pat aux e) =
  Let pat aux $ mapExp (inliner inlcallees) e

inlineInLambda :: [FunDef SOACS] -> Lambda -> Lambda
inlineInLambda inlcallees (Lambda params body ret) =
  Lambda params (inlineInBody inlcallees body) ret

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

-- | A composition of 'inlineAggressively' and 'removeDeadFunctions',
-- to avoid the cost of type-checking the intermediate stage.
inlineAndRemoveDeadFunctions :: Pass SOACS SOACS
inlineAndRemoveDeadFunctions =
  Pass { passName = "Inline and remove dead functions"
       , passDescription = "Inline and remove resulting dead functions."
       , passFunction = pass
       }
  where pass prog = do
          let cg = buildCallGraph prog
          Prog <$> aggInlining cg (progFuns prog)

-- | @removeDeadFunctions prog@ removes the functions that are unreachable from
-- the main function from the program.
removeDeadFunctions :: Pass SOACS SOACS
removeDeadFunctions =
  Pass { passName = "Remove dead functions"
       , passDescription = "Remove the functions that are unreachable from the main function"
       , passFunction = return . pass
       }
  where pass prog =
          let cg        = buildCallGraph prog
              live_funs = filter (isFunInCallGraph cg) (progFuns prog)
          in Prog live_funs
        isFunInCallGraph cg fundec = isJust $ M.lookup (funDefName fundec) cg
