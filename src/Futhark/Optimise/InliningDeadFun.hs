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
import Futhark.Transform.Rename
import Futhark.Analysis.CallGraph
import Futhark.Binder
import Futhark.Pass

aggInlining :: CallGraph -> [FunDef] -> [FunDef]
aggInlining cg = filter keep . recurse
  where noInterestingCalls :: S.Set Name -> FunDef -> Bool
        noInterestingCalls interesting fundec =
          case M.lookup (funDefName fundec) cg of
            Just calls | not $ any (`elem` interesting') calls -> True
            _                                                  -> False
            where interesting' = funDefName fundec `S.insert` interesting

        recurse funs =
          let interesting = S.fromList $ map funDefName funs
              (to_be_inlined, to_inline_in) =
                partition (noInterestingCalls interesting) funs
              inlined_but_entry_points =
                filter (isJust . funDefEntryPoint) to_be_inlined
          in if null to_be_inlined then funs
             else inlined_but_entry_points ++
                  recurse (map (`doInlineInCaller` to_be_inlined) to_inline_in)

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
doInlineInCaller :: FunDef ->  [FunDef] -> FunDef
doInlineInCaller (FunDef entry name rtp args body) inlcallees =
  let body' = inlineInBody inlcallees body
  in FunDef entry name rtp args body'

inlineInBody :: [FunDef] -> Body -> Body
inlineInBody inlcallees (Body attr stms res) = Body attr stms' res
  where stms' = stmsFromList (concatMap inline $ stmsToList stms)

        inline (Let pat _ (Apply fname args _ (safety,loc,locs)))
          | fun:_ <- filter ((== fname) . funDefName) inlcallees =
              let param_stms = zipWith reshapeIfNecessary (map paramIdent $ funDefParams fun) (map fst args)
                  body_stms = stmsToList $ addLocations safety (loc:locs) $ bodyStms $ funDefBody fun
                  res_stms = zipWith reshapeIfNecessary (patternIdents pat) (bodyResult $ funDefBody fun)
              in param_stms ++ body_stms ++ res_stms
        inline stm = [inlineInStm inlcallees stm]

        reshapeIfNecessary ident se
          | t@Array{} <- identType ident,
            Var v <- se =
              mkLet [] [ident] $ shapeCoerce (arrayDims t) v
          | otherwise =
            mkLet [] [ident] $ BasicOp $ SubExp se

inliner :: Monad m => [FunDef] -> Mapper SOACS SOACS m
inliner funs = identityMapper { mapOnBody = const $ return . inlineInBody funs
                              , mapOnOp = return . inlineInSOAC funs
                              }

inlineInSOAC :: [FunDef] -> SOAC SOACS -> SOAC SOACS
inlineInSOAC inlcallees = runIdentity . mapSOACM identitySOACMapper
                          { mapOnSOACLambda = return . inlineInLambda inlcallees
                          }

inlineInStm :: [FunDef] -> Stm -> Stm
inlineInStm inlcallees (Let pat aux e) =
  Let pat aux $ mapExp (inliner inlcallees) e

inlineInLambda :: [FunDef] -> Lambda -> Lambda
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
          renameProg $ Prog $ aggInlining cg $ progFunctions prog

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
              live_funs = filter (isFunInCallGraph cg) (progFunctions prog)
          in Prog live_funs
        isFunInCallGraph cg fundec = isJust $ M.lookup (funDefName fundec) cg
