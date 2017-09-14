-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineAndRemoveDeadFunctions
  , removeDeadFunctions
  )
  where

import Control.Monad.Reader
import Control.Monad.Identity

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude

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

        keep fundec = isJust (funDefEntryPoint fundec) || recursive fundec

        recursive fundec = case M.lookup (funDefName fundec) cg of
                             Just calls -> funDefName fundec `elem` calls
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
inlineInBody
  inlcallees
  (Body _ (bnd@(Let pat _ (Apply fname args rtp)):bnds) res) =
  let continue callbnds =
        callbnds `insertStms` inlineInBody inlcallees (mkBody bnds res)
      continue' (Body _ callbnds res') =
        continue $ callbnds ++
        zipWith reshapeIfNecessary (patternIdents pat)
        (runReader (withShapes res') $ scopeOf callbnds)
  in case filter ((== fname) . funDefName) inlcallees of
       [] -> continue [bnd]
       fun:_ ->
         let revbnds = zip (map paramIdent $ funDefParams fun) $ map fst args
         in  continue' $ foldr addArgBnd (funDefBody fun) revbnds
  where

      addArgBnd :: (Ident, SubExp) -> Body -> Body
      addArgBnd (farg, aarg) body =
        reshapeIfNecessary farg aarg `insertStm` body

      withShapes ses = do
        ts <- mapM subExpType ses
        return $
          extractShapeContext (retTypeValues rtp) (map arrayDims ts) ++
          ses

      reshapeIfNecessary ident se
        | t@Array{} <- identType ident,
          Var v <- se =
            mkLet' [] [ident] $ shapeCoerce [] (arrayDims t) v
        | otherwise =
          mkLet' [] [ident] $ BasicOp $ SubExp se
inlineInBody inlcallees (Body () (bnd:bnds) res) =
  let bnd' = inlineInStm inlcallees bnd
      Body () bnds' res' = inlineInBody inlcallees $ Body () bnds res
  in Body () (bnd':bnds') res'
inlineInBody _ (Body () [] res) =
  Body () [] res

inliner :: Monad m => [FunDef] -> Mapper SOACS SOACS m
inliner funs = identityMapper { mapOnBody = const $ return . inlineInBody funs
                              , mapOnOp = return . inlineInSOAC funs
                              }

inlineInSOAC :: [FunDef] -> SOAC SOACS -> SOAC SOACS
inlineInSOAC inlcallees = runIdentity . mapSOACM identitySOACMapper
                          { mapOnSOACLambda = return . inlineInLambda inlcallees
                          , mapOnSOACExtLambda = return . inlineInExtLambda inlcallees
                          }

inlineInStm :: [FunDef] -> Stm -> Stm
inlineInStm inlcallees (Let pat () e) = Let pat () $ mapExp (inliner inlcallees) e

inlineInLambda :: [FunDef] -> Lambda -> Lambda
inlineInLambda inlcallees (Lambda params body ret) =
  Lambda params (inlineInBody inlcallees body) ret

inlineInExtLambda :: [FunDef] -> ExtLambda -> ExtLambda
inlineInExtLambda inlcallees (ExtLambda params body ret) =
  ExtLambda params (inlineInBody inlcallees body) ret

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
