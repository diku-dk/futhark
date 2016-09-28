-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineAggressively
  , removeDeadFunctions
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.SOACS
import Futhark.Transform.Rename
import Futhark.Analysis.CallGraph
import Futhark.Binder
import Futhark.Pass

-- | The symbol table for functions
data CGEnv = CGEnv { envFtable  :: HM.HashMap Name FunDef }

type CGM = Reader CGEnv

runCGM :: CGM a -> CGEnv -> a
runCGM = runReader

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
-- | This pass performs aggressive inlining for all
-- functions in @prog@ by repeatedly inlining the functions with
-- empty-apply-callee set into other callers.
inlineAggressively :: Pass SOACS SOACS
inlineAggressively =
  Pass { passName = "inline functions"
       , passDescription = "Inline all non-recursive functions."
       , passFunction = inline
       }
  where inline prog = do
          let cg = buildCallGraph prog
              env = CGEnv $ buildFunctionTable prog
          renameProg $ runCGM (aggInlining cg) env

-- | Bind a name as a common (non-merge) variable.
bindVarFtab :: CGEnv -> (Name, FunDef) -> CGEnv
bindVarFtab env (name,val) =
  env { envFtable = HM.insert name val $ envFtable env }

bindVarsFtab :: CGEnv -> [(Name, FunDef)] -> CGEnv
bindVarsFtab = foldl bindVarFtab

bindingFtab :: [(Name, FunDef)] -> CGM a -> CGM a
bindingFtab bnds = local (`bindVarsFtab` bnds)

aggInlining :: CallGraph -> CGM Prog
aggInlining cg = do
    to_be_inlined <- filter funHasNoCalls <$> asks (HM.elems . envFtable)
    let names_of_to_be_inlined = map funDefName to_be_inlined

    if not $ any (callsAnyOf names_of_to_be_inlined) $ HM.elems cg
    -- Nothing to inline, hence gather the program from the ftable and
    -- return it.
      then Prog <$> asks (HM.elems . envFtable)

    -- Remove the to-be-inlined functions from the call graph, and
    -- then, for each caller that exhibits to-be-inlined callees
    -- perform the inlining. Finally, update the `ftable' and iterate
    -- to a fix point.
      else do let cg' = HM.map (\\names_of_to_be_inlined) cg
                  processFun fname fundec
                    | Just True <- callsAnyOf names_of_to_be_inlined <$>
                                   HM.lookup fname cg =
                        doInlineInCaller fundec to_be_inlined
                    | otherwise =
                        fundec

              newfuns <- HM.mapWithKey processFun <$> asks envFtable
              bindingFtab (HM.toList newfuns) $ aggInlining cg'
    where
        known_funs = HM.keys cg

        callsAnyOf to_be_inlined = any (`elem` to_be_inlined)

        funHasNoCalls :: FunDef -> Bool
        funHasNoCalls fundec =
          case HM.lookup (funDefName fundec) cg of
            Just calls | not $ any (`elem` known_funs) calls -> True
            _                                                -> False

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
inliner funs = identityMapper { mapOnBody = return . inlineInBody funs
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

-- | @removeDeadFunctions prog@ removes the functions that are unreachable from
-- the main function from the program.
removeDeadFunctions :: Pass SOACS SOACS
removeDeadFunctions =
  Pass { passName = "Remove dead functions"
       , passDescription = "Remove the functions that are unreachable from the main function"
       , passFunction = return . pass
       }
  where pass prog =
          let ftable = buildFunctionTable prog
              cg     = buildCallGraph prog
              ftable' = HM.filter (isFunInCallGraph cg) ftable
          in Prog $ HM.elems ftable'
        isFunInCallGraph cg fundec = isJust $ HM.lookup (funDefName fundec) cg
