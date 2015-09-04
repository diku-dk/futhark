-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineAggressively
  , removeDeadFunctions
  )
  where

import Control.Applicative
import Control.Monad.Reader

import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.Basic
import Futhark.Transform.Rename
import Futhark.Analysis.CallGraph
import Futhark.Optimise.Errors
import Futhark.Binder
import Futhark.Pass

-- | The symbol table for functions
data CGEnv = CGEnv { envFtable  :: HM.HashMap Name FunDec }

type CGM = ReaderT CGEnv (Either Error)

runCGM :: CGM a -> CGEnv -> Either Error a
runCGM = runReaderT

badCGM :: Error -> CGM a
badCGM = lift . Left

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
-- | This pass performs aggressive inlining for all
-- functions in @prog@ by repeatedly inlining the functions with
-- empty-apply-callee set into other callers.
inlineAggressively :: Pass Basic Basic
inlineAggressively =
  Pass { passName = "inline functions"
       , passDescription = "Inline all non-recursive functions."
       , passFunction = inline
       }
  where inline prog = liftEither $ do
          let cg = buildCallGraph prog
              env = CGEnv $ buildFunctionTable prog
          renameProg <$> runCGM (aggInlining cg) env

-- | Bind a name as a common (non-merge) variable.
bindVarFtab :: CGEnv -> (Name, FunDec) -> CGEnv
bindVarFtab env (name,val) =
  env { envFtable = HM.insert name val $ envFtable env }

bindVarsFtab :: CGEnv -> [(Name, FunDec)] -> CGEnv
bindVarsFtab = foldl bindVarFtab

bindingFtab :: [(Name, FunDec)] -> CGM a -> CGM a
bindingFtab bnds = local (`bindVarsFtab` bnds)

-- | Remove the binding for a name.
remVarFtab :: CGEnv -> Name -> CGEnv
remVarFtab env name = env { envFtable = HM.delete name $ envFtable env }

remVarsFtab :: CGEnv -> [Name] -> CGEnv
remVarsFtab = foldl remVarFtab

remBindingsFtab :: [Name] -> CGM a -> CGM a
remBindingsFtab keys = local (`remVarsFtab` keys)



aggInlining :: CallGraph -> CGM Prog
aggInlining cg = do
    let inlcand    = HM.keys (HM.filter funHasNoCalls cg)
    let inlinf     = getInlineOps inlcand cg
    let work       = HM.toList inlinf

    if null work
    -- a fix point has been reached, hence gather the program from the
    -- hashtable and return it.
    then Prog <$> asks (HM.elems . envFtable)

    -- Remove the to-be-inlined functions from the Call Graph, and
    -- then, for each caller that exhibits to-be-inlined callees
    -- perform the inlining. Finally, update the `ftable' and iterate
    -- to a fix point. At this point it is guaranteed that `work' is
    -- not empty, hence there are functions to be inlined in each
    -- caller function (from `work').
    else do let cg'  = HM.map (\\inlcand) cg
            newfuns  <- mapM processfun work
            let newfunnms  = fst (unzip work)
            remBindingsFtab  newfunnms $
                bindingFtab (zip newfunnms newfuns) $
                aggInlining cg'
    where
        processfun :: (Name, [Name]) -> CGM FunDec
        processfun (fname, inlcallees) = do
            f  <- asks $ HM.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> do
                    tobeinl <- mapM getFromFtable inlcallees
                    return $ doInlineInCaller ff tobeinl

        getFromFtable :: Name -> CGM FunDec
        getFromFtable fname = do
            f <- asks $ HM.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> return ff

        getInlineOps :: [Name] -> CallGraph -> CallGraph
        getInlineOps inlcand =
            HM.filter (not . funHasNoCalls) .
            HM.map (intersect inlcand)

        known_funs = HM.keys cg

        funHasNoCalls :: [Name] -> Bool
        funHasNoCalls = not . any (`elem` known_funs)


-- | @doInlineInCaller caller inlcallees@ inlines in @calleer@ the functions
-- in @inlcallees@. At this point the preconditions are that if @inlcallees@
-- is not empty, and, more importantly, the functions in @inlcallees@ do
-- not call any other functions. Further extensions that transform a
-- tail-recursive function to a do or while loop, should do the transformation
-- first and then do the inlining.
doInlineInCaller :: FunDec ->  [FunDec] -> FunDec
doInlineInCaller (FunDec name rtp args body) inlcallees =
  let body' = inlineInBody inlcallees body
  in FunDec name rtp args body'

inlineInBody :: [FunDec] -> Body -> Body
inlineInBody
  inlcallees
  (Body _ (bnd@(Let pat _ (Apply fname args rtp)):bnds) res) =
  let continue callbnds =
        callbnds `insertBindings` inlineInBody inlcallees (mkBody bnds res)
      continue' (Body _ callbnds res') =
        continue $ callbnds ++
        zipWith reshapeIfNecessary (patternIdents pat)
        (runReader (withShapes res') $
         typeEnvFromBindings callbnds)
  in case filter ((== fname) . funDecName) inlcallees of
       [] -> continue [bnd]
       FunDec _ _ fargs body:_ ->
         let revbnds = zip (map paramIdent fargs) $ map fst args
         in  continue' $ foldr addArgBnd body revbnds
  where

      addArgBnd :: (Ident, SubExp) -> Body -> Body
      addArgBnd (farg, aarg) body =
        reshapeIfNecessary farg aarg `insertBinding` body

      withShapes ses = do
        ts <- mapM subExpType ses
        return $
          extractShapeContext (retTypeValues rtp) (map arrayDims ts) ++
          ses

      reshapeIfNecessary ident se
        | t@(Array {}) <- identType ident,
          Var v <- se =
            mkLet' [] [ident] $ shapeCoerce [] (arrayDims t) v
        | otherwise =
          mkLet' [] [ident] $ PrimOp $ SubExp se
inlineInBody inlcallees (Body () (bnd:bnds) res) =
  let bnd' = inlineInBinding inlcallees bnd
      Body () bnds' res' = inlineInBody inlcallees $ Body () bnds res
  in Body () (bnd':bnds') res'
inlineInBody _ (Body () [] res) =
  Body () [] res

inliner :: Monad m => [FunDec] -> Mapper Basic Basic m
inliner funs = identityMapper {
                 mapOnLambda = return . inlineInLambda funs
               , mapOnBody = return . inlineInBody funs
               , mapOnExtLambda = return . inlineInExtLambda funs
               }

inlineInBinding :: [FunDec] -> Binding -> Binding
inlineInBinding inlcallees (Let pat () e) = Let pat () $ mapExp (inliner inlcallees) e

inlineInLambda :: [FunDec] -> Lambda -> Lambda
inlineInLambda inlcallees (Lambda i params body ret) =
  Lambda i params (inlineInBody inlcallees body) ret

inlineInExtLambda :: [FunDec] -> ExtLambda -> ExtLambda
inlineInExtLambda inlcallees (ExtLambda i params body ret) =
  ExtLambda i params (inlineInBody inlcallees body) ret

-- | @removeDeadFunctions prog@ removes the functions that are unreachable from
-- the main function from the program.
removeDeadFunctions :: Pass Basic Basic
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
        isFunInCallGraph cg fundec = isJust $ HM.lookup (funDecName fundec) cg
