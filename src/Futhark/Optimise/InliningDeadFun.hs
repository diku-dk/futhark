{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( CallGraph
  , buildCallGraph
  , aggInlineDriver
  , deadFunElim
  )
  where

import Control.Arrow
import Control.Applicative
import Control.Monad.Reader

import Data.List
import Data.Maybe

import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.Basic
import Futhark.Renamer
import Futhark.Analysis.CallGraph
import Futhark.Optimise.Errors
import Futhark.Binder

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
-- | @aggInlineDriver prog@ performs aggressive inlining for all
-- functions in @prog@ by repeatedly inlining the functions with
-- empty-apply-callee set into other callers.  Afterwards, all dead
-- functions are removed.
aggInlineDriver :: Prog -> Either Error Prog
aggInlineDriver prog = do
  cg  <- buildCallGraph prog
  env <- CGEnv <$> buildFunctionTable prog
  renameProg <$> (deadFunElim =<< runCGM (aggInlining cg) env)

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
    else do let cg'  = HM.map ( \(callees,l) -> ( (\\) callees inlcand, l ) ) cg
            let work'= map ( \(x,(y,_)) -> (x,y) ) work
            newfuns  <- mapM processfun work'
            let newfunnms  = fst (unzip work')
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
            HM.filter (\(callees,_) -> not (null callees)) .
            HM.map (first $ intersect inlcand)

        funHasNoCalls :: ([Name],[Name]) -> Bool
        funHasNoCalls (callees,_) = null callees


-- | @doInlineInCaller caller inlcallees@ inlines in @calleer@ the functions
-- in @inlcallees@. At this point the preconditions are that if @inlcallees@
-- is not empty, and, more importantly, the functions in @inlcallees@ do
-- not call any other functions. Further extensions that transform a
-- tail-recursive function to a do or while loop, should do the transformation
-- first and then do the inlining.
doInlineInCaller :: FunDec ->  [FunDec] -> FunDec
doInlineInCaller (FunDec name rtp args body loc) inlcallees =
  let body' = inlineInBody inlcallees body
  in FunDec name rtp args body' loc

inlineInBody :: [FunDec] -> Body -> Body
inlineInBody inlcallees (Body _ (bnd@(Let pat _ (Apply fname args rtp _)):bnds) res) =
  let continue callbnds =
        callbnds `insertBindings` inlineInBody inlcallees (mkBody bnds res)
      continue' (Body _ callbnds res') =
        continue $ callbnds ++
        [ mkLet [v] $ PrimOp $ SubExp e'
        | (v,e') <- zip (patternIdents pat) $ withShapes $ resultSubExps res' ]
  in  case filter ((== fname) . funDecName) inlcallees of
        [] -> continue [bnd]
        FunDec _ _ fargs body _:_ ->
          let revbnds = zip (map bindeeIdent fargs) $ map fst args
          in  continue' $ foldr addArgBnd body revbnds
  where
      addArgBnd :: (Ident, SubExp) -> Body -> Body
      addArgBnd (farg, aarg) body =
          [mkLet [farg] $ PrimOp $ SubExp aarg] `insertBindings` body
      withShapes ses = existentialShapes rtp (map subExpType ses) ++ ses
inlineInBody inlcallees b = mapBody (inliner inlcallees) b

inliner :: Monad m => [FunDec] -> Mapper Basic Basic m
inliner funs = identityMapper {
                 mapOnLambda = return . inlineInLambda funs
               , mapOnBody = return . inlineInBody funs
               , mapOnBinding = return . inlineInBinding funs
               }

inlineInBinding :: [FunDec] -> Binding -> Binding
inlineInBinding inlcallees (Let pat _ e) = mkLetPat pat $ mapExp (inliner inlcallees) e

inlineInLambda :: [FunDec] -> Lambda -> Lambda
inlineInLambda inlcallees (Lambda params body ret loc) =
  Lambda params (inlineInBody inlcallees body) ret loc

------------------------------------------------------------------
------------------  Dead Function Elimination --------------------
------------------------------------------------------------------
-- | @deadFunElim prog@ removes the functions that are unreachable from
-- the main function from the program.
-- The functions called (indirectly) via SOACs are obviously considered.
deadFunElim :: Prog -> Either Error Prog
deadFunElim prog = do
  ftable <- buildFunctionTable prog
  cg     <- buildCallGraph prog
  let ftable' = HM.filter (isFunInCallGraph cg) ftable
  return $ Prog $ HM.elems ftable'
  where
    isFunInCallGraph cg fundec = isJust $ HM.lookup (funDecName fundec) cg
