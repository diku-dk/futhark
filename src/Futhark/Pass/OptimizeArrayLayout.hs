module Futhark.Pass.OptimizeArrayLayout (optimizeArrayLayout, printAST) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases
import Futhark.Pass
import Futhark.Pass.OptimizeArrayLayout.Layout
import Futhark.Pass.OptimizeArrayLayout.Transform

printAST :: (RepTypes rep) => Pass rep rep
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
optimizeArrayLayout :: (Transform rep, BuilderOps rep) => Pass rep rep
optimizeArrayLayout =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- return
    $ \prog -> do
      -- Analyse the program
      let indexTable = analysisPropagateByTransitivity $ analyzeDimAccesss prog

      -- let analysisRes2 =
      -- Compute permutations to acheive coalescence for all arrays
      let permutationTable = permutationTableFromIndexTable indexTable
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutationTable) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

type PEMap = M.Map VName (PrimExp VName)

funPrimExp :: (RepTypes rep) => Scope rep -> FunDef rep -> PEMap
funPrimExp scope fundef = execState (bodyPrimExps scope (funDefBody fundef)) mempty

bodyPrimExps :: (RepTypes rep) => Scope rep -> Body rep -> State PEMap ()
bodyPrimExps scope body = mapM_ (stmPrimExps scope') (bodyStms body)
  where
    scope' = scope <> scopeOf (bodyStms body)

stmPrimExps :: (RepTypes rep) => Scope rep -> Stm rep -> State PEMap ()
stmPrimExps scope stm = do
  m <- get
  let toPrimExp v = case M.lookup v m of
        Just pe -> Just pe
        Nothing -> case fmap typeOf . M.lookup v $ scope of
          (Just (Prim pt)) -> Just $ LeafExp v pt
          _ -> Nothing
  case stm of
    (Let (Pat [PatElem v _]) aux e)
      | Just pe <- primExpFromExp toPrimExp e -> modify $ M.insert v pe
    _ -> walkExpM walker (stmExp stm)
  where
    walker =
      identityWalker
        { walkOnBody = \body_scope body -> bodyPrimExps (scope <> body_scope) body
        -- , walkOnOp = undefined
        }
