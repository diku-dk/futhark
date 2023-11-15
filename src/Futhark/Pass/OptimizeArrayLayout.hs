module Futhark.Pass.OptimizeArrayLayout (optimizeArrayLayout, printAST) where

import Control.Monad.State.Strict
import Data.Foldable
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
      -- Compute primExps for all variables
      let primExpMap = primExpAnalysis prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutationTable = permutationTableFromIndexTable indexTable
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutationTable) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

-- TODO: move stuff below to a new file

primExpAnalysis :: (RepTypes rep) => Prog rep -> PrimExpTable
primExpAnalysis prog = foldMap' (uncurry funPrimExp) scopesAndFuns
  where
    scopesAndFuns = do
      let funDefs = progFuns prog
      let scopes = map getScope funDefs
      zip scopes funDefs

    getScope funDef = scopeOf (progConsts prog) <> scopeOfFParams (funDefParams funDef)

-- TODO: document
type PrimExpTable = M.Map VName (PrimExp VName)

funPrimExp :: (RepTypes rep) => Scope rep -> FunDef rep -> PrimExpTable
funPrimExp scope fundef = execState (bodyPrimExps scope (funDefBody fundef)) mempty

bodyPrimExps :: (RepTypes rep) => Scope rep -> Body rep -> State PrimExpTable ()
bodyPrimExps scope body = mapM_ (stmPrimExps scope') (bodyStms body)
  where
    scope' = scope <> scopeOf (bodyStms body)

stmPrimExps :: (RepTypes rep) => Scope rep -> Stm rep -> State PrimExpTable ()
stmPrimExps scope stm = do
  primExpTable <- get
  case stm of
    (Let (Pat [PatElem name _]) aux exp)
      | Just patElm <- primExpFromExp (toPrimExp primExpTable) exp -> modify $ M.insert name patElm
    _ -> walkExpM walker (stmExp stm)
  where
    toPrimExp primExpTable name = case M.lookup name primExpTable of
      Just pe -> Just pe
      Nothing -> case fmap typeOf . M.lookup name $ scope of
        (Just (Prim pt)) -> Just $ LeafExp name pt
        _ -> Nothing

    walker =
      identityWalker
        { walkOnBody = \body_scope body -> bodyPrimExps (scope <> body_scope) body
        -- , walkOnOp = undefined
        }
