module Futhark.Analysis.AnalyzePrimExp
  ( primExpAnalysis,
    stmToPrimExps,
    PrimExpAnalysis (..),
    PrimExpTable,
  )
where

import Control.Monad.State.Strict
import Data.Foldable
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem

-- TODO: document
type PrimExpTable = M.Map VName (Maybe (PrimExp VName))

class (Analyze rep) => PrimExpAnalysis rep where
  opPrimExp :: (RepTypes rep) => Scope rep -> Op rep -> State PrimExpTable ()

primExpAnalysis :: (PrimExpAnalysis rep, RepTypes rep) => Prog rep -> PrimExpTable
primExpAnalysis prog = foldMap' (uncurry funToPrimExp) scopesAndFuns
  where
    scopesAndFuns = do
      let fun_defs = progFuns prog
      let scopes = map getScope fun_defs
      zip scopes fun_defs

    getScope funDef = scopeOf (progConsts prog) <> scopeOfFParams (funDefParams funDef)

funToPrimExp :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> FunDef rep -> PrimExpTable
funToPrimExp scope fundef = execState (bodyToPrimExps scope (funDefBody fundef)) initialState
  where
    initialState =
      M.singleton (VName "slice" 0) $ Just $ LeafExp (VName "slice" 0) $ IntType Int64

-- | Adds the statements of a body to the PrimExpTable
bodyToPrimExps :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> Body rep -> State PrimExpTable ()
bodyToPrimExps scope body = mapM_ (stmToPrimExps scope') (bodyStms body)
  where
    scope' = scope <> scopeOf (bodyStms body)

-- | Adds the statements of a kernel body to the PrimExpTable
kernelToBodyPrimExps :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> KernelBody rep -> State PrimExpTable ()
kernelToBodyPrimExps scope kbody = mapM_ (stmToPrimExps scope') (kernelBodyStms kbody)
  where
    scope' = scope <> scopeOf (kernelBodyStms kbody)

-- | Adds a statement to the PrimExpTable. If it can't be resolved as a `PrimExp`,
-- it will be added as `Nothing`.
stmToPrimExps :: forall rep. (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> Stm rep -> State PrimExpTable ()
stmToPrimExps scope stm = do
  primExpTable <- get
  case stm of
    (Let (Pat pat_elems) _ e)
      | Just primExp <- primExpFromExp (toPrimExp scope primExpTable) e ->
          -- The statement can be resolved as a `PrimExp`.
          -- For each pattern element, insert the PrimExp in the primExpTable
          forM_ pat_elems $ \(PatElem name _) -> modify $ M.insert name (Just primExp)
      | otherwise -> do
          -- The statement can't be resolved as a `PrimExp`.
          walk (stmExp stm) -- Traverse the rest of the AST
          primExpTable' <- get -- Get the updated PrimExpTable after traversing the AST
          -- Add pattern elements that can't be resolved as `PrimExp` to the `PrimExpTable` as `Nothing`
          forM_ pat_elems $ \(PatElem name _) -> case M.lookup name primExpTable' of
            Nothing -> modify $ M.insert name Nothing
            _ -> pure ()
  where
    walk e = do
      -- Handle most cases using the walker
      walkExpM walker e
      -- Additionally, handle loop parameters
      case e of
        (Loop _ (ForLoop iter t subExp) _) -> do
          let primExp = primExpFromSubExp (IntType t) subExp
          modify $ M.insert iter (Just primExp)
        _ -> pure ()

    walker =
      (identityWalker @rep)
        { walkOnBody = \body_scope -> bodyToPrimExps (scope <> body_scope),
          walkOnOp = opPrimExp scope,
          walkOnFParam = paramToPrimExp -- Loop parameters
        }

    -- Adds a loop parameter to the PrimExpTable
    paramToPrimExp :: FParam rep -> State PrimExpTable ()
    paramToPrimExp param = do
      let name = paramName param
      -- Construct a `PrimExp` from the type of the parameter
      -- and add it to the `PrimExpTable`
      case typeOf $ paramDec param of
        -- TODO: Handle other types?
        Prim pt ->
          modify $ M.insert name (Just $ LeafExp name pt)
        _ -> pure ()

-- | Checks if a name is in the PrimExpTable and construct a `PrimExp` if it is not
toPrimExp :: (RepTypes rep) => Scope rep -> PrimExpTable -> VName -> Maybe (PrimExp VName)
toPrimExp scope primExpTable name = case M.lookup name primExpTable of
  Just maybePrimExp
    | Just primExp <- maybePrimExp -> Just primExp -- Already in the table
  _ -> case fmap typeOf . M.lookup name $ scope of
    (Just (Prim pt)) -> Just $ LeafExp name pt
    _ -> Nothing

-- | Adds the parameters of a SegOp as well as the statements in its body
-- to the PrimExpTable
segOpToPrimExps :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> SegOp lvl rep -> State PrimExpTable ()
segOpToPrimExps scope op = do
  segOpParamsToPrimExps scope op -- Add the parameters of the SegOp to the PrimExpTable
  kernelToBodyPrimExps scope (segBody op) -- Recurse into the kernel body

-- | Adds the parameters of a SegOp to the PrimExpTable
segOpParamsToPrimExps :: (RepTypes rep) => Scope rep -> SegOp lvl rep -> State PrimExpTable ()
segOpParamsToPrimExps scope op = do
  primExpTable <- get
  let params = unSegSpace $ segSpace op :: [(VName, SubExp)]
  forM_ params $ \(name, subExpr) ->
    case primExpFromSubExpM (toPrimExp scope primExpTable) subExpr of
      Just primExp -> modify $ M.insert name (Just primExp)
      Nothing -> pure ()

instance PrimExpAnalysis GPU where
  opPrimExp scope gpu_op
    | (SegOp op) <- gpu_op = segOpToPrimExps scope op
    | (SizeOp _) <- gpu_op = pure ()
    | (GPUBody _ body) <- gpu_op = bodyToPrimExps scope body
    | (Futhark.IR.GPUMem.OtherOp _) <- gpu_op = pure ()

instance PrimExpAnalysis MC where
  opPrimExp scope mc_op
    | (ParOp maybe_par_segop seq_segop) <- mc_op = do
        -- Add the statements in the parallel part of the ParOp to the PrimExpTable
        case maybe_par_segop of
          Nothing -> pure ()
          Just _ -> forM_ maybe_par_segop $ segOpToPrimExps scope
        -- Add the statements in the sequential part of the ParOp to the PrimExpTable
        segOpToPrimExps scope seq_segop
    | (Futhark.IR.MCMem.OtherOp _) <- mc_op = pure ()
