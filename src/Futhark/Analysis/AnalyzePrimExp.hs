module Futhark.Analysis.AnalyzePrimExp
  ( primExpAnalysis,
    PrimExpAnalysis (..),
  )
where

import Control.Monad.State.Strict
import Data.Foldable
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
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

-- TODO: move stuff below to a new file

class (Analyze rep) => PrimExpAnalysis rep where
  opPrimExp :: (RepTypes rep) => Scope rep -> Op rep -> State PrimExpTable ()

primExpAnalysis :: (PrimExpAnalysis rep, RepTypes rep) => Prog rep -> PrimExpTable
primExpAnalysis prog = foldMap' (uncurry funPrimExp) scopesAndFuns
  where
    scopesAndFuns = do
      let funDefs = progFuns prog
      let scopes = map getScope funDefs
      zip scopes funDefs

    getScope funDef = scopeOf (progConsts prog) <> scopeOfFParams (funDefParams funDef)

-- TODO: document
type PrimExpTable = M.Map VName (Maybe (PrimExp VName))

funPrimExp :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> FunDef rep -> PrimExpTable
funPrimExp scope fundef = execState (bodyPrimExps scope (funDefBody fundef)) mempty

bodyPrimExps :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> Body rep -> State PrimExpTable ()
bodyPrimExps scope body = mapM_ (stmPrimExps scope') (bodyStms body)
  where
    scope' = scope <> scopeOf (bodyStms body)

kernelBodyPrimExps :: (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> KernelBody rep -> State PrimExpTable ()
kernelBodyPrimExps scope kbody = mapM_ (stmPrimExps scope') (kernelBodyStms kbody)
  where
    scope' = scope <> scopeOf (kernelBodyStms kbody)

stmPrimExps :: forall rep. (PrimExpAnalysis rep, RepTypes rep) => Scope rep -> Stm rep -> State PrimExpTable ()
stmPrimExps scope stm = do
  primExpTable <- get
  case stm of
    (Let (Pat patElems) aux exp)
      | Just primExp <- primExpFromExp (toPrimExp primExpTable) exp ->
          -- For each pattern element, insert the PrimExp in the primExpTable
          forM_ patElems $ \(PatElem name _) -> modify $ M.insert name (Just primExp)
      | otherwise -> do
          let state' = walkExpM (walker patElems) (stmExp stm)
          let primExpTable' = execState state' primExpTable
          let state'' = forM_ patElems $ \(PatElem name _) -> case M.lookup name primExpTable' of
                Just pe -> pure ()
                Nothing -> modify $ M.insert name Nothing
          let primExpTable'' = execState state'' primExpTable'
          put (primExpTable'' <> primExpTable')
  where
    toPrimExp :: PrimExpTable -> VName -> Maybe (PrimExp VName)
    toPrimExp primExpTable name = case M.lookup name primExpTable of
      Just maybePrimExp -> case maybePrimExp of -- Already in the table
        Just primExp -> Just primExp
        Nothing -> case fmap typeOf . M.lookup name $ scope of
          (Just (Prim pt)) -> Just $ LeafExp name pt
          _ -> Nothing
      Nothing -> case fmap typeOf . M.lookup name $ scope of
        (Just (Prim pt)) -> Just $ LeafExp name pt
        _ -> Nothing

    walker patElems =
      (identityWalker @rep)
        { walkOnBody = \body_scope -> bodyPrimExps (scope <> body_scope),
          walkOnOp = opPrimExp scope
        }

segOpPrimExpsGPU :: Scope GPU -> SegOp SegLevel GPU -> State PrimExpTable ()
segOpPrimExpsGPU scope (SegMap lvl space ts body) = kernelBodyPrimExps scope body
segOpPrimExpsGPU scope (SegRed lvl space reds ts lam) = undefined -- TODO: Handle this.
segOpPrimExpsGPU scope (SegScan lvl space scans ts body) = kernelBodyPrimExps scope body
segOpPrimExpsGPU scope (SegHist lvl space ops ts body) = kernelBodyPrimExps scope body

instance PrimExpAnalysis GPU where
  opPrimExp scope gpuOp
    | (SegOp op) <- gpuOp = segOpPrimExpsGPU scope op
    | (SizeOp op) <- gpuOp = pure () -- TODO: Handle this differently?
    | (GPUBody ts body) <- gpuOp = undefined -- TODO: Handle this.
    | (Futhark.IR.GPUMem.OtherOp op) <- gpuOp = undefined -- TODO: Handle this.

instance PrimExpAnalysis MC where
  opPrimExp = error $ notImplementedYet "MCMem"

instance PrimExpAnalysis GPUMem where
  opPrimExp = error $ notImplementedYet "GPUMem"

instance PrimExpAnalysis MCMem where
  opPrimExp = error $ notImplementedYet "MCMem"

instance PrimExpAnalysis Seq where
  opPrimExp = error $ notImplementedYet "Seq"

instance PrimExpAnalysis SeqMem where
  opPrimExp = error $ notImplementedYet "SeqMem"

instance PrimExpAnalysis SOACS where
  opPrimExp = error $ notImplementedYet "SOACS"
