module Futhark.Optimise.IntraMMM (intraMMM, intraMMMMemFixup) where

import Futhark.Pass
  ( intraproceduralTransformation,
    Pass (..),
    PassM
  )
-- TODO: add specific imports
import Futhark.IR.GPU.Simplify (simplifyGPU)
import Futhark.IR.GPU
import Futhark.IR.GPU.Op
import Futhark.IR.Prop.Scope (Scope)
import Futhark.IR.Syntax
import Futhark.IR.GPUMem
import Futhark.IR.Mem
import Control.Monad ((>=>))
import Debug.Trace
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.Traversals
import Control.Monad.Identity
import Data.Foldable (find)
import Data.Foldable (fold, toList)
import Data.Maybe (catMaybes)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Lens.Lens
import Futhark.IR.Pretty


traceHelper :: Show a => a -> a
traceHelper x = trace (show x ++ "\n") $ x


intraMMM :: Pass GPU GPU
intraMMM =
  Pass
  "tensor-core-mma"
  "Extracts NVIDIA tensor core MMA operations" $
  transformation >=> simplifyGPU
  where transformation = intraproceduralTransformation onStmts


onStmts :: Scope GPU -> Stms GPU -> PassM (Stms GPU)
onStmts scope stms = pure $ transformStmts stms

-- TODO: use PassM below?
transformStmts :: Stms GPU -> Stms GPU
transformStmts stms = fmap transformStmt stms

transformStmt :: Stm GPU -> Stm GPU
transformStmt (Let pat aux e) = Let pat aux (transformExp e)

transformExp :: Exp GPU -> Exp GPU
--transformExp (BasicOp op) = BasicOp op
--transformExp (Apply name args rets info) = Apply name args rets info
transformExp (Match subExps cases body matchDec) = Match subExps (fmap transformCase cases) (transformBody body) matchDec
transformExp (Loop params form body) = Loop params form (transformBody body)
transformExp (Op op) = Op (transformOp op)
transformExp e = e

transformCase :: Case (Body GPU) -> Case (Body GPU)
transformCase (Case pat body) = Case pat (transformBody body)

transformBody :: Body GPU -> Body GPU
transformBody (Body dec stms res) = Body dec (transformStmts stms) res

transformOp :: Op GPU -> Op GPU
transformOp (SegOp sOp) = SegOp (transformSegOp sOp)
-- TODO: handle these separately?
transformOp op = op

transformSegOp :: SegOp SegLevel GPU -> SegOp SegLevel GPU
-- TODO: may be able to transform here, also match level?
transformSegOp (SegMap level space ts body) = traceHelper $ SegMap level space ts (transformKernelBody body)
--transformSegOp (SegMap level space ts body) = SegMap level space ts (transformKernelBody body)
transformSegOp (SegRed level space ops ts body) = SegRed level space ops ts (transformKernelBody body)
transformSegOp (SegScan level space ops ts body) = SegScan level space ops ts (transformKernelBody body)
transformSegOp (SegHist level space ops hist body) = SegHist level space ops hist (transformKernelBody body)

transformKernelBody :: KernelBody GPU -> KernelBody GPU
transformKernelBody (KernelBody desc stms res) = KernelBody desc (transformStmts stms) res




-- TODO: avoid code duplication with above?
intraMMMMemFixup :: Pass GPUMem GPUMem
intraMMMMemFixup =
-- TODO: don't use intraproceduralTransformation
    Pass "mma-fixup" "Extracts NVIDIA tensor core MMA operations" $ intraproceduralTransformation fixStmtsWithScope


-- TODO: use PassM below?


type FixEnv = Scope GPUMem

-- TODO: use map?
type FixState = [(VName, VName)]

type FixMonad a = RWST FixEnv () FixState PassM a


-- TODO: use scope, genearate scope?
fixStmtsWithScope :: Scope GPUMem -> Stms GPUMem -> PassM (Stms GPUMem)
fixStmtsWithScope scope stms = do
    res <- fmap (\(a, _, _) -> a) $ runRWST (fixStmts stms) scope []
    pure $ (trace . prettyString) res res

fixStmts :: Stms GPUMem -> FixMonad (Stms GPUMem)
--fixStmts stms =
--    mapM fixStmt stms
--    TODO: use fold?
fixStmts stms =
    case stmsHead stms of
    Nothing -> pure mempty
    Just (stm, stms') -> do
        stm' <- fixStmt stm
        stms'' <- inScopeOf stm $ fixStmts stms'
        pure $ oneStm stm' <> stms''

fixStmt :: Stm GPUMem -> FixMonad (Stm GPUMem)
fixStmt stm@(Let (Pat [PatElem resName (MemArray _ _ _ (ArrayIn resMem _))]) _ (BasicOp (Manifest _ inputName))) = do
    info <- lookupInfo inputName
    case info of
--    TODO: match more cases
        LetName (MemArray _ _ _ (ArrayIn inputMem _)) -> do
            modify ([(resName, inputName), (resMem, inputMem)]<>)
            defaultFixStm stm
        _ -> defaultFixStm stm
fixStmt stm = defaultFixStm stm

defaultFixStm :: Stm GPUMem -> FixMonad (Stm GPUMem)
defaultFixStm (Let pat aux e) = Let pat aux <$> fixExp e

-- TODO: add stuff to scope in other places
fixExp :: Exp GPUMem -> FixMonad (Exp GPUMem)
fixExp (Apply "gemm_123456" args rets info) = do
    newArgs <- mapM replaceArg args
    pure $ Apply "gemm_123456" newArgs rets info
fixExp (Match subExps cases body matchDec) = Match subExps <$> (mapM fixCase cases) <*> (fixBody body) <*> pure matchDec
fixExp (Loop params form body) = Loop params form <$> fixBody body
fixExp (Op op) = Op <$> fixOp op
-- TODO: match more?
--fixExp (BasicOp op) = BasicOp op
--fixExp (Apply name args rets info) = Apply name args rets info
fixExp e = pure e

replaceArg :: (SubExp, Diet) -> FixMonad (SubExp, Diet)
replaceArg (Var v, d) = do
    manifestMap <- get
    case lookup v manifestMap of
        Just v' ->
            pure (Var v', d)
        Nothing ->
            pure (Var v, d)
replaceArg a = pure a

fixCase :: Case (Body GPUMem) -> FixMonad (Case (Body GPUMem))
fixCase (Case pat body) = Case pat <$> fixBody body

fixBody :: Body GPUMem -> FixMonad (Body GPUMem)
fixBody (Body dec stms res) = Body dec <$> fixStmts stms <*> pure res


fixOp :: Op GPUMem -> FixMonad (Op GPUMem)
fixOp (Inner hostOp) = Inner <$> fixHostOp hostOp
-- TODO: recurse here?
fixOp op = pure op

fixHostOp :: HostOp NoOp GPUMem -> FixMonad (HostOp NoOp GPUMem)
fixHostOp (SegOp op) = SegOp <$> fixSegOp op
-- TODO: run recurisvely?
fixHostOp op = pure op

fixSegOp :: SegOp SegLevel GPUMem -> FixMonad (SegOp SegLevel GPUMem)
-- TODO: only look in inblock?
--fixSegOp (SegMap (SegThreadInBlock virt) space ts body) = SegMap (SegThreadInBlock virt) space ts (fixKernelBody body)
fixSegOp (SegMap level space ts body) = SegMap level space ts <$> fixKernelBody body
fixSegOp (SegRed level space ops ts body) = SegRed level space ops ts <$> fixKernelBody body
fixSegOp (SegScan level space ops ts body) = SegScan level space ops ts <$> fixKernelBody body
fixSegOp (SegHist level space ops hist body) = SegHist level space ops hist <$> fixKernelBody body

fixKernelBody :: KernelBody GPUMem -> FixMonad (KernelBody GPUMem)
fixKernelBody (KernelBody desc stms res) = KernelBody desc <$> fixStmts stms <*> pure res
