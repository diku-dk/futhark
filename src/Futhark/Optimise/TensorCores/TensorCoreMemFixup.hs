module Futhark.Optimise.TensorCores.TensorCoreMemFixup (fixFuns) where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.List (lookup)
import Data.Semigroup
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.Optimise.TensorCores.Utils
import Futhark.Pass (PassM)
import Prelude hiding (lookup)

type FixEnv = Scope GPUMem

data SpaceType = Device | Shared | Scalar

type FixState = [(VName, (VName, Bool))]

-- The monad the memory fixup runs within.
type FixM a = RWST FixEnv () FixState PassM a

fixFuns :: Stms GPUMem -> FunDef GPUMem -> PassM (FunDef GPUMem)
fixFuns consts fun
  | gemmName `isPrefixOfName` funDefName fun =
      pure $
        fun
          { funDefParams = fixParamsGemmFun $ funDefParams fun,
            funDefRetType = fixRetType Scalar $ funDefRetType fun
          }
  | copyGlobalSharedName `isPrefixOfName` funDefName fun =
      pure $
        fun
          { funDefParams = fixParamsCopyGlobalShared $ funDefParams fun,
            funDefRetType = fixRetType Shared $ funDefRetType fun
          }
  | copyRegistersSharedName `isPrefixOfName` funDefName fun =
      pure $
        fun
          { funDefParams = fixParamsCopyRegistersShared $ funDefParams fun,
            funDefRetType = fixRetType Shared $ funDefRetType fun
          }
  | otherwise = do
      let initScope = scopeOf consts <> scopeOfFParams (funDefParams fun)
      let body = funDefBody fun
      stms' <- fixStmtsWithScope initScope . bodyStms $ body
      pure $ fun {funDefBody = body {bodyStms = stms'}}

fixParamsCopyGlobalShared :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyGlobalShared
  ( Param attrs1 vName1 (MemMem (Space "device"))
      : Param attrs2 vName2 (MemMem (Space "device"))
      : rest
    ) =
    Param attrs1 vName1 (MemMem (Space "device"))
      : Param attrs2 vName2 (MemMem (Space "shared"))
      : rest
fixParamsCopyGlobalShared params = params

fixParamsCopyRegistersShared :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsCopyRegistersShared
  ( Param attrs1 vName1 (MemMem (Space "device"))
      : Param attrs2 vName2 (MemMem (Space "device"))
      : p3@(Param _ _ (MemArray t shp _ (ArrayIn _ _)))
      : rest
    ) =
    Param attrs1 vName1 (MemMem space)
      : Param attrs2 vName2 (MemMem (Space "shared"))
      : p3
      : rest
    where
      space = ScalarSpace (drop 1 $ shapeDims shp) t
fixParamsCopyRegistersShared params = params

fixParamsGemmFun :: [FParam GPUMem] -> [FParam GPUMem]
fixParamsGemmFun
  ( Param attrs1 vName1 (MemMem (Space "device"))
      : Param attrs2 vName2 (MemMem (Space "device"))
      : Param attrs3 vName3 (MemMem (Space "device"))
      : p4
      : p5
      : p6@(Param _ _ (MemArray t shp _ (ArrayIn _ _)))
      : rest
    ) =
    Param attrs1 vName1 (MemMem (Space "shared"))
      : Param attrs2 vName2 (MemMem (Space "shared"))
      : Param attrs3 vName3 (MemMem space)
      : p4
      : p5
      : p6
      : rest
    where
      space = ScalarSpace (shapeDims shp) t
fixParamsGemmFun params = params

fixRetType ::
  SpaceType ->
  [(RetType GPUMem, RetAls)] ->
  [(RetType GPUMem, RetAls)]
fixRetType
  spaceType
  [ (MemMem (Space "device"), als1),
    (MemArray t shp u (ReturnsNewBlock (Space "device") n lmad), als2)
    ] =
    --  TODO: check if ReturnsInBlock is preferred
    [ (MemMem newSpace, als1),
      (MemArray t shp u (ReturnsNewBlock newSpace n lmad), als2)
    ]
    where
      getNewSpace Device = Space "device"
      getNewSpace Shared = Space "shared"
      getNewSpace Scalar = ScalarSpace (fmap extToSubExp (shapeDims shp)) t
      newSpace = getNewSpace spaceType
fixRetType _ rets = rets

extToSubExp :: ExtSize -> SubExp
extToSubExp (Ext n) = mkInt64Const n
extToSubExp (Free se) = se

fixStmtsWithScope :: Scope GPUMem -> Stms GPUMem -> PassM (Stms GPUMem)
fixStmtsWithScope scope stms = do
  (res, _, _) <- runRWST (fixStmts stms) scope []
  pure res

fixStmts :: Stms GPUMem -> FixM (Stms GPUMem)
fixStmts = mapStmsWithScope fixStmt

fixStmt :: Stm GPUMem -> FixM (Stms GPUMem)
fixStmt
  stm@( Let
          (Pat [PatElem resName (MemArray _ _ _ (ArrayIn resMem _))])
          _
          (BasicOp (Manifest inputName _))
        ) = do
    info <- lookupInfo inputName
    case info of
      LetName (MemArray _ _ _ (ArrayIn inputMem _)) -> do
        modify ([(resName, (inputName, False)), (resMem, (inputMem, False))] <>)
        defaultFixStm stm
      _ -> defaultFixStm stm
fixStmt
  ( Let
      ( Pat
          [ PatElem vName1 _,
            PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
            ]
        )
      aux
      (Apply fName args rets info)
    ) | gemmName `isPrefixOfName` fName = do
    let space = ScalarSpace (shapeDims shp2) t2
    let newRets = fixRetType Scalar rets
    -- For each argument we
    (replacedArgs, removedCopy) <- mapAndUnzipM replaceArg args
    let (removedAcopy : removedBcopy : _) = removedCopy
    -- If these are true, and a manifest copy was removed, we can do swizzling,
    -- otherwise the arrays are already in shared and we cannot swizzle them.
    let removedAorB =
          [ (mkInt64Const $ boolToInt $ not removedAcopy, ObservePrim),
            (mkInt64Const $ boolToInt $ not removedBcopy, ObservePrim)
          ]
    let newArgs = take (length replacedArgs - 2) replacedArgs <> removedAorB
    pure $
      oneStm $
        Let
          ( Pat
              [ PatElem vName1 (MemMem space),
                PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
              ]
          )
          aux
          (Apply fName newArgs newRets info)
fixStmt
  ( Let
      ( Pat
          [ PatElem vName1 _,
            PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
            ]
        )
      aux
      (Apply fName args rets info)
    ) | copyGlobalSharedName `isPrefixOfName` fName = do
    let space = Space "shared"
    --  TODO: check if need to handle uniqueness/consumption
    let newRets = fixRetType Shared rets
    (newArgs, _removedCopy) <- mapAndUnzipM replaceArg args
    let ((Var srcMemMem, _) : _ : (Var srcArray, _) : _restArgs) = newArgs
    srcMemInfo <- lookupInfo srcMemMem
    case srcMemInfo of
      LetName (MemMem srcMemSpace)
        | srcMemSpace == space ->
            -- Array is already in shared. Therefore, the copyGlobalShared call
            -- should be removed and we return removedCopy=True.
            modify ([(vName2, (srcArray, True)), (vName1, (srcMemMem, True))] <>)
      _ ->
        pure ()
    pure
      $ oneStm
      $ Let
        ( Pat
            [ PatElem vName1 (MemMem space),
              PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
            ]
        )
        aux
      $ Apply fName newArgs newRets info
fixStmt
  ( Let
      ( Pat
          [ PatElem vName1 _,
            PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
            ]
        )
      aux
      (Apply fName args rets info)
    ) | copyRegistersSharedName `isPrefixOfName` fName = do
    let space = Space "shared"
    let newRets = fixRetType Shared rets
    (newArgs, _removedCopy) <- mapAndUnzipM replaceArg args
    pure $
      oneStm $
        Let
          ( Pat
              [ PatElem vName1 (MemMem space),
                PatElem vName2 (MemArray t2 shp2 u2 (ArrayIn mName2 lmad2))
              ]
          )
          aux
          (Apply fName newArgs newRets info)
fixStmt stm = defaultFixStm stm

defaultFixStm :: Stm GPUMem -> FixM (Stms GPUMem)
defaultFixStm (Let pat aux e) = do
  e' <- fixExp e
  pure $ oneStm $ Let pat aux e'

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- TODO: this may be too aggressive
-- For each argument to a tensor core function call, replace the argument
-- if it comes from a manifest statement
-- Consider the generated call to tensorMMM that performs the GEMM on Tensor Cores:
--   let A' = manifest(A, 0)
--   let C = tensorMMM A' ...
-- We would insted pass:
--   let C = tensorMMM A ...
-- We do this because A' is manifested in global memory, but we want the arguments
-- to be in shared, because we know this function can only be called fromExp
-- kernel code!!!.
-- In case the argument was not caused by a manifest statement, it might already
-- be in shared memory. The removedCopy indicates if a manifest was removed.
replaceArg :: (SubExp, Diet) -> FixM ((SubExp, Diet), Bool)
replaceArg (Var v, d) = do
  manifestMap <- get
  case lookup v manifestMap of
    Just (v', removedCopy) ->
      pure ((Var v', d), removedCopy)
    Nothing ->
      pure ((Var v, d), False)
replaceArg a = pure (a, False)

fixExp :: Exp GPUMem -> FixM (Exp GPUMem)
fixExp (Match subExps cases body matchDec) =
  Match subExps
    <$> mapM fixCase cases
    <*> fixBody body
    <*> pure matchDec
fixExp (Loop params form body) =
  localScope (scopeOfFParams (map fst params) <> scopeOfLoopForm form) $ do
    newBody <- fixBody body
    pure $ Loop params form newBody
fixExp (Op op) = Op <$> fixOp op
fixExp e = pure e

fixCase :: Case (Body GPUMem) -> FixM (Case (Body GPUMem))
fixCase (Case pat body) = Case pat <$> fixBody body

fixBody :: Body GPUMem -> FixM (Body GPUMem)
fixBody (Body dec stms res) = Body dec <$> fixStmts stms <*> pure res

fixOp :: Op GPUMem -> FixM (Op GPUMem)
fixOp (Inner hostOp) = Inner <$> fixHostOp hostOp
fixOp op = pure op

fixHostOp :: HostOp NoOp GPUMem -> FixM (HostOp NoOp GPUMem)
fixHostOp (SegOp op) = SegOp <$> fixSegOp op
fixHostOp op = pure op

fixSegOp :: SegOp SegLevel GPUMem -> FixM (SegOp SegLevel GPUMem)
fixSegOp (SegMap level space ts body) =
  SegMap level space ts <$> fixKernelBody body
fixSegOp (SegRed level space ts body ops) =
  SegRed level space ts <$> (fixKernelBody body) <*> pure ops
fixSegOp (SegScan level space ts body ops) =
  SegScan level space ts <$> (fixKernelBody body) <*> pure ops
fixSegOp (SegHist level space ts body histOps) =
  SegHist level space ts <$> (fixKernelBody body) <*> pure histOps

fixKernelBody :: KernelBody GPUMem -> FixM (KernelBody GPUMem)
fixKernelBody (KernelBody desc stms res) =
  KernelBody desc <$> fixStmts stms <*> pure res
