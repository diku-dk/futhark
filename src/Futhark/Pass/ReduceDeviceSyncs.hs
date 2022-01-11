module Futhark.Pass.ReduceDeviceSyncs (reduceDeviceSyncs) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict hiding (State)
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import Data.Sequence hiding (zip)
import qualified Data.Text as T
import Futhark.Analysis.MigrationTable
import qualified Futhark.FreshNames as FN
import Futhark.IR.GPU
import Futhark.MonadFreshNames (VNameSource, getNameSource, putNameSource)
import Futhark.Pass

reduceDeviceSyncs :: Pass GPU GPU
reduceDeviceSyncs =
  Pass
    "reduce device synchronizations"
    "Move host statements to device to reduce blocking memory operations."
    run
  where
    run prog = do
      ns <- getNameSource
      let mt = analyseProg prog
      let st = initialState ns
      let (prog', st') = runReader (runStateT (optimizeProgram prog) st) mt
      putNameSource (stateNameSource st')
      pure prog'

type ReduceM = StateT State (Reader MigrationTable)

data State = State
  { stateNameSource :: VNameSource,
    stateDevSrc :: IM.IntMap (Exp GPU),
    stateRenames :: IM.IntMap VName,
    stateDevReads :: Stms GPU
  }

initialState :: VNameSource -> State
initialState ns =
  State
    { stateNameSource = ns,
      stateDevSrc = IM.empty,
      stateRenames = IM.empty,
      stateDevReads = empty
    }

captureReads :: ReduceM a -> ReduceM (a, Stms GPU)
captureReads m = do
  modify $ \st -> st {stateDevReads = empty}
  res <- m
  rds <- gets stateDevReads
  pure (res, rds)

optimizeProgram :: Prog GPU -> ReduceM (Prog GPU)
optimizeProgram (Prog consts funs) = do
  consts' <- optimizeStms consts
  funs' <- sequence $ parMap rpar optimizeFunDef funs
  pure (Prog consts' funs')

optimizeFunDef :: FunDef GPU -> ReduceM (FunDef GPU)
optimizeFunDef fd = do
  let body = funDefBody fd
  stms' <- optimizeStms (bodyStms body)
  pure $ fd {funDefBody = body {bodyStms = stms'}}

optimizeStms :: Stms GPU -> ReduceM (Stms GPU)
optimizeStms Empty =
  pure Empty
optimizeStms (stm :<| stms) = do
  stms0 <- optimizeStm stm
  stms1 <- optimizeStms stms
  pure (stms0 >< stms1)

optimizeStm :: Stm GPU -> ReduceM (Stms GPU)
optimizeStm stm = do
  move <- lift $ asks (moveToDevice stm)
  if move
    then moveStm stm
    else case stmExp stm of
      BasicOp {} ->
        pure (singleton stm)
      Apply {} ->
        pure (singleton stm)
      If cond tbody fbody dec ->
        pure (singleton stm) -- TODO
      DoLoop params lform body ->
        pure (singleton stm) -- TODO
      WithAcc inputs lambda ->
        pure (singleton stm) -- TODO
      Op op ->
        pure (singleton stm) -- TODO

moveStm :: Stm GPU -> ReduceM (Stms GPU)
moveStm stm = do
  gpubody <- inGPUBody (cloneStm stm)
  let arrs = zip (patElems $ stmPat stm) (patElems $ stmPat gpubody)
  foldM addRead (oneStm gpubody) arrs
  where
    addRead stms (pe@(PatElem v t), PatElem v_arr t_arr) =
      let pat = Pat [pe]
          aux = StmAux mempty mempty ()
          add e = pure $ stms |> Let pat aux e
       in case arrayRank t_arr of
            0 -> add $ BasicOp $ SubExp (Var v_arr)
            1 -> do
              used <- lift $ asks (usedOnHost v)
              if used || t == Prim Unit
                then add (eIndex v_arr)
                else v `movedTo` v_arr >> pure stms
            -- TODO: THIS IS WRONG. USE INDEX INSTEAD.
            _ -> add $ shapeCoerce (tail $ shapeDims $ arrayShape t_arr) v_arr

eIndex :: VName -> Exp GPU
eIndex arr = BasicOp $ Index arr (Slice [DimFix $ intConst Int64 0])

inGPUBody :: ReduceM (Stm GPU) -> ReduceM (Stm GPU)
inGPUBody m = do
  (stm, stms) <- captureReads m

  let pes = patElems (stmPat stm)
  pat <- Pat <$> mapM arrayize pes

  let aux = StmAux mempty mempty ()

  let types = map patElemDec pes
  let res = map (SubExpRes mempty . Var . patElemName) pes
  let body = Body () (stms |> stm) res
  let e = Op (GPUBody types body)

  pure (Let pat aux e)
  where
    arrayize (PatElem v t) = do
      let name = nameFromText $ T.append (nameToText $ baseName v) dev
      v_arr <- newName (VName name 0)
      let t_arr = t `arrayOfRow` intConst Int64 1
      pure (PatElem v_arr t_arr)

    dev = T.pack "_dev"

cloneStm :: Stm GPU -> ReduceM (Stm GPU)
cloneStm (Let pat aux e) = do
  e' <- cloneExp e
  pat' <- Pat <$> mapM clonePatElem (patElems pat)
  -- Certificates in aux have not been renamed
  pure (Let pat' aux e')
  where
    clonePatElem (PatElem v t) = do
      v' <- cloneName v
      t' <- renameType t
      pure (PatElem v' t')

cloneExp :: Exp GPU -> ReduceM (Exp GPU)
cloneExp e = do
  pure e

-- TODO:

-- * Rename cloned types (array dimensions can be variables)

cloneName :: VName -> ReduceM VName
cloneName v = do
  v' <- newName v
  modify $ \st -> st {stateRenames = IM.insert (baseTag v) v' (stateRenames st)}
  pure v'

newName :: VName -> ReduceM VName
newName v = do
  st <- get
  let ns = stateNameSource st
  let (v', ns') = FN.newName ns v
  put (st {stateNameSource = ns'})
  pure v'

-- | @movedTo v v_arr@ registers that the value of @v@ is stored at @v_arr[0]@.
movedTo :: VName -> VName -> ReduceM ()
movedTo v v_arr = do
  st <- get
  let src' = IM.insert (baseTag v) (eIndex v_arr) (stateDevSrc st)
  put (st {stateDevSrc = src'})

-- TODO: Run ormolu and hlint
