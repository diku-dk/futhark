module Futhark.Pass.ReduceDeviceSyncs (reduceDeviceSyncs) where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State.Strict hiding (State)
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import Data.List (unzip4, zip4)
import Data.Maybe (isNothing)
import Data.Sequence hiding (reverse, sort, zip, zip4)
import qualified Data.Text as T
import Futhark.Analysis.MigrationTable
import Futhark.Construct (fullSlice)
import Futhark.Error
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
      let (prog', st') = R.runReader (runStateT (optimizeProgram prog) st) mt
      putNameSource (stateNameSource st')
      pure prog'

type ReduceM = StateT State (R.Reader MigrationTable)

data State = State
  { -- | A source to generate new 'VName's from.
    stateNameSource :: VNameSource,
    -- | Variables that have been computed on device, their orignal names,
    -- types, and 'Index' expressions that allow them to be read.
    stateDevSrc :: IM.IntMap (Name, Type, Exp GPU),
    -- | Maps names in the original program to local names used within a GPUBody
    -- under construction.
    stateRenames :: IM.IntMap VName,
    -- | Index statements to be added as a prologue to a GPUBody under
    -- construction.
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

-- | Retrieve a function of the current environment.
asks :: (MigrationTable -> a) -> ReduceM a
asks = lift . R.asks

-- | Create a PatElem that binds the array of a migrated variable binding.
arrayizePatElem :: PatElemT Type -> ReduceM (PatElemT Type)
arrayizePatElem (PatElem n t) = do
  let name = baseName n `withSuffix` "_dev"
  dev <- newName (VName name 0)
  let dev_t = t `arrayOfRow` intConst Int64 1
  pure (PatElem dev dev_t)
  where
    withSuffix name sfx = nameFromText $ T.append (nameToText name) (T.pack sfx)

-- | Create an expression that reads the first element of a 1-dimensional array.
eIndex :: VName -> Exp GPU
eIndex arr = BasicOp $ Index arr (Slice [DimFix $ intConst Int64 0])

optimizeProgram :: Prog GPU -> ReduceM (Prog GPU)
optimizeProgram (Prog consts funs) = do
  consts' <- optimizeStms empty consts
  funs' <- sequence $ parMap rpar optimizeFunDef funs
  pure (Prog consts' funs')

optimizeFunDef :: FunDef GPU -> ReduceM (FunDef GPU)
optimizeFunDef fd = do
  let body = funDefBody fd
  stms' <- optimizeStms empty (bodyStms body)
  pure $ fd {funDefBody = body {bodyStms = stms'}}

optimizeStms :: Stms GPU -> Stms GPU -> ReduceM (Stms GPU)
optimizeStms out Empty =
  pure out
optimizeStms out (stm :<| stms) = do
  out' <- optimizeStm out stm
  optimizeStms out' stms

optimizeStm :: Stms GPU -> Stm GPU -> ReduceM (Stms GPU)
optimizeStm out stm = do
  move <- asks (moveToDevice stm)
  if move
    then moveStm out stm
    else case stmExp stm of
      BasicOp {} ->
        pure (out |> stm)
      Apply {} ->
        pure (out |> stm)
      If cond (Body _ tstms0 tres) (Body _ fstms0 fres) (IfDec btypes sort) ->
        do
          tstms1 <- optimizeStms empty tstms0
          fstms1 <- optimizeStms empty fstms0

          let pes = patElems (stmPat stm)

          -- Ensure return values and types match if one or both branches
          -- return a result that now reside on device.
          let res = zip4 pes tres fres btypes
          (res', tstms2, fstms2) <- foldM bmerge ([], tstms1, fstms1) res

          let (pes', tres', fres', btypes') = unzip4 (reverse res')
          let tbranch' = Body () tstms2 tres'
          let fbranch' = Body () fstms2 fres'
          let e' = If cond tbranch' fbranch' (IfDec btypes' sort)
          let stm' = Let (Pat pes') (stmAux stm) e'

          -- Read scalars that are used on host.
          foldM addRead (out |> stm') (zip pes pes')
      DoLoop params lform body ->
        pure (out |> stm) -- TODO (rewrite loop)
      WithAcc inputs lambda ->
        pure (out |> stm) -- TODO
      Op op ->
        pure (out |> stm) -- TODO
  where
    addRead stms (pe@(PatElem n _), PatElem dev _)
      | n == dev = pure stms
      | otherwise = do
        let pat = Pat [pe]
        let aux = StmAux mempty mempty ()
        let add e = pure $ stms |> Let pat aux e

        used <- asks (usedOnHost n)
        if used
          then add (eIndex dev)
          else pe `movedTo` dev >> pure stms

    dst srcs (Var n) =
      (\(_, _, BasicOp (Index arr _)) -> arr) <$> IM.lookup (baseTag n) srcs
    dst _ _ = Nothing

    bmerge (res, tstms, fstms) (pe, tr, fr, bt) = do
      let tres = resSubExp tr
      let fres = resSubExp fr

      srcs <- gets stateDevSrc
      let tdst = dst srcs tres
      let fdst = dst srcs fres

      let no_diff = isNothing tdst && isNothing fdst

      if no_diff
        then -- No result has been migrated ==> nothing to do.
          pure ((pe, tr, fr, bt) : res, tstms, fstms)
        else -- Otherwise, ensure both results are migrated.
        do
          let t = patElemDec pe
          (tstms', tarr) <- case tdst of
            Just arr -> pure (tstms, arr)
            _ -> moveSubExp tstms tres t
          (fstms', farr) <- case fdst of
            Just arr -> pure (fstms, arr)
            _ -> moveSubExp fstms fres t

          pe' <- arrayizePatElem pe
          let bt' = staticShapes1 (patElemDec pe')
          let tr' = tr {resSubExp = Var tarr}
          let fr' = fr {resSubExp = Var farr}
          pure ((pe', tr', fr', bt') : res, tstms', fstms')

-- | Migrate a statement to device, ensuring all its bound variables used on
-- host will remain available with the same names.
moveStm :: Stms GPU -> Stm GPU -> ReduceM (Stms GPU)
moveStm out stm = do
  -- Move the statement to device.
  gpubody <- inGPUBody (cloneStm stm)

  -- Read non-scalars and scalars that are used on host.
  let arrs = zip (patElems $ stmPat stm) (patElems $ stmPat gpubody)
  foldM addRead (out |> gpubody) arrs
  where
    addRead stms (pe@(PatElem n t), PatElem dev dev_t) =
      let pat = Pat [pe]
          aux = StmAux mempty mempty ()
          add e = pure $ stms |> Let pat aux e
          add' = add . BasicOp
       in case arrayRank dev_t of
            -- Alias non-arrays with their prior name.
            0 -> add' $ SubExp (Var dev)
            -- Read scalars used on host and certificates, which are free to
            -- read from device, or register the name of the 1-element array
            -- storing the scalar.
            1 -> do
              used <- asks (usedOnHost n)
              if used || t == Prim Unit
                then add (eIndex dev)
                else pe `movedTo` dev >> pure stms
            -- Drop the first dimension of multidimensional arrays.
            _ -> add' $ Index dev (fullSlice dev_t [DimFix $ intConst Int64 0])

-- | Move a copy of some value to device, adding its array binding to the given
-- statements and returning its name.
moveSubExp :: Stms GPU -> SubExp -> Type -> ReduceM (Stms GPU, VName)
moveSubExp out se t = do
  n <- newName $ case se of
    Var n -> n
    _ -> VName (nameFromString "const") 0

  let pat = Pat [PatElem n t]
  let aux = StmAux mempty mempty ()
  let e = BasicOp (SubExp se)
  let stm = Let pat aux e

  -- Expression cannot be hoisted, because it is a result of the gpubody.
  gpubody <- inGPUBody (pure stm)
  let dev = patElemName $ head $ patElems (stmPat gpubody)

  pure (out |> gpubody, dev)

-- | Create a GPUBody kernel that executes a single statement. Device memory
inGPUBody :: ReduceM (Stm GPU) -> ReduceM (Stm GPU)
inGPUBody m = do
  -- Clear GPUBody mappings from previous use.
  modify $ \st -> st {stateRenames = IM.empty, stateDevReads = empty}

  -- Compute statement and dependencies to add in a prologue.
  stm <- m
  prologue <- gets stateDevReads

  -- Construct the GPUBody.
  let pes = patElems (stmPat stm)
  pat <- Pat <$> mapM arrayizePatElem pes
  let aux = StmAux mempty mempty ()
  let types = map patElemDec pes
  let res = map (SubExpRes mempty . Var . patElemName) pes
  let body = Body () (prologue |> stm) res
  let e = Op (GPUBody types body)
  pure (Let pat aux e)

cloneBody :: BodyT GPU -> ReduceM (BodyT GPU)
cloneBody (Body _ stms res) = do
  stms' <- cloneStms stms
  res' <- renameResult res
  pure (Body () stms' res')

cloneStms :: Stms GPU -> ReduceM (Stms GPU)
cloneStms = mapM cloneStm

cloneStm :: Stm GPU -> ReduceM (Stm GPU)
cloneStm (Let pat aux e) = do
  e' <- cloneExp e
  pat' <- clonePat pat
  aux' <- cloneStmAux aux
  pure (Let pat' aux' e')

clonePat :: Pat GPU -> ReduceM (Pat GPU)
clonePat pat = Pat <$> mapM clonePatElem (patElems pat)

clonePatElem :: PatElemT Type -> ReduceM (PatElemT Type)
clonePatElem (PatElem n t) = do
  n' <- cloneName n
  t' <- renameType t
  pure (PatElem n' t')

cloneStmAux :: StmAux () -> ReduceM (StmAux ())
cloneStmAux (StmAux certs attrs _) = do
  certs' <- renameCerts certs
  pure (StmAux certs' attrs ())

cloneExp :: Exp GPU -> ReduceM (Exp GPU)
cloneExp =
  mapExpM $
    Mapper
      { mapOnSubExp = renameSubExp,
        mapOnBody = const cloneBody,
        mapOnVName = rename,
        mapOnRetType = renameExtType,
        mapOnBranchType = renameExtType,
        mapOnFParam = cloneParam,
        mapOnLParam = cloneParam,
        mapOnOp = const opError
      }
  where
    -- This indicates that something fundamentally is wrong with the migration
    -- table produced by the MigrationTable module.
    opError = compilerBugS "Cannot migrate a host operation to device."

cloneParam :: Param (TypeBase Shape u) -> ReduceM (Param (TypeBase Shape u))
cloneParam (Param attrs n t) = do
  n' <- cloneName n
  t' <- renameType t
  pure (Param attrs n' t')

-- | Create a fresh name, registering which name it is based on.
cloneName :: VName -> ReduceM VName
cloneName n = do
  n' <- newName n
  modify $ \st -> st {stateRenames = IM.insert (baseTag n) n' (stateRenames st)}
  pure n'

-- | Produce a fresh name, using the given name as a template.
newName :: VName -> ReduceM VName
newName n = do
  st <- get
  let ns = stateNameSource st
  let (n', ns') = FN.newName ns n
  put (st {stateNameSource = ns'})
  pure n'

-- | @movedTo (PatElem x t) arr@ registers that the value of @x@ is stored at
-- @arr[0]@, with @x@ being of type @t@.
movedTo :: PatElemT Type -> VName -> ReduceM ()
movedTo (PatElem x t) arr = do
  st <- get
  let src' = IM.insert (baseTag x) (baseName x, t, eIndex arr) (stateDevSrc st)
  put (st {stateDevSrc = src'})

-- | Setup the read of a device-located variable, returning the name that the
-- read scalar is bound to. The 'Exp' is expected to be a result of 'eIndex'.
setupRead :: (Name, Type, Exp GPU) -> ReduceM VName
setupRead (name, t, e) = do
  n <- newName (VName name 0)
  let pat = Pat [PatElem n t]
  let aux = StmAux mempty mempty ()
  let stm = Let pat aux e
  modify $ \st -> st {stateDevReads = stateDevReads st |> stm}
  pure n

-- | Return the name to use for a dependency which may have been migrated or
-- cloned.
rename :: VName -> ReduceM VName
rename n = do
  st <- get
  let renames = stateRenames st
  let idx = baseTag n
  case IM.lookup idx renames of
    Just n' -> pure n'
    _ ->
      do
        n' <- case IM.lookup idx (stateDevSrc st) of
          Nothing -> pure n -- neither migrated nor cloned
          Just x -> setupRead x -- migrated
        modify $ \st' -> st' {stateRenames = IM.insert idx n' renames}
        pure n'

renameResult :: Result -> ReduceM Result
renameResult = mapM renameSubExpRes

renameSubExpRes :: SubExpRes -> ReduceM SubExpRes
renameSubExpRes (SubExpRes certs se) = do
  certs' <- renameCerts certs
  se' <- renameSubExp se
  pure (SubExpRes certs' se')

renameCerts :: Certs -> ReduceM Certs
renameCerts cs = Certs <$> mapM rename (unCerts cs)

renameSubExp :: SubExp -> ReduceM SubExp
renameSubExp (Var n) = Var <$> rename n
renameSubExp se = pure se

renameType :: TypeBase Shape u -> ReduceM (TypeBase Shape u)
-- Note: mapOnType also maps the VName token of accumulators
renameType = mapOnType renameSubExp

renameExtType :: TypeBase ExtShape u -> ReduceM (TypeBase ExtShape u)
-- Note: mapOnExtType also maps the VName token of accumulators
renameExtType = mapOnExtType renameSubExp

-- TODO: Run ormolu and hlint
