module Futhark.Pass.ReduceDeviceSyncs (reduceDeviceSyncs) where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State.Strict hiding (State)
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import Data.List (unzip4, zip4)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..), empty, (<|), (><), (|>))
import qualified Data.Text as T
import Futhark.Analysis.MigrationTable
import Futhark.Construct (fullSlice, sliceDim)
import Futhark.Error
import qualified Futhark.FreshNames as FN
import Futhark.IR.GPU
import Futhark.MonadFreshNames (VNameSource, getNameSource, putNameSource)
import Futhark.Pass
import Futhark.Transform.Substitute

-- TODO: Run ormolu and hlint

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
    -- | A table of variables in the original program which have been migrated
    -- to device. Each variable maps to a tuple that describes:
    --   * 'baseName' of the original variable.
    --   * Type of the original variable.
    --   * Name of the single element array holding the migrated value.
    --   * Whether the original variable still can be used on the host.
    stateMigrated :: IM.IntMap (Name, Type, VName, Bool)
  }

initialState :: VNameSource -> State
initialState ns =
  State
    { stateNameSource = ns,
      stateMigrated = IM.empty
    }

-- | Retrieve a function of the current environment.
asks :: (MigrationTable -> a) -> ReduceM a
asks = lift . R.asks

-- | Fetch the value of the environment.
ask :: ReduceM MigrationTable
ask = lift R.ask

-- | Produce a fresh name, using the given name as a template.
newName :: VName -> ReduceM VName
newName n = do
  st <- get
  let ns = stateNameSource st
  let (n', ns') = FN.newName ns n
  put (st {stateNameSource = ns'})
  pure n'

-- | Create a PatElem that binds the array of a migrated variable binding.
arrayizePatElem :: PatElemT Type -> ReduceM (PatElemT Type)
arrayizePatElem (PatElem n t) = do
  let name = baseName n `withSuffix` "_dev"
  dev <- newName (VName name 0)
  let dev_t = t `arrayOfRow` intConst Int64 1
  pure (PatElem dev dev_t)
  where
    withSuffix name sfx = nameFromText $ T.append (nameToText name) (T.pack sfx)

-- | @(PatElem x t) `movedTo` arr@ registers that the value of @x@ has been
-- migrated to @arr[0]@, with @x@ being of type @t@.
movedTo :: PatElemT Type -> VName -> ReduceM ()
movedTo = recordMigration False

-- | @(PatElem x t) `aliasedBy` arr@ registers that the value of @x@ also is
-- available on device as @arr[0]@, with @x@ being of type @t@.
aliasedBy :: PatElemT Type -> VName -> ReduceM ()
aliasedBy = recordMigration True

-- | Records the migration of a variable and whether the original variable
-- still can be used on host.
recordMigration :: Bool -> PatElemT Type -> VName -> ReduceM ()
recordMigration alias (PatElem x t) arr =
  modify $ \st ->
    let migrated = stateMigrated st
        entry = (baseName x, t, arr, alias)
        migrated' = IM.insert (baseTag x) entry migrated
     in st {stateMigrated = migrated'}

-- | @pe `migratedTo` (dev, stms)@ registers that the variable @pe@ in the
-- original program has been migrated to @dev@ and rebinds the variable if
-- deemed necessary, adding an index statement to the given statements.
migratedTo :: PatElemT Type -> (VName, Stms GPU) -> ReduceM (Stms GPU)
migratedTo pe (dev, stms) = do
  used <- asks (usedOnHost $ patElemName pe)
  if used
    then pe `aliasedBy` dev >> pure (stms |> bind pe (eIndex dev))
    else pe `movedTo` dev >> pure stms

-- | @useScalar stms n@ returns a variable that binds the result bound by @n@
-- in the original program. If the variable has been migrated to device and have
-- not been copied back to host a new variable binding will be added to the
-- provided statements and be returned.
useScalar :: Stms GPU -> VName -> ReduceM (Stms GPU, VName)
useScalar stms n = do
  entry <- IM.lookup (baseTag n) <$> gets stateMigrated
  case entry of
    Nothing ->
      pure (stms, n)
    Just (_, _, _, True) ->
      pure (stms, n)
    Just (name, t, arr, _) ->
      do
        n' <- newName (VName name 0)
        let stm = bind (PatElem n' t) (eIndex arr)
        pure (stms |> stm, n')

-- | Create an expression that reads the first element of a 1-dimensional array.
eIndex :: VName -> Exp GPU
eIndex arr = BasicOp $ Index arr (Slice [DimFix $ intConst Int64 0])

-- | A shorthand for binding a single variable to an expression.
bind :: PatElemT Type -> Exp GPU -> Stm GPU
bind pe = Let (Pat [pe]) (StmAux mempty mempty ())

-- | @storeScalar stms se t@ returns a variable that binds a single element
-- array that contains the value of @se@ in the original program. If @se@ is a
-- variable that has been migrated to device, its existing array alias will be
-- used. Otherwise a new variable binding will be added to the provided
-- statements and be returned. @t@ is the type of @se@.
storeScalar :: Stms GPU -> SubExp -> Type -> ReduceM (Stms GPU, VName)
storeScalar stms se t = do
  entry <- case se of
    Var n -> IM.lookup (baseTag n) <$> gets stateMigrated
    _ -> pure Nothing
  case entry of
    Just (_, _, arr, _) -> pure (stms, arr)
    Nothing ->
      do
        n <- newName $ case se of
          Var n -> n
          _ -> VName (nameFromString "const") 0

        let stm = bind (PatElem n t) (BasicOp $ SubExp se)

        gpubody <- inGPUBody (pure stm)
        let dev = patElemName $ head $ patElems (stmPat gpubody)

        pure (stms |> gpubody, dev)

-- | Map a variable name to itself or, if the variable no longer can be used on
-- host, the name of a single element array containing its value.
resolveName :: VName -> ReduceM VName
resolveName n = do
  entry <- IM.lookup (baseTag n) <$> gets stateMigrated
  case entry of
    Nothing -> pure n
    Just (_, _, _, True) -> pure n
    Just (_, _, arr, _) -> pure arr

resolveSubExp :: SubExp -> ReduceM SubExp
resolveSubExp (Var n) = Var <$> resolveName n
resolveSubExp cnst = pure cnst

resolveSubExpRes :: SubExpRes -> ReduceM SubExpRes
resolveSubExpRes (SubExpRes certs se) =
  -- Certificates are always read back to host.
  SubExpRes certs <$> resolveSubExp se

resolveResult :: Result -> ReduceM Result
resolveResult = mapM resolveSubExpRes

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

optimizeBody :: BodyT GPU -> ReduceM (BodyT GPU)
optimizeBody (Body _ stms res) = do
  stms' <- optimizeStms empty stms
  res' <- resolveResult res
  pure (Body () stms' res')

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
          -- Rewrite branches.
          tstms1 <- optimizeStms empty tstms0
          fstms1 <- optimizeStms empty fstms0

          -- Ensure return values and types match if one or both branches
          -- return a result that now reside on device.
          let bmerge (res, tstms, fstms) (pe, tr, fr, bt) =
                do
                  tr_on_host <- onHost (resSubExp tr)
                  fr_on_host <- onHost (resSubExp fr)

                  if tr_on_host && fr_on_host
                    then -- No result resides on device ==> nothing to do.
                      pure ((pe, tr, fr, bt) : res, tstms, fstms)
                    else -- Otherwise, ensure both results are migrated.
                    do
                      let t = patElemDec pe
                      (tstms', tarr) <- storeScalar tstms (resSubExp tr) t
                      (fstms', farr) <- storeScalar fstms (resSubExp fr) t

                      pe' <- arrayizePatElem pe
                      let bt' = staticShapes1 (patElemDec pe')
                      let tr' = tr {resSubExp = Var tarr}
                      let fr' = fr {resSubExp = Var farr}
                      pure ((pe', tr', fr', bt') : res, tstms', fstms')

          let pes = patElems (stmPat stm)
          let res = zip4 pes tres fres btypes
          (res', tstms2, fstms2) <- foldM bmerge ([], tstms1, fstms1) res
          let (pes', tres', fres', btypes') = unzip4 (reverse res')

          -- Rewrite statement.
          let tbranch' = Body () tstms2 tres'
          let fbranch' = Body () fstms2 fres'
          let e' = If cond tbranch' fbranch' (IfDec btypes' sort)
          let stm' = Let (Pat pes') (stmAux stm) e'

          -- Read scalars that are used on host.
          foldM addRead (out |> stm') (zip pes pes')
      DoLoop ps lf b -> do
        (params, lform, body) <- rewriteForIn (ps, lf, b)

        -- Update statement bound variables and parameters if their values
        -- have been migrated to device.
        let lmerge (res, stms) (pe, (Param attrs pn pt, pval)) =
              do
                moved <- asks (shouldMove pn)
                if not moved
                  then pure ((pe, (Param attrs pn pt, pval)) : res, stms)
                  else do
                    pe' <- arrayizePatElem pe

                    -- Move the initial value to device if not already there.
                    (stms', arr) <- storeScalar stms pval (fromDecl pt)

                    pn' <- newName pn
                    let pt' = toDecl (patElemDec pe') Nonunique
                    let pval' = Var arr

                    PatElem pn (fromDecl pt) `movedTo` pn'

                    pure ((pe', (Param mempty pn' pt', pval')) : res, stms')

        let pes = patElems (stmPat stm)
        (ps', out') <- foldM lmerge ([], out) (zip pes params)
        let (pes', params') = unzip (reverse ps')

        body' <- optimizeBody body
        let e' = DoLoop params' lform body'
        let stm' = Let (Pat pes') (stmAux stm) e'

        -- Read scalars that are used on host.
        foldM addRead (out' |> stm') (zip pes pes')
      WithAcc inputs lmd -> do
        inputs' <- mapM optimizeWithAccInput inputs

        let body = lambdaBody lmd
        stms' <- optimizeStms empty (bodyStms body)

        let rewrite (pe, SubExpRes certs se, t) =
              do
                se' <- resolveSubExp se
                if se == se'
                  then pure (pe, SubExpRes certs se, t)
                  else do
                    pe' <- arrayizePatElem pe
                    let t' = patElemDec pe'
                    pure (pe', SubExpRes certs se', t')

        let pes = patElems (stmPat stm)
        let res = bodyResult body
        let rts = lambdaReturnType lmd
        -- No rewriting of lambda parameters as they are all accumulators.
        (pes', res', rts') <- unzip3 <$> mapM rewrite (zip3 pes res rts)

        let body' = Body () stms' res'
        let lmd' = lmd {lambdaBody = body', lambdaReturnType = rts'}
        let e' = WithAcc inputs' lmd'
        let stm' = Let (Pat pes') (stmAux stm) e'

        -- Read scalars that are used on host.
        foldM addRead (out |> stm') (zip pes pes')
      Op op -> do
        op' <- optimizeHostOp op
        pure (out |> stm {stmExp = Op op'})
  where
    onHost (Var v) = (v ==) <$> resolveName v
    onHost _ = pure True

    addRead stms (pe@(PatElem n _), PatElem dev _)
      | n == dev = pure stms
      | otherwise = pe `migratedTo` (dev, stms)

-- | Rewrite a for-in loop such that relevant source array reads can be delayed.
rewriteForIn ::
  ([(FParam GPU, SubExp)], LoopForm GPU, BodyT GPU) ->
  ReduceM ([(FParam GPU, SubExp)], LoopForm GPU, BodyT GPU)
rewriteForIn loop@(_, WhileLoop {}, _) =
  pure loop
rewriteForIn (params, ForLoop i t n elems, body) = do
  mt <- ask
  let (elems', stms') = foldr (inline mt) ([], bodyStms body) elems
  pure (params, ForLoop i t n elems', body {bodyStms = stms'})
  where
    inline ::
      MigrationTable ->
      (Param Type, VName) ->
      ([(Param Type, VName)], Stms GPU) ->
      ([(Param Type, VName)], Stms GPU)
    inline mt (x, arr) (arrs, stms)
      | pn <- paramName x,
        not (usedOnHost pn mt) =
        let pt = paramDec x
            stm = bind (PatElem pn pt) (BasicOp $ index arr pt)
         in (arrs, stm <| stms)
      | otherwise =
        ((x, arr) : arrs, stms)

    index arr ofType =
      Index arr $ Slice $ DimFix (Var i) : map sliceDim (arrayDims ofType)

optimizeWithAccInput :: WithAccInput GPU -> ReduceM (WithAccInput GPU)
optimizeWithAccInput (shape, arrs, Nothing) = pure (shape, arrs, Nothing)
optimizeWithAccInput (shape, arrs, Just (op, nes)) = do
  let body = lambdaBody op
  -- Neither parameters nor results can change types for WithAcc to type check.
  stms' <- optimizeStms empty (bodyStms body)
  let op' = op {lambdaBody = body {bodyStms = stms'}}
  pure (shape, arrs, Just (op', nes))

optimizeHostOp :: HostOp GPU op -> ReduceM (HostOp GPU op)
optimizeHostOp (SegOp (SegMap lvl space types kbody)) =
  SegOp . SegMap lvl space types <$> rewriteKernelBody kbody
optimizeHostOp (SegOp (SegRed lvl space ops types kbody)) = do
  ops' <- mapM rewriteSegBinOp ops
  SegOp . SegRed lvl space ops' types <$> rewriteKernelBody kbody
optimizeHostOp (SegOp (SegScan lvl space ops types kbody)) = do
  ops' <- mapM rewriteSegBinOp ops
  SegOp . SegScan lvl space ops' types <$> rewriteKernelBody kbody
optimizeHostOp (SegOp (SegHist lvl space ops types kbody)) = do
  ops' <- mapM rewriteHistOp ops
  SegOp . SegHist lvl space ops' types <$> rewriteKernelBody kbody
optimizeHostOp (SizeOp op) =
  pure (SizeOp op)
optimizeHostOp OtherOp {} =
  -- These should all have been taken care of in the unstreamGPU pass.
  compilerBugS "optimizeHostOp: unhandled OtherOp"
optimizeHostOp (GPUBody types body) =
  GPUBody types <$> rewriteBody body

rewriteSegBinOp :: SegBinOp GPU -> ReduceM (SegBinOp GPU)
rewriteSegBinOp op = do
  f' <- rewriteLambda (segBinOpLambda op)
  pure (op {segBinOpLambda = f'})

rewriteHistOp :: HistOp GPU -> ReduceM (HistOp GPU)
rewriteHistOp op = do
  f' <- rewriteLambda (histOp op)
  pure (op {histOp = f'})

rewriteLambda :: Lambda GPU -> ReduceM (Lambda GPU)
rewriteLambda f = do
  body' <- rewriteBody (lambdaBody f)
  pure (f {lambdaBody = body'})

rewriteBody :: BodyT GPU -> ReduceM (BodyT GPU)
rewriteBody body = do
  (body', st) <- runStateT (cloneBody body) (initialCloneState Reuse)
  pure body' {bodyStms = clonePrologue st >< bodyStms body'}

rewriteKernelBody :: KernelBody GPU -> ReduceM (KernelBody GPU)
rewriteKernelBody kbody = do
  (kbody', st) <- runStateT (cloneKernelBody kbody) (initialCloneState Reuse)
  pure kbody' {kernelBodyStms = clonePrologue st >< kernelBodyStms kbody'}

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
    addRead stms (pe@(PatElem _ t), PatElem dev dev_t) =
      let add' e = pure $ stms |> bind pe e
          add = add' . BasicOp
       in case arrayRank dev_t of
            -- Alias non-arrays with their prior name.
            0 -> add $ SubExp (Var dev)
            -- Read all certificates for free.
            1 | t == Prim Unit -> add' (eIndex dev)
            -- Record the device alias of each scalar variable.
            -- Read scalars used on host.
            1 -> pe `migratedTo` (dev, stms)
            -- Drop the added dimension of multidimensional arrays.
            _ -> add $ Index dev (fullSlice dev_t [DimFix $ intConst Int64 0])

-- | Create a GPUBody kernel that executes a single statement, returning its
-- result values wrapped in single element arrays.
inGPUBody :: CloneM (Stm GPU) -> ReduceM (Stm GPU)
inGPUBody m = do
  (stm, st) <- runStateT m (initialCloneState Rebind)
  let prologue = clonePrologue st

  let pes = patElems (stmPat stm)
  pat <- Pat <$> mapM arrayizePatElem pes
  let aux = StmAux mempty mempty ()
  let types = map patElemDec pes
  let res = map (SubExpRes mempty . Var . patElemName) pes
  let body = Body () (prologue |> stm) res
  let e = Op (GPUBody types body)
  pure (Let pat aux e)

type CloneM = StateT CloneState ReduceM

data CloneState = CloneState
  { -- | Maps variables in the original program to names to be used by clones.
    cloneRenames :: IM.IntMap VName,
    -- | Statements to be added as a prologue before cloned statements.
    clonePrologue :: Stms GPU,
    -- | Whether cloned name bindings should be substituted with new names.
    cloneRebind :: RebindOption
  }

data RebindOption
  = -- | Replace cloned names with new 'VName's.
    Rebind
  | -- | Reuse names when cloned.
    Reuse

initialCloneState :: RebindOption -> CloneState
initialCloneState rebind =
  CloneState
    { cloneRenames = IM.empty,
      clonePrologue = empty,
      cloneRebind = rebind
    }

-- | Reuse or replace a name with a fresh name, based on configuration.
cloneName :: VName -> CloneM VName
cloneName n = do
  rebind <- gets cloneRebind
  case rebind of
    Rebind -> do
      n' <- lift (newName n)
      modify $ \st ->
        let renames' = IM.insert (baseTag n) n' (cloneRenames st)
         in st {cloneRenames = renames'}
      pure n'
    Reuse -> pure n

cloneKernelBody :: KernelBody GPU -> CloneM (KernelBody GPU)
cloneKernelBody (KernelBody _ stms res) = do
  stms' <- cloneStms stms
  res' <- mapM renameKernelResult res
  pure (KernelBody () stms' res')

cloneBody :: BodyT GPU -> CloneM (BodyT GPU)
cloneBody (Body _ stms res) = do
  stms' <- cloneStms stms
  res' <- renameResult res
  pure (Body () stms' res')

cloneStms :: Stms GPU -> CloneM (Stms GPU)
cloneStms = mapM cloneStm

cloneStm :: Stm GPU -> CloneM (Stm GPU)
cloneStm (Let pat aux e) = do
  e' <- cloneExp e
  pat' <- clonePat pat
  aux' <- cloneStmAux aux
  pure (Let pat' aux' e')

clonePat :: Pat GPU -> CloneM (Pat GPU)
clonePat pat = Pat <$> mapM clonePatElem (patElems pat)

clonePatElem :: PatElemT Type -> CloneM (PatElemT Type)
clonePatElem (PatElem n t) = do
  n' <- cloneName n
  t' <- renameType t
  pure (PatElem n' t')

cloneStmAux :: StmAux () -> CloneM (StmAux ())
cloneStmAux (StmAux certs attrs _) = do
  certs' <- renameCerts certs
  pure (StmAux certs' attrs ())

cloneExp :: Exp GPU -> CloneM (Exp GPU)
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
    opError = compilerBugS "Cannot migrate a host-only operation to device."

cloneParam :: Param (TypeBase Shape u) -> CloneM (Param (TypeBase Shape u))
cloneParam (Param attrs n t) = do
  n' <- cloneName n
  t' <- renameType t
  pure (Param attrs n' t')

-- | Return the name to use for a clone dependency.
rename :: VName -> CloneM VName
rename n = do
  st <- get
  let renames = cloneRenames st
  let idx = baseTag n
  case IM.lookup idx renames of
    Just n' -> pure n'
    _ ->
      do
        let stms = clonePrologue st
        (stms', n') <- lift $ useScalar stms n
        modify $ \st' ->
          st'
            { cloneRenames = IM.insert idx n' renames,
              clonePrologue = stms'
            }
        pure n'

renameResult :: Result -> CloneM Result
renameResult = mapM renameSubExpRes

renameKernelResult :: KernelResult -> CloneM KernelResult
renameKernelResult kres = do
  let from = namesToList (freeIn kres)
  to <- mapM rename from
  let rename_map = M.fromList (zip from to)
  pure (substituteNames rename_map kres)

renameSubExpRes :: SubExpRes -> CloneM SubExpRes
renameSubExpRes (SubExpRes certs se) = do
  certs' <- renameCerts certs
  se' <- renameSubExp se
  pure (SubExpRes certs' se')

renameCerts :: Certs -> CloneM Certs
renameCerts cs = Certs <$> mapM rename (unCerts cs)

renameSubExp :: SubExp -> CloneM SubExp
renameSubExp (Var n) = Var <$> rename n
renameSubExp se = pure se

renameType :: TypeBase Shape u -> CloneM (TypeBase Shape u)
-- Note: mapOnType also maps the VName token of accumulators
renameType = mapOnType renameSubExp

renameExtType :: TypeBase ExtShape u -> CloneM (TypeBase ExtShape u)
-- Note: mapOnExtType also maps the VName token of accumulators
renameExtType = mapOnExtType renameSubExp
