-- | This module implements an optimization that migrates host
-- statements into 'GPUBody' kernels to reduce the number of
-- host-device synchronizations that occur when a scalar variable is
-- written to or read from device memory. Which statements that should
-- be migrated are determined by a 'MigrationTable' produced by the
-- "Futhark.Optimise.ReduceDeviceSyncs.MigrationTable" module; this module
-- merely performs the migration and rewriting dictated by that table.
module Futhark.Optimise.ReduceDeviceSyncs (reduceDeviceSyncs) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Data.Bifunctor (second)
import Data.IntMap.Strict qualified as IM
import Data.List (transpose, zip4)
import Data.Map.Strict qualified as M
import Data.Sequence ((><), (|>))
import Data.Text qualified as T
import Futhark.Construct (fullSlice, mkBody, sliceDim)
import Futhark.Error
import Futhark.IR.GPU
import Futhark.MonadFreshNames
import Futhark.Optimise.ReduceDeviceSyncs.MigrationTable
import Futhark.Pass
import Futhark.Transform.Substitute

-- | An optimization pass that migrates host statements into 'GPUBody' kernels
-- to reduce the number of host-device synchronizations.
reduceDeviceSyncs :: Pass GPU GPU
reduceDeviceSyncs =
  Pass
    "reduce device synchronizations"
    "Move host statements to device to reduce blocking memory operations."
    $ \prog -> do
      let hof = hostOnlyFunDefs $ progFuns prog
          consts_mt = analyseConsts hof (progFuns prog) (progConsts prog)
      consts <- onConsts consts_mt $ progConsts prog
      funs <- parPass (onFun hof consts_mt) (progFuns prog)
      pure $ prog {progConsts = consts, progFuns = funs}
  where
    onConsts consts_mt stms =
      runReduceM consts_mt (optimizeStms stms)
    onFun hof consts_mt fd = do
      let mt = consts_mt <> analyseFunDef hof fd
      runReduceM mt (optimizeFunDef fd)

--------------------------------------------------------------------------------
--                            AD HOC OPTIMIZATION                             --
--------------------------------------------------------------------------------

-- | Optimize a function definition. Its type signature will remain unchanged.
optimizeFunDef :: FunDef GPU -> ReduceM (FunDef GPU)
optimizeFunDef fd = do
  let body = funDefBody fd
  stms' <- optimizeStms (bodyStms body)
  pure $ fd {funDefBody = body {bodyStms = stms'}}

-- | Optimize a body. Scalar results may be replaced with single-element arrays.
optimizeBody :: Body GPU -> ReduceM (Body GPU)
optimizeBody (Body _ stms res) = do
  stms' <- optimizeStms stms
  res' <- resolveResult res
  pure (Body () stms' res')

-- | Optimize a sequence of statements.
optimizeStms :: Stms GPU -> ReduceM (Stms GPU)
optimizeStms = foldM optimizeStm mempty

-- | Optimize a single statement, rewriting it into one or more statements to
-- be appended to the provided 'Stms'. Only variables with continued host usage
-- will remain in scope if their statement is migrated.
optimizeStm :: Stms GPU -> Stm GPU -> ReduceM (Stms GPU)
optimizeStm out stm = do
  move <- asks (shouldMoveStm stm)
  if move
    then moveStm out stm
    else case stmExp stm of
      BasicOp (Update safety arr slice (Var v))
        | Just _ <- sliceIndices slice -> do
            -- Rewrite the Update if its write value has been migrated. Copying
            -- is faster than doing a synchronous write, so we use the device
            -- array even if the value has been made available to the host.
            dev <- storedScalar (Var v)
            case dev of
              Nothing -> pure (out |> stm)
              Just dst -> do
                -- Transform the single element Update into a slice Update.
                let dims = unSlice slice
                let (outer, [DimFix i]) = splitAt (length dims - 1) dims
                let one = intConst Int64 1
                let slice' = Slice $ outer ++ [DimSlice i one one]
                let e = BasicOp (Update safety arr slice' (Var dst))
                let stm' = stm {stmExp = e}

                pure (out |> stm')
      BasicOp (Replicate (Shape dims) (Var v))
        | Pat [PatElem n arr_t] <- stmPat stm -> do
            -- A Replicate can be rewritten to not require its replication value
            -- to be available on host. If its value is migrated the Replicate
            -- thus needs to be transformed.
            --
            -- If the inner dimension of the replication array is one then the
            -- rewrite can be performed more efficiently than the general case.
            v' <- resolveName v
            let v_kept_on_device = v /= v'

            gpubody_ok <- gets stateGPUBodyOk

            case v_kept_on_device of
              False -> pure (out |> stm)
              True
                | all (== intConst Int64 1) dims,
                  Just t' <- peelArray 1 arr_t,
                  gpubody_ok -> do
                    let n' = VName (baseName n `withSuffix` "_inner") 0
                    let pat' = Pat [PatElem n' t']
                    let e' = BasicOp $ Replicate (Shape $ tail dims) (Var v)
                    let stm' = Let pat' (stmAux stm) e'

                    -- `gpu { v }` is slightly faster than `replicate 1 v` and
                    -- can merge with the GPUBody that v was computed by.
                    gpubody <- inGPUBody (rewriteStm stm')
                    pure (out |> gpubody {stmPat = stmPat stm})
              True
                | last dims == intConst Int64 1 ->
                    let e' = BasicOp $ Replicate (Shape $ init dims) (Var v')
                        stm' = stm {stmExp = e'}
                     in pure (out |> stm')
              True -> do
                n' <- newName n
                -- v_kept_on_device implies that v is a scalar.
                let dims' = dims ++ [intConst Int64 1]
                let arr_t' = Array (elemType arr_t) (Shape dims') NoUniqueness
                let pat' = Pat [PatElem n' arr_t']
                let e' = BasicOp $ Replicate (Shape dims) (Var v')
                let repl = Let pat' (stmAux stm) e'

                let slice = map sliceDim (arrayDims arr_t)
                let slice' = slice ++ [DimFix $ intConst Int64 0]
                let idx = BasicOp $ Index n' (Slice slice')
                let index = Let (stmPat stm) (defAux ()) idx

                pure (out |> repl |> index)
      BasicOp {} ->
        pure (out |> stm)
      Apply {} ->
        pure (out |> stm)
      Match ses cases defbody (MatchDec btypes sort) -> do
        -- Rewrite branches.
        cases_stms <- mapM (optimizeStms . bodyStms . caseBody) cases
        let cases_res = map (bodyResult . caseBody) cases
        defbody_stms <- optimizeStms $ bodyStms defbody
        let defbody_res = bodyResult defbody

        -- Ensure return values and types match if one or both branches
        -- return a result that now reside on device.
        let bmerge (acc, all_stms) (pe, reses, bt) = do
              let onHost (Var v) = (v ==) <$> resolveName v
                  onHost _ = pure True

              on_host <- and <$> mapM (onHost . resSubExp) reses

              if on_host
                then -- No result resides on device ==> nothing to do.
                  pure ((pe, reses, bt) : acc, all_stms)
                else do
                  -- Otherwise, ensure all results are migrated.
                  (all_stms', arrs) <-
                    fmap unzip $
                      forM (zip all_stms reses) $ \(stms, res) ->
                        storeScalar stms (resSubExp res) (patElemType pe)

                  pe' <- arrayizePatElem pe
                  let bt' = staticShapes1 (patElemType pe')
                      reses' = zipWith SubExpRes (map resCerts reses) (map Var arrs)
                  pure ((pe', reses', bt') : acc, all_stms')

            pes = patElems (stmPat stm)
        (acc, ~(defbody_stms' : cases_stms')) <-
          foldM bmerge ([], defbody_stms : cases_stms) $
            zip3 pes (transpose $ defbody_res : cases_res) btypes
        let (pes', reses, btypes') = unzip3 (reverse acc)

        -- Rewrite statement.
        let cases' =
              zipWith Case (map casePat cases) $
                zipWith mkBody cases_stms' $
                  drop 1 $
                    transpose reses
            defbody' = mkBody defbody_stms' $ map head reses
            e' = Match ses cases' defbody' (MatchDec btypes' sort)
            stm' = Let (Pat pes') (stmAux stm) e'

        -- Read migrated scalars that are used on host.
        foldM addRead (out |> stm') (zip pes pes')
      Loop params lform body -> do
        -- Update statement bound variables and parameters if their values
        -- have been migrated to device.
        let lmerge (res, stms, rebinds) (pe, param, StayOnHost) =
              pure ((pe, param) : res, stms, rebinds)
            lmerge (res, stms, rebinds) (pe, (Param _ pn pt, pval), _) = do
              -- Migrate the bound variable.
              pe' <- arrayizePatElem pe

              -- Move the initial value to device if not already there to
              -- ensure that the parameter argument and loop return value
              -- converge.
              (stms', arr) <- storeScalar stms pval (fromDecl pt)

              -- Migrate the parameter.
              pn' <- newName pn
              let pt' = toDecl (patElemType pe') Nonunique
              let pval' = Var arr
              let param' = (Param mempty pn' pt', pval')

              -- Record the migration and rebind the parameter inside the
              -- loop body if necessary.
              rebinds' <- (pe {patElemName = pn}) `migratedTo` (pn', rebinds)

              pure ((pe', param') : res, stms', rebinds')

        mt <- ask
        let pes = patElems (stmPat stm)
        let mss = map (\(Param _ n _, _) -> statusOf n mt) params
        (zipped', out', rebinds) <-
          foldM lmerge ([], out, mempty) (zip3 pes params mss)
        let (pes', params') = unzip (reverse zipped')

        -- Rewrite body.
        let body1 = body {bodyStms = rebinds >< bodyStms body}
        body2 <- optimizeBody body1
        let zipped =
              zip4
                mss
                (bodyResult body2)
                (map resSubExp $ bodyResult body)
                (map patElemType pes)
        let rstore (bstms, res) (StayOnHost, r, _, _) =
              pure (bstms, r : res)
            rstore (bstms, res) (_, SubExpRes certs _, se, t) = do
              (bstms', dev) <- storeScalar bstms se t
              pure (bstms', SubExpRes certs (Var dev) : res)
        (bstms, res) <- foldM rstore (bodyStms body2, []) zipped
        let body3 = body2 {bodyStms = bstms, bodyResult = reverse res}

        -- Rewrite statement.
        let e' = Loop params' lform body3
        let stm' = Let (Pat pes') (stmAux stm) e'

        -- Read migrated scalars that are used on host.
        foldM addRead (out' |> stm') (zip pes pes')
      WithAcc inputs lmd -> do
        let getAcc (Acc a _ _ _) = a
            getAcc _ =
              compilerBugS
                "Type error: WithAcc expression did not return accumulator."

        let accs = zipWith (\t i -> (getAcc t, i)) (lambdaReturnType lmd) inputs
        inputs' <- mapM (uncurry optimizeWithAccInput) accs

        let body = lambdaBody lmd
        stms' <- optimizeStms (bodyStms body)

        let rewrite (SubExpRes certs se, t, pe) =
              do
                se' <- resolveSubExp se
                if se == se'
                  then pure (SubExpRes certs se, t, pe)
                  else do
                    pe' <- arrayizePatElem pe
                    let t' = patElemType pe'
                    pure (SubExpRes certs se', t', pe')

        -- Rewrite non-accumulator results that have been migrated.
        --
        -- Accumulator return values do not map to arrays one-to-one but
        -- one-to-many. They are not transformed however and can be mapped
        -- as a no-op.
        let len = length inputs
        let (res0, res1) = splitAt len (bodyResult body)
        let (rts0, rts1) = splitAt len (lambdaReturnType lmd)
        let pes = patElems (stmPat stm)
        let (pes0, pes1) = splitAt (length pes - length res1) pes
        (res1', rts1', pes1') <- unzip3 <$> mapM rewrite (zip3 res1 rts1 pes1)
        let res' = res0 ++ res1'
        let rts' = rts0 ++ rts1'
        let pes' = pes0 ++ pes1'

        -- Rewrite statement.
        let body' = Body () stms' res'
        let lmd' = lmd {lambdaBody = body', lambdaReturnType = rts'}
        let e' = WithAcc inputs' lmd'
        let stm' = Let (Pat pes') (stmAux stm) e'

        -- Read migrated scalars that are used on host.
        foldM addRead (out |> stm') (zip pes pes')
      Op op -> do
        op' <- optimizeHostOp op
        pure (out |> stm {stmExp = Op op'})
  where
    addRead stms (pe@(PatElem n _), PatElem dev _)
      | n == dev = pure stms
      | otherwise = pe `migratedTo` (dev, stms)

-- | Optimize an accumulator input. The 'VName' is the accumulator token.
optimizeWithAccInput :: VName -> WithAccInput GPU -> ReduceM (WithAccInput GPU)
optimizeWithAccInput _ (shape, arrs, Nothing) = pure (shape, arrs, Nothing)
optimizeWithAccInput acc (shape, arrs, Just (op, nes)) = do
  device_only <- asks (shouldMove acc)
  if device_only
    then do
      op' <- addReadsToLambda op
      pure (shape, arrs, Just (op', nes))
    else do
      let body = lambdaBody op
      -- To pass type check neither parameters nor results can change.
      --
      -- op may be used on both host and device so we must avoid introducing
      -- any GPUBody statements.
      stms' <- noGPUBody $ optimizeStms (bodyStms body)
      let op' = op {lambdaBody = body {bodyStms = stms'}}
      pure (shape, arrs, Just (op', nes))

-- | Optimize a host operation. 'Index' statements are added to kernel code
-- that depends on migrated scalars.
optimizeHostOp :: HostOp op GPU -> ReduceM (HostOp op GPU)
optimizeHostOp (SegOp (SegMap lvl space types kbody)) =
  SegOp . SegMap lvl space types <$> addReadsToBody kbody
optimizeHostOp (SegOp (SegRed lvl space types kbody ops)) = do
  ops' <- mapM addReadsToSegBinOp ops
  kbody' <- addReadsToBody kbody
  pure . SegOp $ SegRed lvl space types kbody' ops'
optimizeHostOp (SegOp (SegScan lvl space types kbody ops)) = do
  ops' <- mapM addReadsToSegBinOp ops
  kbody' <- addReadsToBody kbody
  pure . SegOp $ SegScan lvl space types kbody' ops'
optimizeHostOp (SegOp (SegHist lvl space types kbody ops)) = do
  ops' <- mapM addReadsToHistOp ops
  kbody' <- addReadsToBody kbody
  pure . SegOp $ SegHist lvl space types kbody' ops'
optimizeHostOp (SizeOp op) =
  pure (SizeOp op)
optimizeHostOp OtherOp {} =
  -- These should all have been taken care of in the unstreamGPU pass.
  compilerBugS "optimizeHostOp: unhandled OtherOp"
optimizeHostOp (GPUBody types body) =
  GPUBody types <$> addReadsToBody body

--------------------------------------------------------------------------------
--                               COMMON HELPERS                               --
--------------------------------------------------------------------------------

-- | Append the given string to a name.
withSuffix :: Name -> String -> Name
withSuffix name sfx = nameFromText $ T.append (nameToText name) (T.pack sfx)

--------------------------------------------------------------------------------
--                             MIGRATION - TYPES                              --
--------------------------------------------------------------------------------

-- | The monad used to perform migration-based synchronization reductions.
newtype ReduceM a = ReduceM (StateT State (Reader MigrationTable) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState State,
      MonadReader MigrationTable
    )

runReduceM :: (MonadFreshNames m) => MigrationTable -> ReduceM a -> m a
runReduceM mt (ReduceM m) = modifyNameSource $ \src ->
  second stateNameSource (runReader (runStateT m (initialState src)) mt)

instance MonadFreshNames ReduceM where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

-- | The state used by a 'ReduceM' monad.
data State = State
  { -- | A source to generate new 'VName's from.
    stateNameSource :: VNameSource,
    -- | A table of variables in the original program which have been migrated
    -- to device. Each variable maps to a tuple that describes:
    --   * 'baseName' of the original variable.
    --   * Type of the original variable.
    --   * Name of the single element array holding the migrated value.
    --   * Whether the original variable still can be used on the host.
    stateMigrated :: IM.IntMap (Name, Type, VName, Bool),
    -- | Whether non-migration optimizations may introduce 'GPUBody' kernels at
    -- the current location.
    stateGPUBodyOk :: Bool
  }

--------------------------------------------------------------------------------
--                           MIGRATION - PRIMITIVES                           --
--------------------------------------------------------------------------------

-- | An initial state to use when running a 'ReduceM' monad.
initialState :: VNameSource -> State
initialState ns =
  State
    { stateNameSource = ns,
      stateMigrated = mempty,
      stateGPUBodyOk = True
    }

-- | Perform non-migration optimizations without introducing any GPUBody
-- kernels.
noGPUBody :: ReduceM a -> ReduceM a
noGPUBody m = do
  prev <- gets stateGPUBodyOk
  modify $ \st -> st {stateGPUBodyOk = False}
  res <- m
  modify $ \st -> st {stateGPUBodyOk = prev}
  pure res

-- | Create a 'PatElem' that binds the array of a migrated variable binding.
arrayizePatElem :: PatElem Type -> ReduceM (PatElem Type)
arrayizePatElem (PatElem n t) = do
  let name = baseName n `withSuffix` "_dev"
  dev <- newName (VName name 0)
  let dev_t = t `arrayOfRow` intConst Int64 1
  pure (PatElem dev dev_t)

-- | @x `movedTo` arr@ registers that the value of @x@ has been migrated to
-- @arr[0]@.
movedTo :: Ident -> VName -> ReduceM ()
movedTo = recordMigration False

-- | @x `aliasedBy` arr@ registers that the value of @x@ also is available on
-- device as @arr[0]@.
aliasedBy :: Ident -> VName -> ReduceM ()
aliasedBy = recordMigration True

-- | @recordMigration host x arr@ records the migration of variable @x@ to
-- @arr[0]@. If @host@ then the original binding can still be used on host.
recordMigration :: Bool -> Ident -> VName -> ReduceM ()
recordMigration host (Ident x t) arr =
  modify $ \st ->
    let migrated = stateMigrated st
        entry = (baseName x, t, arr, host)
        migrated' = IM.insert (baseTag x) entry migrated
     in st {stateMigrated = migrated'}

-- | @pe `migratedTo` (dev, stms)@ registers that the variable @pe@ in the
-- original program has been migrated to @dev@ and rebinds the variable if
-- deemed necessary, adding an index statement to the given statements.
migratedTo :: PatElem Type -> (VName, Stms GPU) -> ReduceM (Stms GPU)
migratedTo pe (dev, stms) = do
  used <- asks (usedOnHost $ patElemName pe)
  if used
    then patElemIdent pe `aliasedBy` dev >> pure (stms |> bind pe (eIndex dev))
    else patElemIdent pe `movedTo` dev >> pure stms

-- | @useScalar stms n@ returns a variable that binds the result bound by @n@
-- in the original program. If the variable has been migrated to device and have
-- not been copied back to host a new variable binding will be added to the
-- provided statements and be returned.
useScalar :: Stms GPU -> VName -> ReduceM (Stms GPU, VName)
useScalar stms n = do
  entry <- gets $ IM.lookup (baseTag n) . stateMigrated
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
bind :: PatElem Type -> Exp GPU -> Stm GPU
bind pe = Let (Pat [pe]) (defAux ())

-- | Returns the array alias of @se@ if it is a variable that has been migrated
-- to device. Otherwise returns @Nothing@.
storedScalar :: SubExp -> ReduceM (Maybe VName)
storedScalar (Constant _) = pure Nothing
storedScalar (Var n) = do
  entry <- gets $ IM.lookup (baseTag n) . stateMigrated
  pure $ fmap (\(_, _, arr, _) -> arr) entry

-- | @storeScalar stms se t@ returns a variable that binds a single element
-- array that contains the value of @se@ in the original program. If @se@ is a
-- variable that has been migrated to device, its existing array alias will be
-- used. Otherwise a new variable binding will be added to the provided
-- statements and be returned. @t@ is the type of @se@.
storeScalar :: Stms GPU -> SubExp -> Type -> ReduceM (Stms GPU, VName)
storeScalar stms se t = do
  entry <- case se of
    Var n -> gets $ IM.lookup (baseTag n) . stateMigrated
    _ -> pure Nothing
  case entry of
    Just (_, _, arr, _) -> pure (stms, arr)
    Nothing -> do
      -- How to most efficiently create an array containing the given value
      -- depends on whether it is a variable or a constant. Creating a constant
      -- array is a runtime copy of static memory, while creating an array that
      -- contains a variable results in a synchronous write. The latter is thus
      -- replaced with either a mergeable GPUBody kernel or a Replicate.
      --
      -- Whether it makes sense to hoist arrays out of bodies to enable CSE is
      -- left to the simplifier to figure out. Duplicates will be eliminated if
      -- a scalar is stored multiple times within a body.
      --
      -- TODO: Enable the simplifier to hoist non-consumed, non-returned arrays
      --       out of top-level function definitions. All constant arrays
      --       produced here are in principle top-level hoistable.
      gpubody_ok <- gets stateGPUBodyOk
      case se of
        Var n | gpubody_ok -> do
          n' <- newName n
          let stm = bind (PatElem n' t) (BasicOp $ SubExp se)

          gpubody <- inGPUBody (pure stm)
          let dev = patElemName $ head $ patElems (stmPat gpubody)

          pure (stms |> gpubody, dev)
        Var n -> do
          pe <- arrayizePatElem (PatElem n t)
          let shape = Shape [intConst Int64 1]
          let stm = bind pe (BasicOp $ Replicate shape se)
          pure (stms |> stm, patElemName pe)
        _ -> do
          let n = VName (nameFromString "const") 0
          pe <- arrayizePatElem (PatElem n t)
          let stm = bind pe (BasicOp $ ArrayLit [se] t)
          pure (stms |> stm, patElemName pe)

-- | Map a variable name to itself or, if the variable no longer can be used on
-- host, the name of a single element array containing its value.
resolveName :: VName -> ReduceM VName
resolveName n = do
  entry <- gets $ IM.lookup (baseTag n) . stateMigrated
  case entry of
    Nothing -> pure n
    Just (_, _, _, True) -> pure n
    Just (_, _, arr, _) -> pure arr

-- | Like 'resolveName' but for a t'SubExp'. Constants are mapped to themselves.
resolveSubExp :: SubExp -> ReduceM SubExp
resolveSubExp (Var n) = Var <$> resolveName n
resolveSubExp cnst = pure cnst

-- | Like 'resolveSubExp' but for a 'SubExpRes'.
resolveSubExpRes :: SubExpRes -> ReduceM SubExpRes
resolveSubExpRes (SubExpRes certs se) =
  -- Certificates are always read back to host.
  SubExpRes certs <$> resolveSubExp se

-- | Apply 'resolveSubExpRes' to a list of results.
resolveResult :: Result -> ReduceM Result
resolveResult = mapM resolveSubExpRes

-- | Migrate a statement to device, ensuring all its bound variables used on
-- host will remain available with the same names.
moveStm :: Stms GPU -> Stm GPU -> ReduceM (Stms GPU)
moveStm out (Let pat aux (BasicOp (ArrayLit [se] t')))
  | Pat [PatElem n _] <- pat =
      do
        -- Save an 'Index' by rewriting the 'ArrayLit' rather than migrating it.
        let n' = VName (baseName n `withSuffix` "_inner") 0
        let pat' = Pat [PatElem n' t']
        let e' = BasicOp (SubExp se)
        let stm' = Let pat' aux e'

        gpubody <- inGPUBody (rewriteStm stm')
        pure (out |> gpubody {stmPat = pat})
moveStm out stm = do
  -- Move the statement to device.
  gpubody <- inGPUBody (rewriteStm stm)

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
            -- Record the device alias of each scalar variable and read them
            -- if used on host.
            1 -> pe `migratedTo` (dev, stms)
            -- Drop the added dimension of multidimensional arrays.
            _ -> add $ Index dev (fullSlice dev_t [DimFix $ intConst Int64 0])

-- | Create a GPUBody kernel that executes a single statement and stores its
-- results in single element arrays.
inGPUBody :: RewriteM (Stm GPU) -> ReduceM (Stm GPU)
inGPUBody m = do
  (stm, st) <- runStateT m initialRState
  let prologue = rewritePrologue st

  let pes = patElems (stmPat stm)
  pat <- Pat <$> mapM arrayizePatElem pes
  let aux = defAux ()
  let types = map patElemType pes
  let res = map (SubExpRes mempty . Var . patElemName) pes
  let body = Body () (prologue |> stm) res
  let e = Op (GPUBody types body)
  pure (Let pat aux e)

--------------------------------------------------------------------------------
--                          KERNEL REWRITING - TYPES                          --
--------------------------------------------------------------------------------

-- The monad used to rewrite (migrated) kernel code.
type RewriteM = StateT RState ReduceM

-- | The state used by a 'RewriteM' monad.
data RState = RState
  { -- | Maps variables in the original program to names to be used by rewrites.
    rewriteRenames :: IM.IntMap VName,
    -- | Statements to be added as a prologue before rewritten statements.
    rewritePrologue :: Stms GPU
  }

--------------------------------------------------------------------------------
--                        KERNEL REWRITING - FUNCTIONS                        --
--------------------------------------------------------------------------------

-- | An initial state to use when running a 'RewriteM' monad.
initialRState :: RState
initialRState =
  RState
    { rewriteRenames = mempty,
      rewritePrologue = mempty
    }

-- | Rewrite 'SegBinOp' dependencies to scalars that have been migrated.
addReadsToSegBinOp :: SegBinOp GPU -> ReduceM (SegBinOp GPU)
addReadsToSegBinOp op = do
  f' <- addReadsToLambda (segBinOpLambda op)
  pure (op {segBinOpLambda = f'})

-- | Rewrite 'HistOp' dependencies to scalars that have been migrated.
addReadsToHistOp :: HistOp GPU -> ReduceM (HistOp GPU)
addReadsToHistOp op = do
  f' <- addReadsToLambda (histOp op)
  pure (op {histOp = f'})

-- | Rewrite generic lambda dependencies to scalars that have been migrated.
addReadsToLambda :: Lambda GPU -> ReduceM (Lambda GPU)
addReadsToLambda f = do
  body' <- addReadsToBody (lambdaBody f)
  pure (f {lambdaBody = body'})

-- | Rewrite generic body dependencies to scalars that have been migrated.
addReadsToBody ::
  (FreeIn res, Substitute res) =>
  GBody GPU res ->
  ReduceM (GBody GPU res)
addReadsToBody body = do
  (body', prologue) <- addReadsHelper body
  pure body' {bodyStms = prologue >< bodyStms body'}

-- | Rewrite migrated scalar dependencies within anything. The returned
-- statements must be added to the scope of the rewritten construct.
addReadsHelper :: (FreeIn a, Substitute a) => a -> ReduceM (a, Stms GPU)
addReadsHelper x = do
  let from = namesToList (freeIn x)
  (to, st) <- runStateT (mapM rename from) initialRState
  let rename_map = M.fromList (zip from to)
  pure (substituteNames rename_map x, rewritePrologue st)

-- | Create a fresh name, registering which name it is a rewrite of.
rewriteName :: VName -> RewriteM VName
rewriteName n = do
  n' <- lift (newName n)
  modify $ \st -> st {rewriteRenames = IM.insert (baseTag n) n' (rewriteRenames st)}
  pure n'

-- | Rewrite all bindings introduced by a body (to ensure they are unique) and
-- fix any dependencies that are broken as a result of migration or rewriting.
rewriteBody :: Body GPU -> RewriteM (Body GPU)
rewriteBody (Body _ stms res) = do
  stms' <- rewriteStms stms
  res' <- renameResult res
  pure (Body () stms' res')

-- | Rewrite all bindings introduced by a sequence of statements (to ensure they
-- are unique) and fix any dependencies that are broken as a result of migration
-- or rewriting.
rewriteStms :: Stms GPU -> RewriteM (Stms GPU)
rewriteStms = foldM rewriteTo mempty
  where
    rewriteTo out stm = do
      stm' <- rewriteStm stm
      pure $ case stmExp stm' of
        Op (GPUBody _ (Body _ stms res)) ->
          let pes = patElems (stmPat stm')
           in foldl' bnd (out >< stms) (zip pes res)
        _ -> out |> stm'

    bnd :: Stms GPU -> (PatElem Type, SubExpRes) -> Stms GPU
    bnd out (pe, SubExpRes cs se)
      | Just t' <- peelArray 1 (typeOf pe) =
          out |> Let (Pat [pe]) (StmAux cs mempty mempty ()) (BasicOp $ ArrayLit [se] t')
      | otherwise =
          out |> Let (Pat [pe]) (StmAux cs mempty mempty ()) (BasicOp $ SubExp se)

-- | Rewrite all bindings introduced by a single statement (to ensure they are
-- unique) and fix any dependencies that are broken as a result of migration or
-- rewriting.
--
-- NOTE: GPUBody kernels must be rewritten through 'rewriteStms'.
rewriteStm :: Stm GPU -> RewriteM (Stm GPU)
rewriteStm (Let pat aux e) = do
  e' <- rewriteExp e
  pat' <- rewritePat pat
  aux' <- rewriteStmAux aux
  pure (Let pat' aux' e')

-- | Rewrite all bindings introduced by a pattern (to ensure they are unique)
-- and fix any dependencies that are broken as a result of migration or
-- rewriting.
rewritePat :: Pat Type -> RewriteM (Pat Type)
rewritePat pat = Pat <$> mapM rewritePatElem (patElems pat)

-- | Rewrite the binding introduced by a single pattern element (to ensure it is
-- unique) and fix any dependencies that are broken as a result of migration or
-- rewriting.
rewritePatElem :: PatElem Type -> RewriteM (PatElem Type)
rewritePatElem (PatElem n t) = do
  n' <- rewriteName n
  t' <- renameType t
  pure (PatElem n' t')

-- | Fix any 'StmAux' certificate references that are broken as a result of
-- migration or rewriting.
rewriteStmAux :: StmAux () -> RewriteM (StmAux ())
rewriteStmAux aux = do
  certs' <- renameCerts $ stmAuxCerts aux
  pure $ aux {stmAuxCerts = certs'}

-- | Rewrite the bindings introduced by an expression (to ensure they are
-- unique) and fix any dependencies that are broken as a result of migration or
-- rewriting.
rewriteExp :: Exp GPU -> RewriteM (Exp GPU)
rewriteExp =
  mapExpM $
    Mapper
      { mapOnSubExp = renameSubExp,
        mapOnBody = const rewriteBody,
        mapOnVName = rename,
        mapOnRetType = renameExtType,
        mapOnBranchType = renameExtType,
        mapOnFParam = rewriteParam,
        mapOnLParam = rewriteParam,
        mapOnOp = const opError
      }
  where
    -- This indicates that something fundamentally is wrong with the migration
    -- table produced by the "Futhark.Analysis.MigrationTable" module.
    opError = compilerBugS "Cannot migrate a host-only operation to device."

-- | Rewrite the binding introduced by a single parameter (to ensure it is
-- unique) and fix any dependencies that are broken as a result of migration or
-- rewriting.
rewriteParam :: Param (TypeBase Shape u) -> RewriteM (Param (TypeBase Shape u))
rewriteParam (Param attrs n t) = do
  n' <- rewriteName n
  t' <- renameType t
  pure (Param attrs n' t')

-- | Return the name to use for a rewritten dependency.
rename :: VName -> RewriteM VName
rename n = do
  st <- get
  let renames = rewriteRenames st
  let idx = baseTag n
  case IM.lookup idx renames of
    Just n' -> pure n'
    _ ->
      do
        let stms = rewritePrologue st
        (stms', n') <- lift $ useScalar stms n
        modify $ \st' ->
          st'
            { rewriteRenames = IM.insert idx n' renames,
              rewritePrologue = stms'
            }
        pure n'

-- | Update the variable names within a 'Result' to account for migration and
-- rewriting.
renameResult :: Result -> RewriteM Result
renameResult = mapM renameSubExpRes

-- | Update the variable names within a 'SubExpRes' to account for migration and
-- rewriting.
renameSubExpRes :: SubExpRes -> RewriteM SubExpRes
renameSubExpRes (SubExpRes certs se) = do
  certs' <- renameCerts certs
  se' <- renameSubExp se
  pure (SubExpRes certs' se')

-- | Update the variable names of certificates to account for migration and
-- rewriting.
renameCerts :: Certs -> RewriteM Certs
renameCerts cs = Certs <$> mapM rename (unCerts cs)

-- | Update any variable name within a t'SubExp' to account for migration and
-- rewriting.
renameSubExp :: SubExp -> RewriteM SubExp
renameSubExp (Var n) = Var <$> rename n
renameSubExp se = pure se

-- | Update the variable names within a type to account for migration and
-- rewriting.
renameType :: TypeBase Shape u -> RewriteM (TypeBase Shape u)
-- Note: mapOnType also maps the VName token of accumulators
renameType = mapOnType renameSubExp

-- | Update the variable names within an existential type to account for
-- migration and rewriting.
renameExtType :: TypeBase ExtShape u -> RewriteM (TypeBase ExtShape u)
-- Note: mapOnExtType also maps the VName token of accumulators
renameExtType = mapOnExtType renameSubExp
