{-# LANGUAGE TypeFamilies #-}

-- Naming scheme:
--
-- An adjoint-related object for "x" is named "x_adj".  This means
-- both actual adjoints and statements.
--
-- Do not assume "x'" means anything related to derivatives.
module Futhark.AD.Rev.Monad
  ( ADM,
    RState (..),
    runADM,
    Adj (..),
    InBounds (..),
    Sparse (..),
    adjFromParam,
    adjFromVar,
    lookupAdj,
    lookupAdjVal,
    adjVal,
    updateAdj,
    updateSubExpAdj,
    updateAdjSlice,
    updateAdjIndex,
    setAdj,
    insAdj,
    adjsReps,
    --
    copyConsumedArrsInStm,
    copyConsumedArrsInBody,
    addSubstitution,
    returnSweepCode,
    --
    adjVName,
    subAD,
    noAdjsFor,
    subSubsts,
    isActive,
    --
    tabNest,
    oneExp,
    zeroExp,
    zeroArray,
    unitAdjOfType,
    addLambda,
    --
    VjpOps (..),
    --
    setLoopTape,
    lookupLoopTape,
    substLoopTape,
    renameLoopTape,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.Map qualified as M
import Data.Maybe
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases (consumedInStms)
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Substitute
import Futhark.Util (chunks)

zeroExp :: Type -> Exp rep
zeroExp (Prim pt) =
  BasicOp $ SubExp $ Constant $ blankPrimValue pt
zeroExp (Array pt shape _) =
  BasicOp $ Replicate shape $ Constant $ blankPrimValue pt
zeroExp t = error $ "zeroExp: " ++ prettyString t

onePrim :: PrimType -> PrimValue
onePrim (IntType it) = IntValue $ intValue it (1 :: Int)
onePrim (FloatType ft) = FloatValue $ floatValue ft (1 :: Double)
onePrim Bool = BoolValue True
onePrim Unit = UnitValue

oneExp :: Type -> Exp rep
oneExp (Prim t) = BasicOp $ SubExp $ constant $ onePrim t
oneExp (Array pt shape _) =
  BasicOp $ Replicate shape $ Constant $ onePrim pt
oneExp t = error $ "oneExp: " ++ prettyString t

-- | Whether 'Sparse' should check bounds or assume they are correct.
-- The latter results in simpler code.
data InBounds
  = -- | If a SubExp is provided, it references a boolean that is true
    -- when in-bounds.
    CheckBounds (Maybe SubExp)
  | -- | Assume that these are always in-bounds.
    AssumeBounds
  | -- | Dynamically these will always fail, so don't bother
    -- generating code for the update.  This is only needed to ensure
    -- a consistent representation of sparse Jacobians.
    OutOfBounds
  deriving (Eq, Ord, Show)

-- | A symbolic representation of an array that is all zeroes, except
-- at certain indexes.
data Sparse = Sparse
  { -- | The shape of the array.
    sparseShape :: Shape,
    -- | Element type of the array.
    sparseType :: PrimType,
    -- | Locations and values of nonzero values.  Indexes may be
    -- negative, in which case the value is ignored (unless
    -- 'AssumeBounds' is used).
    sparseIdxVals :: [(InBounds, SubExp, SubExp)]
  }
  deriving (Eq, Ord, Show)

-- | The adjoint of a variable.
data Adj
  = AdjSparse Sparse
  | AdjVal SubExp
  | AdjZero Shape PrimType
  deriving (Eq, Ord, Show)

instance Substitute Adj where
  substituteNames m (AdjVal (Var v)) = AdjVal $ Var $ substituteNames m v
  substituteNames _ adj = adj

-- | Create an array of the given shape and element type consisting of zeroes.
-- The shape may be empty, meaning this function can (despite its name) also
-- create non-arrays.
zeroArray :: (MonadBuilder m) => Shape -> Type -> m VName
zeroArray shape t
  | shapeRank shape == 0 =
      letExp "zero" $ zeroExp t
  | otherwise = do
      zero <- letSubExp "zero" $ zeroExp t
      attributing (oneAttr "sequential") $
        letExp "zeroes_" . BasicOp $
          Replicate shape zero

sparseArray :: (MonadBuilder m, Rep m ~ SOACS) => Sparse -> m VName
sparseArray (Sparse shape t ivs) = do
  flip (foldM f) ivs =<< zeroArray shape (Prim t)
  where
    arr_t = Prim t `arrayOfShape` shape
    f arr (check, i, se) = do
      let stm s =
            letExp "sparse" . BasicOp $
              Update s arr (fullSlice arr_t [DimFix i]) se
      case check of
        AssumeBounds -> stm Unsafe
        CheckBounds _ -> stm Safe
        OutOfBounds -> pure arr

adjFromVar :: VName -> Adj
adjFromVar = AdjVal . Var

adjFromParam :: Param t -> Adj
adjFromParam = adjFromVar . paramName

unitAdjOfType :: Type -> ADM Adj
unitAdjOfType t = AdjVal <$> letSubExp "adj_unit" (oneExp t)

-- | The values representing an adjoint in symbolic form.  This is
-- used for when we wish to return an Adj from a Body or similar
-- without forcing manifestation.  Also returns a function for
-- reassembling the Adj from a new representation (the list must have
-- the same length).
adjRep :: Adj -> ([SubExp], [SubExp] -> Adj)
adjRep (AdjVal se) = ([se], \[se'] -> AdjVal se')
adjRep (AdjZero shape pt) = ([], \[] -> AdjZero shape pt)
adjRep (AdjSparse (Sparse shape pt ivs)) =
  (concatMap ivRep ivs, AdjSparse . Sparse shape pt . repIvs ivs)
  where
    ivRep (_, i, v) = [i, v]
    repIvs ((check, _, _) : ivs') (i : v : ses) =
      (check', i, v) : repIvs ivs' ses
      where
        check' = case check of
          AssumeBounds -> AssumeBounds
          CheckBounds b -> CheckBounds b
          OutOfBounds -> CheckBounds (Just (constant False)) -- sic!
    repIvs _ _ = []

-- | Conveniently convert a list of Adjs to their representation, as
-- well as produce a function for converting back.
adjsReps :: [Adj] -> ([SubExp], [SubExp] -> [Adj])
adjsReps adjs =
  let (reps, fs) = unzip $ map adjRep adjs
   in (concat reps, zipWith ($) fs . chunks (map length reps))

data RState = RState
  { stateAdjs :: M.Map VName Adj,
    stateLoopTape :: Substitutions,
    stateSubsts :: Substitutions,
    stateNameSource :: VNameSource
  }

newtype ADM a = ADM (BuilderT SOACS (State RState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState RState,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadBuilder ADM where
  type Rep ADM = SOACS
  mkExpDecM pat e = ADM $ mkExpDecM pat e
  mkBodyM bnds res = ADM $ mkBodyM bnds res
  mkLetNamesM pat e = ADM $ mkLetNamesM pat e

  addStms = ADM . addStms
  collectStms (ADM m) = ADM $ collectStms m

instance MonadFreshNames (State RState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify (\env -> env {stateNameSource = src})

runADM :: (MonadFreshNames m) => ADM a -> m a
runADM (ADM m) =
  modifyNameSource $ \vn ->
    second stateNameSource $
      runState
        (fst <$> runBuilderT m mempty)
        (RState mempty mempty mempty vn)

adjVal :: Adj -> ADM VName
adjVal (AdjVal se) = letExp "const_adj" $ BasicOp $ SubExp se
adjVal (AdjSparse sparse) = sparseArray sparse
adjVal (AdjZero shape t) = zeroArray shape $ Prim t

-- | Set a specific adjoint.
setAdj :: VName -> Adj -> ADM ()
setAdj v v_adj = modify $ \env ->
  env {stateAdjs = M.insert v v_adj $ stateAdjs env}

-- | Set an 'AdjVal' adjoint.  Simple wrapper around 'setAdj'.
insAdj :: VName -> VName -> ADM ()
insAdj v = setAdj v . AdjVal . Var

adjVName :: VName -> ADM VName
adjVName v = newVName (baseName v <> "_adj")

-- | Create copies of all arrays consumed in the given statement, and
-- return statements which include copies of the consumed arrays.
--
-- See Note [Consumption].
copyConsumedArrsInStm :: Stm SOACS -> ADM (Substitutions, Stms SOACS)
copyConsumedArrsInStm s = inScopeOf s $ collectStms $ copyConsumedArrsInStm' s
  where
    copyConsumedArrsInStm' stm =
      let onConsumed v = inScopeOf s $ do
            v_t <- lookupType v
            case v_t of
              Array {} -> do
                v' <-
                  letExp (baseName v <> "_ad_copy") . BasicOp $
                    Replicate mempty (Var v)
                addSubstitution v' v
                pure [(v, v')]
              _ -> pure mempty

          consumed =
            namesToList . consumedInStms $
              fst (Alias.analyseStms mempty (oneStm stm))
       in M.fromList . mconcat <$> mapM onConsumed consumed

copyConsumedArrsInBody :: [VName] -> Body SOACS -> ADM Substitutions
copyConsumedArrsInBody dontCopy b =
  mconcat <$> mapM onConsumed (filter (`notElem` dontCopy) $ namesToList $ consumedInBody (Alias.analyseBody mempty b))
  where
    onConsumed v = do
      v_t <- lookupType v
      case v_t of
        Acc {} -> error $ "copyConsumedArrsInBody: Acc " <> prettyString v
        Array {} ->
          M.singleton v
            <$> letExp
              (baseName v <> "_ad_copy")
              (BasicOp $ Replicate mempty (Var v))
        _ -> pure mempty

returnSweepCode :: ADM a -> ADM a
returnSweepCode m = do
  (a, stms) <- collectStms m
  substs <- gets stateSubsts
  addStms $ substituteNames substs stms
  pure a

addSubstitution :: VName -> VName -> ADM ()
addSubstitution v v' = modify $ \env ->
  env {stateSubsts = M.insert v v' $ stateSubsts env}

-- While evaluating this action, pretend these variables have no
-- adjoints.  Restore current adjoints afterwards.  This is used for
-- handling certain nested operations. XXX: feels like this should
-- really be part of subAD, somehow.  Main challenge is that we don't
-- want to blank out Accumulator adjoints.  Also, might be inefficient
-- to blank out array adjoints.
noAdjsFor :: Names -> ADM a -> ADM a
noAdjsFor names m = do
  old <- gets $ \env -> mapMaybe (`M.lookup` stateAdjs env) names'
  modify $ \env -> env {stateAdjs = foldl' (flip M.delete) (stateAdjs env) names'}
  x <- m
  modify $ \env -> env {stateAdjs = M.fromList (zip names' old) <> stateAdjs env}
  pure x
  where
    names' = namesToList names

addBinOp :: PrimType -> BinOp
addBinOp (IntType it) = Add it OverflowWrap
addBinOp (FloatType ft) = FAdd ft
addBinOp Bool = LogAnd
addBinOp Unit = LogAnd

tabNest :: Int -> [VName] -> ([VName] -> [VName] -> ADM [VName]) -> ADM [VName]
tabNest = tabNest' []
  where
    tabNest' is 0 vs f = f (reverse is) vs
    tabNest' is n vs f = do
      vs_ts <- mapM lookupType vs
      let w = arraysSize 0 vs_ts
      iota <-
        letExp "tab_iota" . BasicOp $
          Iota w (intConst Int64 0) (intConst Int64 1) Int64
      iparam <- newParam "i" $ Prim int64
      params <- forM vs $ \v ->
        newParam (baseName v <> "_p") . rowType =<< lookupType v
      ((ret, res), stms) <- collectStms . localScope (scopeOfLParams (iparam : params)) $ do
        res <- tabNest' (paramName iparam : is) (n - 1) (map paramName params) f
        ret <- mapM lookupType res
        pure (ret, varsRes res)
      let lam = Lambda (iparam : params) ret (Body () stms res)
      letTupExp "tab" $ Op $ Screma w (iota : vs) (mapSOAC lam)

-- | Construct a lambda for adding two values of the given type.
addLambda :: Type -> ADM (Lambda SOACS)
addLambda (Prim pt) = binOpLambda (addBinOp pt) pt
addLambda t@Array {} = do
  xs_p <- newParam "xs" t
  ys_p <- newParam "ys" t
  lam <- addLambda $ rowType t
  body <- insertStmsM $ do
    res <-
      letSubExp "lam_map" . Op $
        Screma (arraySize 0 t) [paramName xs_p, paramName ys_p] (mapSOAC lam)
    pure $ resultBody [res]
  pure
    Lambda
      { lambdaParams = [xs_p, ys_p],
        lambdaReturnType = [t],
        lambdaBody = body
      }
addLambda t =
  error $ "addLambda: " ++ show t

-- Construct an expression for adding the two variables.
addExp :: VName -> VName -> ADM (Exp SOACS)
addExp x y = do
  x_t <- lookupType x
  case x_t of
    Prim pt ->
      pure $ BasicOp $ BinOp (addBinOp pt) (Var x) (Var y)
    Array {} -> do
      lam <- addLambda $ rowType x_t
      pure $ Op $ Screma (arraySize 0 x_t) [x, y] (mapSOAC lam)
    _ ->
      error $ "addExp: unexpected type: " ++ prettyString x_t

lookupAdj :: VName -> ADM Adj
lookupAdj v = do
  maybeAdj <- gets $ M.lookup v . stateAdjs
  case maybeAdj of
    Nothing -> do
      v_t <- lookupType v
      case v_t of
        Acc _ shape [Prim t] _ -> pure $ AdjZero shape t
        Acc _ shape [t] _ -> pure $ AdjZero (shape <> arrayShape t) (elemType t)
        Acc {} -> error $ "lookupAdj: Non-singleton accumulator adjoint: " <> prettyString v_t
        _ -> pure $ AdjZero (arrayShape v_t) (elemType v_t)
    Just v_adj -> pure v_adj

lookupAdjVal :: VName -> ADM VName
lookupAdjVal v = adjVal =<< lookupAdj v

updateAdjIndex :: VName -> (InBounds, SubExp) -> SubExp -> ADM ()
updateAdjIndex v (check, i) se = do
  maybeAdj <- gets $ M.lookup v . stateAdjs
  t <- lookupType v
  let iv = (check, i, se)
  case maybeAdj of
    Nothing -> do
      setAdj v $ AdjSparse $ Sparse (arrayShape t) (elemType t) [iv]
    Just AdjZero {} ->
      setAdj v $ AdjSparse $ Sparse (arrayShape t) (elemType t) [iv]
    Just (AdjSparse (Sparse shape pt ivs)) ->
      setAdj v $ AdjSparse $ Sparse shape pt $ iv : ivs
    Just adj@AdjVal {} -> do
      v_adj <- adjVal adj
      v_adj_t <- lookupType v_adj
      se_v <- letExp "se_v" $ BasicOp $ SubExp se
      insAdj v
        =<< case v_adj_t of
          Acc {} -> do
            let stms s = do
                  dims <- arrayDims <$> lookupType se_v
                  ~[v_adj'] <-
                    tabNest (length dims) [se_v, v_adj] $ \is [se_v', v_adj'] ->
                      letTupExp "acc" . BasicOp $
                        UpdateAcc s v_adj' (i : map Var is) [Var se_v']
                  pure v_adj'
            case check of
              CheckBounds _ -> stms Safe
              AssumeBounds -> stms Unsafe
              OutOfBounds -> pure v_adj
          _ -> do
            let stms s = do
                  v_adj_i <-
                    letExp (baseName v_adj <> "_i") . BasicOp $
                      Index v_adj $
                        fullSlice v_adj_t [DimFix i]
                  se_update <- letSubExp "updated_adj_i" =<< addExp se_v v_adj_i
                  letExp (baseName v_adj) . BasicOp $
                    Update s v_adj (fullSlice v_adj_t [DimFix i]) se_update
            case check of
              CheckBounds _ -> stms Safe
              AssumeBounds -> stms Unsafe
              OutOfBounds -> pure v_adj

updateAdjWithSafety :: VName -> VName -> Safety -> ADM ()
updateAdjWithSafety v d safety = do
  maybeAdj <- gets $ M.lookup v . stateAdjs
  case maybeAdj of
    Nothing ->
      insAdj v d
    Just adj -> do
      v_adj <- adjVal adj
      v_adj_t <- lookupType v_adj
      case v_adj_t of
        Acc {} -> do
          dims <- arrayDims <$> lookupType d
          ~[v_adj'] <-
            tabNest (length dims) [d, v_adj] $ \is [d', v_adj'] ->
              letTupExp "acc" . BasicOp $
                UpdateAcc safety v_adj' (map Var is) [Var d']
          insAdj v v_adj'
        _ -> do
          v_adj' <- letExp (baseName v <> "_adj") =<< addExp v_adj d
          insAdj v v_adj'

updateAdjSliceWithSafety :: Slice SubExp -> VName -> VName -> Safety -> ADM ()
updateAdjSliceWithSafety (Slice [DimFix i]) v d safety =
  updateAdjIndex v (bounds, i) (Var d)
  where
    bounds = case safety of
      Safe -> CheckBounds Nothing
      Unsafe -> AssumeBounds
updateAdjSliceWithSafety slice v d safety = do
  t <- lookupType v
  v_adj <- lookupAdjVal v
  v_adj_t <- lookupType v_adj
  v_adj' <- case v_adj_t of
    Acc {} -> do
      let dims = sliceDims slice
      ~[v_adj'] <-
        tabNest (length dims) [d, v_adj] $ \is [d', v_adj'] -> do
          slice' <-
            traverse (toSubExp "index") $
              fixSlice (fmap pe64 slice) $
                map le64 is
          letTupExp (baseName v_adj') . BasicOp $
            UpdateAcc safety v_adj' slice' [Var d']
      pure v_adj'
    _ -> do
      v_adjslice <-
        if primType t
          then pure v_adj
          else letExp (baseName v <> "_slice") $ BasicOp $ Index v_adj slice
      letInPlace "updated_adj" v_adj slice =<< addExp v_adjslice d
  insAdj v v_adj'

updateAdj :: VName -> VName -> ADM ()
updateAdj v d = updateAdjWithSafety v d Unsafe

updateAdjSlice :: Slice SubExp -> VName -> VName -> ADM ()
updateAdjSlice slice v d = updateAdjSliceWithSafety slice v d Unsafe

updateSubExpAdj :: SubExp -> VName -> ADM ()
updateSubExpAdj Constant {} _ = pure ()
updateSubExpAdj (Var v) d = void $ updateAdj v d

-- | Is this primal variable active in the AD sense?  FIXME: this is
-- (obviously) much too conservative.
isActive :: VName -> ADM Bool
isActive = fmap (/= Prim Unit) . lookupType

-- | Ignore any changes to adjoints made while evaluating this action.
subAD :: ADM a -> ADM a
subAD m = do
  old_state_adjs <- gets stateAdjs
  x <- m
  modify $ \s -> s {stateAdjs = old_state_adjs}
  pure x

subSubsts :: ADM a -> ADM a
subSubsts m = do
  old_state_substs <- gets stateSubsts
  x <- m
  modify $ \s -> s {stateSubsts = old_state_substs}
  pure x

data VjpOps = VjpOps
  { vjpLambda :: [Adj] -> [VName] -> Lambda SOACS -> ADM (Lambda SOACS),
    vjpStm :: Stm SOACS -> ADM () -> ADM ()
  }

-- | @setLoopTape v vs@ establishes @vs@ as the name of the array
-- where values of loop parameter @v@ from the forward pass are
-- stored.
setLoopTape :: VName -> VName -> ADM ()
setLoopTape v vs = modify $ \env ->
  env {stateLoopTape = M.insert v vs $ stateLoopTape env}

-- | Look-up the name of the array where @v@ is stored.
lookupLoopTape :: VName -> ADM (Maybe VName)
lookupLoopTape v = gets $ M.lookup v . stateLoopTape

-- | @substLoopTape v v'@ substitutes the key @v@ for @v'@. That is,
-- if @v |-> vs@ then after the substitution @v' |-> vs@ (and @v@
-- points to nothing).
substLoopTape :: VName -> VName -> ADM ()
substLoopTape v v' = mapM_ (setLoopTape v') =<< lookupLoopTape v

-- | Renames the keys of the loop tape. Useful for fixing the
-- the names in the loop tape after a loop rename.
renameLoopTape :: Substitutions -> ADM ()
renameLoopTape = mapM_ (uncurry substLoopTape) . M.toList

-- Note [Consumption]
--
-- Parts of this transformation depends on duplicating computation.
-- This is a problem when a primal expression consumes arrays (via
-- e.g. Update).  For example, consider how we handle this conditional:
--
--   if b then ys with [0] = 0 else ys
--
-- This consumes the array 'ys', which means that when we later
-- generate code for the return sweep, we can no longer use 'ys'.
-- This is a problem, because when we call 'diffBody' on the branch
-- bodies, we'll keep the primal code (maybe it'll be removed by
-- simplification later - we cannot know).  A similar issue occurs for
-- SOACs.  Our solution is to make copies of all consumes arrays:
--
--  let ys_copy = copy ys
--
-- Then we generate code for the return sweep as normal, but replace
-- _every instance_ of 'ys' in the generated code with 'ys_copy'.
-- This works because Futhark does not have *semantic* in-place
-- updates - any uniqueness violation can be replaced with copies (on
-- arrays, anyway).
--
-- If we are lucky, the uses of 'ys_copy' will be removed by
-- simplification, and there will be no overhead.  But even if not,
-- this is still (asymptotically) efficient because the array that is
-- being consumed must in any case have been produced within the code
-- that we are differentiating, so a copy is at most a scalar
-- overhead.  This is _not_ the case when loops are involved.
--
-- Also, the above only works for arrays, not accumulator variables.
-- Those will need some other mechanism.
