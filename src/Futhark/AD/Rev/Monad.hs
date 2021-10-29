{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
    insSubExpAdj,
    adjsReps,
    --
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
    unitAdjOfType,
    addLambda,
    --
    VjpOps (..),
    --
    setLoopTape,
    lookupLoopTape,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Substitute
import Futhark.Util (chunks)

zeroExp :: Type -> ExpT rep
zeroExp (Prim pt) =
  BasicOp $ SubExp $ Constant $ blankPrimValue pt
zeroExp (Array pt shape _) =
  BasicOp $ Replicate shape $ Constant $ blankPrimValue pt
zeroExp t = error $ "zeroExp: " ++ pretty t

onePrim :: PrimType -> PrimValue
onePrim (IntType it) = IntValue $ intValue it (1 :: Int)
onePrim (FloatType ft) = FloatValue $ floatValue ft (1 :: Double)
onePrim Bool = BoolValue True
onePrim Unit = UnitValue

oneExp :: Type -> ExpT rep
oneExp (Prim t) = BasicOp $ SubExp $ constant $ onePrim t
oneExp (Array pt shape _) =
  BasicOp $ Replicate shape $ Constant $ onePrim pt
oneExp t = error $ "oneExp: " ++ pretty t

-- | Whether 'Sparse' should check bounds or assume they are correct.
-- The latter results in simpler code.
data InBounds
  = -- | If a SubExp is provided, it references a boolean that is true
    -- when in-bounds.
    CheckBounds (Maybe SubExp)
  | AssumeBounds
  | -- | Dynamically these will always fail, so don't bother
    -- generating code for the update.  This is only needed to ensure
    -- a consistent representation of sparse Jacobians.
    OutOfBounds
  deriving (Eq, Ord, Show)

-- | A symbolic representation of an array that is all zeroes, except at one
-- index.
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

zeroArray :: MonadBuilder m => Shape -> Type -> m VName
zeroArray shape t = do
  zero <- letSubExp "zero" $ zeroExp t
  attributing (oneAttr "sequential") $
    letExp "zeroes" $ BasicOp $ Replicate shape zero

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

runADM :: MonadFreshNames m => ADM a -> m a
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

setAdj :: VName -> Adj -> ADM ()
setAdj v v_adj = modify $ \env ->
  env {stateAdjs = M.insert v v_adj $ stateAdjs env}

insAdj :: VName -> VName -> ADM ()
insAdj v = setAdj v . AdjVal . Var

adjVName :: VName -> ADM VName
adjVName v = newVName (baseString v <> "_adj")

returnSweepCode :: ADM a -> ADM a
returnSweepCode m = do
  (a, stms) <- collectStms m
  substs <- gets stateSubsts
  addStms $ substituteNames substs stms
  modify $ \env -> env {stateAdjs = M.fromList $ map (substituteNames substs) $ M.toList $ stateAdjs env}
  pure a

addSubstitution :: VName -> VName -> ADM ()
addSubstitution v v' = modify $ \env ->
  env {stateSubsts = M.insert v v' $ stateSubsts env}

-- While evaluationg this action, pretend these variables have no
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
        newParam (baseString v <> "_p") . rowType =<< lookupType v
      ((ret, res), stms) <- collectStms . localScope (scopeOfLParams (iparam : params)) $ do
        res <- tabNest' (paramName iparam : is) (n - 1) (map paramName params) f
        ret <- mapM lookupType res
        pure (ret, varsRes res)
      let lam = Lambda (iparam : params) (Body () stms res) ret
      letTupExp "tab" $ Op $ Screma w (iota : vs) (mapSOAC lam)

-- | Construct a lambda for adding two values of the given type.
addLambda :: Type -> ADM Lambda
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
addExp :: VName -> VName -> ADM Exp
addExp x y = do
  x_t <- lookupType x
  case x_t of
    Prim pt ->
      pure $ BasicOp $ BinOp (addBinOp pt) (Var x) (Var y)
    Array {} -> do
      lam <- addLambda $ rowType x_t
      pure $ Op $ Screma (arraySize 0 x_t) [x, y] (mapSOAC lam)
    _ ->
      error $ "addExp: unexpected type: " ++ pretty x_t

lookupAdj :: VName -> ADM Adj
lookupAdj v = do
  maybeAdj <- gets $ M.lookup v . stateAdjs
  case maybeAdj of
    Nothing -> do
      v_t <- lookupType v
      pure $ AdjZero (arrayShape v_t) (elemType v_t)
    Just v_adj -> pure v_adj

lookupAdjVal :: VName -> ADM VName
lookupAdjVal v = adjVal =<< lookupAdj v

updateAdj :: VName -> VName -> ADM ()
updateAdj v d = do
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
              letTupExp "acc" $
                BasicOp $ UpdateAcc v_adj' (map Var is) [Var d']
          insAdj v v_adj'
        _ -> do
          v_adj' <- letExp (baseString v <> "_adj") =<< addExp v_adj d
          insAdj v v_adj'

updateAdjSlice :: Slice SubExp -> VName -> VName -> ADM ()
updateAdjSlice (Slice [DimFix i]) v d =
  updateAdjIndex v (AssumeBounds, i) (Var d)
updateAdjSlice slice v d = do
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
              fixSlice (fmap pe64 slice) $ map le64 is
          letTupExp (baseString v_adj') . BasicOp $
            UpdateAcc v_adj' slice' [Var d']
      pure v_adj'
    _ -> do
      v_adjslice <-
        if primType t
          then pure v_adj
          else letExp (baseString v ++ "_slice") $ BasicOp $ Index v_adj slice
      letInPlace "updated_adj" v_adj slice =<< addExp v_adjslice d
  insAdj v v_adj'

updateSubExpAdj :: SubExp -> VName -> ADM ()
updateSubExpAdj Constant {} _ = pure ()
updateSubExpAdj (Var v) d = void $ updateAdj v d

insSubExpAdj :: SubExp -> VName -> ADM ()
insSubExpAdj Constant {} _ = pure ()
insSubExpAdj (Var v) d = void $ insAdj v d

-- The index may be negative, in which case the update has no effect.
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
          Acc {}
            | check == OutOfBounds ->
              pure v_adj
            | otherwise -> do
              dims <- arrayDims <$> lookupType se_v
              ~[v_adj'] <-
                tabNest (length dims) [se_v, v_adj] $ \is [se_v', v_adj'] ->
                  letTupExp "acc" $
                    BasicOp $ UpdateAcc v_adj' (i : map Var is) [Var se_v']
              pure v_adj'
          _ -> do
            let stms s = do
                  v_adj_i <- letExp (baseString v_adj <> "_i") $ BasicOp $ Index v_adj $ fullSlice v_adj_t [DimFix i]
                  se_update <- letSubExp "updated_adj_i" =<< addExp se_v v_adj_i
                  letExp (baseString v_adj) $
                    BasicOp $ Update s v_adj (fullSlice v_adj_t [DimFix i]) se_update
            case check of
              CheckBounds _ -> stms Safe
              AssumeBounds -> stms Unsafe
              OutOfBounds -> pure v_adj

-- | Is this primal variable active in the AD sense?  FIXME: this is
-- (obviously) much too conservative.
isActive :: VName -> ADM Bool
isActive = fmap (/= Prim Unit) . lookupType

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
  { vjpLambda :: [Adj] -> [VName] -> Lambda -> ADM Lambda,
    vjpStm :: Stm -> ADM () -> ADM ()
  }

setLoopTape :: VName -> VName -> ADM ()
setLoopTape v vs = modify $ \env ->
  env {stateLoopTape = M.insert v vs $ stateLoopTape env}

lookupLoopTape :: VName -> ADM (Maybe VName)
lookupLoopTape v = gets $ M.lookup v . stateLoopTape
