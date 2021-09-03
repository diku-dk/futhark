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
module Futhark.AD.Rev (revVJP) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (first, second)
import Data.List (foldl', (\\))
import qualified Data.Map as M
import Data.Maybe
import Futhark.AD.Derivatives
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases (consumedInStms)
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, pairs, splitAt3, takeLast, unpairs)

--- First some general utility functions that are not specific to AD.

eReverse :: MonadBuilder m => VName -> m VName
eReverse arr = do
  arr_t <- lookupType arr
  let w = arraySize 0 arr_t
  start <-
    letSubExp "rev_start" $
      BasicOp $ BinOp (Sub Int64 OverflowUndef) w (intConst Int64 1)
  let stride = intConst Int64 (-1)
      slice = fullSlice arr_t [DimSlice start w stride]
  letExp (baseString arr <> "_rev") $ BasicOp $ Index arr slice

eRotate :: MonadBuilder m => [SubExp] -> VName -> m VName
eRotate rots arr = letExp (baseString arr <> "_rot") $ BasicOp $ Rotate rots arr

scanExc ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  String ->
  Scan SOACS ->
  [VName] ->
  m [VName]
scanExc desc scan arrs = do
  w <- arraysSize 0 <$> mapM lookupType arrs
  form <- scanSOAC [scan]
  res_incl <- letTupExp (desc <> "_incl") $ Op $ Screma w arrs form
  res_incl_rot <- mapM (eRotate [intConst Int64 (-1)]) res_incl

  iota <-
    letExp "iota" . BasicOp $
      Iota w (intConst Int64 0) (intConst Int64 1) Int64

  iparam <- newParam "iota_param" $ Prim int64
  vparams <- mapM (newParam "vp") ts
  let params = iparam : vparams

  body <- runBodyBuilder . localScope (scopeOfLParams params) $ do
    let first_elem =
          eCmpOp
            (CmpEq int64)
            (eSubExp (Var (paramName iparam)))
            (eSubExp (intConst Int64 0))
    eBody
      [ eIf
          first_elem
          (resultBodyM nes)
          (resultBodyM $ map (Var . paramName) vparams)
      ]

  let lam = Lambda params body ts
  letTupExp desc $ Op $ Screma w (iota : res_incl_rot) (mapSOAC lam)
  where
    nes = scanNeutral scan
    ts = lambdaReturnType $ scanLambda scan

--- Now comes the AD.

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

adjVName :: VName -> ADM VName
adjVName v = newVName (baseString v <> "_adj")

adjVal :: Adj -> ADM VName
adjVal (AdjVal se) = letExp "const_adj" $ BasicOp $ SubExp se
adjVal (AdjSparse sparse) = sparseArray sparse
adjVal (AdjZero shape t) = zeroArray shape $ Prim t

setAdj :: VName -> Adj -> ADM ()
setAdj v v_adj = modify $ \env ->
  env {stateAdjs = M.insert v v_adj $ stateAdjs env}

insAdj :: VName -> VName -> ADM ()
insAdj v = setAdj v . AdjVal . Var

setLoopTape :: VName -> VName -> ADM ()
setLoopTape v vs = modify $ \env ->
  env {stateLoopTape = M.insert v vs $ stateLoopTape env}

lookupLoopTape :: VName -> ADM VName
lookupLoopTape v = do
  maybeVs <- gets $ M.lookup v . stateLoopTape
  case maybeVs of
    Nothing -> error "lookupLoopTape: didn't find stored values"
    Just vs -> return vs

returnSweepCode :: ADM a -> ADM a
returnSweepCode m = do
  (a, stms) <- collectStms m
  substs <- gets stateSubsts
  addStms $ substituteNames substs stms
  return a

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

-- Construct a lambda for adding two values of the given type.
addLambda :: Type -> ADM Lambda
addLambda (Prim pt) = binOpLambda (addBinOp pt) pt
addLambda t@Array {} = do
  xs <- newVName "xs"
  ys <- newVName "ys"
  lam <- addLambda $ rowType t
  body <- insertStmsM $ do
    res <- letSubExp "lam_map" $ Op $ Screma (arraySize 0 t) [xs, ys] (mapSOAC lam)
    pure $ resultBody [res]
  pure
    Lambda
      { lambdaParams = [Param xs t, Param ys t],
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
  let isDimFix (DimFix i) = i
      isDimFix _ =
        error $ "Invalid slice for accumulator update: " ++ pretty slice
  v_adj_t <- lookupType v_adj
  v_adj' <- case v_adj_t of
    Acc {} ->
      letExp (baseString v_adj) . BasicOp $
        UpdateAcc v_adj (map isDimFix (unSlice slice)) [Var d]
    _ -> do
      v_adjslice <-
        if primType t
          then pure v_adj
          else
            letExp (baseString v ++ "_slice") $
              BasicOp $ Index v_adj slice
      letInPlace "updated_adj" v_adj slice =<< addExp v_adjslice d
  insAdj v v_adj'

updateSubExpAdj :: SubExp -> VName -> ADM ()
updateSubExpAdj Constant {} _ = pure ()
updateSubExpAdj (Var v) d = void $ updateAdj v d

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

-- | Create copies of all arrays consumed in the expression, and
-- return a substituion mapping from the old names to the names of the
-- copies.
--
-- See Note [Consumption].
copyConsumedArrs :: Exp -> ADM Substitutions
copyConsumedArrs e =
  mconcat <$> mapM onConsumed (namesToList $ consumedInExp (Alias.analyseExp mempty e))
  where
    onConsumed v = do
      v_t <- lookupType v
      case v_t of
        Acc {} -> error $ "copyConsumedArrs: Acc " <> pretty v
        Array {} -> M.singleton v <$> letExp (baseString v <> "_ad_copy") (BasicOp $ Copy v)
        _ -> pure mempty

copyConsumedArrsInStms :: Stms SOACS -> ADM (Stms SOACS)
copyConsumedArrsInStms ss = inScopeOf ss $ collectStms_ $ copyConsumedArrsInStms' ss
  where
    copyConsumedArrsInStms' all_stms
      | Just (stm, stms) <- stmsHead all_stms = do
        let onConsumed v = inScopeOf all_stms $ do
              v_t <- lookupType v
              case v_t of
                Acc {} -> error $ "copyConsumedArrsInStms: Acc " <> pretty v
                Array {} -> do
                  addSubstitution v =<< letExp (baseString v <> "_ad_copy") (BasicOp $ Copy v)
                _ -> pure mempty
        addStm stm
        mconcat <$> mapM onConsumed (namesToList $ consumedInStms $ fst (Alias.analyseStms mempty (oneStm stm)))
        copyConsumedArrsInStms' stms
      | otherwise = pure ()

copyConsumedArrsInBody :: Body -> ADM Substitutions
copyConsumedArrsInBody b =
  mconcat <$> mapM onConsumed (namesToList $ consumedInBody (Alias.analyseBody mempty b))
  where
    onConsumed v = do
      v_t <- lookupType v
      case v_t of
        Acc {} -> error $ "copyConsumedArrs: Acc " <> pretty v
        Array {} -> M.singleton v <$> letExp (baseString v <> "_ad_copy") (BasicOp $ Copy v)
        _ -> pure mempty

patName :: Pat -> ADM VName
patName (Pat [pe]) = pure $ patElemName pe
patName pat = error $ "Expected single-element pattern: " ++ pretty pat

-- The vast majority of BasicOps require no special treatment in the
-- forward pass and produce one value (and hence once adjoint).  We
-- deal with that case here.
commonBasicOp :: Pat -> StmAux () -> BasicOp -> ADM () -> ADM (VName, VName)
commonBasicOp pat aux op m = do
  addStm $ Let pat aux $ BasicOp op
  m
  pat_v <- patName pat
  pat_adj <- lookupAdjVal pat_v
  pure (pat_v, pat_adj)

diffBasicOp :: Pat -> StmAux () -> BasicOp -> ADM () -> ADM ()
diffBasicOp pat aux e m =
  case e of
    CmpOp cmp x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        let t = cmpOpType cmp
            update contrib = do
              void $ updateSubExpAdj x contrib
              void $ updateSubExpAdj y contrib

        case t of
          FloatType ft ->
            update <=< letExp "contrib" $
              If
                (Var pat_adj)
                (resultBody [constant (floatValue ft (1 :: Int))])
                (resultBody [constant (floatValue ft (0 :: Int))])
                (IfDec [Prim (FloatType ft)] IfNormal)
          IntType it ->
            update <=< letExp "contrib" $ BasicOp $ ConvOp (BToI it) (Var pat_adj)
          Bool ->
            update pat_adj
          Unit ->
            pure ()
    --
    ConvOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        contrib <-
          letExp "contrib" $ BasicOp $ ConvOp (flipConvOp op) $ Var pat_adj
        updateSubExpAdj x contrib
    --
    UnOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      returnSweepCode $ do
        let t = unOpType op
        contrib <- do
          let x_pe = primExpFromSubExp t x
              pat_adj' = primExpFromSubExp t (Var pat_adj)
              dx = pdUnOp op x_pe
          letExp "contrib" <=< toExp $ pat_adj' ~*~ dx

        updateSubExpAdj x contrib
    --
    BinOp op x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      returnSweepCode $ do
        let t = binOpType op
            (wrt_x, wrt_y) =
              pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

            pat_adj' = primExpFromSubExp t $ Var pat_adj

        adj_x <- letExp "binop_x_adj" <=< toExp $ pat_adj' ~*~ wrt_x
        adj_y <- letExp "binop_y_adj" <=< toExp $ pat_adj' ~*~ wrt_y
        updateSubExpAdj x adj_x
        updateSubExpAdj y adj_y
    --
    SubExp se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ updateSubExpAdj se pat_adj
    --
    Assert {} ->
      void $ commonBasicOp pat aux e m
    --
    ArrayLit elems t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        forM_ (zip [(0 :: Int64) ..] elems) $ \(i, se) -> do
          let slice = fullSlice t [DimFix (constant i)]
          updateSubExpAdj se <=< letExp "elem_adj" $ BasicOp $ Index pat_adj slice
    --
    Index arr slice -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        void $ updateAdjSlice slice arr pat_adj
    FlatIndex {} -> error "FlatIndex not handled by AD yet."
    FlatUpdate {} -> error "FlatUpdate not handled by AD yet."
    --
    Opaque _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ updateSubExpAdj se pat_adj
    --
    Reshape _ arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        arr_dims <- arrayDims <$> lookupType arr
        void $
          updateAdj arr <=< letExp "adj_reshape" $
            BasicOp $ Reshape (map DimNew arr_dims) pat_adj
    --
    Rearrange perm arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $
        void $
          updateAdj arr <=< letExp "adj_rearrange" $
            BasicOp $ Rearrange (rearrangeInverse perm) pat_adj
    --
    Rotate rots arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        let neg = BasicOp . BinOp (Sub Int64 OverflowWrap) (intConst Int64 0)
        rots' <- mapM (letSubExp "rot_neg" . neg) rots
        void $
          updateAdj arr <=< letExp "adj_rotate" $
            BasicOp $ Rotate rots' pat_adj
    --
    Replicate (Shape ns) x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        x_t <- subExpType x
        lam <- addLambda x_t
        ne <- letSubExp "zero" $ zeroExp x_t
        n <- letSubExp "rep_size" =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ns
        pat_adj_flat <-
          letExp (baseString pat_adj <> "_flat") $
            BasicOp $ Reshape (map DimNew $ n : arrayDims x_t) pat_adj
        reduce <- reduceSOAC [Reduce Commutative lam [ne]]
        updateSubExpAdj x
          =<< letExp "rep_contrib" (Op $ Screma n [pat_adj_flat] reduce)
    --
    Concat d arr arrs _ -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        let sliceAdj _ [] = pure []
            sliceAdj start (v : vs) = do
              v_t <- lookupType v
              let w = arraySize 0 v_t
                  slice = DimSlice start w (intConst Int64 1)
              pat_adj_slice <-
                letExp (baseString pat_adj <> "_slice") $
                  BasicOp $ Index pat_adj (sliceAt v_t d [slice])
              start' <- letSubExp "start" $ BasicOp $ BinOp (Add Int64 OverflowUndef) start w
              slices <- sliceAdj start' vs
              pure $ pat_adj_slice : slices

        slices <- sliceAdj (intConst Int64 0) $ arr : arrs

        zipWithM_ updateAdj (arr : arrs) slices
    --
    Copy se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ void $ updateAdj se pat_adj
    --
    Manifest _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ void $ updateAdj se pat_adj
    --
    Scratch {} ->
      void $ commonBasicOp pat aux e m
    --
    Iota n _ _ t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        ne <- letSubExp "zero" $ zeroExp $ Prim $ IntType t
        lam <- addLambda $ Prim $ IntType t
        reduce <- reduceSOAC [Reduce Commutative lam [ne]]
        updateSubExpAdj n
          =<< letExp "iota_contrib" (Op $ Screma n [pat_adj] reduce)
    --
    Update safety arr slice v -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        v_adj <- letExp "update_val_adj" $ BasicOp $ Index pat_adj slice
        t <- lookupType v_adj
        v_adj_copy <-
          case t of
            Array {} -> letExp "update_val_adj_copy" $ BasicOp $ Copy v_adj
            _ -> return v_adj
        updateSubExpAdj v v_adj_copy
        zeroes <- letSubExp "update_zero" . zeroExp =<< subExpType v
        void $
          updateAdj arr
            =<< letExp "update_src_adj" (BasicOp $ Update safety pat_adj slice zeroes)
    UpdateAcc {} -> error "Reverse-mode UpdateAcc not handled yet."

commonSOAC :: Pat -> StmAux () -> SOAC SOACS -> ADM () -> ADM [Adj]
commonSOAC pat aux soac m = do
  addStm $ Let pat aux $ Op soac
  m
  returnSweepCode $
    mapM lookupAdj $ patNames pat

-- | A classification of a free variable based on its adjoint.  The
-- 'VName' stored is *not* the adjoint, but the primal variable.
data AdjVar
  = -- | Adjoint is already an accumulator.
    FreeAcc VName
  | -- | Currently has no adjoint, but should be given one, and is an
    -- array with this shape and element type.
    FreeArr VName Shape PrimType
  | -- | Does not need an accumulator adjoint (might still be an array).
    FreeNonAcc VName

classifyAdjVars :: [VName] -> ADM [AdjVar]
classifyAdjVars = mapM f
  where
    f v = do
      v_adj <- lookupAdjVal v
      v_adj_t <- lookupType v_adj
      case v_adj_t of
        Array pt shape _ ->
          pure $ FreeArr v shape pt
        Acc {} ->
          pure $ FreeAcc v
        _ ->
          pure $ FreeNonAcc v

partitionAdjVars :: [AdjVar] -> ([(VName, (Shape, PrimType))], [VName], [VName])
partitionAdjVars [] = ([], [], [])
partitionAdjVars (fv : fvs) =
  case fv of
    FreeArr v shape t -> ((v, (shape, t)) : xs, ys, zs)
    FreeAcc v -> (xs, v : ys, zs)
    FreeNonAcc v -> (xs, ys, v : zs)
  where
    (xs, ys, zs) = partitionAdjVars fvs

buildRenamedBody ::
  MonadBuilder m =>
  m (Result, a) ->
  m (BodyT (Rep m), a)
buildRenamedBody m = do
  (body, x) <- buildBody m
  body' <- renameBody body
  pure (body', x)

diffMap :: [Adj] -> SubExp -> Lambda -> [VName] -> Substitutions -> ADM ()
diffMap res_adjs w map_lam as substs
  | Just res_ivs <- mapM isSparse res_adjs,
    M.null substs = returnSweepCode $ do
    -- Since at most only a constant number of adjoint are nonzero
    -- (length res_ivs), there is no need for the return sweep code to
    -- contain a Map at all.

    free <- filterM isActive $ namesToList $ freeIn map_lam
    free_ts <- mapM lookupType free
    let adjs_for = map paramName (lambdaParams map_lam) ++ free
        adjs_ts = map paramType (lambdaParams map_lam) ++ free_ts

    let oneHot res_i adj_v = zipWith f [0 :: Int ..] $ lambdaReturnType map_lam
          where
            f j t
              | res_i == j = adj_v
              | otherwise = AdjZero (arrayShape t) (elemType t)
        -- Values for the out-of-bounds case does not matter, as we will
        -- be writing to an out-of-bounds index anyway, which is ignored.
        ooBounds adj_i = subAD . buildRenamedBody $ do
          forM_ (zip as adjs_ts) $ \(a, t) -> do
            scratch <- letSubExp "oo_scratch" =<< eBlank t
            updateAdjIndex a (OutOfBounds, adj_i) scratch
          first subExpsRes . adjsReps <$> mapM lookupAdj as
        inBounds res_i adj_i adj_v = subAD . buildRenamedBody $ do
          forM_ (zip (lambdaParams map_lam) as) $ \(p, a) -> do
            a_t <- lookupType a
            letBindNames [paramName p] $
              BasicOp $ Index a $ fullSlice a_t [DimFix adj_i]
          adj_elems <-
            fmap (map resSubExp) . bodyBind . lambdaBody
              =<< diffLambda (oneHot res_i (AdjVal adj_v)) adjs_for map_lam
          forM_ (zip as adj_elems) $ \(a, a_adj_elem) -> do
            updateAdjIndex a (AssumeBounds, adj_i) a_adj_elem
          first subExpsRes . adjsReps <$> mapM lookupAdj as

        -- Generate an iteration of the map function for every
        -- position.  This is a bit inefficient - probably we could do
        -- some deduplication.
        forPos res_i (check, adj_i, adj_v) = do
          as_adj <-
            case check of
              CheckBounds b -> do
                (obbranch, mkadjs) <- ooBounds adj_i
                (ibbranch, _) <- inBounds res_i adj_i adj_v
                fmap mkadjs . letTupExp' "map_adj_elem"
                  =<< eIf
                    (maybe (eDimInBounds (eSubExp w) (eSubExp adj_i)) eSubExp b)
                    (pure ibbranch)
                    (pure obbranch)
              AssumeBounds -> do
                (body, mkadjs) <- inBounds res_i adj_i adj_v
                mkadjs . map resSubExp <$> bodyBind body
              OutOfBounds ->
                mapM lookupAdj as

          zipWithM setAdj as as_adj

        -- Generate an iteration of the map function for every result.
        forRes res_i ivs =
          mapM_ (forPos res_i) ivs

    zipWithM_ forRes [0 ..] res_ivs
  where
    isSparse (AdjSparse (Sparse shape _ ivs)) = do
      guard $ shapeDims shape == [w]
      Just ivs
    isSparse _ =
      Nothing
diffMap pat_adj w map_lam as _ =
  returnSweepCode $ do
    pat_adj_vals <- mapM adjVal pat_adj
    pat_adj_params <-
      mapM (newParam "map_adj_p" . rowType <=< lookupType) pat_adj_vals
    map_lam' <- renameLambda map_lam

    free <- filterM isActive $ namesToList $ freeIn map_lam'

    accAdjoints free $ \free_with_adjs free_without_adjs -> do
      free_adjs <- mapM lookupAdjVal free_with_adjs
      free_adjs_ts <- mapM lookupType free_adjs
      free_adjs_params <- mapM (newParam "free_adj_p") free_adjs_ts
      let lam_rev_params =
            lambdaParams map_lam' ++ pat_adj_params ++ free_adjs_params
          adjs_for = map paramName (lambdaParams map_lam') ++ free
      lam_rev <-
        mkLambda lam_rev_params $
          subAD $
            noAdjsFor free_without_adjs $ do
              zipWithM_ insAdj free_with_adjs $ map paramName free_adjs_params
              bodyBind . lambdaBody
                =<< diffLambda (map adjFromParam pat_adj_params) adjs_for map_lam'

      (param_contribs, free_contribs) <-
        fmap (splitAt (length (lambdaParams map_lam'))) $
          letTupExp "map_adjs" . Op $
            Screma w (as ++ pat_adj_vals ++ free_adjs) (mapSOAC lam_rev)

      -- Crucial that we handle the free contribs first in case 'free'
      -- and 'as' intersect.
      zipWithM_ freeContrib free free_contribs
      zipWithM_ updateAdj as param_contribs
  where
    addIdxParams n lam = do
      idxs <- replicateM n $ newParam "idx" $ Prim int64
      pure $ lam {lambdaParams = idxs ++ lambdaParams lam}

    accAddLambda n t = addIdxParams n =<< addLambda t

    withAcc ::
      [(Shape, [VName], Maybe (Lambda, [SubExp]))] ->
      ([VName] -> ADM Result) ->
      ADM [VName]
    withAcc [] m =
      mapM (letExp "withacc_res" . BasicOp . SubExp . resSubExp) =<< m []
    withAcc inputs m = do
      (cert_params, acc_params) <- fmap unzip $
        forM inputs $ \(shape, arrs, _) -> do
          cert_param <- newParam "acc_cert_p" $ Prim Unit
          ts <- mapM (fmap (stripArray (shapeRank shape)) . lookupType) arrs
          acc_param <- newParam "acc_p" $ Acc (paramName cert_param) shape ts NoUniqueness
          pure (cert_param, acc_param)
      acc_lam <-
        subAD $ mkLambda (cert_params ++ acc_params) $ m $ map paramName acc_params
      letTupExp "withacc_res" $ WithAcc inputs acc_lam

    withAccInput (v, (shape, pt)) = do
      v_adj <- lookupAdjVal v
      add_lam <- accAddLambda (shapeRank shape) $ Prim pt
      zero <- letSubExp "zero" $ zeroExp $ Prim pt
      pure (shape, [v_adj], Just (add_lam, [zero]))

    accAdjoints free m = do
      (arr_free, acc_free, nonacc_free) <-
        partitionAdjVars <$> classifyAdjVars free
      arr_free' <- mapM withAccInput arr_free
      -- We only consider those input arrays that are also not free in
      -- the lambda.
      let as_nonfree = filter (`notElem` free) as
      (arr_adjs, acc_adjs, rest_adjs) <-
        fmap (splitAt3 (length arr_free) (length acc_free)) . withAcc arr_free' $ \accs -> do
          zipWithM_ insAdj (map fst arr_free) accs
          () <- m (acc_free ++ map fst arr_free) (namesFromList nonacc_free)
          acc_free_adj <- mapM lookupAdjVal acc_free
          arr_free_adj <- mapM (lookupAdjVal . fst) arr_free
          nonacc_free_adj <- mapM lookupAdjVal nonacc_free
          as_nonfree_adj <- mapM lookupAdjVal as_nonfree
          pure $ varsRes $ arr_free_adj <> acc_free_adj <> nonacc_free_adj <> as_nonfree_adj
      zipWithM_ insAdj acc_free acc_adjs
      zipWithM_ insAdj (map fst arr_free) arr_adjs
      let (nonacc_adjs, as_nonfree_adjs) = splitAt (length nonacc_free) rest_adjs
      zipWithM_ insAdj nonacc_free nonacc_adjs
      zipWithM_ insAdj as_nonfree as_nonfree_adjs

    freeContrib v contribs = do
      contribs_t <- lookupType contribs
      case rowType contribs_t of
        Acc {} -> void $ insAdj v contribs
        t -> do
          lam <- addLambda t
          zero <- letSubExp "zero" $ zeroExp t
          reduce <- reduceSOAC [Reduce Commutative lam [zero]]
          contrib_sum <-
            letExp (baseString v <> "_contrib_sum") $
              Op $ Screma w [contribs] reduce
          void $ updateAdj v contrib_sum

diffReduce :: [VName] -> SubExp -> [VName] -> Reduce SOACS -> ADM ()
diffReduce [adj] w [a] red
  | Just [(op, _, _, _)] <- lamIsBinOp $ redLambda red,
    isAdd op = do
    adj_rep <-
      letExp (baseString adj <> "_rep") $
        BasicOp $ Replicate (Shape [w]) $ Var adj
    void $ updateAdj a adj_rep
  where
    isAdd FAdd {} = True
    isAdd Add {} = True
    isAdd _ = False
diffReduce pat_adj w as red = do
  red' <- renameRed red
  flip_red <- renameRed =<< flipReduce red
  ls <- scanExc "ls" (redToScan red') as
  rs <-
    mapM eReverse
      =<< scanExc "ls" (redToScan flip_red)
      =<< mapM eReverse as

  (as_params, f) <- mkF $ redLambda red

  f_adj <- diffLambda (map adjFromVar pat_adj) as_params f

  as_adj <- letTupExp "adjs" $ Op $ Screma w (ls ++ as ++ rs) (mapSOAC f_adj)

  zipWithM_ updateAdj as as_adj
  where
    renameRed (Reduce comm lam nes) =
      Reduce comm <$> renameLambda lam <*> pure nes

    redToScan :: Reduce SOACS -> Scan SOACS
    redToScan (Reduce _ lam nes) = Scan lam nes
    flipReduce (Reduce comm lam nes) = do
      lam' <- renameLambda lam {lambdaParams = flipParams $ lambdaParams lam}
      pure $ Reduce comm lam' nes
    flipParams ps = uncurry (flip (++)) $ splitAt (length ps `div` 2) ps

    mkF lam = do
      lam_l <- renameLambda lam
      lam_r <- renameLambda lam
      let n = length $ lambdaReturnType lam
          (lps, aps) = splitAt n $ lambdaParams lam_l
          (ips, rps) = splitAt n $ lambdaParams lam_r
      lam' <- mkLambda (lps <> aps <> rps) $ do
        lam_l_res <- bodyBind $ lambdaBody lam_l
        forM_ (zip ips lam_l_res) $ \(ip, SubExpRes cs se) ->
          certifying cs $ letBindNames [paramName ip] $ BasicOp $ SubExp se
        bodyBind $ lambdaBody lam_r
      pure (map paramName aps, lam')

--
-- Special case of reduce with min/max:
--    let x = reduce minmax ne as
-- Forward trace (assuming w = length as):
--    let (x, x_ind) =
--      reduce (\ acc_v acc_i v i ->
--                 if (acc_v == v) then (acc_v, min acc_i i)
--                 else if (acc_v == minmax acc_v v)
--                      then (acc_v, acc_i)
--                      else (v, i))
--             (ne_min, -1)
--             (zip as (iota w))
-- Reverse trace:
--    num_elems = i64.bool (0 <= x_ind)
--    m_bar_repl = replicate num_elems m_bar
--    as_bar[x_ind:num_elems:1] += m_bar_repl
diffMinMaxRed ::
  VName -> StmAux () -> SubExp -> BinOp -> SubExp -> VName -> ADM () -> ADM ()
diffMinMaxRed x aux w minmax ne as m = do
  let t = binOpType minmax

  acc_v_p <- newParam "acc_v" $ Prim t
  acc_i_p <- newParam "acc_i" $ Prim int64
  v_p <- newParam "v" $ Prim t
  i_p <- newParam "i" $ Prim int64
  red_lam <-
    mkLambda [acc_v_p, acc_i_p, v_p, i_p] $
      fmap varsRes . letTupExp "idx_res"
        =<< eIf
          (eCmpOp (CmpEq t) (eParam acc_v_p) (eParam v_p))
          ( eBody
              [ eParam acc_v_p,
                eBinOp (SMin Int64) (eParam acc_i_p) (eParam i_p)
              ]
          )
          ( eBody
              [ eIf
                  ( eCmpOp
                      (CmpEq t)
                      (eParam acc_v_p)
                      (eBinOp minmax (eParam acc_v_p) (eParam v_p))
                  )
                  (eBody [eParam acc_v_p, eParam acc_i_p])
                  (eBody [eParam v_p, eParam i_p])
              ]
          )

  red_iota <-
    letExp "red_iota" $
      BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  form <- reduceSOAC [Reduce Commutative red_lam [ne, intConst Int64 (-1)]]
  x_ind <- newVName (baseString x <> "_ind")
  auxing aux $ letBindNames [x, x_ind] $ Op $ Screma w [as, red_iota] form

  m

  x_adj <- lookupAdjVal x
  in_bounds <-
    letSubExp "minmax_in_bounds" . BasicOp $
      CmpOp (CmpSlt Int64) (intConst Int64 0) w
  updateAdjIndex as (CheckBounds (Just in_bounds), Var x_ind) (Var x_adj)

data FirstOrSecond = WrtFirst | WrtSecond

-- computes `d(x op y)/dx` or d(x op y)/dy
mkScanAdjointLam :: Lambda -> FirstOrSecond -> ADM Lambda
mkScanAdjointLam lam0 which = do
  let len = length $ lambdaReturnType lam0
  lam <- renameLambda lam0
  let p2diff =
        case which of
          WrtFirst -> take len $ lambdaParams lam
          WrtSecond -> drop len $ lambdaParams lam
  p_adjs <- mapM unitAdjOfType (lambdaReturnType lam)
  diffLambda p_adjs (map paramName p2diff) lam

-- Should generate something like:
-- `\ j -> let i = n - 1 - j
--         if i < n-1 then ( ys_adj[i], df2dx ys[i] xs[i+1]) else (0,1) )`
-- where `ys` is  the result of scan
--       `xs` is  the input  of scan
--       `ys_adj` is the known adjoint of ys
--       `j` draw values from `iota n`
mkScanFusedMapLam :: SubExp -> Lambda -> [VName] -> [VName] -> [VName] -> ADM Lambda
mkScanFusedMapLam w scn_lam xs ys ys_adj = do
  lam <- mkScanAdjointLam scn_lam WrtFirst
  ys_ts <- mapM lookupType ys
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  mkLambda [par_i] $
    fmap varsRes . letTupExp "x"
      =<< eIf
        (toExp $ le64 i .==. 0)
        ( buildBody_ $ do
            zs <- mapM (letSubExp "ct_zero" . zeroExp . rowType) ys_ts
            os <- mapM (letSubExp "ct_one" . oneExp . rowType) ys_ts
            pure $ subExpsRes $ unpairs $ zip zs os
        )
        ( buildBody_ $ do
            j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))
            j1 <- letSubExp "j1" =<< toExp (pe64 w - le64 i)
            let index idx arr t = BasicOp $ Index arr $ fullSlice t [DimFix idx]
            y_s <- forM (zip ys_adj ys_ts) $ \(y_, t) ->
              letSubExp (baseString y_ ++ "_j") $ index j y_ t
            lam_rs <-
              eLambda lam . map pure $
                zipWith (index j) ys ys_ts ++ zipWith (index j1) xs ys_ts
            pure $ unpairs $ zip (subExpsRes y_s) lam_rs
        )

-- \(a1, b1) (a2, b2) -> (a2 + b2 * a1, b1 * b2)
mkScanLinFunO :: Type -> ADM (Scan SOACS)
mkScanLinFunO t = do
  let pt = elemType t
  zero <- letSubExp "zeros" $ zeroExp t
  one <- letSubExp "ones" $ oneExp t
  tmp <- mapM newVName ["a1", "b1", "a2", "b2"]
  let [a1, b1, a2, b2] = tmp
      pet = primExpFromSubExp pt . Var
  lam <- mkLambda (map (`Param` t) [a1, b1, a2, b2]) . fmap varsRes $
    tabNest (arrayRank t) [a1, b1, a2, b2] $ \_ [a1', b1', a2', b2'] -> do
      x <- letExp "x" <=< toExp $ pet a2' ~+~ pet b2' ~*~ pet a1'
      y <- letExp "y" <=< toExp $ pet b1' ~*~ pet b2'
      pure [x, y]
  return $ Scan lam [zero, one]

-- build the map following the scan with linear-function-composition:
-- for each (ds,cs) length-n array results of the scan, combine them as:
--    `let rs = map2 (\ d_i c_i -> d_i + c_i * y_adj[n-1]) d c |> reverse`
-- but insert explicit indexing to reverse inside the map.
mkScan2ndMaps :: SubExp -> (Type, VName, (VName, VName)) -> ADM VName
mkScan2ndMaps w (arr_tp, y_adj, (ds, cs)) = do
  nm1 <- letSubExp "nm1" =<< toExp (pe64 w - 1)
  y_adj_last <-
    letExp (baseString y_adj ++ "_last") $
      BasicOp $ Index y_adj $ fullSlice arr_tp [DimFix nm1]

  par_i <- newParam "i" $ Prim int64
  lam <- mkLambda [par_i] $ do
    let i = paramName par_i
    j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))
    dj <- letExp (baseString ds ++ "_dj") $ BasicOp $ Index ds $ fullSlice arr_tp [DimFix j]
    cj <- letExp (baseString cs ++ "_cj") $ BasicOp $ Index cs $ fullSlice arr_tp [DimFix j]

    let pet = primExpFromSubExp (elemType arr_tp) . Var
    fmap varsRes . tabNest (arrayRank (rowType arr_tp)) [y_adj_last, dj, cj] $ \_ [y_adj_last', dj', cj'] ->
      letTupExp "res" <=< toExp $ pet dj' ~+~ pet cj' ~*~ pet y_adj_last'

  iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letExp "after_scan" $ Op (Screma w [iota] (ScremaForm [] [] lam))

-- perform the final map, which is fusable with the maps obtained from `mkScan2ndMaps`
-- let xs_contribs =
--    map3 (\ i a r -> if i==0 then r else (df2dy (ys[i-1]) a) \bar{*} r)
--         (iota n) xs rs
mkScanFinalMap :: SubExp -> Lambda -> [VName] -> [VName] -> [VName] -> ADM [VName]
mkScanFinalMap w scan_lam xs ys rs = do
  let eltps = lambdaReturnType scan_lam
  lam <- mkScanAdjointLam scan_lam WrtSecond
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  par_x <- mapM (\(x, t) -> newParam (baseString x ++ "_par_x") t) $ zip xs eltps
  par_r <- mapM (\(r, t) -> newParam (baseString r ++ "_par_r") t) $ zip rs eltps

  map_lam <-
    mkLambda (par_i : par_x ++ par_r) $
      fmap varsRes . letTupExp "scan_contribs"
        =<< eIf
          (toExp $ le64 i .==. 0)
          (resultBodyM $ map (Var . paramName) par_r)
          ( buildBody_ $ do
              im1 <- letSubExp "im1" =<< toExp (le64 i - 1)
              ys_im1 <- forM ys $ \y -> do
                y_t <- lookupType y
                letSubExp (baseString y ++ "_last") $ BasicOp $ Index y $ fullSlice y_t [DimFix im1]

              lam_res <-
                mapM (letExp "const" . BasicOp . SubExp . resSubExp)
                  =<< eLambda lam (map eSubExp $ ys_im1 ++ map (Var . paramName) par_x)

              fmap (varsRes . mconcat) . forM (zip3 lam_res (map paramName par_r) eltps) $
                \(lam_r, r, eltp) -> do
                  let pet = primExpFromSubExp (elemType eltp) . Var

                  tabNest (arrayRank eltp) [lam_r, r] $ \_ [lam_r', r'] ->
                    letTupExp "res" <=< toExp $ pet lam_r' ~*~ pet r'
          )

  iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letTupExp "scan_contribs" $ Op (Screma w (iota : xs ++ rs) (ScremaForm [] [] map_lam))

diffScan :: [VName] -> SubExp -> [VName] -> Scan SOACS -> ADM ()
diffScan ys w as scan = do
  ys_adj <- mapM lookupAdjVal ys
  as_ts <- mapM lookupType as
  map1_lam <- mkScanFusedMapLam w (scanLambda scan) as ys ys_adj
  scans_lin_fun_o <- mapM mkScanLinFunO $ lambdaReturnType $ scanLambda scan
  iota <-
    letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  r_scan <-
    letTupExp "adj_ctrb_scan" $
      Op (Screma w [iota] (ScremaForm scans_lin_fun_o [] map1_lam))
  red_nms <- mapM (mkScan2ndMaps w) (zip3 as_ts ys_adj (pairs r_scan))
  as_contribs <- mkScanFinalMap w (scanLambda scan) as ys red_nms
  zipWithM_ updateAdj as as_contribs

-- We split any multi-op scan or reduction into multiple operations so
-- we can detect special cases.  Post-AD, the result may be fused
-- again.
splitScanRed ::
  ([a] -> ADM (ScremaForm SOACS), a -> [SubExp]) ->
  (Pat, StmAux (), [a], SubExp, [VName]) ->
  ADM () ->
  ADM ()
splitScanRed (opSOAC, opNeutral) (pat, aux, ops, w, as) m = do
  let ks = map (length . opNeutral) ops
      pat_per_op = map Pat $ chunks ks $ patElems pat
      as_per_op = chunks ks as
      onOps (op : ops') (op_pat : op_pats') (op_as : op_as') = do
        op_form <- opSOAC [op]
        diffSOAC op_pat aux (Screma w op_as op_form) $
          onOps ops' op_pats' op_as'
      onOps _ _ _ = m
  onOps ops pat_per_op as_per_op

diffSOAC :: Pat -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
diffSOAC pat aux soac@(Screma w as form) m
  | Just reds <- isReduceSOAC form,
    length reds > 1 =
    splitScanRed (reduceSOAC, redNeutral) (pat, aux, reds, w, as) m
  | Just [red] <- isReduceSOAC form,
    [x] <- patNames pat,
    [ne] <- redNeutral red,
    [a] <- as,
    Just [(op, _, _, _)] <- lamIsBinOp $ redLambda red,
    isMinMaxOp op =
    diffMinMaxRed x aux w op ne a m
  | Just red <- singleReduce <$> isReduceSOAC form = do
    pat_adj <- mapM adjVal =<< commonSOAC pat aux soac m
    diffReduce pat_adj w as red
  where
    isMinMaxOp (SMin _) = True
    isMinMaxOp (UMin _) = True
    isMinMaxOp (FMin _) = True
    isMinMaxOp (SMax _) = True
    isMinMaxOp (UMax _) = True
    isMinMaxOp (FMax _) = True
    isMinMaxOp _ = False
diffSOAC pat aux soac@(Screma w as form) m
  | Just scans <- isScanSOAC form,
    length scans > 1 =
    splitScanRed (scanSOAC, scanNeutral) (pat, aux, scans, w, as) m
  | Just red <- singleScan <$> isScanSOAC form = do
    void $ commonSOAC pat aux soac m
    diffScan (patNames pat) w as red
diffSOAC pat aux soac@(Screma w as form) m
  | Just lam <- isMapSOAC form = do
    copy_substs <- copyConsumedArrs $ Op soac
    pat_adj <- commonSOAC pat aux soac m
    diffMap pat_adj w lam as copy_substs
diffSOAC pat _aux (Screma w as form) m
  | Just (reds, map_lam) <-
      isRedomapSOAC form = do
    (mapstm, redstm) <-
      redomapToMapAndReduce pat (w, reds, map_lam, as)
    diffStm mapstm $ diffStm redstm m
diffSOAC _ _ soac _ =
  error $ "diffSOAC unhandled:\n" ++ pretty soac

diffLoop :: Pat -> StmAux () -> ExpT SOACS -> ADM () -> ADM ()
diffLoop
  (Pat pats)
  aux
  loop@( DoLoop
           param_tuples
           form@(ForLoop i _it bound _loop_vars)
           body@(Body () stms res)
         )
  m = do
    loop_acc_pats <- fwdLoop
    m
    revLoop loop_acc_pats
    where
      loop_params = map fst param_tuples
      loop_param_names = map paramName loop_params

      fwdLoop :: ADM [PatElem]
      fwdLoop = do
        bound64 <- asIntS Int64 bound
        (loop_acc_pats, loop_acc_params) <- fmap unzip $
          forM loop_params $ \(Param v t) -> do
            acc <- newVName $ baseString v <> "_acc"
            pat_acc <- newVName $ baseString v <> "_acc"
            setLoopTape v pat_acc
            let loop_acc_param = Param acc $ arrayOf t (Shape [bound64]) Unique
                loop_acc_pat = PatElem pat_acc $ arrayOf t (Shape [bound64]) NoUniqueness
            return (loop_acc_pat, loop_acc_param)

        zero_accs <- forM loop_params $ \(Param v t) ->
          letSubExp (baseString v <> "_zero_acc")
            =<< eBlank (arrayOf t (Shape [bound64]) NoUniqueness)

        (loop_acc_params_ret, stms') <- collectStms $
          localScope (scopeOfFParams $ loop_acc_params <> loop_params) $
            inScopeOf form $ do
              copy_substs <- copyConsumedArrsInBody body
              addStms stms
              i64 <- asIntS Int64 $ Var i
              forM (zip loop_params loop_acc_params) $ \(Param v _, Param acc t) -> do
                acc' <-
                  letInPlace "loop_acc" acc (fullSlice (fromDecl t) [DimFix i64]) $
                    substituteNames copy_substs $ BasicOp $ SubExp $ Var v
                return $ Param acc' (fromDecl t)

        let body' = mkBody stms' $ res <> varsRes (map paramName loop_acc_params_ret)
            pat' = Pat $ pats <> loop_acc_pats
            param_tuples' = param_tuples <> zip loop_acc_params zero_accs
        addStm $ Let pat' aux $ DoLoop param_tuples' form body'
        return loop_acc_pats

      revLoop :: [PatElem] -> ADM ()
      revLoop accs = do
        loop' <- renameSomething loop

        case loop' of
          DoLoop param_tuples' form'@(ForLoop i' it' bound' loop_vars') (Body () stms' res') -> do
            let loop_params' = map fst param_tuples'
                loop_param_names' = map paramName loop_params'
                subst_loop_tape v v' = lookupLoopTape v >>= setLoopTape v'
                res_vname (SubExpRes _ (Constant _)) = Nothing
                res_vname (SubExpRes _ (Var v)) = Just v
                loop_res = mapMaybe res_vname res'
                loop_var_arrays = map snd loop_vars'
                loop_var_param_names = map (paramName . fst) loop_vars'
                loop_free = namesToList (freeIn loop') \\ loop_var_arrays

            zipWithM_ subst_loop_tape loop_param_names loop_param_names'

            let build_param_tuples_adj r (PatElem pat _, Param _ t) =
                  case res_vname r of
                    Just v -> do
                      pat_adj <- lookupAdjVal pat
                      v_adj <- adjVName v
                      return $ Just (Param v_adj t, Var pat_adj)
                    _ -> return Nothing

            param_tuples_res_adj <- catMaybes <$> zipWithM build_param_tuples_adj res' (zip pats loop_params')

            param_tuples_free_adj <-
              forM loop_free $ \v -> do
                adj_v <- adjVName v
                adj_init <- lookupAdjVal v
                t <- lookupType v
                return (Param adj_v (toDecl t Unique), Var adj_init)

            param_tuples_loop_vars_adj <- forM loop_vars' $ \(_, vs) -> do
              adj_vs <- adjVName vs
              adj_init <- lookupAdjVal vs
              t <- lookupType vs
              return (Param adj_vs (toDecl t Unique), Var adj_init)

            let param_tuples_adj = param_tuples_res_adj <> param_tuples_free_adj <> param_tuples_loop_vars_adj

            bound_minus_one <-
              inScopeOf form $
                let one = Constant $ IntValue $ intValue it' (1 :: Int)
                 in letSubExp "bound_minus_one" $ BasicOp $ BinOp (Sub it' OverflowUndef) bound' one
            loop_var_arrays_substs <- fmap M.fromList $
              inScopeOf form $ do
                forM loop_var_arrays $ \xs -> do
                  xs_t <- lookupType xs
                  xs_rev <-
                    letExp "reverse" $
                      BasicOp $
                        Index xs $
                          fullSlice
                            xs_t
                            [DimSlice bound_minus_one bound' (Constant (IntValue (Int64Value (-1))))]
                  return (xs, xs_rev)

            ((i_reverse, i_reverse64), index_stms) <- collectStms $
              inScopeOf form' $ do
                i_reverse <- letExp "i" $ BasicOp $ BinOp (Sub it' OverflowWrap) bound_minus_one (Var i')
                i_reverse64 <- asIntS Int64 $ Var i_reverse
                return (i_reverse, i_reverse64)

            ((loop_param_adjs, loop_free_adjs, loop_vars_adjs), stms_adj) <- collectStms $
              subAD $
                localScope (scopeOfFParams $ map fst param_tuples_adj <> loop_params') $
                  inScopeOf form' $ do
                    zipWithM_ (\(Param p _, _) v -> insAdj v p) param_tuples_adj (loop_res <> loop_free <> loop_var_arrays)
                    diffStms stms'
                    loop_free_adjs <- mapM lookupAdjVal loop_free
                    loop_param_adjs <- mapM (lookupAdjVal . paramName) loop_params'
                    let f vs v = do
                          vs_t <- lookupType vs
                          v_adj <- lookupAdjVal v
                          updateAdjSlice (fullSlice vs_t [DimFix i_reverse64]) vs v_adj
                    zipWithM_ f loop_var_arrays loop_var_param_names
                    loop_vars_adjs <- mapM lookupAdjVal loop_var_arrays
                    return (loop_param_adjs, loop_free_adjs, loop_vars_adjs)

            let restore v = localScope (scopeOfFParams loop_params') $ do
                  vs <- lookupLoopTape v
                  vs_t <- lookupType vs
                  v' <- letExp "restore" $ BasicOp $ Index vs $ fullSlice vs_t [DimFix i_reverse64]
                  t <- lookupType v
                  let consumed = namesToList $ consumedInStms $ fst $ Alias.analyseStms mempty stms_adj
                  v'' <- case (t, v `elem` consumed) of
                    (Array {}, True) -> letExp "restore_copy" $ BasicOp $ Copy v'
                    _ -> return v'
                  return (v, v'')

            (_, stms_adj') <-
              inScopeOf form' $
                localScope (mconcat $ map scopeOfPatElem accs) $
                  collectStms $ do
                    addStms index_stms
                    substs <- mapM (restore . paramName) loop_params'
                    addStms $ substituteNames (M.insert i' i_reverse $ M.fromList substs) stms_adj

            inScopeOf stms_adj' $
              localScope (scopeOfFParams $ map fst param_tuples_adj) $
                localScope (scopeOfPat $ Pat accs) $ do
                  let body_adj =
                        Body () stms_adj' $ varsRes (loop_param_adjs <> loop_free_adjs <> loop_vars_adjs)

                  adjs' <-
                    letTupExp "loop_adj" $
                      substituteNames loop_var_arrays_substs $
                        DoLoop
                          param_tuples_adj
                          form'
                          body_adj
                  zipWithM_ updateSubExpAdj (map snd param_tuples') $ take (length loop_param_adjs) adjs'
                  zipWithM_ updateAdj loop_free $ take (length loop_free_adjs) $ drop (length loop_param_adjs) adjs'
                  zipWithM_ updateAdj loop_var_arrays $ drop (length loop_param_adjs + length loop_free_adjs) adjs'
          _ -> error "diffLoop: unexpected non-loop expression."
diffLoop _ _ _ _ = error "diffLoop: unexpected non-loop expression."

diffStm :: Stm -> ADM () -> ADM ()
diffStm (Let pat aux (BasicOp e)) m =
  diffBasicOp pat aux e m
diffStm stm@(Let pat _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    m

    pat_adj <- lookupAdjVal =<< patName pat
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
        pat_adj' = primExpFromSubExp ret (Var pat_adj)

    contribs <-
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          mapM (letExp "contrib" <=< toExp . (pat_adj' ~*~)) derivs

    zipWithM_ updateSubExpAdj (map fst args) contribs
diffStm stm@(Let pat _ (If cond tbody fbody _)) m = do
  addStm stm
  m
  returnSweepCode $ do
    let tbody_free = freeIn tbody
        fbody_free = freeIn fbody
        branches_free = namesToList $ tbody_free <> fbody_free

    adjs <- mapM lookupAdj $ patNames pat

    branches_free_adj <-
      ( pure . takeLast (length branches_free)
          <=< letTupExp "branch_adj"
          <=< renameExp
        )
        =<< eIf
          (eSubExp cond)
          (diffBody adjs branches_free tbody)
          (diffBody adjs branches_free fbody)
    zipWithM_ insAdj branches_free branches_free_adj
diffStm (Let pat aux (Op soac)) m =
  diffSOAC pat aux soac m
diffStm (Let pat aux loop@DoLoop {}) m =
  diffLoop pat aux loop m
diffStm stm _ = error $ "diffStm unhandled:\n" ++ pretty stm

diffStms :: Stms SOACS -> ADM ()
diffStms all_stms
  | Just (stm, stms) <- stmsHead all_stms =
    diffStm stm $ diffStms stms
  | otherwise =
    pure ()

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

diffBody :: [Adj] -> [VName] -> Body -> ADM Body
diffBody res_adjs get_adjs_for (Body () stms res) = subAD $
  subSubsts $ do
    stms_c <- copyConsumedArrsInStms stms
    let onResult (SubExpRes _ (Constant _)) _ = pure ()
        onResult (SubExpRes _ (Var v)) v_adj = void $ updateAdj v =<< adjVal v_adj
    (adjs, stms') <- collectStms $ do
      zipWithM_ onResult (takeLast (length res_adjs) res) res_adjs
      diffStms stms_c
      mapM lookupAdjVal get_adjs_for
    pure $ Body () stms' $ res <> varsRes adjs

diffLambda :: [Adj] -> [VName] -> Lambda -> ADM Lambda
diffLambda res_adjs get_adjs_for (Lambda params body _) =
  localScope (scopeOfLParams params) $ do
    Body () stms res <- diffBody res_adjs get_adjs_for body
    let body' = Body () stms $ takeLast (length get_adjs_for) res
    ts' <- mapM lookupType get_adjs_for
    pure $ Lambda params body' ts'

revVJP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
revVJP scope (Lambda params body ts) =
  runADM . localScope (scope <> scopeOfLParams params) $ do
    params_adj <- forM (zip (map resSubExp (bodyResult body)) ts) $ \(se, t) ->
      Param <$> maybe (newVName "const_adj") adjVName (subExpVar se) <*> pure t

    Body () stms res <-
      localScope (scopeOfLParams params_adj) $
        diffBody
          (map adjFromParam params_adj)
          (map paramName params)
          body
    let body' = Body () stms res

    pure $ Lambda (params ++ params_adj) body' (ts <> map paramType params)

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
