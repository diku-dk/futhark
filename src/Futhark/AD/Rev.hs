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
import Data.Bifunctor (second)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import Futhark.AD.Derivatives
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, splitAt3, takeLast)

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

-- | Whether 'Sparse' should check bounds or assume they are correct.
-- The latter results in simpler code.
data InBounds = CheckBounds | AssumeBounds
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
      let safety = case check of
            AssumeBounds -> Unsafe
            CheckBounds -> Safe
      letExp "sparse" . BasicOp $
        Update safety arr (fullSlice arr_t [DimFix i]) se

adjFromVar :: VName -> Adj
adjFromVar = AdjVal . Var

data RState = RState
  { stateAdjs :: M.Map VName Adj,
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
        (RState mempty vn)

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
        res <- tabNest' (paramName iparam : is) (n -1) (map paramName params) f
        ret <- mapM lookupType res
        pure (ret, map Var res)
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
    return $ resultBody [res]
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
updateAdjSlice [DimFix i] v d =
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
        UpdateAcc v_adj (map isDimFix slice) [Var d]
    _ -> do
      v_adjslice <-
        if primType t
          then return v_adj
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
      insAdj v
        =<< case v_adj_t of
          Acc {} ->
            letExp (baseString v_adj) . BasicOp $ UpdateAcc v_adj [i] [se]
          _ -> do
            let safety = case check of
                  CheckBounds -> Safe
                  AssumeBounds -> Unsafe
            letExp (baseString v_adj) $
              BasicOp $ Update safety v_adj (fullSlice v_adj_t [DimFix i]) se

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

patternName :: Pattern -> ADM VName
patternName (Pattern [] [pe]) = pure $ patElemName pe
patternName pat = error $ "Expected single-element pattern: " ++ pretty pat

-- The vast majority of BasicOps require no special treatment in the
-- forward pass and produce one value (and hence once adjoint).  We
-- deal with that case here.
commonBasicOp :: Pattern -> StmAux () -> BasicOp -> ADM () -> ADM (VName, VName)
commonBasicOp pat aux op m = do
  addStm $ Let pat aux $ BasicOp op
  m
  pat_v <- patternName pat
  pat_adj <- lookupAdjVal pat_v
  pure (pat_v, pat_adj)

diffBasicOp :: Pattern -> StmAux () -> BasicOp -> ADM () -> ADM ()
diffBasicOp pat aux e m =
  case e of
    CmpOp cmp x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
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
      contrib <-
        letExp "contrib" $ BasicOp $ ConvOp (flipConvOp op) $ Var pat_adj
      updateSubExpAdj x contrib
    --
    UnOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

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
      updateSubExpAdj se pat_adj
    --
    Assert {} ->
      void $ commonBasicOp pat aux e m
    --
    ArrayLit elems t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      forM_ (zip [(0 :: Int64) ..] elems) $ \(i, se) -> do
        let slice = fullSlice t [DimFix (constant i)]
        updateSubExpAdj se <=< letExp "elem_adj" $ BasicOp $ Index pat_adj slice
    --
    Index arr slice -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdjSlice slice arr pat_adj
    --
    Opaque se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      updateSubExpAdj se pat_adj
    --
    Reshape _ arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      arr_dims <- arrayDims <$> lookupType arr
      void $
        updateAdj arr <=< letExp "adj_reshape" $
          BasicOp $ Reshape (map DimNew arr_dims) pat_adj
    --
    Rearrange perm arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $
        updateAdj arr <=< letExp "adj_rearrange" $
          BasicOp $ Rearrange (rearrangeInverse perm) pat_adj
    --
    Rotate rots arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      let neg = BasicOp . BinOp (Sub Int64 OverflowWrap) (intConst Int64 0)
      rots' <- mapM (letSubExp "rot_neg" . neg) rots
      void $
        updateAdj arr <=< letExp "adj_rotate" $
          BasicOp $ Rotate rots' pat_adj
    --
    Replicate (Shape ns) x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
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
      void $ updateAdj se pat_adj
    --
    Manifest _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      void $ updateAdj se pat_adj
    --
    Scratch {} ->
      void $ commonBasicOp pat aux e m
    --
    Iota n _ _ t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      ne <- letSubExp "zero" $ zeroExp $ Prim $ IntType t
      lam <- addLambda $ Prim $ IntType t
      reduce <- reduceSOAC [Reduce Commutative lam [ne]]
      updateSubExpAdj n
        =<< letExp "iota_contrib" (Op $ Screma n [pat_adj] reduce)
    --
    Update safety arr slice v -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      v_adj <- letExp "update_val_adj" $ BasicOp $ Index pat_adj slice
      updateSubExpAdj v v_adj
      zeroes <- letSubExp "update_zero" . zeroExp =<< subExpType v
      void $
        updateAdj arr
          =<< letExp "update_src_adj" (BasicOp $ Update safety pat_adj slice zeroes)
    --
    UpdateAcc {} -> error "Reverse-mode UpdateAcc not handled yet."

commonSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM () -> ADM [Adj]
commonSOAC pat aux soac m = do
  addStm $ Let pat aux $ Op soac
  m
  mapM lookupAdj $ patternNames pat

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

diffMap :: [Adj] -> SubExp -> Lambda -> [VName] -> Substitutions -> ADM ()
diffMap res_adjs w map_lam as substs
  | Just res_ivs <- mapM isSparse res_adjs,
    freeIn map_lam == mempty, -- FIXME? Too conservative.
    M.null substs = do
    -- Since at most only a constant number of adjoint are nonzero
    -- (length adj_ivs), there is no need for the return sweep code to
    -- contain a Map at all.

    free <- filterM isActive $ namesToList $ freeIn map_lam
    free_ts <- mapM lookupType free
    let adjs_for = map paramName (lambdaParams map_lam) ++ free

    let oneHot res_i adj_v = zipWith f [0 :: Int ..] $ lambdaReturnType map_lam
          where
            f j t
              | res_i == j = adj_v
              | otherwise = AdjZero (arrayShape t) (elemType t)
        -- Values for the out-of-bounds case does not matter, as we will
        -- be writing to an out-of-bounds index anyway (which is ignored.
        ooBounds =
          eBody
            ( map (pure . zeroExp) $
                map paramType (lambdaParams map_lam) ++ free_ts
            )
        inBounds res_i adj_i adj_v = renameBody <=< buildBody_ $ do
          forM_ (zip (lambdaParams map_lam) as) $ \(p, a) -> do
            a_t <- lookupType a
            letBindNames [paramName p] $
              BasicOp $ Index a $ fullSlice a_t [DimFix adj_i]
          bodyBind . lambdaBody
            =<< diffLambda (oneHot res_i (AdjVal adj_v)) adjs_for map_lam

        -- Generate an iteration of the map function for every
        -- position.  This is a bit inefficient - probably we could do
        -- some deduplication.
        forPos res_i (check, adj_i, adj_v) = do
          as_adj_elems <-
            case check of
              CheckBounds ->
                fmap (map Var) . letTupExp "map_adj_elem"
                  =<< eIf
                    (eOutOfBounds (head as) [eSubExp adj_i])
                    ooBounds
                    (inBounds res_i adj_i adj_v)
              AssumeBounds ->
                bodyBind =<< inBounds res_i adj_i adj_v

          forM_ (zip as as_adj_elems) $ \(a, a_adj_elem) ->
            updateAdjIndex a (check, adj_i) a_adj_elem

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
--
diffMap pat_adj w map_lam as substs = do
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
              =<< diffLambda (map (adjFromVar . paramName) pat_adj_params) adjs_for map_lam'

    (param_contribs, free_contribs) <-
      fmap (splitAt (length (lambdaParams map_lam'))) $
        letTupExp "map_adjs" . Op $
          Screma w (substituteNames substs as ++ pat_adj_vals ++ free_adjs) (mapSOAC lam_rev)

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
      mapM (letExp "withacc_res" . BasicOp . SubExp) =<< m []
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
          pure $ map Var $ arr_free_adj <> acc_free_adj <> nonacc_free_adj <> as_nonfree_adj
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
        forM_ (zip ips lam_l_res) $ \(ip, se) ->
          letBindNames [paramName ip] $ BasicOp $ SubExp se
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
      fmap (map Var) . letTupExp "idx_res"
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
  updateAdjIndex as (CheckBounds, Var x_ind) (Var x_adj)

diffSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM () -> ADM ()
diffSOAC pat aux soac@(Screma w as form) m
  | Just reds <- isReduceSOAC form,
    length reds > 1 = do
    -- We split any horizontally reduction into multiple reductions
    -- so we can detect special cases.  Post-AD, the result may be
    -- fused again.
    let ks = map (length . redNeutral) reds
        pat_per_red = map (Pattern []) $ chunks ks $ patternElements pat
        as_per_red = chunks ks as
        onReds (red : reds') (red_pat : red_pats') (red_as : red_as') = do
          red_form <- reduceSOAC [red]
          diffSOAC red_pat aux (Screma w red_as red_form) $
            onReds reds' red_pats' red_as'
        onReds _ _ _ = m
    onReds reds pat_per_red as_per_red
  | Just [red] <- isReduceSOAC form,
    [x] <- patternNames pat,
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

diffStm :: Stm -> ADM () -> ADM ()
diffStm (Let pat aux (BasicOp e)) m =
  diffBasicOp pat aux e m
diffStm stm@(Let pat _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    m

    pat_adj <- lookupAdjVal =<< patternName pat
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
        pat_adj' = primExpFromSubExp ret (Var pat_adj)

    contribs <-
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          mapM (letExp "contrib" <=< toExp . (pat_adj' ~*~)) derivs

    zipWithM_ updateSubExpAdj (map fst args) contribs
diffStm stm@(Let pat _ e@(If cond tbody fbody _)) m = do
  copy_substs <- copyConsumedArrs e
  addStm stm
  m

  let tbody_free = freeIn tbody
      fbody_free = freeIn fbody
      branches_free = namesToList $ tbody_free <> fbody_free

  adjs <- mapM lookupAdj $ patternValueNames pat

  -- We need to discard any context, as this never contributes to
  -- adjoints.
  branches_free_adj <-
    ( pure . takeLast (length branches_free)
        <=< letTupExp "branch_adj"
        <=< renameExp
      )
      =<< eIf
        (eSubExp cond)
        (substituteNames copy_substs <$> diffBody adjs branches_free tbody)
        (substituteNames copy_substs <$> diffBody adjs branches_free fbody)

  zipWithM_ insAdj branches_free branches_free_adj
diffStm (Let pat aux (Op soac)) m =
  diffSOAC pat aux soac m
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

diffBody :: [Adj] -> [VName] -> Body -> ADM Body
diffBody res_adjs get_adjs_for (Body () stms res) = subAD $ do
  let onResult (Constant _) _ = pure ()
      onResult (Var v) v_adj = void $ updateAdj v =<< adjVal v_adj
  (adjs, stms') <- collectStms $ do
    zipWithM_ onResult (takeLast (length res_adjs) res) res_adjs
    diffStms stms
    mapM lookupAdjVal get_adjs_for
  pure $ Body () stms' $ res <> map Var adjs

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
    params_adj <- forM (zip (bodyResult body) ts) $ \(se, t) ->
      Param <$> maybe (newVName "const_adj") adjVName (subExpVar se) <*> pure t

    Body () stms res <-
      localScope (scopeOfLParams params_adj) $
        diffBody
          (map (adjFromVar . paramName) params_adj)
          (map paramName params)
          body
    let body' = Body () stms $ takeLast (length params) res

    pure $ Lambda (params ++ params_adj) body' (map paramType params)

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
