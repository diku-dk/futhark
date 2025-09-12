module Futhark.AD.Rev.Scan (diffScan, diffScanVec, diffScanAdd) where

import Control.Monad
import Data.List (transpose)
import Futhark.AD.Rev.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util (chunk)

data FirstOrSecond = WrtFirst | WrtSecond

identityM :: Int -> Type -> ADM [[SubExp]]
identityM n t =
  traverse
    (traverse (letSubExp "id"))
    [[if i == j then oneExp t else zeroExp t | i <- [1 .. n]] | j <- [1 .. n]]

matrixMul :: [[PrimExp VName]] -> [[PrimExp VName]] -> PrimType -> [[PrimExp VName]]
matrixMul m1 m2 t =
  let zero = primExpFromSubExp t $ Constant $ blankPrimValue t
   in [[foldl (~+~) zero $ zipWith (~*~) r q | q <- transpose m2] | r <- m1]

matrixVecMul :: [[PrimExp VName]] -> [PrimExp VName] -> PrimType -> [PrimExp VName]
matrixVecMul m v t =
  let zero = primExpFromSubExp t $ Constant $ blankPrimValue t
   in [foldl (~+~) zero $ zipWith (~*~) v r | r <- m]

vectorAdd :: [PrimExp VName] -> [PrimExp VName] -> [PrimExp VName]
vectorAdd = zipWith (~+~)

orderArgs :: Special -> [a] -> [[a]]
orderArgs s lst = chunk (div (length lst) $ specialScans s) lst

-- computes `d(x op y)/dx` or d(x op y)/dy
mkScanAdjointLam :: VjpOps -> Lambda SOACS -> FirstOrSecond -> [SubExp] -> ADM (Lambda SOACS)
mkScanAdjointLam ops lam0 which adjs = do
  let len = length $ lambdaReturnType lam0
  lam <- renameLambda lam0
  let p2diff =
        case which of
          WrtFirst -> take len $ lambdaParams lam
          WrtSecond -> drop len $ lambdaParams lam
  vjpLambda ops (fmap AdjVal adjs) (map paramName p2diff) lam

-- Should generate something like:
-- `\ j -> let i = n - 1 - j
--         if i < n-1 then ( ys_adj[i], df2dx ys[i] xs[i+1]) else (ys_adj[i],1) )`
-- where `ys` is  the result of scan
--       `xs` is  the input  of scan
--       `ys_adj` is the known adjoint of ys
--       `j` draw values from `iota n`
mkScanFusedMapLam :: -- i and j above are probably swapped in the code below
  VjpOps -> -- (ops) helper functions
  SubExp -> -- (w) ~length of arrays e.g. xs
  Lambda SOACS -> -- (scn_lam) the scan to be differentiated ('scan' turned into a lambda)
  [VName] -> -- (xs) input of the scan (actually as)
  [VName] -> -- (ys) output of the scan
  [VName] -> -- (ys_adj) adjoint of ys
  Special -> -- (s) information about which special case we're working with for the scan derivative
  Int -> -- (d) dimension of the input (number of elements in the input tuple)
  ADM (Lambda SOACS) -- output: some kind of codegen for the lambda
mkScanFusedMapLam ops w scn_lam xs ys ys_adj s d = do
  let sc = specialCase s
      k = specialSubSize s
  ys_ts <- traverse lookupType ys
  idmat <- identityM (length ys) $ rowType $ head ys_ts
  lams <- traverse (mkScanAdjointLam ops scn_lam WrtFirst) idmat
  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  mkLambda [par_i] $
    fmap varsRes . letTupExp "x"
      =<< eIf
        (toExp $ le64 i .==. 0)
        ( buildBody_ $ do
            j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))
            y_s <- forM ys_adj $ \y_ ->
              letSubExp (baseName y_ <> "_j") =<< eIndex y_ [eSubExp j]
            let zso = orderArgs s y_s
            let ido = orderArgs s $ caseJac k sc idmat
            pure $ subExpsRes $ concat $ zipWith (++) zso $ fmap concat ido
        )
        ( buildBody_ $ do
            j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))
            j1 <- letSubExp "j1" =<< toExp (pe64 w - le64 i)
            y_s <- forM ys_adj $ \y_ ->
              letSubExp (baseName y_ <> "_j") =<< eIndex y_ [eSubExp j]

            let args =
                  map (`eIndex` [eSubExp j]) ys ++ map (`eIndex` [eSubExp j1]) xs
            lam_rs <- traverse (`eLambda` args) lams

            let yso = orderArgs s $ subExpsRes y_s
            let jaco = orderArgs s $ caseJac k sc $ transpose lam_rs

            pure $ concat $ zipWith (++) yso $ fmap concat jaco
        )
  where
    caseJac :: Int -> Maybe SpecialCase -> [[a]] -> [[a]]
    caseJac _ Nothing jac = jac
    caseJac k (Just ZeroQuadrant) jac =
      concat $
        zipWith (\i -> map (take k . drop (i * k))) [0 .. d `div` k] $
          chunk k jac
    caseJac k (Just MatrixMul) jac =
      take k <$> take k jac

-- a1 a2 b -> a2 + b * a1
linFunT0 :: [PrimExp VName] -> [PrimExp VName] -> [[PrimExp VName]] -> Special -> PrimType -> [PrimExp VName]
linFunT0 a1 a2 b s pt =
  let t = case specialCase s of
        Just MatrixMul ->
          concatMap (\v -> matrixVecMul b v pt) $ chunk (specialSubSize s) a1
        _ -> matrixVecMul b a1 pt
   in a2 `vectorAdd` t

-- \(a1, b1) (a2, b2) -> (a2 + b2 * a1, b2 * b1)
mkScanLinFunO :: Type -> Special -> ADM (Scan SOACS) -- a is an instance of y_bar, b is a Jacobian (a 'c' in the 2023 paper)
mkScanLinFunO t s = do
  let pt = elemType t
  neu_elm <- mkNeutral $ specialNeutral s
  let (as, bs) = specialParams s -- input size, Jacobian element count
  (a1s, b1s, a2s, b2s) <- mkParams (as, bs) -- create sufficient free variables to bind every element of the vectors / matrices
  let pet = primExpFromSubExp pt . Var -- manifest variable names as expressions
  let (_, n) = specialNeutral s -- output size (one side of the Jacobian)
  lam <- mkLambda (map (\v -> Param mempty v (rowType t)) (a1s ++ b1s ++ a2s ++ b2s)) . fmap subExpsRes $ do
    let [a1s', b1s', a2s', b2s'] = (fmap . fmap) pet [a1s, b1s, a2s, b2s]
    let (b1sm, b2sm) = (chunk n b1s', chunk n b2s')

    let t0 = linFunT0 a1s' a2s' b2sm s pt
    let t1 = concat $ matrixMul b2sm b1sm pt
    traverse (letSubExp "r" <=< toExp) $ t0 ++ t1

  pure $ Scan lam neu_elm
  where
    mkNeutral (a, b) = do
      zeros <- replicateM a $ letSubExp "zeros" $ zeroExp $ rowType t
      idmat <- identityM b $ Prim $ elemType t
      pure $ zeros ++ concat idmat

    mkParams (a, b) = do
      a1s <- replicateM a $ newVName "a1"
      b1s <- replicateM b $ newVName "b1"
      a2s <- replicateM a $ newVName "a2"
      b2s <- replicateM b $ newVName "b2"
      pure (a1s, b1s, a2s, b2s)

-- perform the final map
-- let xs_contribs =
--    map3 (\ i a r -> if i==0 then r else (df2dy (ys[i-1]) a) \bar{*} r)
--         (iota n) xs (reverse ds)
mkScanFinalMap :: VjpOps -> SubExp -> Lambda SOACS -> [VName] -> [VName] -> [VName] -> ADM [VName]
mkScanFinalMap ops w scan_lam xs ys ds = do
  let eltps = lambdaReturnType scan_lam

  par_i <- newParam "i" $ Prim int64
  let i = paramName par_i
  par_x <- zipWithM (\x -> newParam (baseName x <> "_par_x")) xs eltps

  map_lam <-
    mkLambda (par_i : par_x) $ do
      j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))

      dj <-
        forM ds $ \dd ->
          letExp (baseName dd <> "_dj") =<< eIndex dd [eSubExp j]

      fmap varsRes . letTupExp "scan_contribs"
        =<< eIf
          (toExp $ le64 i .==. 0)
          (resultBodyM $ fmap Var dj)
          ( buildBody_ $ do
              lam <- mkScanAdjointLam ops scan_lam WrtSecond $ fmap Var dj

              im1 <- letSubExp "im1" =<< toExp (le64 i - 1)
              ys_im1 <- forM ys $ \y ->
                letSubExp (baseName y <> "_im1") =<< eIndex y [eSubExp im1]

              let args = map eSubExp $ ys_im1 ++ map (Var . paramName) par_x
              eLambda lam args
          )

  iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letTupExp "scan_contribs" $ Op $ Screma w (iota : xs) $ mapSOAC map_lam

-- | Scan special cases.
data SpecialCase = ZeroQuadrant | MatrixMul deriving (Show)

-- | Metadata for how to perform the scan for the return sweep.
data Special = Special
  { -- | Size of one of the two dimensions of the Jacobian (e.g. 3 if
    --  it's 3x3, must be square because scan must be a->a->a). It's
    --  the size of the special neutral element, not the element itself
    specialNeutral :: (Int, Int),
    -- | Size of input (nr params); Flat size of Jacobian (dim1 *
    -- dim2)). Number of params for the special lambda.
    specialParams :: (Int, Int),
    -- | The number of scans to do, 1 in most cases, k in the
    -- ZeroQuadrant (block diagonal?) case.
    specialScans :: Int,
    -- | Probably: the size of submatrices for the ZeroQuadrant (block
    -- diagonal?) case, or 1 otherwise.
    specialSubSize :: Int,
    -- | Which case.
    specialCase :: Maybe SpecialCase
  }
  deriving (Show)

-- | The different ways to handle scans. The best one is chosen
-- heuristically by looking at the operator.
data ScanAlgo
  = -- | Construct and compose the Jacobians; the approach presented
    -- in *Reverse-Mode AD of Multi-Reduce and Scan in Futhark*.
    GenericIFL23 Special
  | -- | The approach from *Parallelism-preserving automatic
    -- differentiation for second-order array languages*.
    GenericPPAD
  deriving (Show)

subMats :: Int -> [[Exp SOACS]] -> Exp SOACS -> Maybe Int
subMats d mat zero =
  let sub_d = filter (\x -> d `mod` x == 0) [1 .. (d `div` 2)]
      poss = map (\m -> all (ok m) $ zip mat [0 .. d - 1]) sub_d
      tmp = filter fst (zip poss sub_d)
   in if null tmp then Nothing else Just $ snd $ head tmp
  where
    ok m (row, i) =
      all (\(v, j) -> v == zero || i `div` m == j `div` m) $
        zip row [0 .. d - 1]

cases :: Int -> Type -> [[Exp SOACS]] -> ScanAlgo
cases d t mat = case subMats d mat $ zeroExp t of
  Just k ->
    let nonZeros = zipWith (\i -> map (take k . drop (i * k))) [0 .. d `div` k] $ chunk k mat
     in if all (== head nonZeros) $ tail nonZeros
          then GenericIFL23 $ Special (d, k) (d, k * k) 1 k $ Just MatrixMul
          else GenericIFL23 $ Special (k, k) (k, k * k) (d `div` k) k $ Just ZeroQuadrant
  Nothing ->
    case d of
      1 -> GenericIFL23 $ Special (d, d) (d, d * d) 1 d Nothing
      _ -> GenericPPAD

-- | construct and optimise a temporary lambda, that calculates the
-- Jacobian of the scan op. Figure out if the Jacobian has some
-- special shape, discarding the temporary lambda.
identifyCase :: VjpOps -> Lambda SOACS -> ADM ScanAlgo
identifyCase ops lam = do
  let t = lambdaReturnType lam
  let d = length t
  idmat <- identityM d $ head t
  lams <- traverse (mkScanAdjointLam ops lam WrtFirst) idmat
  par1 <- traverse (newParam "tmp1") t
  par2 <- traverse (newParam "tmp2") t
  jac_lam <- mkLambda (par1 ++ par2) $ do
    let args = fmap eParam $ par1 ++ par2
    lam_rs <- traverse (`eLambda` args) lams

    pure $ concat (transpose lam_rs)

  simp <- simplifyLambda jac_lam
  let jac = chunk d $ fmap (BasicOp . SubExp . resSubExp) $ bodyResult $ lambdaBody simp
  pure $ cases d (head t) jac

scanRight :: [VName] -> SubExp -> Scan SOACS -> ADM [VName]
scanRight as w scan = do
  as_types <- mapM lookupType as
  let arg_type_row = map rowType as_types

  par_a1 <- zipWithM (\x -> newParam (baseName x <> "_par_a1")) as arg_type_row
  par_a2 <- zipWithM (\x -> newParam (baseName x <> "_par_a2")) as arg_type_row
  -- Just the original operator but with par_a1 and par_a2 swapped.
  rev_op <- mkLambda (par_a1 <> par_a2) $ do
    op <- renameLambda $ scanLambda scan
    eLambda op (map (toExp . paramName) (par_a2 <> par_a1))
  -- same neutral element
  let e = scanNeutral scan
  let rev_scan = Scan rev_op e

  iota <-
    letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  -- flip the input array (this code is inspired from the code in
  -- diffScanAdd, but made to work with [VName] instead VName)
  map_scan <- revArrLam as
  -- perform the scan
  scan_res <-
    letTupExp "adj_ctrb_scan" . Op . Screma w [iota] $
      scanomapSOAC [rev_scan] map_scan
  -- flip the output array again
  rev_lam <- revArrLam scan_res
  letTupExp "reverse_scan_result" $ Op $ Screma w [iota] $ mapSOAC rev_lam
  where
    revArrLam :: [VName] -> ADM (Lambda SOACS)
    revArrLam arrs = do
      par_i <- newParam "i" $ Prim int64
      mkLambda [par_i] . forM arrs $ \arr ->
        fmap varRes . letExp "ys_bar_rev"
          =<< eIndex arr [toExp (pe64 w - le64 (paramName par_i) - 1)]

mkPPADOpLifted :: VjpOps -> [VName] -> Scan SOACS -> ADM (Lambda SOACS)
mkPPADOpLifted ops as scan = do
  as_types <- mapM lookupType as
  let arg_type_row = map rowType as_types
  par_x1 <- zipWithM (\x -> newParam (baseName x <> "_par_x1")) as arg_type_row
  par_x2_unused <- zipWithM (\x -> newParam (baseName x <> "_par_x2_unused")) as arg_type_row
  par_a1 <- zipWithM (\x -> newParam (baseName x <> "_par_a1")) as arg_type_row
  par_a2 <- zipWithM (\x -> newParam (baseName x <> "_par_a2")) as arg_type_row
  par_y1_h <- zipWithM (\x -> newParam (baseName x <> "_par_y1_h")) as arg_type_row
  par_y2_h <- zipWithM (\x -> newParam (baseName x <> "_par_y2_h")) as arg_type_row

  add_lams <- mapM addLambda arg_type_row

  mkLambda
    (par_x1 ++ par_a1 ++ par_y1_h ++ par_x2_unused ++ par_a2 ++ par_y2_h)
    (op_lift par_x1 par_a1 par_y1_h par_a2 par_y2_h add_lams)
  where
    op_lift px1 pa1 py1 pa2 py2 adds = do
      op_bar_1 <- mkScanAdjointLam ops (scanLambda scan) WrtFirst (Var . paramName <$> py2)
      let op_bar_args = toExp . Var . paramName <$> px1 ++ pa1
      z_term <- map resSubExp <$> eLambda op_bar_1 op_bar_args
      let z =
            mapM
              (\(z_t, y_1, add) -> head <$> eLambda add [toExp z_t, toExp y_1])
              (zip3 z_term (Var . paramName <$> py1) adds)

      let x1 = subExpsRes <$> mapM (toSubExp "x1" . Var . paramName) px1
      op <- renameLambda $ scanLambda scan
      let a3 = eLambda op (toExp . paramName <$> pa1 ++ pa2)

      concat <$> sequence [x1, a3, z]

asLiftPPAD :: [VName] -> SubExp -> [SubExp] -> ADM [VName]
asLiftPPAD as w e = do
  par_i <- newParam "i" $ Prim int64
  lmb <- mkLambda [par_i] $ do
    forM (zip as e) $ \(arr, arr_e) -> do
      a_lift <-
        letExp "a_lift"
          =<< eIf
            ( do
                nm1 <- toSubExp "n_minus_one" $ pe64 w - 1
                pure $ BasicOp $ CmpOp (CmpSlt Int64) (Var $ paramName par_i) nm1
            )
            ( buildBody_ $ (\x -> [subExpRes x]) <$> (letSubExp "val" =<< eIndex arr [toExp $ le64 (paramName par_i) + 1])
            )
            (buildBody_ $ pure [subExpRes arr_e])
      pure $ varRes a_lift

  iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letTupExp "as_lift" $ Op $ Screma w [iota] $ mapSOAC lmb

ysRightPPAD :: [VName] -> SubExp -> [SubExp] -> ADM [VName]
ysRightPPAD ys w e = do
  par_i <- newParam "i" $ Prim int64
  lmb <- mkLambda [par_i] $ do
    forM (zip ys e) $ \(arr, arr_e) -> do
      a_lift <-
        letExp "y_right"
          =<< eIf
            ( pure $ BasicOp $ CmpOp (CmpEq int64) (Var $ paramName par_i) (constant (0 :: Int64))
            )
            (buildBody_ $ pure [subExpRes arr_e])
            ( buildBody_ $ (\x -> [subExpRes x]) <$> (letSubExp "val" =<< eIndex arr [toExp $ le64 (paramName par_i) - 1])
            )
      pure $ varRes a_lift

  iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letTupExp "ys_right" $ Op $ Screma w [iota] $ mapSOAC lmb

finalMapPPAD :: VjpOps -> [VName] -> Scan SOACS -> ADM (Lambda SOACS)
finalMapPPAD ops as scan = do
  as_types <- mapM lookupType as
  let arg_type_row = map rowType as_types
  par_y_right <- zipWithM (\x -> newParam (baseName x <> "_par_y_right")) as arg_type_row
  par_a <- zipWithM (\x -> newParam (baseName x <> "_par_a")) as arg_type_row
  par_r_adj <- zipWithM (\x -> newParam (baseName x <> "_par_r_adj")) as arg_type_row

  mkLambda (par_y_right ++ par_a ++ par_r_adj) $ do
    op_bar_2 <- mkScanAdjointLam ops (scanLambda scan) WrtSecond (Var . paramName <$> par_r_adj)
    eLambda op_bar_2 $ toExp . Var . paramName <$> par_y_right ++ par_a

diffScan :: VjpOps -> [VName] -> SubExp -> [VName] -> Scan SOACS -> ADM ()
diffScan ops ys w as scan = do
  -- ys ~ results of scan, w ~ size of input array, as ~ (unzipped)
  -- arrays, scan ~ scan: operator with ne
  scan_case <- identifyCase ops $ scanLambda scan
  let d = length as
  ys_adj <- mapM lookupAdjVal ys -- ys_bar
  as_ts <- mapM lookupType as

  as_contribs <- case scan_case of
    GenericPPAD -> do
      let e = scanNeutral scan
      as_lift <- asLiftPPAD as w e

      let m = ys ++ as_lift ++ ys_adj

      op_lft <- mkPPADOpLifted ops as scan
      a_zero <- mapM (fmap Var . letExp "rscan_zero" . zeroExp . rowType) as_ts
      let lft_scan = Scan op_lft $ e ++ e ++ a_zero
      rs_adj <- (!! 2) . chunk d <$> scanRight m w lft_scan

      ys_right <- ysRightPPAD ys w e

      final_lmb <- finalMapPPAD ops as scan
      letTupExp "as_bar" $ Op $ Screma w (ys_right ++ as ++ rs_adj) $ mapSOAC final_lmb
    GenericIFL23 sc -> do
      -- IFL23
      map1_lam <- mkScanFusedMapLam ops w (scanLambda scan) as ys ys_adj sc d
      scans_lin_fun_o <- mkScanLinFunO (head as_ts) sc
      scan_lams <- mkScans (specialScans sc) scans_lin_fun_o
      iota <-
        letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      r_scan <-
        letTupExp "adj_ctrb_scan" . Op . Screma w [iota] $
          scanomapSOAC scan_lams map1_lam
      mkScanFinalMap ops w (scanLambda scan) as ys (splitScanRes sc r_scan d)
  -- Goal: calculate as_contribs in new way
  -- zipWithM_ updateAdj as as_contribs -- as_bar += new adjoint
  zipWithM_ updateAdj as as_contribs
  where
    mkScans :: Int -> Scan SOACS -> ADM [Scan SOACS]
    mkScans d s =
      replicateM d $ do
        lam' <- renameLambda $ scanLambda s
        pure $ Scan lam' $ scanNeutral s
    splitScanRes sc res d =
      concatMap (take (div d $ specialScans sc)) (orderArgs sc res)

diffScanVec ::
  VjpOps ->
  [VName] ->
  StmAux () ->
  SubExp ->
  Lambda SOACS ->
  [SubExp] ->
  [VName] ->
  ADM () ->
  ADM ()
diffScanVec ops ys aux w lam ne as m = do
  stmts <- collectStms_ $ do
    rank <- arrayRank <$> lookupType (head as)
    let rear = [1, 0] ++ drop 2 [0 .. rank - 1]

    transp_as <-
      forM as $ \a ->
        letExp (baseName a <> "_transp") $ BasicOp $ Rearrange a rear

    ts <- traverse lookupType transp_as
    let n = arraysSize 0 ts

    as_par <- traverse (newParam "as_par" . rowType) ts
    ne_par <- traverse (newParam "ne_par") $ lambdaReturnType lam

    scan_form <- scanSOAC [Scan lam (map (Var . paramName) ne_par)]

    map_lam <-
      mkLambda (as_par ++ ne_par) . fmap varsRes . letTupExp "map_res" . Op $
        Screma w (map paramName as_par) scan_form

    transp_ys <-
      letTupExp "trans_ys" . Op $
        Screma n (transp_as ++ subExpVars ne) (mapSOAC map_lam)

    forM (zip ys transp_ys) $ \(y, x) ->
      auxing aux $ letBindNames [y] $ BasicOp $ Rearrange x rear

  foldr (vjpStm ops) m stmts

diffScanAdd :: VjpOps -> VName -> SubExp -> Lambda SOACS -> SubExp -> VName -> ADM ()
diffScanAdd _ops ys n lam' ne as = do
  lam <- renameLambda lam'
  ys_bar <- lookupAdjVal ys

  map_scan <- rev_arr_lam ys_bar

  iota <-
    letExp "iota" $ BasicOp $ Iota n (intConst Int64 0) (intConst Int64 1) Int64

  scan_res <-
    letExp "res_rev" $ Op $ Screma n [iota] $ scanomapSOAC [Scan lam [ne]] map_scan

  rev_lam <- rev_arr_lam scan_res
  contrb <- letExp "contrb" $ Op $ Screma n [iota] $ mapSOAC rev_lam

  updateAdj as contrb
  where
    rev_arr_lam :: VName -> ADM (Lambda SOACS)
    rev_arr_lam arr = do
      par_i <- newParam "i" $ Prim int64
      mkLambda [par_i] $ do
        a <-
          letExp "ys_bar_rev"
            =<< eIndex arr [toExp (pe64 n - le64 (paramName par_i) - 1)]
        pure [varRes a]
