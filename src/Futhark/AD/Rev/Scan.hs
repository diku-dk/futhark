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
orderArgs s lst =
  let d = div (length lst) $ specialScans s
   in chunk d lst

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
mkScanFusedMapLam ::
  VjpOps ->
  SubExp ->
  Lambda SOACS ->
  [VName] ->
  [VName] ->
  [VName] ->
  Special ->
  Int ->
  ADM (Lambda SOACS)
mkScanFusedMapLam ops w scn_lam xs ys ys_adj s d = do
  let sc = specialCase s
  let k = specialSubSize s
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
              letSubExp (baseString y_ ++ "_j") =<< eIndex y_ [eSubExp j]
            let zso = orderArgs s y_s
            let ido = orderArgs s $ case_jac k sc idmat
            pure $ subExpsRes $ concat $ zipWith (++) zso $ fmap concat ido
        )
        ( buildBody_ $ do
            j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))
            j1 <- letSubExp "j1" =<< toExp (pe64 w - le64 i)
            y_s <- forM ys_adj $ \y_ ->
              letSubExp (baseString y_ ++ "_j") =<< eIndex y_ [eSubExp j]

            let args =
                  map (`eIndex` [eSubExp j]) ys ++ map (`eIndex` [eSubExp j1]) xs
            lam_rs <- traverse (`eLambda` args) lams

            let yso = orderArgs s $ subExpsRes y_s
            let jaco = orderArgs s $ case_jac k sc $ transpose lam_rs

            pure $ concat $ zipWith (++) yso $ fmap concat jaco
        )
  where
    case_jac :: Int -> SpecialCase -> [[a]] -> [[a]]
    case_jac _ Generic jac = jac
    case_jac k ZeroQuadrant jac =
      concat $
        zipWith
          (\i -> map (take k . drop (i * k)))
          [0 .. d `div` k]
          $ chunk k jac
    case_jac k MatrixMul jac =
      take k <$> take k jac

-- a1 a2 b -> a2 + b * a1
linFunT0 :: [PrimExp VName] -> [PrimExp VName] -> [[PrimExp VName]] -> Special -> PrimType -> [PrimExp VName]
linFunT0 a1 a2 b s pt =
  let t = case specialCase s of
        MatrixMul ->
          concatMap (\v -> matrixVecMul b v pt) $ chunk (specialSubSize s) a1
        _ -> matrixVecMul b a1 pt
   in a2 `vectorAdd` t

-- \(a1, b1) (a2, b2) -> (a2 + b2 * a1, b2 * b1)
mkScanLinFunO :: Type -> Special -> ADM (Scan SOACS)
mkScanLinFunO t s = do
  let pt = elemType t
  neu_elm <- mkNeutral $ specialNeutral s
  let (as, bs) = specialParams s
  (a1s, b1s, a2s, b2s) <- mkParams (as, bs)
  let pet = primExpFromSubExp pt . Var
  let (_, n) = specialNeutral s

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
  par_x <- zipWithM (\x -> newParam (baseString x ++ "_par_x")) xs eltps

  map_lam <-
    mkLambda (par_i : par_x) $ do
      j <- letSubExp "j" =<< toExp (pe64 w - (le64 i + 1))

      dj <-
        traverse
          (\dd -> letExp (baseString dd ++ "_dj") =<< eIndex dd [eSubExp j])
          ds

      fmap varsRes . letTupExp "scan_contribs"
        =<< eIf
          (toExp $ le64 i .==. 0)
          (resultBodyM $ fmap Var dj)
          ( buildBody_ $ do
              lam <- mkScanAdjointLam ops scan_lam WrtSecond $ fmap Var dj

              im1 <- letSubExp "im1" =<< toExp (le64 i - 1)
              ys_im1 <- forM ys $ \y ->
                letSubExp (baseString y <> "_im1") =<< eIndex y [eSubExp im1]

              let args = map eSubExp $ ys_im1 ++ map (Var . paramName) par_x
              eLambda lam args
          )

  iota <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  letTupExp "scan_contribs" $ Op $ Screma w (iota : xs) $ mapSOAC map_lam

data SpecialCase
  = Generic
  | ZeroQuadrant
  | MatrixMul
  deriving (Show)

data Special = Special
  { specialNeutral :: (Int, Int),
    specialParams :: (Int, Int),
    specialScans :: Int,
    specialSubSize :: Int,
    specialCase :: SpecialCase
  }
  deriving (Show)

subMats :: Int -> [[Exp SOACS]] -> Exp SOACS -> Maybe Int
subMats d mat zero =
  let sub_d = filter (\x -> d `mod` x == 0) [1 .. (d `div` 2)]
      poss = map (\m -> all (ok m) $ zip mat [0 .. d - 1]) sub_d
      tmp = filter fst (zip poss sub_d)
   in if null tmp then Nothing else Just $ snd $ head tmp
  where
    ok m (row, i) =
      all (\(v, j) -> v == zero || (i `div` m == j `div` m)) $
        zip row [0 .. d - 1]

cases :: Int -> Type -> [[Exp SOACS]] -> Special
cases d t mat
  | Just k <- subMats d mat $ zeroExp t =
    let nonZeros = zipWith (\i -> map (take k . drop (i * k))) [0 .. d `div` k] $ chunk k mat
     in if all (== head nonZeros) $ tail nonZeros
          then Special (d, k) (d, k * k) 1 k MatrixMul
          else Special (k, k) (k, k * k) (d `div` k) k ZeroQuadrant
cases d _ _ = Special (d, d) (d, d * d) 1 d Generic

identifyCase :: VjpOps -> Lambda SOACS -> ADM Special
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

diffScan :: VjpOps -> [VName] -> SubExp -> [VName] -> Scan SOACS -> ADM ()
diffScan ops ys w as scan = do
  sc <- identifyCase ops (scanLambda scan)
  let d = length as
  ys_adj <- mapM lookupAdjVal ys
  as_ts <- mapM lookupType as
  map1_lam <- mkScanFusedMapLam ops w (scanLambda scan) as ys ys_adj sc d
  scans_lin_fun_o <- mkScanLinFunO (head as_ts) sc
  scan_lams <- mkScans (specialScans sc) scans_lin_fun_o
  iota <-
    letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  r_scan <-
    letTupExp "adj_ctrb_scan" . Op . Screma w [iota] $
      scanomapSOAC scan_lams map1_lam

  as_contribs <- mkScanFinalMap ops w (scanLambda scan) as ys (splitScanRes sc r_scan d)
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
      traverse
        (\a -> letExp (baseString a ++ "_transp") $ BasicOp $ Rearrange rear a)
        as

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

    zipWithM
      (\y x -> auxing aux $ letBindNames [y] $ BasicOp $ Rearrange rear x)
      ys
      transp_ys

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
