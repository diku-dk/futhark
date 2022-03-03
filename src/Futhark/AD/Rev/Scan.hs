module Futhark.AD.Rev.Scan (diffScan) where

import Control.Monad
import Futhark.AD.Rev.Monad
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util (pairs, unpairs)

data FirstOrSecond = WrtFirst | WrtSecond

-- computes `d(x op y)/dx` or d(x op y)/dy
mkScanAdjointLam :: VjpOps -> Lambda SOACS -> FirstOrSecond -> ADM (Lambda SOACS)
mkScanAdjointLam ops lam0 which = do
  let len = length $ lambdaReturnType lam0
  lam <- renameLambda lam0
  let p2diff =
        case which of
          WrtFirst -> take len $ lambdaParams lam
          WrtSecond -> drop len $ lambdaParams lam
  p_adjs <- mapM unitAdjOfType (lambdaReturnType lam)
  vjpLambda ops p_adjs (map paramName p2diff) lam

-- Should generate something like:
-- `\ j -> let i = n - 1 - j
--         if i < n-1 then ( ys_adj[i], df2dx ys[i] xs[i+1]) else (0,1) )`
-- where `ys` is  the result of scan
--       `xs` is  the input  of scan
--       `ys_adj` is the known adjoint of ys
--       `j` draw values from `iota n`
mkScanFusedMapLam :: VjpOps -> SubExp -> Lambda SOACS -> [VName] -> [VName] -> [VName] -> ADM (Lambda SOACS)
mkScanFusedMapLam ops w scn_lam xs ys ys_adj = do
  lam <- mkScanAdjointLam ops scn_lam WrtFirst
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
  lam <- mkLambda (map (\v -> Param mempty v t) [a1, b1, a2, b2]) . fmap varsRes $
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
mkScanFinalMap :: VjpOps -> SubExp -> Lambda SOACS -> [VName] -> [VName] -> [VName] -> ADM [VName]
mkScanFinalMap ops w scan_lam xs ys rs = do
  let eltps = lambdaReturnType scan_lam
  lam <- mkScanAdjointLam ops scan_lam WrtSecond
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

diffScan :: VjpOps -> [VName] -> SubExp -> [VName] -> Scan SOACS -> ADM ()
diffScan ops ys w as scan = do
  ys_adj <- mapM lookupAdjVal ys
  as_ts <- mapM lookupType as
  map1_lam <- mkScanFusedMapLam ops w (scanLambda scan) as ys ys_adj
  scans_lin_fun_o <- mapM mkScanLinFunO $ lambdaReturnType $ scanLambda scan
  iota <-
    letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
  r_scan <-
    letTupExp "adj_ctrb_scan" $
      Op (Screma w [iota] (ScremaForm scans_lin_fun_o [] map1_lam))
  red_nms <- mapM (mkScan2ndMaps w) (zip3 as_ts ys_adj (pairs r_scan))
  as_contribs <- mkScanFinalMap ops w (scanLambda scan) as ys red_nms
  zipWithM_ updateAdj as as_contribs
