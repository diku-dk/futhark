{-# LANGUAGE TypeFamilies #-}

-- | Interchanging scans with inner maps.
module Futhark.Pass.ExtractKernels.ISRWIM
  ( iswim,
    irwim,
    rwimPossible,
  )
where

import Control.Arrow (first)
import Control.Monad
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

-- | Interchange Scan With Inner Map. Tries to turn a @scan(map)@ into a
-- @map(scan)
iswim ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  Pat Type ->
  SubExp ->
  Lambda SOACS ->
  [(SubExp, VName)] ->
  Maybe (m ())
iswim res_pat w scan_fun scan_input
  | Just (map_pat, map_aux, map_w, map_fun) <- rwimPossible scan_fun = Just $ do
      let (accs, arrs) = unzip scan_input
      arrs' <- transposedArrays arrs
      accs' <- mapM (letExp "acc" . BasicOp . SubExp) accs

      let map_arrs' = accs' ++ arrs'
          (scan_acc_params, scan_elem_params) =
            splitAt (length arrs) $ lambdaParams scan_fun
          map_params =
            map removeParamOuterDim scan_acc_params
              ++ map (setParamOuterDimTo w) scan_elem_params
          map_rettype = map (setOuterDimTo w) $ lambdaReturnType scan_fun

          scan_params = lambdaParams map_fun
          scan_body = lambdaBody map_fun
          scan_rettype = lambdaReturnType map_fun
          scan_fun' = Lambda scan_params scan_rettype scan_body
          scan_input' =
            map (first Var) $
              uncurry zip $
                splitAt (length arrs') $
                  map paramName map_params
          (nes', scan_arrs) = unzip scan_input'

      scan_soac <- scanSOAC [Scan scan_fun' nes']
      let map_body =
            mkBody
              ( oneStm $
                  Let (setPatOuterDimTo w map_pat) (defAux ()) $
                    Op $
                      Screma w scan_arrs scan_soac
              )
              $ varsRes
              $ patNames map_pat
          map_fun' = Lambda map_params map_rettype map_body

      res_pat' <-
        fmap basicPat $
          mapM (newIdent' (<> "_transposed") . transposeIdentType) $
            patIdents res_pat

      addStm . Let res_pat' map_aux . Op . Screma map_w map_arrs'
        =<< mapSOAC map_fun'

      forM_ (zip (patIdents res_pat) (patIdents res_pat')) $ \(to, from) -> do
        let perm = [1, 0] ++ [2 .. arrayRank (identType from) - 1]
        addStm $
          Let (basicPat [to]) (defAux ()) . BasicOp $
            Rearrange (identName from) perm
  | otherwise = Nothing

-- | Interchange Reduce With Inner Map. Tries to turn a @reduce(map)@ into a
-- @map(reduce)
irwim ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  Pat Type ->
  SubExp ->
  Commutativity ->
  Lambda SOACS ->
  [(SubExp, VName)] ->
  Maybe (m ())
irwim res_pat w comm red_fun red_input
  | Just (map_pat, map_aux, map_w, map_fun) <- rwimPossible red_fun = Just $ do
      let (accs, arrs) = unzip red_input
      arrs' <- transposedArrays arrs
      -- FIXME?  Can we reasonably assume that the accumulator is a
      -- replicate?  We also assume that it is non-empty.
      let indexAcc (Var v) = do
            v_t <- lookupType v
            letSubExp "acc" $
              BasicOp $
                Index v $
                  fullSlice v_t [DimFix $ intConst Int64 0]
          indexAcc Constant {} =
            error "irwim: array accumulator is a constant."
      accs' <- mapM indexAcc accs

      let (_red_acc_params, red_elem_params) =
            splitAt (length arrs) $ lambdaParams red_fun
          map_rettype = map rowType $ lambdaReturnType red_fun
          map_params = map (setParamOuterDimTo w) red_elem_params

          red_params = lambdaParams map_fun
          red_body = lambdaBody map_fun
          red_rettype = lambdaReturnType map_fun
          red_fun' = Lambda red_params red_rettype red_body
          red_input' = zip accs' $ map paramName map_params
          red_pat = stripPatOuterDim map_pat

      map_body <-
        case irwim red_pat w comm red_fun' red_input' of
          Nothing -> do
            reduce_soac <- reduceSOAC [Reduce comm red_fun' $ map fst red_input']
            pure
              $ mkBody
                ( oneStm $
                    Let red_pat (defAux ()) $
                      Op $
                        Screma w (map snd red_input') reduce_soac
                )
              $ varsRes
              $ patNames map_pat
          Just m -> localScope (scopeOfLParams map_params) $ do
            map_body_stms <- collectStms_ m
            pure $ mkBody map_body_stms $ varsRes $ patNames map_pat

      let map_fun' = Lambda map_params map_rettype map_body

      addStm . Let res_pat map_aux . Op . Screma map_w arrs'
        =<< mapSOAC map_fun'
  | otherwise = Nothing

-- | Does this reduce operator contain an inner map, and if so, what
-- does that map look like?
rwimPossible ::
  Lambda SOACS ->
  Maybe (Pat Type, StmAux (), SubExp, Lambda SOACS)
rwimPossible fun
  | Body _ stms res <- lambdaBody fun,
    [stm] <- stmsToList stms, -- Body has a single binding
    map_pat <- stmPat stm,
    map Var (patNames map_pat) == map resSubExp res, -- Returned verbatim
    Op (Screma map_w map_arrs form) <- stmExp stm,
    Just map_fun <- isMapSOAC form,
    map paramName (lambdaParams fun) == map_arrs =
      Just (map_pat, stmAux stm, map_w, map_fun)
  | otherwise =
      Nothing

transposedArrays :: (MonadBuilder m) => [VName] -> m [VName]
transposedArrays arrs = forM arrs $ \arr -> do
  t <- lookupType arr
  let perm = [1, 0] ++ [2 .. arrayRank t - 1]
  letExp (baseName arr) $ BasicOp $ Rearrange arr perm

removeParamOuterDim :: LParam SOACS -> LParam SOACS
removeParamOuterDim param =
  let t = rowType $ paramType param
   in param {paramDec = t}

setParamOuterDimTo :: SubExp -> LParam SOACS -> LParam SOACS
setParamOuterDimTo w param =
  let t = setOuterDimTo w $ paramType param
   in param {paramDec = t}

setIdentOuterDimTo :: SubExp -> Ident -> Ident
setIdentOuterDimTo w ident =
  let t = setOuterDimTo w $ identType ident
   in ident {identType = t}

setOuterDimTo :: SubExp -> Type -> Type
setOuterDimTo w t =
  arrayOfRow (rowType t) w

setPatOuterDimTo :: SubExp -> Pat Type -> Pat Type
setPatOuterDimTo w pat =
  basicPat $ map (setIdentOuterDimTo w) $ patIdents pat

transposeIdentType :: Ident -> Ident
transposeIdentType ident =
  ident {identType = transposeType $ identType ident}

stripIdentOuterDim :: Ident -> Ident
stripIdentOuterDim ident =
  ident {identType = rowType $ identType ident}

stripPatOuterDim :: Pat Type -> Pat Type
stripPatOuterDim pat =
  basicPat $ map stripIdentOuterDim $ patIdents pat
