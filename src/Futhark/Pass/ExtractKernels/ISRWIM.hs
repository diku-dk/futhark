{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.ISRWIM
       ( iswim
       , irwim
       , rwimPossible
       )
       where

import Control.Arrow (first)
import Control.Monad.State
import Data.Semigroup ((<>))

import Futhark.MonadFreshNames
import Futhark.Representation.SOACS
import Futhark.Tools

-- | Interchange Scan With Inner Map. Tries to turn a @scan(map)@ into a
-- @map(scan)
iswim :: (MonadBinder m, Lore m ~ SOACS) =>
         Pattern
      -> SubExp
      -> Lambda
      -> [(SubExp, VName)]
      -> Maybe (m ())
iswim res_pat w scan_fun scan_input
  | Just (map_pat, map_cs, map_w, map_fun) <- rwimPossible scan_fun = Just $ do
      let (accs, arrs) = unzip scan_input
      arrs' <- transposedArrays arrs
      accs' <- mapM (letExp "acc" . BasicOp . SubExp) accs

      let map_arrs' = accs' ++ arrs'
          (scan_acc_params, scan_elem_params) =
            splitAt (length arrs) $ lambdaParams scan_fun
          map_params = map removeParamOuterDim scan_acc_params ++
                       map (setParamOuterDimTo w) scan_elem_params
          map_rettype = map (setOuterDimTo w) $ lambdaReturnType scan_fun
          map_fun' = Lambda map_params map_body map_rettype

          scan_params = lambdaParams map_fun
          scan_body = lambdaBody map_fun
          scan_rettype = lambdaReturnType map_fun
          scan_fun' = Lambda scan_params scan_body scan_rettype
          scan_input' = map (first Var) $
                        uncurry zip $ splitAt (length arrs') $ map paramName map_params

          map_body = mkBody (oneStm $ Let (setPatternOuterDimTo w map_pat) (defAux ()) $
                             Op $ Scan w scan_fun' scan_input') $
                            map Var $ patternNames map_pat

      res_pat' <- fmap (basicPattern []) $
                  mapM (newIdent' (<>"_transposed") . transposeIdentType) $
                  patternValueIdents res_pat

      addStm $ Let res_pat' (StmAux map_cs ()) $ Op $ Map map_w map_fun' map_arrs'

      forM_ (zip (patternValueIdents res_pat)
                 (patternValueIdents res_pat')) $ \(to, from) -> do
        let perm = [1,0] ++ [2..arrayRank (identType from)-1]
        addStm $ Let (basicPattern [] [to]) (defAux ()) $
                     BasicOp $ Rearrange perm $ identName from
  | otherwise = Nothing

-- | Interchange Reduce With Inner Map. Tries to turn a @reduce(map)@ into a
-- @map(reduce)
irwim :: (MonadBinder m, Lore m ~ SOACS, LocalScope SOACS m) =>
         Pattern
      -> SubExp
      -> Commutativity -> Lambda
      -> [(SubExp, VName)]
      -> Maybe (m ())
irwim res_pat w comm red_fun red_input
  | Just (map_pat, map_cs, map_w, map_fun) <- rwimPossible red_fun = Just $ do
      let (accs, arrs) = unzip red_input
      arrs' <- transposedArrays arrs
      -- FIXME?  Can we reasonably assume that the accumulator is a
      -- replicate?  We also assume that it is non-empty.
      let indexAcc (Var v) = do
            v_t <- lookupType v
            letSubExp "acc" $ BasicOp $ Index v $
              fullSlice v_t [DimFix $ intConst Int32 0]
          indexAcc Constant{} =
            fail "irwim: array accumulator is a constant."
      accs' <- mapM indexAcc accs

      let (_red_acc_params, red_elem_params) =
            splitAt (length arrs) $ lambdaParams red_fun
          map_rettype = map rowType $ lambdaReturnType red_fun
          map_params = map (setParamOuterDimTo w) red_elem_params

          red_params = lambdaParams map_fun
          red_body = lambdaBody map_fun
          red_rettype = lambdaReturnType map_fun
          red_fun' = Lambda red_params red_body red_rettype
          red_input' = zip accs' $ map paramName map_params
          red_pat = stripPatternOuterDim map_pat

      map_body <-
        case irwim red_pat w comm red_fun' red_input' of
          Nothing ->
            return $ mkBody (oneStm $ Let red_pat (defAux ()) $
                              Op $ Reduce w comm red_fun' red_input') $
            map Var $ patternNames map_pat
          Just m -> localScope (scopeOfLParams map_params) $ do
            map_body_bnds <- collectStms_ m
            return $ mkBody map_body_bnds $ map Var $ patternNames map_pat

      let map_fun' = Lambda map_params map_body map_rettype

      addStm $ Let res_pat (StmAux map_cs ()) $ Op $ Map map_w map_fun' arrs'
  | otherwise = Nothing

rwimPossible :: Lambda
             -> Maybe (Pattern, Certificates, SubExp, Lambda)
rwimPossible fun
  | Body _ stms res <- lambdaBody fun,
    [bnd] <- stmsToList stms, -- Body has a single binding
    map_pat <- stmPattern bnd,
    map Var (patternNames map_pat) == res, -- Returned verbatim
    Op (Map map_w map_fun map_arrs) <- stmExp bnd,
    map paramName (lambdaParams fun) == map_arrs =
      Just (map_pat, stmCerts bnd, map_w, map_fun)
  | otherwise =
      Nothing

transposedArrays :: MonadBinder m => [VName] -> m [VName]
transposedArrays arrs = forM arrs $ \arr -> do
  t <- lookupType arr
  let perm = [1,0] ++ [2..arrayRank t-1]
  letExp (baseString arr) $ BasicOp $ Rearrange perm arr

removeParamOuterDim :: LParam -> LParam
removeParamOuterDim param =
  let t = rowType $ paramType param
  in param { paramAttr = t }

setParamOuterDimTo :: SubExp -> LParam -> LParam
setParamOuterDimTo w param =
  let t = setOuterDimTo w $ paramType param
  in param { paramAttr = t }

setIdentOuterDimTo :: SubExp -> Ident -> Ident
setIdentOuterDimTo w ident =
  let t = setOuterDimTo w $ identType ident
  in ident { identType = t }

setOuterDimTo :: SubExp -> Type -> Type
setOuterDimTo w t =
  arrayOfRow (rowType t) w

setPatternOuterDimTo :: SubExp -> Pattern -> Pattern
setPatternOuterDimTo w pat =
  basicPattern [] $ map (setIdentOuterDimTo w) $ patternValueIdents pat

transposeIdentType :: Ident -> Ident
transposeIdentType ident =
  ident { identType = transposeType $ identType ident }

stripIdentOuterDim :: Ident -> Ident
stripIdentOuterDim ident =
  ident { identType = rowType $ identType ident }

stripPatternOuterDim :: Pattern -> Pattern
stripPatternOuterDim pat =
  basicPattern [] $ map stripIdentOuterDim $ patternValueIdents pat
