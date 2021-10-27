{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.Loop (diffLoop, stripmineStms) where

import Control.Monad
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe
import Futhark.AD.Rev.Monad
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases (consumedInStms)
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute

bindForLoop ::
  ExpT rep ->
  ( [(Param (FParamInfo rep), SubExp)] ->
    LoopForm rep ->
    VName ->
    IntType ->
    SubExp ->
    [(Param (LParamInfo rep), VName)] ->
    BodyT rep ->
    a
  ) ->
  a
bindForLoop (DoLoop param_tuples form@(ForLoop i it bound loop_vars) body) f =
  f param_tuples form i it bound loop_vars body
bindForLoop _ _ = error "bindForLoop: not a for loop."

-- fix type
renameForLoop ::
  ExpT SOACS ->
  ( [(Param (FParamInfo SOACS), SubExp)] ->
    LoopForm SOACS ->
    VName ->
    IntType ->
    SubExp ->
    [(Param (LParamInfo SOACS), VName)] ->
    BodyT SOACS ->
    ADM a
  ) ->
  ADM a
renameForLoop loop f = renameSomething loop >>= (`bindForLoop` f)

removeLoopVars :: MonadBuilder m => ExpT (Rep m) -> m (ExpT (Rep m))
removeLoopVars loop =
  bindForLoop loop $ \param_tuples form i _it _bound loop_vars body -> do
    let indexify (x_param, xs) = do
          xs_t <- lookupType xs
          x' <- letExp "indexed" $ BasicOp $ Index xs $ fullSlice xs_t [DimFix (Var i)]
          return (paramName x_param, x')
    (substs_list, subst_stms) <- collectStms $ mapM indexify loop_vars
    let Body aux' stms' res' = substituteNames (M.fromList substs_list) body
    return $ DoLoop param_tuples form $ Body aux' (subst_stms <> stms') res'

convertWhileLoop :: Integer -> ExpT SOACS -> ADM (ExpT SOACS)
convertWhileLoop bound (DoLoop param_tuples (WhileLoop cond) body) =
  localScope (scopeOfFParams (map fst param_tuples)) $ do
    let it = Int64
    (i, _) <- collectStms $ letExp "i" =<< eBlank (Prim $ IntType it)
    let bound_se = Constant $ IntValue $ intValue Int64 bound
    body' <-
      eBody
        [ eIf
            (return $ BasicOp $ SubExp $ Var cond)
            (return body)
            (resultBodyM $ map (Var . paramName . fst) param_tuples)
        ]
    return $ DoLoop param_tuples (ForLoop i it bound_se mempty) body'
convertWhileLoop _ _ = error "convertWhileLoop: not a while loop!"

splitLoop :: Integer -> SubExp -> ExpT SOACS -> ADM (ExpT SOACS)
splitLoop n bound_se = splitLoop' n bound_se bound_se
  where
    splitLoop' n' bound_se' offset loop@(DoLoop param_tuples (ForLoop i it _bound loop_vars) body)
      | n' > 1 = do
        let loop_params = map fst param_tuples
        renameForLoop loop $ \param_tuples' _form' i' it' _bound' loop_vars' body' -> do
          let loop_params' = map fst param_tuples'
              loop_inits' = map (Var . paramName) loop_params
              param_tuples'' = zip loop_params' loop_inits'
          (offset', offset_stms) <-
            collectStms $
              letSubExp
                "offset"
                $ BasicOp $ BinOp (Mul it OverflowUndef) offset (Var i)
          (i_mod, i_stms) <-
            inScopeOf offset_stms $
              collectStms $
                letExp
                  "i_inner"
                  $ BasicOp $ BinOp (Add it OverflowUndef) offset' (Var i')
          body'' <- insertStmsM $ do
            addStms i_stms
            return $ substituteNames (M.singleton i' i_mod) body'
          inner_loop <-
            splitLoop'
              (n' - 1)
              bound_se'
              offset'
              (DoLoop param_tuples'' (ForLoop i' it' bound_se' loop_vars') body'')
          (inner_loop_res, inner_loop_stms) <- collectStms $ letTupExp "inner_loop" inner_loop
          let outer_body = mkBody (offset_stms <> inner_loop_stms) $ varsRes inner_loop_res
          return $ DoLoop param_tuples (ForLoop i it bound_se' loop_vars) outer_body
      | n' == 1 =
        return $ DoLoop param_tuples (ForLoop i it bound_se' loop_vars) body
      | otherwise = return loop
    splitLoop' _ _ _ _ = error "splitLoop: only loops can be split!"

stripmine :: Integer -> Pat -> ExpT SOACS -> ADM (ExpT SOACS, ExpT SOACS, Pat)
stripmine n pat loop =
  bindForLoop loop $ \_param_tuples _form _i it bound _loop_vars _body -> do
    let n_se = Constant $ FloatValue $ floatValue Float64 (1 / fromIntegral n :: Double)
    bound_float <- letSubExp "bound_float" $ BasicOp $ ConvOp (UIToFP it Float64) bound
    bound' <- letSubExp "bound" $ BasicOp $ BinOp (FPow Float64) bound_float n_se
    bound_int <- letSubExp "convert" $ BasicOp $ ConvOp (FPToUI Float64 it) bound'
    total_iters <-
      letSubExp "total_iters" $
        BasicOp $ BinOp (Pow it) bound_int (Constant $ IntValue $ intValue it n)
    remaining <- letSubExp "remaining" $ BasicOp $ BinOp (Sub it OverflowUndef) bound total_iters
    mined_loop <- splitLoop n bound_int loop
    pat'@(Pat pats') <- renamePat pat
    renameForLoop loop $ \param_tuples' _form' i' it' _bound' loop_vars' body' -> do
      (i_mod, i_stms) <-
        collectStms $
          letExp "new_i" $ BasicOp $ BinOp (Add it OverflowUndef) total_iters (Var i')
      body'' <- insertStmsM $ do
        addStms i_stms
        return $ substituteNames (M.singleton i' i_mod) body'
      let loop_params'' = map fst param_tuples'
          loop_inits' = map (Var . patElemName) pats'
          param_tuples''' = zip loop_params'' loop_inits'
          remained_loop' = DoLoop param_tuples''' (ForLoop i' it' remaining loop_vars') body''
      return (mined_loop, remained_loop', pat')

stripmineStm :: Stm -> ADM (Stms SOACS)
stripmineStm stm@(Let pat aux loop@(DoLoop _ ForLoop {} _)) = do
  loop' <- removeLoopVars loop
  let extractNum (AttrComp "stripmine" [AttrInt n]) = Just n
      extractNum _ = Nothing
      nums = catMaybes $ mapAttrs extractNum $ stmAuxAttrs aux
   in case nums of
        (n : _) -> do
          (mined_loop, remain_loop, pat') <- stripmine n pat loop'
          collectStms_ $ do
            letBind pat' mined_loop
            letBind pat remain_loop
        _ -> return $ oneStm stm
stripmineStm stm = return $ oneStm stm

stripmineStms :: Stms SOACS -> ADM (Stms SOACS)
stripmineStms stms
  | Just (stm, stms') <- stmsHead stms = do
    stm' <- stripmineStm stm
    stms'' <- stripmineStms stms'
    return $ stm' <> stms''
  | otherwise = return mempty

diffLoop ::
  (Stms SOACS -> ADM ()) ->
  Pat ->
  StmAux () ->
  ExpT SOACS ->
  ADM () ->
  ADM ()
diffLoop
  diffStms
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

      is_true_dep = inAttrs (AttrName "true_dep") . paramAttrs

      dont_copy_params = filter is_true_dep loop_params
      dont_copy = map paramName dont_copy_params
      loop_params_to_copy = loop_params \\ dont_copy_params

      fwdLoop :: ADM [PatElem]
      fwdLoop = do
        bound64 <- asIntS Int64 bound

        (loop_acc_pats, loop_acc_params) <- fmap unzip $
          forM loop_params_to_copy $ \(Param _ v t) -> do
            acc <- newVName $ baseString v <> "_acc"
            pat_acc <- newVName $ baseString v <> "_acc"
            setLoopTape v pat_acc
            let loop_acc_param = Param mempty acc $ arrayOf t (Shape [bound64]) Unique
                loop_acc_pat = PatElem pat_acc $ arrayOf t (Shape [bound64]) NoUniqueness
            return (loop_acc_pat, loop_acc_param)

        zero_accs <- forM loop_params_to_copy $ \(Param _ v t) ->
          letSubExp (baseString v <> "_zero_acc")
            =<< eBlank (arrayOf t (Shape [bound64]) NoUniqueness)

        (loop_acc_params_ret, stms') <- collectStms $
          localScope (scopeOfFParams $ loop_acc_params <> loop_params) $
            inScopeOf form $ do
              copy_substs <- copyConsumedArrsInBody dont_copy body
              addStms stms
              i64 <- asIntS Int64 $ Var i
              forM (zip loop_params_to_copy loop_acc_params) $ \(Param _ v _, Param _ acc t) -> do
                acc' <-
                  letInPlace "loop_acc" acc (fullSlice (fromDecl t) [DimFix i64]) $
                    substituteNames copy_substs $ BasicOp $ SubExp $ Var v
                return $ Param mempty acc' (fromDecl t)

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
                dont_copy_params' = filter is_true_dep loop_params'
                subst_loop_tape v v' = mapM_ (setLoopTape v') =<< lookupLoopTape v
                res_vname (SubExpRes _ (Constant _)) = Nothing
                res_vname (SubExpRes _ (Var v)) = Just v
                loop_res = mapMaybe res_vname res'
                loop_var_arrays = map snd loop_vars'
                loop_var_param_names = map (paramName . fst) loop_vars'
                getVName Constant {} = Nothing
                getVName (Var v) = Just v
                loop_free = (namesToList (freeIn loop') \\ loop_var_arrays) \\ mapMaybe (getVName . snd) param_tuples'

            zipWithM_ subst_loop_tape loop_param_names loop_param_names'

            let dont_copy_subst =
                  M.fromList $
                    catMaybes $
                      zipWith
                        ( \pat p ->
                            if p `elem` dont_copy_params'
                              then Just (paramName p, patElemName pat)
                              else Nothing
                        )
                        pats
                        loop_params'

            let build_param_tuples_adj r (PatElem pat _, Param _ _ t) =
                  case res_vname r of
                    Just v -> do
                      pat_adj <- lookupAdjVal pat
                      v_adj <- adjVName v
                      return $ Just (Param mempty v_adj (toDecl (fromDecl t) Unique), Var pat_adj)
                    _ -> return Nothing

            param_tuples_res_adj <- catMaybes <$> zipWithM build_param_tuples_adj res' (zip pats loop_params')

            param_tuples_free_adj <-
              forM loop_free $ \v -> do
                adj_v <- adjVName v
                adj_init <- lookupAdjVal v
                t <- lookupType adj_init
                return (Param mempty adj_v (toDecl t Unique), Var adj_init)

            param_tuples_loop_vars_adj <- forM loop_vars' $ \(_, vs) -> do
              adj_vs <- adjVName vs
              adj_init <- lookupAdjVal vs
              t <- lookupType adj_init
              return (Param mempty adj_vs (toDecl t Unique), Var adj_init)

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
                localScope (scopeOfFParams $ map fst param_tuples_adj) $
                  localScope (scopeOfFParams loop_params') $
                    inScopeOf form' $ do
                      zipWithM_
                        (\(p, _) v -> insAdj v (paramName p))
                        param_tuples_adj
                        (loop_res <> loop_free <> loop_var_arrays)
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

            let restore v
                  | v `notElem` dont_copy =
                    localScope (scopeOfFParams loop_params') $ do
                      m_vs <- lookupLoopTape v
                      case m_vs of
                        Nothing -> return Nothing
                        Just vs -> do
                          vs_t <- lookupType vs
                          v' <- letExp "restore" $ BasicOp $ Index vs $ fullSlice vs_t [DimFix i_reverse64]
                          t <- lookupType v
                          let consumed = namesToList $ consumedInStms $ fst $ Alias.analyseStms mempty stms_adj
                          v'' <- case (t, v `elem` consumed) of
                            (Array {}, True) -> letExp "restore_copy" $ BasicOp $ Copy v'
                            _ -> return v'
                          return $ Just (v, v'')
                  | otherwise = return Nothing

            (_, stms_adj') <-
              inScopeOf form' $
                localScope (mconcat $ map scopeOfPatElem accs) $
                  collectStms $ do
                    addStms index_stms
                    substs <- catMaybes <$> mapM (restore . paramName) loop_params'
                    addStms $ substituteNames (M.insert i' i_reverse $ M.fromList substs) stms_adj

            inScopeOf stms_adj' $
              localScope (scopeOfFParams $ map fst param_tuples_adj) $
                localScope (scopeOfPat $ Pat accs) $ do
                  let body_adj =
                        Body () stms_adj' $ varsRes (loop_param_adjs <> loop_free_adjs <> loop_vars_adjs)
                  adjs' <-
                    letTupExp "loop_adj" $
                      substituteNames (dont_copy_subst `M.union` loop_var_arrays_substs) $
                        DoLoop
                          param_tuples_adj
                          form'
                          body_adj

                  returnSweepCode $ do
                    zipWithM_ insSubExpAdj (map snd param_tuples') $ take (length loop_param_adjs) adjs'
                    zipWithM_ (\v v_adj -> do insAdj v v_adj; (void . lookupAdjVal) v) loop_free $ take (length loop_free_adjs) $ drop (length loop_param_adjs) adjs'
                    zipWithM_ (\v v_adj -> do insAdj v v_adj; void $ lookupAdjVal v) loop_var_arrays $ drop (length loop_param_adjs + length loop_free_adjs) adjs'
          _ -> error "diffLoop: unexpected non-loop expression."
diffLoop diffStms pat aux loop@(DoLoop _param_tuples (WhileLoop _cond) _body) m =
  let getBound (AttrComp "bound" [AttrInt b]) = Just b
      getBound _ = Nothing
      bounds = catMaybes $ mapAttrs getBound $ stmAuxAttrs aux
   in case bounds of
        (bound : _) -> do
          for_loop <- convertWhileLoop bound loop
          diffLoop diffStms pat aux for_loop m
        _ -> error "diffLoop: while loops requre a bound attribute" -- this should be a type error
diffLoop _ _ _ _ _ = error "diffLoop: unexpected non-loop expression."

copyConsumedArrsInBody :: [VName] -> Body -> ADM Substitutions
copyConsumedArrsInBody dontCopy b =
  mconcat <$> mapM onConsumed (filter (`notElem` dontCopy) $ namesToList $ consumedInBody (Alias.analyseBody mempty b))
  where
    onConsumed v = do
      v_t <- lookupType v
      case v_t of
        Acc {} -> error $ "copyConsumedArrs: Acc " <> pretty v
        Array {} -> M.singleton v <$> letExp (baseString v <> "_ad_copy") (BasicOp $ Copy v)
        _ -> pure mempty
