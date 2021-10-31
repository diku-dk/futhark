{-# LANGUAGE FlexibleContexts #-}
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
import Futhark.Util

-- | A convenience function to bring the components of a for-loop into
-- scope and throw an error if the passed 'ExpT' is not a for-loop.
bindForLoop ::
  PrettyRep rep =>
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
bindForLoop (DoLoop val_pats form@(ForLoop i it bound loop_vars) body) f =
  f val_pats form i it bound loop_vars body
bindForLoop e _ = error $ "bindForLoop: not a for-loop:\n" <> pretty e

-- | A convenience function to rename a for-loop and then bind the
-- renamed components.
renameForLoop ::
  (MonadFreshNames m, Renameable rep, PrettyRep rep) =>
  ExpT rep ->
  ( [(Param (FParamInfo rep), SubExp)] ->
    LoopForm rep ->
    VName ->
    IntType ->
    SubExp ->
    [(Param (LParamInfo rep), VName)] ->
    BodyT rep ->
    m a
  ) ->
  m a
renameForLoop loop f = renameExp loop >>= (`bindForLoop` f)

-- | Transforms a 'ForLoop' into a 'ForLoop' with an empty list of
-- loop variables.
removeLoopVars :: MonadBuilder m => ExpT (Rep m) -> m (ExpT (Rep m))
removeLoopVars loop =
  bindForLoop loop $ \val_pats form i _it _bound loop_vars body -> do
    let indexify (x_param, xs) = do
          xs_t <- lookupType xs
          x' <-
            letExp (baseString $ paramName x_param) $
              BasicOp $ Index xs $ fullSlice xs_t [DimFix (Var i)]
          return (paramName x_param, x')
    (substs_list, subst_stms) <- collectStms $ mapM indexify loop_vars
    let Body aux' stms' res' = substituteNames (M.fromList substs_list) body
    return $ DoLoop val_pats form $ Body aux' (subst_stms <> stms') res'

-- | Converts a 'WhileLoop' into a 'ForLoop'. Requires that the
-- surrounding 'DoLoop' is annotated with a @#[bound(n)]@ attribute,
-- where @n@ is an upper bound on the number of iterations of the
-- while-loop. The resulting for-loop will execute for @n@ iterations on
-- all inputs, so the tighter the bound the better.
convertWhileLoop :: Integer -> ExpT SOACS -> ADM (ExpT SOACS)
convertWhileLoop bound (DoLoop val_pats (WhileLoop cond) body) =
  localScope (scopeOfFParams $ map fst val_pats) $ do
    let it = Int64
        bound_se = Constant $ IntValue $ intValue it bound
    i <- newVName "i"
    body' <-
      eBody
        [ eIf
            (return $ BasicOp $ SubExp $ Var cond)
            (return body)
            (resultBodyM $ map (Var . paramName . fst) val_pats)
        ]
    return $ DoLoop val_pats (ForLoop i it bound_se mempty) body'
convertWhileLoop _ e = error $ "convertWhileLoop: not a while-loop:\n" <> pretty e

-- | @nestifyLoop n bound loop@ transforms a loop into a depth-@n@ loop nest
-- of @bound@-iteration loops. This transformation does not preserve
-- the original semantics of the loop: @n@ and @bound@ may be arbitrary and have
-- no relation to the number of iterations of @loop@.
nestifyLoop ::
  SubExp ->
  Integer ->
  ExpT SOACS ->
  ADM (ExpT SOACS)
nestifyLoop bound_se = nestifyLoop' bound_se
  where
    nestifyLoop' offset n loop = bindForLoop loop nestify
      where
        nestify val_pats _form i it _bound loop_vars body
          | n > 1 = do
            renameForLoop loop $ \val_pats' _form' i' it' _bound' loop_vars' body' -> do
              let loop_params = map fst val_pats
                  loop_params' = map fst val_pats'
                  loop_inits' = map (Var . paramName) loop_params
                  val_pats'' = zip loop_params' loop_inits'
              outer_body <-
                buildBody_ $
                  do
                    offset' <-
                      letSubExp "offset" $
                        BasicOp $ BinOp (Mul it OverflowUndef) offset (Var i)

                    inner_body <- insertStmsM $ do
                      i_inner <-
                        letExp "i_inner" $
                          BasicOp $ BinOp (Add it OverflowUndef) offset' (Var i')
                      return $ substituteNames (M.singleton i' i_inner) body'

                    inner_loop <-
                      letTupExp "inner_loop"
                        =<< nestifyLoop'
                          offset'
                          (n - 1)
                          (DoLoop val_pats'' (ForLoop i' it' bound_se loop_vars') inner_body)
                    return $ varsRes inner_loop
              return $
                DoLoop val_pats (ForLoop i it bound_se loop_vars) outer_body
          | n == 1 =
            return $ DoLoop val_pats (ForLoop i it bound_se loop_vars) body
          | otherwise = return loop

-- | @stripmine n pat loop@ stripmines a loop into a depth-@n@ loop nest.
-- An additional @bound - (floor(bound^(1/n)))^n@-iteration remainder loop is
-- inserted after the stripmined loop which executes the remaining iterations
-- so that the stripmined loop is semantically equivalent to the original loop.
stripmine :: Integer -> Pat -> ExpT SOACS -> ADM (Stms SOACS)
stripmine n pat loop = do
  loop' <- removeLoopVars loop
  bindForLoop loop' $ \_val_pats _form _i it bound _loop_vars _body -> do
    let n_root = Constant $ FloatValue $ floatValue Float64 (1 / fromIntegral n :: Double)
    bound_float <- letSubExp "bound_f64" $ BasicOp $ ConvOp (UIToFP it Float64) bound
    bound' <- letSubExp "bound" $ BasicOp $ BinOp (FPow Float64) bound_float n_root
    bound_int <- letSubExp "bound_int" $ BasicOp $ ConvOp (FPToUI Float64 it) bound'
    total_iters <-
      letSubExp "total_iters" $
        BasicOp $ BinOp (Pow it) bound_int (Constant $ IntValue $ intValue it n)
    remain_iters <-
      letSubExp "remain_iters" $ BasicOp $ BinOp (Sub it OverflowUndef) bound total_iters
    mined_loop <- nestifyLoop bound_int n loop
    pat' <- renamePat pat
    renameForLoop loop $ \val_pats' _form' i' it' _bound' loop_vars' body' -> do
      remain_body <- insertStmsM $ do
        i_remain <-
          letExp "i_remain" $
            BasicOp $ BinOp (Add it OverflowUndef) total_iters (Var i')
        return $ substituteNames (M.singleton i' i_remain) body'
      let loop_params_rem = map fst val_pats'
          loop_inits_rem = map (Var . patElemName) $ patElems pat'
          val_pats_rem = zip loop_params_rem loop_inits_rem
          remain_loop = DoLoop val_pats_rem (ForLoop i' it' remain_iters loop_vars') remain_body
      collectStms_ $ do
        letBind pat' mined_loop
        letBind pat remain_loop

-- | Stripmines a statement. Only has an effect when the statement's
-- expression is a for-loop with a @#[stripmine(n)]@ attribute, where
-- @n@ is the nesting depth.
stripmineStm :: Stm -> ADM (Stms SOACS)
stripmineStm stm@(Let pat aux loop@(DoLoop _ ForLoop {} _)) =
  case nums of
    (n : _) -> stripmine n pat loop
    _ -> return $ oneStm stm
  where
    extractNum (AttrComp "stripmine" [AttrInt n]) = Just n
    extractNum _ = Nothing
    nums = catMaybes $ mapAttrs extractNum $ stmAuxAttrs aux
stripmineStm stm = return $ oneStm stm

stripmineStms :: Stms SOACS -> ADM (Stms SOACS)
stripmineStms = traverseFold stripmineStm

-- | Forward pass transformation of a loop. This includes modifying the loop
-- to save the loop values at each iteration onto a tape as well as copying
-- any consumed arrays in the loop's body and consuming said copies in lieu of
-- the originals (which will be consumed later in the reverse pass).
fwdLoop ::
  Pat ->
  StmAux () ->
  ExpT SOACS ->
  ADM ()
fwdLoop (Pat pats) aux loop =
  bindForLoop loop $ \val_pats form i _it bound _loop_vars body -> do
    bound64 <- asIntS Int64 bound
    let loop_params = map fst val_pats
        is_true_dep = inAttrs (AttrName "true_dep") . paramAttrs
        dont_copy_params = filter is_true_dep loop_params
        dont_copy = map paramName dont_copy_params
        loop_params_to_copy = loop_params \\ dont_copy_params

    empty_saved_array <-
      forM loop_params_to_copy $ \p ->
        letSubExp (baseString (paramName p) <> "_empty_saved")
          =<< eBlank (arrayOf (paramDec p) (Shape [bound64]) NoUniqueness)

    (body', (saved_pats, saved_params)) <- buildBody $
      localScope (scopeOfFParams loop_params) $
        inScopeOf form $ do
          copy_substs <- copyConsumedArrsInBody dont_copy body
          addStms $ bodyStms body
          i_i64 <- asIntS Int64 $ Var i
          (saved_updates, saved_pats_params) <- fmap unzip $
            forM loop_params_to_copy $ \p -> do
              let v = paramName p
                  t = paramDec p
              saved_param_v <- newVName $ baseString v <> "_saved"
              saved_pat_v <- newVName $ baseString v <> "_saved"
              setLoopTape v saved_pat_v
              let saved_param = Param mempty saved_param_v $ arrayOf t (Shape [bound64]) Unique
                  saved_pat = PatElem saved_pat_v $ arrayOf t (Shape [bound64]) NoUniqueness
              saved_update <-
                localScope (scopeOfFParams [saved_param]) $
                  letInPlace
                    (baseString v <> "_saved_update")
                    saved_param_v
                    (fullSlice (fromDecl $ paramDec saved_param) [DimFix i_i64])
                    $ substituteNames copy_substs $ BasicOp $ SubExp $ Var v
              return (saved_update, (saved_pat, saved_param))
          return (bodyResult body <> varsRes saved_updates, unzip saved_pats_params)

    let pat' = Pat $ pats <> saved_pats
        val_pats' = val_pats <> zip saved_params empty_saved_array
    addStm $ Let pat' aux $ DoLoop val_pats' form body'

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
           val_pats
           form@ForLoop {}
           _body
         )
  m = do
    fwdLoop (Pat pats) aux loop
    m
    revLoop
    where
      loop_params = map fst val_pats
      loop_param_names = map paramName loop_params

      is_true_dep = inAttrs (AttrName "true_dep") . paramAttrs

      dont_copy_params = filter is_true_dep loop_params
      dont_copy = map paramName dont_copy_params

      revLoop :: ADM ()
      revLoop = do
        loop' <- renameSomething loop

        case loop' of
          DoLoop val_pats' form'@(ForLoop i' it' bound' loop_vars') (Body () stms' res') -> do
            let loop_params' = map fst val_pats'
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
                loop_free = (namesToList (freeIn loop') \\ loop_var_arrays) \\ mapMaybe (getVName . snd) val_pats'

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

            let build_val_pats_adj r (PatElem pat _, Param _ _ t) =
                  case res_vname r of
                    Just v -> do
                      pat_adj <- lookupAdjVal pat
                      v_adj <- adjVName v
                      return $ Just (Param mempty v_adj (toDecl (fromDecl t) Unique), Var pat_adj)
                    _ -> return Nothing

            val_pats_res_adj <- catMaybes <$> zipWithM build_val_pats_adj res' (zip pats loop_params')

            val_pats_free_adj <-
              forM loop_free $ \v -> do
                adj_v <- adjVName v
                adj_init <- lookupAdjVal v
                t <- lookupType adj_init
                return (Param mempty adj_v (toDecl t Unique), Var adj_init)

            val_pats_loop_vars_adj <- forM loop_vars' $ \(_, vs) -> do
              adj_vs <- adjVName vs
              adj_init <- lookupAdjVal vs
              t <- lookupType adj_init
              return (Param mempty adj_vs (toDecl t Unique), Var adj_init)

            let val_pats_adj = val_pats_res_adj <> val_pats_free_adj <> val_pats_loop_vars_adj

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
                localScope (scopeOfFParams $ map fst val_pats_adj) $
                  localScope (scopeOfFParams loop_params') $
                    inScopeOf form' $ do
                      zipWithM_
                        (\(p, _) v -> insAdj v (paramName p))
                        val_pats_adj
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
                collectStms $ do
                  addStms index_stms
                  substs <- catMaybes <$> mapM (restore . paramName) loop_params'

                  addStms $ substituteNames (M.insert i' i_reverse $ M.fromList substs) stms_adj

            inScopeOf stms_adj' $
              localScope (scopeOfFParams $ map fst val_pats_adj) $
                do
                  let body_adj =
                        Body () stms_adj' $ varsRes (loop_param_adjs <> loop_free_adjs <> loop_vars_adjs)
                  adjs' <-
                    letTupExp "loop_adj" $
                      substituteNames (dont_copy_subst `M.union` loop_var_arrays_substs) $
                        DoLoop
                          val_pats_adj
                          form'
                          body_adj

                  returnSweepCode $ do
                    zipWithM_ insSubExpAdj (map snd val_pats') $ take (length loop_param_adjs) adjs'
                    zipWithM_ (\v v_adj -> do insAdj v v_adj; (void . lookupAdjVal) v) loop_free $ take (length loop_free_adjs) $ drop (length loop_param_adjs) adjs'
                    zipWithM_ (\v v_adj -> do insAdj v v_adj; void $ lookupAdjVal v) loop_var_arrays $ drop (length loop_param_adjs + length loop_free_adjs) adjs'
          _ -> error "diffLoop: unexpected non-loop expression."
diffLoop diffStms pat aux loop@(DoLoop _val_pats (WhileLoop _cond) _body) m =
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
