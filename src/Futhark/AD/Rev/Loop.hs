{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Rev.Loop (diffLoop, stripmineStms) where

import Control.Monad
import Data.Foldable (toList)
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe
import Futhark.AD.Rev.Monad
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases (consumedInStms)
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (traverseFold)

-- | A convenience function to bring the components of a for-loop into
-- scope and throw an error if the passed 'Exp' is not a for-loop.
bindForLoop ::
  PrettyRep rep =>
  Exp rep ->
  ( [(Param (FParamInfo rep), SubExp)] ->
    LoopForm rep ->
    VName ->
    IntType ->
    SubExp ->
    [(Param (LParamInfo rep), VName)] ->
    Body rep ->
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
  Exp rep ->
  ( Exp rep ->
    [(Param (FParamInfo rep), SubExp)] ->
    LoopForm rep ->
    VName ->
    IntType ->
    SubExp ->
    [(Param (LParamInfo rep), VName)] ->
    Body rep ->
    m a
  ) ->
  m a
renameForLoop loop f = renameExp loop >>= \loop' -> bindForLoop loop' (f loop')

-- | Is the loop a while-loop?
isWhileLoop :: Exp rep -> Bool
isWhileLoop (DoLoop _ WhileLoop {} _) = True
isWhileLoop _ = False

-- | Transforms a 'ForLoop' into a 'ForLoop' with an empty list of
-- loop variables.
removeLoopVars :: MonadBuilder m => Exp (Rep m) -> m (Exp (Rep m))
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

-- | Augments a while-loop to also compute the number of iterations.
computeWhileIters :: Exp SOACS -> ADM SubExp
computeWhileIters (DoLoop val_pats (WhileLoop b) body) = do
  bound_v <- newVName "bound"
  let t = Prim $ IntType Int64
      bound_param = Param mempty bound_v t
  bound_init <- letSubExp "bound_init" $ zeroExp t
  body' <- localScope (scopeOfFParams [bound_param]) $
    buildBody_ $ do
      bound_plus_one <-
        let one = Constant $ IntValue $ intValue Int64 (1 :: Int)
         in letSubExp "bound+1" $ BasicOp $ BinOp (Add Int64 OverflowUndef) (Var bound_v) one
      addStms $ bodyStms body
      return (pure (subExpRes bound_plus_one) <> bodyResult body)
  res <- letTupExp' "loop" $ DoLoop ((bound_param, bound_init) : val_pats) (WhileLoop b) body'
  return $ head res
computeWhileIters e = error $ "convertWhileIters: not a while-loop:\n" <> pretty e

-- | Converts a 'WhileLoop' into a 'ForLoop'. Requires that the
-- surrounding 'DoLoop' is annotated with a @#[bound(n)]@ attribute,
-- where @n@ is an upper bound on the number of iterations of the
-- while-loop. The resulting for-loop will execute for @n@ iterations on
-- all inputs, so the tighter the bound the better.
convertWhileLoop :: SubExp -> Exp SOACS -> ADM (Exp SOACS)
convertWhileLoop bound_se (DoLoop val_pats (WhileLoop cond) body) =
  localScope (scopeOfFParams $ map fst val_pats) $ do
    i <- newVName "i"
    body' <-
      eBody
        [ eIf
            (return $ BasicOp $ SubExp $ Var cond)
            (return body)
            (resultBodyM $ map (Var . paramName . fst) val_pats)
        ]
    return $ DoLoop val_pats (ForLoop i Int64 bound_se mempty) body'
convertWhileLoop _ e = error $ "convertWhileLoopBound: not a while-loop:\n" <> pretty e

-- | @nestifyLoop n bound loop@ transforms a loop into a depth-@n@ loop nest
-- of @bound@-iteration loops. This transformation does not preserve
-- the original semantics of the loop: @n@ and @bound@ may be arbitrary and have
-- no relation to the number of iterations of @loop@.
nestifyLoop ::
  SubExp ->
  Integer ->
  Exp SOACS ->
  ADM (Exp SOACS)
nestifyLoop bound_se = nestifyLoop' bound_se
  where
    nestifyLoop' offset n loop = bindForLoop loop nestify
      where
        nestify val_pats _form i it _bound loop_vars body
          | n > 1 = do
            renameForLoop loop $ \_loop' val_pats' _form' i' it' _bound' loop_vars' body' -> do
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
stripmine :: Integer -> Pat Type -> Exp SOACS -> ADM (Stms SOACS)
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
    renameForLoop loop $ \_loop' val_pats' _form' i' it' _bound' loop_vars' body' -> do
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
stripmineStm :: Stm SOACS -> ADM (Stms SOACS)
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
fwdLoop :: Pat Type -> StmAux () -> Exp SOACS -> ADM ()
fwdLoop pat aux loop =
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

    let pat' = pat <> Pat saved_pats
        val_pats' = val_pats <> zip saved_params empty_saved_array
    addStm $ Let pat' aux $ DoLoop val_pats' form body'

-- | Construct a loop value-pattern for the adjoint of the
-- given variable.
valPatAdj :: VName -> ADM (Param DeclType, SubExp)
valPatAdj v = do
  v_adj <- adjVName v
  init_adj <- lookupAdjVal v
  t <- lookupType init_adj
  return (Param mempty v_adj (toDecl t Unique), Var init_adj)

valPatAdjs :: LoopInfo [VName] -> ADM (LoopInfo [(Param DeclType, SubExp)])
valPatAdjs = (mapM . mapM) valPatAdj

-- | Reverses a loop by substituting the loop index as well as reversing
-- the arrays that loop variables are bound to.
reverseIndices :: Exp SOACS -> ADM (Substitutions, Substitutions, Stms SOACS)
reverseIndices loop = do
  bindForLoop loop $ \_val_pats form i it bound loop_vars _body -> do
    bound_minus_one <-
      inScopeOf form $
        let one = Constant $ IntValue $ intValue it (1 :: Int)
         in letSubExp "bound-1" $ BasicOp $ BinOp (Sub it OverflowUndef) bound one

    var_arrays_substs <- fmap M.fromList $
      inScopeOf form $ do
        forM (map snd loop_vars) $ \xs -> do
          xs_t <- lookupType xs
          xs_rev <-
            letExp "reverse" $
              BasicOp $
                Index xs $
                  fullSlice
                    xs_t
                    [DimSlice bound_minus_one bound (Constant (IntValue (Int64Value (-1))))]
          return (xs, xs_rev)

    (i_rev, i_stms) <- collectStms $
      inScopeOf form $ do
        letExp (baseString i <> "_rev") $
          BasicOp $ BinOp (Sub it OverflowWrap) bound_minus_one (Var i)

    return (var_arrays_substs, M.singleton i i_rev, i_stms)

-- | Returns a substitution which substitutes values in the reverse
-- loop body with values from the tape.
restore :: Stms SOACS -> [Param DeclType] -> VName -> ADM Substitutions
restore stms_adj loop_params' i' =
  M.fromList . catMaybes <$> mapM f loop_params'
  where
    dont_copy =
      map paramName $ filter (inAttrs (AttrName "true_dep") . paramAttrs) loop_params'
    f p
      | v `notElem` dont_copy = do
        m_vs <- lookupLoopTape v
        case m_vs of
          Nothing -> return Nothing
          Just vs -> do
            vs_t <- lookupType vs
            i_i64' <- asIntS Int64 $ Var i'
            v' <- letExp "restore" $ BasicOp $ Index vs $ fullSlice vs_t [DimFix i_i64']
            t <- lookupType v
            v'' <- case (t, v `elem` consumed) of
              (Array {}, True) -> letExp "restore_copy" $ BasicOp $ Copy v'
              _ -> return v'
            return $ Just (v, v'')
      | otherwise = return Nothing
      where
        v = paramName p
        consumed = namesToList $ consumedInStms $ fst $ Alias.analyseStms mempty stms_adj

-- | A type to keep track of and seperate values corresponding to different
-- parts of the loop.
data LoopInfo a = LoopInfo
  { loopRes :: a,
    loopFree :: a,
    loopVars :: a,
    loopVals :: a
  }
  deriving (Functor, Foldable, Traversable, Show)

-- | Transforms a for-loop into its reverse-mode derivative.
revLoop :: (Stms SOACS -> ADM ()) -> Pat Type -> Exp SOACS -> ADM ()
revLoop diffStms pat loop =
  bindForLoop loop $ \val_pats _form _i _it _bound _loop_vars _body ->
    renameForLoop loop $
      \loop' val_pats' form' i' _it' _bound' loop_vars' body' -> do
        let loop_params = map fst val_pats
            (loop_params', loop_vals') = unzip val_pats'
            loop_var_arrays' = map snd loop_vars'
            getVName Constant {} = Nothing
            getVName (Var v) = Just v
            loop_vnames =
              LoopInfo
                { loopRes = mapMaybe subExpResVName $ bodyResult body',
                  loopFree =
                    (namesToList (freeIn loop') \\ loop_var_arrays') \\ mapMaybe getVName loop_vals',
                  loopVars = loop_var_arrays',
                  loopVals = mapMaybe getVName loop_vals'
                }

        renameLoopTape $ M.fromList $ zip (map paramName loop_params) (map paramName loop_params')

        forM_ (zip (bodyResult body') $ patElems pat) $ \(se_res, pe) ->
          case subExpResVName se_res of
            Just v -> setAdj v =<< lookupAdj (patElemName pe)
            Nothing -> return ()

        (var_array_substs, i_subst, i_stms) <-
          reverseIndices loop'

        val_pat_adjs <- valPatAdjs loop_vnames
        let val_pat_adjs_list = concat $ toList val_pat_adjs

        (loop_adjs, stms_adj) <- collectStms $
          inScopeOf form' $
            localScope (scopeOfFParams (map fst val_pat_adjs_list <> loop_params')) $ do
              addStms i_stms
              (loop_adjs, stms_adj) <- collectStms $
                subAD $ do
                  zipWithM_
                    (\val_pat v -> insAdj v (paramName $ fst val_pat))
                    val_pat_adjs_list
                    (concat $ toList loop_vnames)
                  diffStms $ bodyStms body'

                  let update_var_arrays v vs = do
                        vs_t <- lookupType vs
                        v_adj <- lookupAdjVal v
                        updateAdjSlice (fullSlice vs_t [DimFix $ Var i']) vs v_adj
                  zipWithM_
                    update_var_arrays
                    (map (paramName . fst) loop_vars')
                    (loopVars loop_vnames)

                  loop_res_adjs <- mapM (lookupAdjVal . paramName) loop_params'
                  loop_free_adjs <- mapM lookupAdjVal $ loopFree loop_vnames
                  loop_vars_adjs <- mapM lookupAdjVal $ loopVars loop_vnames
                  loop_vals_adjs <- mapM lookupAdjVal $ loopVals loop_vnames

                  return $
                    LoopInfo
                      { loopRes = loop_res_adjs,
                        loopFree = loop_free_adjs,
                        loopVars = loop_vars_adjs,
                        loopVals = loop_vals_adjs
                      }
              (substs, restore_stms) <-
                collectStms $ restore stms_adj loop_params' i'
              addStms $ substituteNames i_subst restore_stms
              addStms $ substituteNames i_subst $ substituteNames substs stms_adj
              return loop_adjs

        inScopeOf stms_adj $
          localScope (scopeOfFParams $ map fst val_pat_adjs_list) $ do
            let body_adj = mkBody stms_adj $ varsRes $ concat $ toList loop_adjs
                restore_true_deps = M.fromList $
                  flip mapMaybe (zip loop_params' $ patElems pat) $ \(p, pe) ->
                    if p `elem` filter (inAttrs (AttrName "true_dep") . paramAttrs) loop_params'
                      then Just (paramName p, patElemName pe)
                      else Nothing
            adjs' <-
              letTupExp "loop_adj" $
                substituteNames (restore_true_deps <> var_array_substs) $
                  DoLoop val_pat_adjs_list form' body_adj
            let (loop_res_adjs, loop_free_var_val_adjs) =
                  splitAt (length $ loopRes loop_adjs) adjs'
                (loop_free_adjs, loop_var_val_adjs) =
                  splitAt (length $ loopFree loop_adjs) loop_free_var_val_adjs
                (loop_var_adjs, loop_val_adjs) =
                  splitAt (length $ loopVars loop_adjs) loop_var_val_adjs
            returnSweepCode $ do
              zipWithM_ insSubExpAdj loop_vals' loop_res_adjs
              zipWithM_ insAdj (loopFree loop_vnames) loop_free_adjs
              zipWithM_ insAdj (loopVars loop_vnames) loop_var_adjs
              zipWithM_ updateAdj (loopVals loop_vnames) loop_val_adjs

-- | Transforms a loop into its reverse-mode derivative.
diffLoop :: (Stms SOACS -> ADM ()) -> Pat Type -> StmAux () -> Exp SOACS -> ADM () -> ADM ()
diffLoop diffStms pat aux loop m
  | isWhileLoop loop =
    let getBound (AttrComp "bound" [AttrInt b]) = Just b
        getBound _ = Nothing
        bounds = catMaybes $ mapAttrs getBound $ stmAuxAttrs aux
     in case bounds of
          (bound : _) -> do
            let bound_se = Constant $ IntValue $ intValue Int64 bound
            for_loop <- convertWhileLoop bound_se loop
            diffLoop diffStms pat aux for_loop m
          _ -> do
            bound <- computeWhileIters loop
            for_loop <- convertWhileLoop bound =<< renameExp loop
            diffLoop diffStms pat aux for_loop m
  | otherwise = do
    fwdLoop pat aux loop
    m
    revLoop diffStms pat loop
