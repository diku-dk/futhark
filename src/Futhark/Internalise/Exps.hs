{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

-- | Conversion of a monomorphic, first-order, defunctorised source
-- program to a core Futhark program.
module Futhark.Internalise.Exps (transformProg) where

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor
import Data.Either
import Data.Foldable (toList)
import Data.List (elemIndex, find, intercalate, intersperse, maximumBy, transpose, zip4)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.IR.SOACS as I hiding (stmPat)
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.Bindings
import Futhark.Internalise.Entry
import Futhark.Internalise.Lambdas
import Futhark.Internalise.Monad as I
import Futhark.Internalise.TypesValues
import Futhark.Transform.Rename as I
import Futhark.Util (lookupWithIndex, splitAt3)
import Futhark.Util.Pretty (align, docText, pretty)
import Language.Futhark as E hiding (TypeArg)
import Language.Futhark.TypeChecker.Types qualified as E

-- | Convert a program in source Futhark to a program in the Futhark
-- core language.
transformProg :: (MonadFreshNames m) => Bool -> VisibleTypes -> [E.ValBind] -> m (I.Prog SOACS)
transformProg always_safe types vbinds = do
  (opaques, consts, funs) <-
    runInternaliseM always_safe (internaliseValBinds types vbinds)
  I.renameProg $ I.Prog opaques consts funs

internaliseValBinds :: VisibleTypes -> [E.ValBind] -> InternaliseM ()
internaliseValBinds types = mapM_ $ internaliseValBind types

internaliseFunName :: VName -> Name
internaliseFunName = nameFromString . prettyString

shiftRetAls :: Int -> RetAls -> RetAls
shiftRetAls d (RetAls pals rals) = RetAls pals $ map (+ d) rals

internaliseValBind :: VisibleTypes -> E.ValBind -> InternaliseM ()
internaliseValBind types fb@(E.ValBind entry fname _ (Info rettype) tparams params body _ attrs loc) = do
  bindingFParams tparams params $ \shapeparams params' -> do
    let shapenames = map I.paramName shapeparams
        all_params = map pure shapeparams ++ concat params'
        msg = errorMsg ["Function return value does not match shape of declared return type."]

    (body', rettype') <- buildBody $ do
      body_res <- internaliseExp (baseString fname <> "_res") body
      (rettype', retals) <-
        first zeroExts . unzip . internaliseReturnType (map (fmap paramDeclType) all_params) rettype
          <$> mapM subExpType body_res

      when (null params') $
        bindExtSizes (E.AppRes (E.toStruct $ E.retType rettype) (E.retDims rettype)) body_res

      body_res' <-
        ensureResultExtShape msg loc (map I.fromDecl rettype') $ subExpsRes body_res
      let num_ctx = length (shapeContext rettype')
      pure
        ( body_res',
          replicate num_ctx (I.Prim int64, mempty)
            ++ zip rettype' (map (shiftRetAls num_ctx) retals)
        )

    attrs' <- internaliseAttrs attrs

    let fd =
          I.FunDef
            Nothing
            attrs'
            (internaliseFunName fname)
            rettype'
            (foldMap toList all_params)
            body'

    if null params'
      then bindConstant fname fd
      else
        bindFunction
          fname
          fd
          ( shapenames,
            map declTypeOf $ foldMap (foldMap toList) params',
            foldMap toList all_params,
            fmap (`zip` map snd rettype')
              . applyRetType (map fst rettype') (foldMap toList all_params)
          )

  case entry of
    Just (Info entry') -> generateEntryPoint types entry' fb
    Nothing -> pure ()
  where
    zeroExts ts = generaliseExtTypes ts ts

generateEntryPoint :: VisibleTypes -> E.EntryPoint -> E.ValBind -> InternaliseM ()
generateEntryPoint types (E.EntryPoint e_params e_rettype) vb = do
  let (E.ValBind _ ofname _ (Info rettype) tparams params _ _ attrs loc) = vb
  bindingFParams tparams params $ \shapeparams params' -> do
    let all_params = map pure shapeparams ++ concat params'
        (entry_rettype, retals) =
          unzip $ map unzip $ internaliseEntryReturnType (map (fmap paramDeclType) all_params) rettype
        (entry', opaques) =
          entryPoint
            types
            (baseName ofname)
            (zip e_params $ map (foldMap toList) params')
            (e_rettype, map (map I.rankShaped) entry_rettype)
        args = map (I.Var . I.paramName) $ foldMap (foldMap toList) params'

    addOpaques opaques

    (entry_body, ctx_ts) <- buildBody $ do
      -- Special case the (rare) situation where the entry point is
      -- not a function.
      maybe_const <- lookupConst ofname
      vals <- case maybe_const of
        Just ses ->
          pure ses
        Nothing ->
          funcall "entry_result" (E.qualName ofname) args loc
      ctx <-
        extractShapeContext (zeroExts $ concat entry_rettype)
          <$> mapM (fmap I.arrayDims . subExpType) vals
      pure (subExpsRes $ ctx ++ vals, map (const (I.Prim int64, mempty)) ctx)

    attrs' <- internaliseAttrs attrs
    let num_ctx = length ctx_ts
    addFunDef $
      I.FunDef
        (Just entry')
        attrs'
        ("entry_" <> baseName ofname)
        ( ctx_ts
            ++ zip
              (zeroExts (concat entry_rettype))
              (map (shiftRetAls num_ctx) $ concat retals)
        )
        (shapeparams ++ foldMap (foldMap toList) params')
        entry_body
  where
    zeroExts ts = generaliseExtTypes ts ts

internaliseBody :: String -> E.Exp -> InternaliseM (Body SOACS)
internaliseBody desc e =
  buildBody_ $ subExpsRes <$> internaliseExp (desc <> "_res") e

bodyFromStms ::
  InternaliseM (Result, a) ->
  InternaliseM (Body SOACS, a)
bodyFromStms m = do
  ((res, a), stms) <- collectStms m
  (,a) <$> mkBodyM stms res

-- | Only returns those pattern names that are not used in the pattern
-- itself (the "non-existential" part, you could say).
letValExp :: String -> I.Exp SOACS -> InternaliseM [VName]
letValExp name e = do
  e_t <- expExtType e
  names <- replicateM (length e_t) $ newVName name
  letBindNames names e
  let ctx = shapeContext e_t
  pure $ map fst $ filter ((`S.notMember` ctx) . snd) $ zip names [0 ..]

letValExp' :: String -> I.Exp SOACS -> InternaliseM [SubExp]
letValExp' _ (BasicOp (SubExp se)) = pure [se]
letValExp' name ses = map I.Var <$> letValExp name ses

internaliseAppExp :: String -> E.AppRes -> E.AppExp -> InternaliseM [I.SubExp]
internaliseAppExp desc _ (E.Index e idxs loc) = do
  vs <- internaliseExpToVars "indexed" e
  dims <- case vs of
    [] -> pure [] -- Will this happen?
    v : _ -> I.arrayDims <$> lookupType v
  (idxs', cs) <- internaliseSlice loc dims idxs
  let index v = do
        v_t <- lookupType v
        pure $ I.BasicOp $ I.Index v $ fullSlice v_t idxs'
  certifying cs $ mapM (letSubExp desc <=< index) vs
internaliseAppExp desc _ (E.Range start maybe_second end loc) = do
  start' <- internaliseExp1 "range_start" start
  end' <- internaliseExp1 "range_end" $ case end of
    DownToExclusive e -> e
    ToInclusive e -> e
    UpToExclusive e -> e
  maybe_second' <-
    traverse (internaliseExp1 "range_second") maybe_second

  -- Construct an error message in case the range is invalid.
  let conv = case E.typeOf start of
        E.Scalar (E.Prim (E.Unsigned _)) -> asIntZ Int64
        _ -> asIntS Int64
  start'_i64 <- conv start'
  end'_i64 <- conv end'
  maybe_second'_i64 <- traverse conv maybe_second'
  let errmsg =
        errorMsg $
          ["Range "]
            ++ [ErrorVal int64 start'_i64]
            ++ ( case maybe_second'_i64 of
                   Nothing -> []
                   Just second_i64 -> ["..", ErrorVal int64 second_i64]
               )
            ++ ( case end of
                   DownToExclusive {} -> ["..>"]
                   ToInclusive {} -> ["..."]
                   UpToExclusive {} -> ["..<"]
               )
            ++ [ErrorVal int64 end'_i64, " is invalid."]

  (it, le_op, lt_op) <-
    case E.typeOf start of
      E.Scalar (E.Prim (E.Signed it)) -> pure (it, CmpSle it, CmpSlt it)
      E.Scalar (E.Prim (E.Unsigned it)) -> pure (it, CmpUle it, CmpUlt it)
      start_t -> error $ "Start value in range has type " ++ prettyString start_t

  let one = intConst it 1
      negone = intConst it (-1)
      default_step = case end of
        DownToExclusive {} -> negone
        ToInclusive {} -> one
        UpToExclusive {} -> one

  (step, step_zero) <- case maybe_second' of
    Just second' -> do
      subtracted_step <-
        letSubExp "subtracted_step" $
          I.BasicOp $
            I.BinOp (I.Sub it I.OverflowWrap) second' start'
      step_zero <- letSubExp "step_zero" $ I.BasicOp $ I.CmpOp (I.CmpEq $ IntType it) start' second'
      pure (subtracted_step, step_zero)
    Nothing ->
      pure (default_step, constant False)

  step_sign <- letSubExp "s_sign" $ BasicOp $ I.UnOp (I.SSignum it) step
  step_sign_i64 <- asIntS Int64 step_sign

  bounds_invalid_downwards <-
    letSubExp "bounds_invalid_downwards" $
      I.BasicOp $
        I.CmpOp le_op start' end'
  bounds_invalid_upwards <-
    letSubExp "bounds_invalid_upwards" $
      I.BasicOp $
        I.CmpOp lt_op end' start'

  (distance, step_wrong_dir, bounds_invalid) <- case end of
    DownToExclusive {} -> do
      step_wrong_dir <-
        letSubExp "step_wrong_dir" $
          I.BasicOp $
            I.CmpOp (I.CmpEq $ IntType it) step_sign one
      distance <-
        letSubExp "distance" $
          I.BasicOp $
            I.BinOp (Sub it I.OverflowWrap) start' end'
      distance_i64 <- asIntS Int64 distance
      pure (distance_i64, step_wrong_dir, bounds_invalid_downwards)
    UpToExclusive {} -> do
      step_wrong_dir <-
        letSubExp "step_wrong_dir" $
          I.BasicOp $
            I.CmpOp (I.CmpEq $ IntType it) step_sign negone
      distance <- letSubExp "distance" $ I.BasicOp $ I.BinOp (Sub it I.OverflowWrap) end' start'
      distance_i64 <- asIntS Int64 distance
      pure (distance_i64, step_wrong_dir, bounds_invalid_upwards)
    ToInclusive {} -> do
      downwards <-
        letSubExp "downwards" $
          I.BasicOp $
            I.CmpOp (I.CmpEq $ IntType it) step_sign negone
      distance_downwards_exclusive <-
        letSubExp "distance_downwards_exclusive" $
          I.BasicOp $
            I.BinOp (Sub it I.OverflowWrap) start' end'
      distance_upwards_exclusive <-
        letSubExp "distance_upwards_exclusive" $
          I.BasicOp $
            I.BinOp (Sub it I.OverflowWrap) end' start'

      bounds_invalid <-
        letSubExp "bounds_invalid"
          =<< eIf
            (eSubExp downwards)
            (resultBodyM [bounds_invalid_downwards])
            (resultBodyM [bounds_invalid_upwards])
      distance_exclusive <-
        letSubExp "distance_exclusive"
          =<< eIf
            (eSubExp downwards)
            (resultBodyM [distance_downwards_exclusive])
            (resultBodyM [distance_upwards_exclusive])
      distance_exclusive_i64 <- asIntS Int64 distance_exclusive
      distance <-
        letSubExp "distance" $
          I.BasicOp $
            I.BinOp
              (Add Int64 I.OverflowWrap)
              distance_exclusive_i64
              (intConst Int64 1)
      pure (distance, constant False, bounds_invalid)

  step_invalid <-
    letSubExp "step_invalid" $
      I.BasicOp $
        I.BinOp I.LogOr step_wrong_dir step_zero

  invalid <-
    letSubExp "range_invalid" $
      I.BasicOp $
        I.BinOp I.LogOr step_invalid bounds_invalid
  valid <- letSubExp "valid" $ I.BasicOp $ I.UnOp I.Not invalid
  cs <- assert "range_valid_c" valid errmsg loc

  step_i64 <- asIntS Int64 step
  pos_step <-
    letSubExp "pos_step" $
      I.BasicOp $
        I.BinOp (Mul Int64 I.OverflowWrap) step_i64 step_sign_i64

  num_elems <-
    certifying cs $
      letSubExp "num_elems" $
        I.BasicOp $
          I.BinOp (SDivUp Int64 I.Unsafe) distance pos_step

  se <- letSubExp desc (I.BasicOp $ I.Iota num_elems start' step it)
  pure [se]
internaliseAppExp desc (E.AppRes et ext) e@E.Apply {} =
  case findFuncall e of
    (FunctionHole loc, _args) -> do
      -- The function we are supposed to call doesn't exist, but we
      -- have to synthesize some fake values of the right type.  The
      -- easy way to do this is to just ignore the arguments and
      -- create a hole whose type is the type of the entire
      -- application.  One caveat is that we need to replace any
      -- existential sizes, too (with zeroes, because they don't
      -- matter).
      let subst = map (,E.ExpSubst (E.sizeFromInteger 0 mempty)) ext
          et' = E.applySubst (`lookup` subst) et
      internaliseExp desc (E.Hole (Info et') loc)
    (FunctionName qfname, argsam) -> do
      -- Argument evaluation is outermost-in so that any existential sizes
      -- created by function applications can be brought into scope.
      let fname = nameFromString $ prettyString $ baseName $ qualLeaf qfname
          loc = srclocOf e
          (args, ams) = unzip argsam
          args_am_desc = zip3 args ams (repeat (nameToString fname ++ "_arg"))

      -- Some functions are magical (overloaded) and we handle that here.
      case () of
        ()
          -- Overloaded and intrinsic functions never take array
          -- arguments (except equality, but those cannot be
          -- existential), so we can safely ignore the existential
          -- dimensions.
          | Just internalise <- isOverloadedFunction qfname desc loc -> do
              withAutoMap args_am_desc $ \args' -> do
                let prepareArg ((arg, _), am, _) arg' =
                      (E.toStruct $ E.stripArray (E.shapeRank $ autoMap am) (E.typeOf arg), arg')
                internalise $ zipWith prepareArg args_am_desc args'
          | Just internalise <- isIntrinsicFunction qfname (map fst args) loc ->
              internalise desc
          | baseTag (qualLeaf qfname) <= maxIntrinsicTag,
            Just (rettype, _) <- M.lookup fname I.builtInFunctions ->
              withAutoMap args_am_desc $ \args' -> do
                let tag ses = [(se, I.Observe) | se <- ses]
                let args'' = concatMap tag args'
                letValExp' desc $ I.Apply fname args'' [(I.Prim rettype, mempty)] (Safe, loc, [])
          | otherwise -> do
              withAutoMap args_am_desc $ \args' -> do
                funcall desc qfname (concat args') loc
internaliseAppExp desc _ (E.LetPat sizes pat e body _) =
  internalisePat desc sizes pat e $ internaliseExp desc body
internaliseAppExp _ _ (E.LetFun ofname _ _ _) =
  error $ "Unexpected LetFun " ++ prettyString ofname
internaliseAppExp desc _ (E.Loop sparams mergepat mergeexp form loopbody loc) = do
  ses <- internaliseExp "loop_init" mergeexp
  ((loopbody', (form', shapepat, mergepat', mergeinit')), initstms) <-
    collectStms $ handleForm ses form

  addStms initstms
  mergeinit_ts' <- mapM subExpType mergeinit'

  ctxinit <- argShapes (map I.paramName shapepat) mergepat' mergeinit_ts'

  -- Ensure that the initial loop values match the shapes of the loop
  -- parameters.  XXX: Ideally they should already match (by the
  -- source language type rules), but some of our transformations
  -- (esp. defunctionalisation) strips out some size information.  For
  -- a type-correct source program, these reshapes should simplify
  -- away.
  let args = ctxinit ++ mergeinit'
  args' <-
    ensureArgShapes
      "initial loop values have right shape"
      loc
      (map I.paramName shapepat)
      (map paramType $ shapepat ++ mergepat')
      args

  let dropCond = case form of
        E.While {} -> drop 1
        _ -> id

  -- As above, ensure that the result has the right shape.
  let merge = zip (shapepat ++ mergepat') args'
      merge_ts = map (I.paramType . fst) merge
  loopbody'' <-
    localScope (scopeOfFParams (map fst merge) <> scopeOfLoopForm form') . buildBody_ $
      fmap subExpsRes
        . ensureArgShapes
          "shape of loop result does not match shapes in loop parameter"
          loc
          (map (I.paramName . fst) merge)
          merge_ts
        . map resSubExp
        =<< bodyBind loopbody'

  attrs <- asks envAttrs
  map I.Var . dropCond
    <$> attributing
      attrs
      (letValExp desc (I.Loop merge form' loopbody''))
  where
    sparams' = map (`TypeParamDim` mempty) sparams

    forLoop mergepat' shapepat mergeinit i loopvars form' =
      bodyFromStms . localScope (scopeOfLoopForm form') $ do
        forM_ loopvars $ \(p, arr) ->
          letBindNames [I.paramName p] =<< eIndex arr [eSubExp (I.Var i)]
        ses <- internaliseExp "loopres" loopbody
        sets <- mapM subExpType ses
        shapeargs <- argShapes (map I.paramName shapepat) mergepat' sets
        pure
          ( subExpsRes $ shapeargs ++ ses,
            ( form',
              shapepat,
              mergepat',
              mergeinit
            )
          )

    handleForm mergeinit (E.ForIn x arr) = do
      arr' <- internaliseExpToVars "for_in_arr" arr
      arr_ts <- mapM lookupType arr'
      let w = arraysSize 0 arr_ts

      i <- newVName "i"

      ts <- mapM subExpType mergeinit
      bindingLoopParams sparams' mergepat ts $ \shapepat mergepat' ->
        bindingLambdaParams [toParam E.Observe <$> x] (map rowType arr_ts) $ \x_params -> do
          let loopvars = zip x_params arr'
          forLoop mergepat' shapepat mergeinit i loopvars $ I.ForLoop i Int64 w
    handleForm mergeinit (E.For i num_iterations) = do
      num_iterations' <- internaliseExp1 "upper_bound" num_iterations
      num_iterations_t <- I.subExpType num_iterations'
      it <- case num_iterations_t of
        I.Prim (IntType it) -> pure it
        _ -> error "internaliseExp Loop: invalid type"

      ts <- mapM subExpType mergeinit
      bindingLoopParams sparams' mergepat ts $ \shapepat mergepat' ->
        forLoop mergepat' shapepat mergeinit (E.identName i) [] $
          I.ForLoop (E.identName i) it num_iterations'
    handleForm mergeinit (E.While cond) = do
      ts <- mapM subExpType mergeinit
      bindingLoopParams sparams' mergepat ts $ \shapepat mergepat' -> do
        mergeinit_ts <- mapM subExpType mergeinit
        -- We need to insert 'cond' twice - once for the initial
        -- condition (do we enter the loop at all?), and once with the
        -- result values of the loop (do we continue into the next
        -- iteration?).  This is safe, as the type rules for the
        -- external language guarantees that 'cond' does not consume
        -- anything.
        shapeinit <- argShapes (map I.paramName shapepat) mergepat' mergeinit_ts

        (loop_initial_cond, init_loop_cond_stms) <- collectStms $ do
          forM_ (zip shapepat shapeinit) $ \(p, se) ->
            letBindNames [I.paramName p] $ BasicOp $ SubExp se
          forM_ (zip mergepat' mergeinit) $ \(p, se) ->
            unless (se == I.Var (I.paramName p)) $
              letBindNames [I.paramName p] $
                BasicOp $
                  case se of
                    I.Var v
                      | not $ primType $ paramType p ->
                          Reshape I.ReshapeCoerce (I.arrayShape $ paramType p) v
                    _ -> SubExp se

          -- As the condition expression is inserted twice, we have to
          -- avoid shadowing (#1935).
          (cond_stms, cond') <-
            uncurry (flip renameStmsWith)
              =<< collectStms (internaliseExp1 "loop_cond" cond)
          addStms cond_stms
          pure cond'

        addStms init_loop_cond_stms

        bodyFromStms $ do
          ses <- internaliseExp "loopres" loopbody
          sets <- mapM subExpType ses
          loop_while <- newParam "loop_while" $ I.Prim I.Bool
          shapeargs <- argShapes (map I.paramName shapepat) mergepat' sets

          -- Careful not to clobber anything.
          loop_end_cond_body <- renameBody <=< buildBody_ $ do
            forM_ (zip shapepat shapeargs) $ \(p, se) ->
              unless (se == I.Var (I.paramName p)) $
                letBindNames [I.paramName p] $
                  BasicOp $
                    SubExp se
            forM_ (zip mergepat' ses) $ \(p, se) ->
              unless (se == I.Var (I.paramName p)) $
                letBindNames [I.paramName p] $
                  BasicOp $
                    case se of
                      I.Var v
                        | not $ primType $ paramType p ->
                            Reshape I.ReshapeCoerce (I.arrayShape $ paramType p) v
                      _ -> SubExp se
            subExpsRes <$> internaliseExp "loop_cond" cond
          loop_end_cond <- bodyBind loop_end_cond_body

          pure
            ( subExpsRes shapeargs ++ loop_end_cond ++ subExpsRes ses,
              ( I.WhileLoop $ I.paramName loop_while,
                shapepat,
                loop_while : mergepat',
                loop_initial_cond : mergeinit
              )
            )
internaliseAppExp desc _ (E.LetWith name src idxs ve body loc) = do
  let pat = E.Id (E.identName name) (E.identType name) loc
      src_t = E.identType src
      e = E.Update (E.Var (E.qualName $ E.identName src) src_t loc) idxs ve loc
  internaliseExp desc $
    E.AppExp
      (E.LetPat [] pat e body loc)
      (Info (AppRes (E.typeOf body) mempty))
internaliseAppExp desc _ (E.Match e orig_cs _) = do
  ses <- internaliseExp (desc ++ "_scrutinee") e
  cs <- mapM (onCase ses) orig_cs
  case NE.uncons cs of
    (I.Case _ body, Nothing) ->
      fmap (map resSubExp) $ bodyBind =<< body
    _ -> do
      letValExp' desc =<< eMatch ses (NE.init cs) (I.caseBody $ NE.last cs)
  where
    onCase ses (E.CasePat p case_e _) = do
      (cmps, pertinent) <- generateCond p ses
      pure . I.Case cmps $
        internalisePat' [] p pertinent $
          internaliseBody "case" case_e
internaliseAppExp desc _ (E.If ce te fe _) =
  letValExp' desc
    =<< eIf
      (BasicOp . SubExp <$> internaliseExp1 "cond" ce)
      (internaliseBody (desc <> "_t") te)
      (internaliseBody (desc <> "_f") fe)
internaliseAppExp _ _ e@E.BinOp {} =
  error $ "internaliseAppExp: Unexpected BinOp " ++ prettyString e

internaliseExp :: String -> E.Exp -> InternaliseM [I.SubExp]
internaliseExp desc (E.Parens e _) =
  internaliseExp desc e
internaliseExp desc (E.Hole (Info t) loc) = do
  let msg = docText $ "Reached hole of type: " <> align (pretty t)
      ts = foldMap toList $ internaliseType (E.toStruct t)
  c <- assert "hole_c" (constant False) (errorMsg [ErrorString msg]) loc
  case mapM hasStaticShape ts of
    Nothing ->
      error $ "Hole at " <> locStr loc <> " has existential type:\n" <> show ts
    Just ts' ->
      -- Make sure we always generate a binding, even for primitives.
      certifying c $ mapM (fmap I.Var . letExp desc <=< eBlank . I.fromDecl) ts'
internaliseExp desc (E.QualParens _ e _) =
  internaliseExp desc e
internaliseExp desc (E.StringLit vs _) =
  fmap pure . letSubExp desc $
    I.BasicOp $
      I.ArrayLit (map constant vs) $
        I.Prim int8
internaliseExp _ (E.Var (E.QualName _ name) _ _) = do
  subst <- lookupSubst name
  case subst of
    Just substs -> pure substs
    Nothing -> pure [I.Var name]
internaliseExp desc (E.AppExp e (Info appres)) = do
  ses <- internaliseAppExp desc appres e
  bindExtSizes appres ses
  pure ses
internaliseExp _ (E.TupLit [] _) =
  pure [constant UnitValue]
internaliseExp _ (E.RecordLit [] _) =
  pure [constant UnitValue]
internaliseExp desc (E.TupLit es _) = concat <$> mapM (internaliseExp desc) es
internaliseExp desc (E.RecordLit orig_fields _) =
  concatMap snd . sortFields . M.unions <$> mapM internaliseField orig_fields
  where
    internaliseField (E.RecordFieldExplicit name e _) =
      M.singleton name <$> internaliseExp desc e
    internaliseField (E.RecordFieldImplicit name t loc) =
      internaliseField $
        E.RecordFieldExplicit
          (baseName name)
          (E.Var (E.qualName name) t loc)
          loc
internaliseExp desc (E.ArrayLit es (Info arr_t) loc)
  -- If this is a multidimensional array literal of primitives, we
  -- treat it specially by flattening it out followed by a reshape.
  -- This cuts down on the amount of statements that are produced, and
  -- thus allows us to efficiently handle huge array literals - a
  -- corner case, but an important one.
  | Just ((eshape, e') : es') <- mapM isArrayLiteral es,
    not $ null eshape,
    all ((eshape ==) . fst) es',
    Just basetype <- E.peelArray (length eshape) arr_t = do
      let flat_lit = E.ArrayLit (e' ++ concatMap snd es') (Info basetype) loc
          new_shape = length es : eshape
      flat_arrs <- internaliseExpToVars "flat_literal" flat_lit
      forM flat_arrs $ \flat_arr -> do
        flat_arr_t <- lookupType flat_arr
        let new_shape' =
              reshapeOuter
                (I.Shape $ map (intConst Int64 . toInteger) new_shape)
                1
                $ I.arrayShape flat_arr_t
        letSubExp desc $ I.BasicOp $ I.Reshape I.ReshapeArbitrary new_shape' flat_arr
  | otherwise = do
      es' <- mapM (internaliseExp "arr_elem") es
      let arr_t_ext = foldMap toList $ internaliseType $ E.toStruct arr_t

      rowtypes <-
        case mapM (fmap rowType . hasStaticShape . I.fromDecl) arr_t_ext of
          Just ts -> pure ts
          Nothing ->
            -- XXX: the monomorphiser may create single-element array
            -- literals with an unknown row type.  In those cases we
            -- need to look at the types of the actual elements.
            -- Fixing this in the monomorphiser is a lot more tricky
            -- than just working around it here.
            case es' of
              [] -> error $ "internaliseExp ArrayLit: existential type: " ++ prettyString arr_t
              e' : _ -> mapM subExpType e'

      let arraylit ks rt = do
            ks' <-
              mapM
                ( ensureShape
                    "shape of element differs from shape of first element"
                    loc
                    rt
                    "elem_reshaped"
                )
                ks
            pure $ I.BasicOp $ I.ArrayLit ks' rt

      mapM (letSubExp desc)
        =<< if null es'
          then mapM (arraylit []) rowtypes
          else zipWithM arraylit (transpose es') rowtypes
  where
    isArrayLiteral :: E.Exp -> Maybe ([Int], [E.Exp])
    isArrayLiteral (E.ArrayLit inner_es _ _) = do
      (eshape, e) : inner_es' <- mapM isArrayLiteral inner_es
      guard $ all ((eshape ==) . fst) inner_es'
      pure (length inner_es : eshape, e ++ concatMap snd inner_es')
    isArrayLiteral e =
      Just ([], [e])
internaliseExp desc (E.Ascript e _ _) =
  internaliseExp desc e
internaliseExp desc (E.Coerce e _ (Info et) loc) = do
  ses <- internaliseExp desc e
  ts <- internaliseCoerceType (E.toStruct et) <$> mapM subExpType ses
  dt' <- typeExpForError $ toStruct et
  forM (zip ses ts) $ \(e', t') -> do
    dims <- arrayDims <$> subExpType e'
    let parts =
          ["Value of (core language) shape ("]
            ++ intersperse ", " (map (ErrorVal int64) dims)
            ++ [") cannot match shape of type `"]
            ++ dt'
            ++ ["`."]
    ensureExtShape (errorMsg parts) loc (I.fromDecl t') desc e'
internaliseExp desc (E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of
    I.Prim (I.IntType t) ->
      letTupExp' desc $ I.BasicOp $ I.BinOp (I.Sub t I.OverflowWrap) (I.intConst t 0) e'
    I.Prim (I.FloatType t) ->
      letTupExp' desc $ I.BasicOp $ I.BinOp (I.FSub t) (I.floatConst t 0) e'
    _ -> error "Futhark.Internalise.internaliseExp: non-numeric type in Negate"
internaliseExp desc (E.Not e _) = do
  e' <- internaliseExp1 "not_arg" e
  et <- subExpType e'
  case et of
    I.Prim (I.IntType t) ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.Complement t) e'
    I.Prim I.Bool ->
      letTupExp' desc $ I.BasicOp $ I.UnOp I.Not e'
    _ ->
      error "Futhark.Internalise.internaliseExp: non-int/bool type in Not"
internaliseExp desc (E.Update src slice ve loc) = do
  ves <- internaliseExp "lw_val" ve
  srcs <- internaliseExpToVars "src" src
  dims <- case srcs of
    [] -> pure [] -- Will this happen?
    v : _ -> I.arrayDims <$> lookupType v
  (idxs', cs) <- internaliseSlice loc dims slice

  let comb sname ve' = do
        sname_t <- lookupType sname
        let full_slice = fullSlice sname_t idxs'
            rowtype = sname_t `setArrayDims` sliceDims full_slice
        ve'' <-
          ensureShape
            "shape of value does not match shape of source array"
            loc
            rowtype
            "lw_val_correct_shape"
            ve'
        letInPlace desc sname full_slice $ BasicOp $ SubExp ve''
  certifying cs $ map I.Var <$> zipWithM comb srcs ves
internaliseExp desc (E.RecordUpdate src fields ve _ _) = do
  src' <- internaliseExp desc src
  ve' <- internaliseExp desc ve
  replace (E.typeOf src) fields ve' src'
  where
    replace (E.Scalar (E.Record m)) (f : fs) ve' src'
      | Just t <- M.lookup f m = do
          let i =
                sum . map (internalisedTypeSize . snd) $
                  takeWhile ((/= f) . fst) . sortFields $
                    m
              k = internalisedTypeSize t
              (bef, to_update, aft) = splitAt3 i k src'
          src'' <- replace t fs ve' to_update
          pure $ bef ++ src'' ++ aft
    replace _ _ ve' _ = pure ve'
internaliseExp desc (E.Attr attr e loc) = do
  attr' <- internaliseAttr attr
  e' <- local (f attr') $ internaliseExp desc e
  case attr' of
    "trace" ->
      traceRes (T.pack $ locStr loc) e'
    I.AttrComp "trace" [I.AttrName tag] ->
      traceRes (nameToText tag) e'
    "opaque" ->
      mapM (letSubExp desc . BasicOp . Opaque OpaqueNil) e'
    _ ->
      pure e'
  where
    traceRes tag' =
      mapM (letSubExp desc . BasicOp . Opaque (OpaqueTrace tag'))
    f attr' env
      | attr' == "unsafe",
        not $ envSafe env =
          env {envDoBoundsChecks = False}
      | otherwise =
          env {envAttrs = envAttrs env <> oneAttr attr'}
internaliseExp desc (E.Assert e1 e2 (Info check) loc) = do
  e1' <- internaliseExp1 "assert_cond" e1
  c <- assert "assert_c" e1' (errorMsg [ErrorString $ "Assertion is false: " <> check]) loc
  -- Make sure there are some bindings to certify.
  certifying c $ mapM rebind =<< internaliseExp desc e2
  where
    rebind v = do
      v' <- newVName "assert_res"
      letBindNames [v'] $ I.BasicOp $ I.SubExp v
      pure $ I.Var v'
internaliseExp _ (E.Constr c es (Info (E.Scalar (E.Sum fs))) _) = do
  (ts, constr_map) <- internaliseSumType $ M.map (map E.toStruct) fs
  es' <- concat <$> mapM (internaliseExp "payload") es

  let noExt _ = pure $ intConst Int64 0
  ts' <- instantiateShapes noExt $ map fromDecl ts

  case lookupWithIndex c constr_map of
    Just (i, js) ->
      (intConst Int8 (toInteger i) :) <$> clauses 0 ts' (zip js es')
    Nothing ->
      error "internaliseExp Constr: missing constructor"
  where
    clauses j (t : ts) js_to_es
      | Just e <- j `lookup` js_to_es =
          (e :) <$> clauses (j + 1) ts js_to_es
      | otherwise = do
          blank <-
            -- Cannot use eBlank here for arrays, because when doing
            -- equality comparisons on sum types, we end up looking at
            -- the array elements. (#2081) This is a bit of an edge
            -- case, but arrays in sum types are known to be
            -- inefficient.
            letSubExp "zero"
              =<< case t of
                I.Array {} ->
                  pure $ BasicOp $ Replicate (I.arrayShape t) $ I.Constant $ blankPrimValue $ elemType t
                _ -> eBlank t
          (blank :) <$> clauses (j + 1) ts js_to_es
    clauses _ [] _ =
      pure []
internaliseExp _ (E.Constr _ _ (Info t) loc) =
  error $ "internaliseExp: constructor with type " ++ prettyString t ++ " at " ++ locStr loc
-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp _ (E.Literal v _) =
  pure [I.Constant $ internalisePrimValue v]
internaliseExp _ (E.IntLit v (Info t) _) =
  case t of
    E.Scalar (E.Prim (E.Signed it)) ->
      pure [I.Constant $ I.IntValue $ intValue it v]
    E.Scalar (E.Prim (E.Unsigned it)) ->
      pure [I.Constant $ I.IntValue $ intValue it v]
    E.Scalar (E.Prim (E.FloatType ft)) ->
      pure [I.Constant $ I.FloatValue $ floatValue ft v]
    _ -> error $ "internaliseExp: nonsensical type for integer literal: " ++ prettyString t
internaliseExp _ (E.FloatLit v (Info t) _) =
  case t of
    E.Scalar (E.Prim (E.FloatType ft)) ->
      pure [I.Constant $ I.FloatValue $ floatValue ft v]
    _ -> error $ "internaliseExp: nonsensical type for float literal: " ++ prettyString t
-- Builtin operators are handled specially because they are
-- overloaded.
internaliseExp desc (E.Project k e (Info rt) _) = do
  let i' = sum . map internalisedTypeSize $
        case E.typeOf e of
          E.Scalar (Record fs) ->
            map snd $ takeWhile ((/= k) . fst) $ sortFields fs
          t -> [t]
  take (internalisedTypeSize rt) . drop i' <$> internaliseExp desc e
internaliseExp _ e@E.Lambda {} =
  error $ "internaliseExp: Unexpected lambda at " ++ locStr (srclocOf e)
internaliseExp _ e@E.OpSection {} =
  error $ "internaliseExp: Unexpected operator section at " ++ locStr (srclocOf e)
internaliseExp _ e@E.OpSectionLeft {} =
  error $ "internaliseExp: Unexpected left operator section at " ++ locStr (srclocOf e)
internaliseExp _ e@E.OpSectionRight {} =
  error $ "internaliseExp: Unexpected right operator section at " ++ locStr (srclocOf e)
internaliseExp _ e@E.ProjectSection {} =
  error $ "internaliseExp: Unexpected projection section at " ++ locStr (srclocOf e)
internaliseExp _ e@E.IndexSection {} =
  error $ "internaliseExp: Unexpected index section at " ++ locStr (srclocOf e)

internaliseArg :: String -> (E.Exp, Maybe VName) -> InternaliseM [SubExp]
internaliseArg desc (arg, argdim) = do
  exists <- askScope
  case argdim of
    Just d | d `M.member` exists -> pure [I.Var d]
    _ -> do
      arg' <- internaliseExp desc arg
      case (arg', argdim) of
        ([se], Just d) -> do
          letBindNames [d] $ BasicOp $ SubExp se
        _ -> pure ()
      pure arg'

internalisePatLit :: E.PatLit -> E.StructType -> I.PrimValue
internalisePatLit (E.PatLitPrim v) _ =
  internalisePrimValue v
internalisePatLit (E.PatLitInt x) (E.Scalar (E.Prim (E.Signed it))) =
  I.IntValue $ intValue it x
internalisePatLit (E.PatLitInt x) (E.Scalar (E.Prim (E.Unsigned it))) =
  I.IntValue $ intValue it x
internalisePatLit (E.PatLitFloat x) (E.Scalar (E.Prim (E.FloatType ft))) =
  I.FloatValue $ floatValue ft x
internalisePatLit l t =
  error $ "Nonsensical pattern and type: " ++ show (l, t)

-- | Internalization of 'AutoMap'-annotated applications.
--
-- Each application @f x@ has an annotation with @AutoMap R M F@ where
-- @R, M, F@ are the autorep, automap, and frame shapes,
-- respectively.
--
-- The application @f x@ will have type @F t@ for some @t@, i.e. @(f
-- x) : F t@. The frame @F@ is a prefix of the type of @f x@; namely
-- it is the total accumulated shape that is due to implicit maps.
-- Another way of thinking about that is that @|F|@ is is the level
-- of the automap-nest that @f x@ is in. For example, if @|F| = 2@
-- then we know that @f x@ implicitly stands for
--
-- > map (\x' -> map (\x'' -> f x'') x') x
--
-- For an application with a non-empty autorep annotation, the frame
-- tells about how many dimensions of the replicate can be eliminated.
-- For example, @[[1,2],[3,4]] + 5@ will yield the following annotations:
--
-- > ([[1,2],[3,4]] +)     -- AutoMap {R = mempty, M = [2][2], F = [2][2]}
-- > (([[1,2],[3,4]] +) 5) -- AutoMap {R = [2][2], M = mempty, F = [2][2]}
--
-- All replicated arguments are pushed down the auto-map nest. Each
-- time a replicated argument is pushed down a level of an
-- automap-nest, one fewer replicates is needed (i.e., the outermost
-- dimension of @R@ can be dropped). Replicated arguments are pushed
-- down the nest until either 1) the bottom of the nest is encountered
-- or 2) no replicate dimensions remain. For example, in the second
-- application above @R@ = @F@, so we can push the replicated argument
-- down two levels. Since each level effectively removes a dimension
-- of the replicate, no replicates will be required:
--
-- > map (\xs -> map (\x -> f x'' 5) xs) [[1,2],[3,4]]
--
-- The number of replicates that are actually required is given by
-- max(|R| - |F|, 0).
--
-- An expression's "true level" is the level at which that expression
-- will appear in the automap-nest. The bottom of a mapnest is level 0.
--
-- * For annotations with @R = mempty@, the true level is @|F|@.
-- * For annotations with @M = mempty@, the true level is @|F| - |R|@.
--
-- If @|R| > |F|@ then actual replicates (namely @|R| - |F|@ of them)
-- will be required at the bottom of the mapnest.
--
-- Note that replicates can only appear at the bottom of a mapnest; any
-- expression of the form
--
-- > map (\ls x' rs -> e) (replicate x)
--
-- can always be written as
--
-- > map (\ls rs -> e[x' -> x])
--
-- Let's look at another example. Consider (with exact sizes omitted for brevity)
--
-- > f    : a -> a -> a -> []a -> [][][]a -> a
-- > xss  : [][]a
-- > ys   : []a
-- > zsss : [][][]a
-- > w    : a
-- > vss  : [][]a
--
-- and the application
--
-- > f xss ys zsss w vss
--
-- which will have the following annotations
--
-- > (f xss)                          -- AutoMap {R = mempty,    M = [][],   F = [][]}    (1)
-- > ((f xss) ys)                     -- AutoMap {R = [],        M = mempty, F = [][]}    (2)
-- > (((f xss) ys) zsss)              -- AutoMap {R = mempty,    M = [],     F = [][][]}  (3)
-- > ((((f xss) ys) zsss) w)          -- AutoMap {R = [][][][],  M = mempty, F = [][][]}  (4)
-- > (((((f xss) ys) zsss) w) vss)    -- AutoMap {R = [],        M = mempty, F = [][][]}  (5)
--
-- This will yield the following mapnest.
--
-- >   map (\zss ->
-- >    map (\xs zs vs ->
-- >      map (\x y z v -> f x y z (replicate w) v) xs ys zs v) xss zss vss) zsss
--
-- Let's see how we'd construct this mapnest from the annotations. We construct
-- the nest bottom-up. We have:
--
-- Application | True level
-- ---------------------------
-- (1)         | |[][]|                = 2
-- (2)         | |[][]| - |[]|         = 1
-- (3)         | |[][][]|              = 3
-- (4)         | |[][][]| - |[][][][]| = -1
-- (5)         | |[][][]| - |[]|       = 2
--
-- We start at level 0.
-- * Any argument with a negative true level of @-n@ will be replicated @n@ times;
--   the exact shapes can be found by removing the @F@ postfix from @R@,
--   i.e. @R = shapes_to_rep_by <> F@.
-- * Any argument with a 0 true level will be included.
-- * For any argument @arg@ with a positive true level, we construct a new parameter
--   whose type is @arg@ with the leading @n@ dimensions (where @n@ is the true level)
--   removed.
--
-- Following the rules above, @w@ will be replicated once. For the remaining arguments,
-- we create new parameters @x : a, y : a, z : a , v : a@. Hence, level 0 becomes
--
-- > f x y z (replicate w) v
--
-- At level l > 0:
-- * There are no replicates.
-- * Any argument with l true level will be included verbatim.
-- * Any argument with true level > l will have a new parameter constructed for it,
--   whose type has the leading @n - l@ dimensions (where @n@ is the true level) removed.
-- * We surround the previous level with a map that binds that levels' new parameters
--   and is passed the current levels' arguments.
--
-- Following the above recipe for level 1, we create parameters
-- @xs : []a, zs : []a, vs :[]a@ and obtain
--
-- > map (\x y z v -> f x y z (replicate w) v) xs ys zs vs
--
-- This process continues until the level is greater than the maximum
-- true level of any application, at which we terminate.
type Level = Int

data AutoMapArg = AutoMapArg
  { amArgs :: [VName]
  }
  deriving (Show)

data AutoMapParam = AutoMapParam
  { amParams :: [LParam SOACS],
    amMapDim :: SubExp
  }
  deriving (Show)

withAutoMap ::
  [((E.Exp, Maybe VName), AutoMap, String)] ->
  ([[SubExp]] -> InternaliseM [SubExp]) ->
  InternaliseM [SubExp]
withAutoMap args_am func = do
  (param_maps, arg_maps) <-
    unzip . reverse
      <$> mapM buildArgMap (reverse args_am)
  let param_map = M.unionsWith (<>) $ (fmap . fmap) pure param_maps
      arg_map = M.unionsWith (<>) $ (fmap . fmap) pure arg_maps
  buildMapNest param_map arg_map $ maximum $ M.keys arg_map
  where
    buildMapNest _ arg_map 0 =
      func $ map (map I.Var . amArgs) $ arg_map M.! 0
    buildMapNest param_map arg_map l =
      case map amMapDim $ param_map M.! l of
        [] -> buildMapNest param_map arg_map (l - 1)
        (map_dim : _) -> do
          let params = map amParams $ param_map M.! l
              args = map amArgs $ arg_map M.! l

          reshaped_args <-
            forM (concat args) $ \argvn ->
              letExp "reshaped" $
                shapeCoerce [map_dim] argvn

          letValExp'
            "automap"
            . Op
            . Screma map_dim reshaped_args
            . mapSOAC
            =<< mkLambda
              (concat params)
              ( subExpsRes <$> buildMapNest param_map arg_map (l - 1)
              )

    buildArgMap ::
      ((E.Exp, Maybe VName), AutoMap, String) ->
      InternaliseM (M.Map Level AutoMapParam, M.Map Level AutoMapArg)
    buildArgMap (arg, am, arg_desc) = do
      ses <- internaliseArg arg_desc arg
      arg_vnames <- mapM (letExp "" <=< eSubExp) ses
      ts <- mapM subExpType ses
      foldM (mkArgsAndParams arg_vnames ses ts) (mempty, mempty) $
        reverse [0 .. trueLevel am]
      where
        mkArgsAndParams arg_vnames ses ts (p_map, a_map) l
          | l == 0 = do
              let as =
                    maybe
                      arg_vnames
                      ( map I.paramName
                          . amParams
                      )
                      (p_map M.!? 1)
              ses <- mkBottomArgs as ts
              pure (p_map, M.insert 0 (AutoMapArg ses) a_map)
          | l == trueLevel am = do
              ps <- mkParams arg_vnames ts l
              d <- outerDim am l
              pure
                ( M.insert l (AutoMapParam ps d) p_map,
                  M.insert l (AutoMapArg arg_vnames) a_map
                )
          | l < trueLevel am && l > 0 = do
              ps <- mkParams arg_vnames ts l
              d <- outerDim am l
              let as =
                    map I.paramName $
                      amParams $
                        p_map M.! (l + 1)
              pure
                ( M.insert l (AutoMapParam ps d) p_map,
                  M.insert l (AutoMapArg as) a_map
                )
          | otherwise = error ""

        mkParams _ ts level =
          forM ts $ \t ->
            newParam ("p_" <> arg_desc) $ argType (level - 1) am t
        mkBottomArgs arg_vnames ts = do
          rep_shape <- internaliseShape $ autoRep am `E.shapePrefix` autoFrame am
          if I.shapeRank rep_shape > 0
            then
              concat
                <$> mapM
                  ( letValExp "autorep"
                      . BasicOp
                      . Replicate rep_shape
                      . I.Var
                  )
                  arg_vnames
            else pure arg_vnames

    internaliseShape :: E.Shape Size -> InternaliseM I.Shape
    internaliseShape =
      fmap I.Shape . mapM (internaliseExp1 "") . E.shapeDims

    trueLevel :: AutoMap -> Int
    trueLevel am
      | autoMap am == mempty = max 0 $ E.shapeRank (autoFrame am) - E.shapeRank (autoRep am)
      | otherwise = E.shapeRank $ autoFrame am

    outerDim :: AutoMap -> Int -> InternaliseM SubExp
    outerDim am level =
      internaliseExp1 "" $ (!! (trueLevel am - level)) $ E.shapeDims $ autoFrame am

    argType level am = I.stripArray (trueLevel am - level)

generateCond ::
  E.Pat StructType ->
  [I.SubExp] ->
  InternaliseM ([Maybe I.PrimValue], [I.SubExp])
generateCond orig_p orig_ses = do
  (cmps, pertinent, _) <- compares orig_p orig_ses
  pure (cmps, pertinent)
  where
    compares (E.PatLit l (Info t) _) (se : ses) =
      pure ([Just $ internalisePatLit l t], [se], ses)
    compares (E.PatConstr c (Info (E.Scalar (E.Sum fs))) pats _) (_ : ses) = do
      (payload_ts, m) <- internaliseSumType $ M.map (map toStruct) fs
      case lookupWithIndex c m of
        Just (tag, payload_is) -> do
          let (payload_ses, ses') = splitAt (length payload_ts) ses
          (cmps, pertinent, _) <-
            comparesMany pats $ map (payload_ses !!) payload_is
          let missingCmps i _ =
                case i `elemIndex` payload_is of
                  Just j -> cmps !! j
                  Nothing -> Nothing
          pure
            ( Just (I.IntValue $ intValue Int8 $ toInteger tag)
                : zipWith missingCmps [0 ..] payload_ses,
              pertinent,
              ses'
            )
        Nothing ->
          error "generateCond: missing constructor"
    compares (E.PatConstr _ (Info t) _ _) _ =
      error $ "generateCond: PatConstr has nonsensical type: " ++ prettyString t
    compares (E.Id _ t loc) ses =
      compares (E.Wildcard t loc) ses
    compares (E.Wildcard (Info t) _) ses = do
      let (id_ses, rest_ses) = splitAt (internalisedTypeSize $ E.toStruct t) ses
      pure (map (const Nothing) id_ses, id_ses, rest_ses)
    compares (E.PatParens pat _) ses =
      compares pat ses
    compares (E.PatAttr _ pat _) ses =
      compares pat ses
    compares (E.TuplePat [] loc) ses =
      compares (E.Wildcard (Info $ E.Scalar $ E.Record mempty) loc) ses
    compares (E.RecordPat [] loc) ses =
      compares (E.Wildcard (Info $ E.Scalar $ E.Record mempty) loc) ses
    compares (E.TuplePat pats _) ses =
      comparesMany pats ses
    compares (E.RecordPat fs _) ses =
      comparesMany (map snd $ E.sortFields $ M.fromList fs) ses
    compares (E.PatAscription pat _ _) ses =
      compares pat ses
    compares pat [] =
      error $ "generateCond: No values left for pattern " ++ prettyString pat

    comparesMany [] ses = pure ([], [], ses)
    comparesMany (pat : pats) ses = do
      (cmps1, pertinent1, ses') <- compares pat ses
      (cmps2, pertinent2, ses'') <- comparesMany pats ses'
      pure
        ( cmps1 <> cmps2,
          pertinent1 <> pertinent2,
          ses''
        )

internalisePat ::
  String ->
  [E.SizeBinder VName] ->
  E.Pat StructType ->
  E.Exp ->
  InternaliseM a ->
  InternaliseM a
internalisePat desc sizes p e m = do
  ses <- internaliseExp desc' e
  internalisePat' sizes p ses m
  where
    desc' = case E.patIdents p of
      [v] -> baseString $ E.identName v
      _ -> desc

internalisePat' ::
  [E.SizeBinder VName] ->
  E.Pat StructType ->
  [I.SubExp] ->
  InternaliseM a ->
  InternaliseM a
internalisePat' sizes p ses m = do
  ses_ts <- mapM subExpType ses
  stmPat (toParam E.Observe <$> p) ses_ts $ \pat_names -> do
    bindExtSizes (AppRes (E.patternType p) (map E.sizeName sizes)) ses
    forM_ (zip pat_names ses) $ \(v, se) ->
      letBindNames [v] $ I.BasicOp $ I.SubExp se
    m

internaliseSlice ::
  SrcLoc ->
  [SubExp] ->
  [E.DimIndex] ->
  InternaliseM ([I.DimIndex SubExp], Certs)
internaliseSlice loc dims idxs = do
  (idxs', oks, parts) <- unzip3 <$> zipWithM internaliseDimIndex dims idxs
  ok <- letSubExp "index_ok" =<< eAll oks
  let msg =
        errorMsg $
          ["Index ["]
            ++ intercalate [", "] parts
            ++ ["] out of bounds for array of shape ["]
            ++ intersperse "][" (map (ErrorVal int64) $ take (length idxs) dims)
            ++ ["]."]
  c <- assert "index_certs" ok msg loc
  pure (idxs', c)

internaliseDimIndex ::
  SubExp ->
  E.DimIndex ->
  InternaliseM (I.DimIndex SubExp, SubExp, [ErrorMsgPart SubExp])
internaliseDimIndex w (E.DimFix i) = do
  (i', _) <- internaliseSizeExp "i" i
  let lowerBound =
        I.BasicOp $ I.CmpOp (I.CmpSle I.Int64) (I.constant (0 :: I.Int64)) i'
      upperBound =
        I.BasicOp $ I.CmpOp (I.CmpSlt I.Int64) i' w
  ok <- letSubExp "bounds_check" =<< eBinOp I.LogAnd (pure lowerBound) (pure upperBound)
  pure (I.DimFix i', ok, [ErrorVal int64 i'])

-- Special-case an important common case that otherwise leads to horrible code.
internaliseDimIndex
  w
  ( E.DimSlice
      Nothing
      Nothing
      (Just (E.Negate (E.IntLit 1 _ _) _))
    ) = do
    w_minus_1 <-
      letSubExp "w_minus_1" $
        BasicOp $
          I.BinOp (Sub Int64 I.OverflowWrap) w one
    pure
      ( I.DimSlice w_minus_1 w $ intConst Int64 (-1),
        constant True,
        mempty
      )
    where
      one = constant (1 :: Int64)
internaliseDimIndex w (E.DimSlice i j s) = do
  s' <- maybe (pure one) (fmap fst . internaliseSizeExp "s") s
  s_sign <- letSubExp "s_sign" $ BasicOp $ I.UnOp (I.SSignum Int64) s'
  backwards <- letSubExp "backwards" $ I.BasicOp $ I.CmpOp (I.CmpEq int64) s_sign negone
  w_minus_1 <- letSubExp "w_minus_1" $ BasicOp $ I.BinOp (Sub Int64 I.OverflowWrap) w one
  let i_def =
        letSubExp "i_def"
          =<< eIf
            (eSubExp backwards)
            (resultBodyM [w_minus_1])
            (resultBodyM [zero])
      j_def =
        letSubExp "j_def"
          =<< eIf
            (eSubExp backwards)
            (resultBodyM [negone])
            (resultBodyM [w])
  i' <- maybe i_def (fmap fst . internaliseSizeExp "i") i
  j' <- maybe j_def (fmap fst . internaliseSizeExp "j") j
  j_m_i <- letSubExp "j_m_i" $ BasicOp $ I.BinOp (Sub Int64 I.OverflowWrap) j' i'
  -- Something like a division-rounding-up, but accomodating negative
  -- operands.
  let divRounding x y =
        eBinOp
          (SQuot Int64 Safe)
          ( eBinOp
              (Add Int64 I.OverflowWrap)
              x
              (eBinOp (Sub Int64 I.OverflowWrap) y (eSignum y))
          )
          y
  n <- letSubExp "n" =<< divRounding (toExp j_m_i) (toExp s')

  zero_stride <- letSubExp "zero_stride" $ I.BasicOp $ I.CmpOp (CmpEq int64) s_sign zero
  nonzero_stride <- letSubExp "nonzero_stride" $ I.BasicOp $ I.UnOp I.Not zero_stride

  -- Bounds checks depend on whether we are slicing forwards or
  -- backwards.  If forwards, we must check '0 <= i && i <= j'.  If
  -- backwards, '-1 <= j && j <= i'.  In both cases, we check '0 <=
  -- i+n*s && i+(n-1)*s < w'.  We only check if the slice is nonempty.
  empty_slice <- letSubExp "empty_slice" $ I.BasicOp $ I.CmpOp (CmpEq int64) n zero

  m <- letSubExp "m" $ I.BasicOp $ I.BinOp (Sub Int64 I.OverflowWrap) n one
  m_t_s <- letSubExp "m_t_s" $ I.BasicOp $ I.BinOp (Mul Int64 I.OverflowWrap) m s'
  i_p_m_t_s <- letSubExp "i_p_m_t_s" $ I.BasicOp $ I.BinOp (Add Int64 I.OverflowWrap) i' m_t_s
  zero_leq_i_p_m_t_s <-
    letSubExp "zero_leq_i_p_m_t_s" $
      I.BasicOp $
        I.CmpOp (I.CmpSle Int64) zero i_p_m_t_s
  i_p_m_t_s_leq_w <-
    letSubExp "i_p_m_t_s_leq_w" $
      I.BasicOp $
        I.CmpOp (I.CmpSle Int64) i_p_m_t_s w
  i_p_m_t_s_lth_w <-
    letSubExp "i_p_m_t_s_leq_w" $
      I.BasicOp $
        I.CmpOp (I.CmpSlt Int64) i_p_m_t_s w

  zero_lte_i <- letSubExp "zero_lte_i" $ I.BasicOp $ I.CmpOp (I.CmpSle Int64) zero i'
  i_lte_j <- letSubExp "i_lte_j" $ I.BasicOp $ I.CmpOp (I.CmpSle Int64) i' j'
  forwards_ok <-
    letSubExp "forwards_ok"
      =<< eAll [zero_lte_i, i_lte_j, zero_leq_i_p_m_t_s, i_p_m_t_s_lth_w]

  negone_lte_j <- letSubExp "negone_lte_j" $ I.BasicOp $ I.CmpOp (I.CmpSle Int64) negone j'
  j_lte_i <- letSubExp "j_lte_i" $ I.BasicOp $ I.CmpOp (I.CmpSle Int64) j' i'
  backwards_ok <-
    letSubExp "backwards_ok"
      =<< eAll
        [negone_lte_j, j_lte_i, zero_leq_i_p_m_t_s, i_p_m_t_s_leq_w]

  slice_ok <-
    letSubExp "slice_ok"
      =<< eIf
        (eSubExp backwards)
        (resultBodyM [backwards_ok])
        (resultBodyM [forwards_ok])

  ok_or_empty <-
    letSubExp "ok_or_empty" $
      I.BasicOp $
        I.BinOp I.LogOr empty_slice slice_ok

  acceptable <-
    letSubExp "slice_acceptable" $
      I.BasicOp $
        I.BinOp I.LogAnd nonzero_stride ok_or_empty

  let parts = case (i, j, s) of
        (_, _, Just {}) ->
          [ maybe "" (const $ ErrorVal int64 i') i,
            ":",
            maybe "" (const $ ErrorVal int64 j') j,
            ":",
            ErrorVal int64 s'
          ]
        (_, Just {}, _) ->
          [ maybe "" (const $ ErrorVal int64 i') i,
            ":",
            ErrorVal int64 j'
          ]
            ++ maybe mempty (const [":", ErrorVal int64 s']) s
        (_, Nothing, Nothing) ->
          [ErrorVal int64 i', ":"]
  pure (I.DimSlice i' n s', acceptable, parts)
  where
    zero = constant (0 :: Int64)
    negone = constant (-1 :: Int64)
    one = constant (1 :: Int64)

internaliseScanOrReduce ::
  String ->
  String ->
  (SubExp -> I.Lambda SOACS -> [SubExp] -> [VName] -> InternaliseM (SOAC SOACS)) ->
  (E.Exp, E.Exp, E.Exp, SrcLoc) ->
  InternaliseM [SubExp]
internaliseScanOrReduce desc what f (lam, ne, arr, loc) = do
  arrs <- internaliseExpToVars (what ++ "_arr") arr
  nes <- internaliseExp (what ++ "_ne") ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape
      "Row shape of input array does not match shape of neutral element"
      loc
      rowtype
      (what ++ "_ne_right_shape")
      ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda lam nests arrts
  w <- arraysSize 0 <$> mapM lookupType arrs
  letValExp' desc . I.Op =<< f w lam' nes' arrs

internaliseHist ::
  Int ->
  String ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  SrcLoc ->
  InternaliseM [SubExp]
internaliseHist dim desc rf hist op ne buckets img loc = do
  rf' <- internaliseExp1 "hist_rf" rf
  ne' <- internaliseExp "hist_ne" ne
  hist' <- internaliseExpToVars "hist_hist" hist
  buckets' <- internaliseExpToVars "hist_buckets" buckets
  img' <- internaliseExpToVars "hist_img" img

  -- reshape neutral element to have same size as the destination array
  ne_shp <- forM (zip ne' hist') $ \(n, h) -> do
    rowtype <- I.stripArray 1 <$> lookupType h
    ensureShape
      "Row shape of destination array does not match shape of neutral element"
      loc
      rowtype
      "hist_ne_right_shape"
      n
  ne_ts <- mapM I.subExpType ne_shp
  his_ts <- mapM (fmap (I.stripArray (dim - 1)) . lookupType) hist'
  op' <- internaliseFoldLambda internaliseLambda op ne_ts his_ts

  -- reshape return type of bucket function to have same size as neutral element
  -- (modulo the index)
  bucket_params <- replicateM dim (newParam "bucket_p" $ I.Prim int64)
  img_params <- mapM (newParam "img_p" . rowType) =<< mapM lookupType img'
  let params = bucket_params ++ img_params
      rettype = replicate dim (I.Prim int64) ++ ne_ts
      body = mkBody mempty $ varsRes $ map I.paramName params
  lam' <-
    mkLambda params $
      ensureResultShape
        "Row shape of value array does not match row shape of hist target"
        (srclocOf img)
        rettype
        =<< bodyBind body

  -- get sizes of histogram and image arrays
  shape_hist <- I.Shape . take dim . I.arrayDims <$> lookupType (head hist')
  w_img <- I.arraySize 0 <$> lookupType (head img')

  letValExp' desc . I.Op $
    I.Hist w_img (buckets' ++ img') [HistOp shape_hist rf' hist' ne_shp op'] lam'

internaliseStreamAcc ::
  String ->
  E.Exp ->
  Maybe (E.Exp, E.Exp) ->
  E.Exp ->
  E.Exp ->
  InternaliseM [SubExp]
internaliseStreamAcc desc dest op lam bs = do
  dest' <- internaliseExpToVars "scatter_dest" dest
  bs' <- internaliseExpToVars "scatter_input" bs

  acc_cert_v <- newVName "acc_cert"
  dest_ts <- mapM lookupType dest'
  let dest_w = arraysSize 0 dest_ts
      acc_t = Acc acc_cert_v (I.Shape [dest_w]) (map rowType dest_ts) NoUniqueness
  acc_p <- newParam "acc_p" acc_t
  withacc_lam <- mkLambda [Param mempty acc_cert_v (I.Prim I.Unit), acc_p] $ do
    bs_ts <- mapM lookupType bs'
    lam' <- internaliseLambdaCoerce lam $ map rowType $ paramType acc_p : bs_ts
    let w = arraysSize 0 bs_ts
    fmap subExpsRes . letValExp' "acc_res" $
      I.Op $
        I.Screma w (I.paramName acc_p : bs') (I.mapSOAC lam')

  op' <-
    case op of
      Just (op_lam, ne) -> do
        ne' <- internaliseExp "hist_ne" ne
        ne_ts <- mapM I.subExpType ne'
        (lam_params, lam_body, lam_rettype) <-
          internaliseLambda op_lam $ ne_ts ++ ne_ts
        idxp <- newParam "idx" $ I.Prim int64
        let op_lam' = I.Lambda (idxp : lam_params) lam_rettype lam_body
        pure $ Just (op_lam', ne')
      Nothing ->
        pure Nothing

  destw <- arraysSize 0 <$> mapM lookupType dest'
  fmap (map I.Var) $
    letTupExp desc $
      WithAcc [(I.Shape [destw], dest', op')] withacc_lam

internaliseExp1 :: String -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of
    [se] -> pure se
    _ -> error "Internalise.internaliseExp1: was passed not just a single subexpression"

-- | Promote to dimension type as appropriate for the original type.
-- Also return original type.
internaliseSizeExp :: String -> E.Exp -> InternaliseM (I.SubExp, IntType)
internaliseSizeExp s e = do
  e' <- internaliseExp1 s e
  case E.typeOf e of
    E.Scalar (E.Prim (E.Signed it)) -> (,it) <$> asIntS Int64 e'
    _ -> error "internaliseSizeExp: bad type"

internaliseExpToVars :: String -> E.Exp -> InternaliseM [I.VName]
internaliseExpToVars desc e =
  mapM asIdent =<< internaliseExp desc e
  where
    asIdent (I.Var v) = pure v
    asIdent se = letExp desc $ I.BasicOp $ I.SubExp se

internaliseOperation ::
  String ->
  E.Exp ->
  (I.VName -> InternaliseM I.BasicOp) ->
  InternaliseM [I.SubExp]
internaliseOperation s e op = do
  vs <- internaliseExpToVars s e
  mapM (letSubExp s . I.BasicOp <=< op) vs

certifyingNonzero ::
  SrcLoc ->
  IntType ->
  SubExp ->
  InternaliseM a ->
  InternaliseM a
certifyingNonzero loc t x m = do
  zero <-
    letSubExp "zero" $
      I.BasicOp $
        CmpOp (CmpEq (IntType t)) x (intConst t 0)
  nonzero <- letSubExp "nonzero" $ I.BasicOp $ UnOp I.Not zero
  c <- assert "nonzero_cert" nonzero "division by zero" loc
  certifying c m

certifyingNonnegative ::
  SrcLoc ->
  IntType ->
  SubExp ->
  InternaliseM a ->
  InternaliseM a
certifyingNonnegative loc t x m = do
  nonnegative <-
    letSubExp "nonnegative" . I.BasicOp $
      CmpOp (CmpSle t) (intConst t 0) x
  c <- assert "nonzero_cert" nonnegative "negative exponent" loc
  certifying c m

internaliseBinOp ::
  SrcLoc ->
  String ->
  E.BinOp ->
  I.SubExp ->
  I.SubExp ->
  E.PrimType ->
  E.PrimType ->
  InternaliseM [I.SubExp]
internaliseBinOp _ desc E.LogAnd x y E.Bool _ =
  simpleBinOp desc I.LogAnd x y
internaliseBinOp _ desc E.LogOr x y E.Bool _ =
  simpleBinOp desc I.LogOr x y
internaliseBinOp _ desc E.Plus x y (E.Signed t) _ =
  simpleBinOp desc (I.Add t I.OverflowWrap) x y
internaliseBinOp _ desc E.Plus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Add t I.OverflowWrap) x y
internaliseBinOp _ desc E.Plus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FAdd t) x y
internaliseBinOp _ desc E.Minus x y (E.Signed t) _ =
  simpleBinOp desc (I.Sub t I.OverflowWrap) x y
internaliseBinOp _ desc E.Minus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Sub t I.OverflowWrap) x y
internaliseBinOp _ desc E.Minus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FSub t) x y
internaliseBinOp _ desc E.Times x y (E.Signed t) _ =
  simpleBinOp desc (I.Mul t I.OverflowWrap) x y
internaliseBinOp _ desc E.Times x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Mul t I.OverflowWrap) x y
internaliseBinOp _ desc E.Times x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMul t) x y
internaliseBinOp loc desc E.Divide x y (E.Signed t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.SDiv t I.Unsafe) x y
internaliseBinOp loc desc E.Divide x y (E.Unsigned t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.UDiv t I.Unsafe) x y
internaliseBinOp _ desc E.Divide x y (E.FloatType t) _ =
  simpleBinOp desc (I.FDiv t) x y
internaliseBinOp _ desc E.Pow x y (E.FloatType t) _ =
  simpleBinOp desc (I.FPow t) x y
internaliseBinOp loc desc E.Pow x y (E.Signed t) _ =
  certifyingNonnegative loc t y $
    simpleBinOp desc (I.Pow t) x y
internaliseBinOp _ desc E.Pow x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Pow t) x y
internaliseBinOp loc desc E.Mod x y (E.Signed t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.SMod t I.Unsafe) x y
internaliseBinOp loc desc E.Mod x y (E.Unsigned t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.UMod t I.Unsafe) x y
internaliseBinOp _ desc E.Mod x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMod t) x y
internaliseBinOp loc desc E.Quot x y (E.Signed t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.SQuot t I.Unsafe) x y
internaliseBinOp loc desc E.Quot x y (E.Unsigned t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.UDiv t I.Unsafe) x y
internaliseBinOp loc desc E.Rem x y (E.Signed t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.SRem t I.Unsafe) x y
internaliseBinOp loc desc E.Rem x y (E.Unsigned t) _ =
  certifyingNonzero loc t y $
    simpleBinOp desc (I.UMod t I.Unsafe) x y
internaliseBinOp _ desc E.ShiftR x y (E.Signed t) _ =
  simpleBinOp desc (I.AShr t) x y
internaliseBinOp _ desc E.ShiftR x y (E.Unsigned t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp _ desc E.ShiftL x y (E.Signed t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp _ desc E.ShiftL x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp _ desc E.Band x y (E.Signed t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp _ desc E.Band x y (E.Unsigned t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp _ desc E.Xor x y (E.Signed t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp _ desc E.Xor x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp _ desc E.Bor x y (E.Signed t) _ =
  simpleBinOp desc (I.Or t) x y
internaliseBinOp _ desc E.Bor x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Or t) x y
internaliseBinOp _ desc E.Equal x y t _ =
  simpleCmpOp desc (I.CmpEq $ internalisePrimType t) x y
internaliseBinOp _ desc E.NotEqual x y t _ = do
  eq <- letSubExp (desc ++ "true") $ I.BasicOp $ I.CmpOp (I.CmpEq $ internalisePrimType t) x y
  fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp I.Not eq
internaliseBinOp _ desc E.Less x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSlt t) x y
internaliseBinOp _ desc E.Less x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUlt t) x y
internaliseBinOp _ desc E.Leq x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSle t) x y
internaliseBinOp _ desc E.Leq x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUle t) x y
internaliseBinOp _ desc E.Greater x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSlt t) y x -- Note the swapped x and y
internaliseBinOp _ desc E.Greater x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUlt t) y x -- Note the swapped x and y
internaliseBinOp _ desc E.Geq x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSle t) y x -- Note the swapped x and y
internaliseBinOp _ desc E.Geq x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUle t) y x -- Note the swapped x and y
internaliseBinOp _ desc E.Less x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLt t) x y
internaliseBinOp _ desc E.Leq x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLe t) x y
internaliseBinOp _ desc E.Greater x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLt t) y x -- Note the swapped x and y
internaliseBinOp _ desc E.Geq x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLe t) y x -- Note the swapped x and y

-- Relational operators for booleans.
internaliseBinOp _ desc E.Less x y E.Bool _ =
  simpleCmpOp desc I.CmpLlt x y
internaliseBinOp _ desc E.Leq x y E.Bool _ =
  simpleCmpOp desc I.CmpLle x y
internaliseBinOp _ desc E.Greater x y E.Bool _ =
  simpleCmpOp desc I.CmpLlt y x -- Note the swapped x and y
internaliseBinOp _ desc E.Geq x y E.Bool _ =
  simpleCmpOp desc I.CmpLle y x -- Note the swapped x and y
internaliseBinOp _ _ op _ _ t1 t2 =
  error $
    "Invalid binary operator "
      ++ prettyString op
      ++ " with operand types "
      ++ prettyString t1
      ++ ", "
      ++ prettyString t2

simpleBinOp ::
  String ->
  I.BinOp ->
  I.SubExp ->
  I.SubExp ->
  InternaliseM [I.SubExp]
simpleBinOp desc bop x y =
  letTupExp' desc $ I.BasicOp $ I.BinOp bop x y

simpleCmpOp ::
  String ->
  I.CmpOp ->
  I.SubExp ->
  I.SubExp ->
  InternaliseM [I.SubExp]
simpleCmpOp desc op x y =
  letTupExp' desc $ I.BasicOp $ I.CmpOp op x y

data Function
  = FunctionName (E.QualName VName)
  | FunctionHole SrcLoc
  deriving (Show)

findFuncall :: E.AppExp -> (Function, [((E.Exp, Maybe VName), AutoMap)])
findFuncall (E.Apply f args _)
  | E.Var fname _ _ <- f =
      (FunctionName fname, map onArg $ NE.toList args)
  | E.Hole (Info _) loc <- f =
      (FunctionHole loc, map onArg $ NE.toList args)
  where
    onArg (Info (argext, am), e) = ((e, argext), am)
findFuncall e =
  error $ "Invalid function expression in application:\n" ++ prettyString e

-- The type of a body.  Watch out: this only works for the degenerate
-- case where the body does not already return its context.
bodyExtType :: Body SOACS -> InternaliseM [ExtType]
bodyExtType (Body _ stms res) =
  existentialiseExtTypes (M.keys stmsscope) . staticShapes
    <$> extendedScope (traverse subExpResType res) stmsscope
  where
    stmsscope = scopeOf stms

internaliseLambda :: InternaliseLambda
internaliseLambda (E.Parens e _) rowtypes =
  internaliseLambda e rowtypes
internaliseLambda (E.Lambda params body _ (Info (RetType _ rettype)) _) rowtypes =
  bindingLambdaParams params rowtypes $ \params' -> do
    body' <- internaliseBody "lam" body
    rettype' <- internaliseLambdaReturnType rettype =<< bodyExtType body'
    pure (params', body', rettype')
internaliseLambda e _ = error $ "internaliseLambda: unexpected expression:\n" ++ prettyString e

internaliseLambdaCoerce :: E.Exp -> [Type] -> InternaliseM (I.Lambda SOACS)
internaliseLambdaCoerce lam argtypes = do
  (params, body, rettype) <- internaliseLambda lam argtypes
  mkLambda params $
    ensureResultShape
      (ErrorMsg [ErrorString "unexpected lambda result size"])
      (srclocOf lam)
      rettype
      =<< bodyBind body

-- | Overloaded operators are treated here.
isOverloadedFunction ::
  E.QualName VName ->
  String ->
  SrcLoc ->
  Maybe ([(E.StructType, [SubExp])] -> InternaliseM [SubExp])
isOverloadedFunction qname desc loc = do
  guard $ baseTag (qualLeaf qname) <= maxIntrinsicTag
  handle $ baseString $ qualLeaf qname
  where
    -- Handle equality and inequality specially, to treat the case of
    -- arrays.
    handle op
      | Just cmp_f <- isEqlOp op = Just $ \[(_, xe'), (_, ye')] -> do
          rs <- zipWithM doComparison xe' ye'
          cmp_f =<< letSubExp "eq" =<< eAll rs
      where
        isEqlOp "!=" = Just $ \eq ->
          letTupExp' desc $ I.BasicOp $ I.UnOp I.Not eq
        isEqlOp "==" = Just $ \eq ->
          pure [eq]
        isEqlOp _ = Nothing

        doComparison x y = do
          x_t <- I.subExpType x
          y_t <- I.subExpType y
          case x_t of
            I.Prim t -> letSubExp desc $ I.BasicOp $ I.CmpOp (I.CmpEq t) x y
            _ -> do
              let x_dims = I.arrayDims x_t
                  y_dims = I.arrayDims y_t
              dims_match <- forM (zip x_dims y_dims) $ \(x_dim, y_dim) ->
                letSubExp "dim_eq" $ I.BasicOp $ I.CmpOp (I.CmpEq int64) x_dim y_dim
              shapes_match <- letSubExp "shapes_match" =<< eAll dims_match
              let compare_elems_body = runBodyBuilder $ do
                    -- Flatten both x and y.
                    x_num_elems <-
                      letSubExp "x_num_elems"
                        =<< foldBinOp (I.Mul Int64 I.OverflowUndef) (constant (1 :: Int64)) x_dims
                    x' <- letExp "x" $ I.BasicOp $ I.SubExp x
                    y' <- letExp "x" $ I.BasicOp $ I.SubExp y
                    x_flat <-
                      letExp "x_flat" $ I.BasicOp $ I.Reshape I.ReshapeArbitrary (I.Shape [x_num_elems]) x'
                    y_flat <-
                      letExp "y_flat" $ I.BasicOp $ I.Reshape I.ReshapeArbitrary (I.Shape [x_num_elems]) y'

                    -- Compare the elements.
                    cmp_lam <- cmpOpLambda $ I.CmpEq (elemType x_t)
                    cmps <-
                      letExp "cmps" $
                        I.Op $
                          I.Screma x_num_elems [x_flat, y_flat] (I.mapSOAC cmp_lam)

                    -- Check that all were equal.
                    and_lam <- binOpLambda I.LogAnd I.Bool
                    reduce <- I.reduceSOAC [Reduce Commutative and_lam [constant True]]
                    all_equal <- letSubExp "all_equal" $ I.Op $ I.Screma x_num_elems [cmps] reduce
                    pure $ resultBody [all_equal]

              letSubExp "arrays_equal"
                =<< eIf (eSubExp shapes_match) compare_elems_body (resultBodyM [constant False])
    handle name
      | Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] =
          Just $ \[(x_t, [x']), (y_t, [y'])] ->
            case (arrayElem x_t, arrayElem y_t) of
              (E.Scalar (E.Prim t1), E.Scalar (E.Prim t2)) ->
                internaliseBinOp loc desc bop x' y' t1 t2
              _ -> error "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."
    handle _ = Nothing

    arrayElem (E.Array _ _ t) = E.Scalar t
    arrayElem t = t

-- | Handle intrinsic functions.  These are only allowed to be called
-- in the prelude, and their internalisation may involve inspecting
-- the AST.
isIntrinsicFunction ::
  E.QualName VName ->
  [E.Exp] ->
  SrcLoc ->
  Maybe (String -> InternaliseM [SubExp])
isIntrinsicFunction qname args loc = do
  guard $ baseTag (qualLeaf qname) <= maxIntrinsicTag
  let handlers =
        [ handleSign,
          handleOps,
          handleSOACs,
          handleAccs,
          handleAD,
          handleRest
        ]
  msum [h args $ baseString $ qualLeaf qname | h <- handlers]
  where
    handleSign [x] "sign_i8" = Just $ toSigned I.Int8 x
    handleSign [x] "sign_i16" = Just $ toSigned I.Int16 x
    handleSign [x] "sign_i32" = Just $ toSigned I.Int32 x
    handleSign [x] "sign_i64" = Just $ toSigned I.Int64 x
    handleSign [x] "unsign_i8" = Just $ toUnsigned I.Int8 x
    handleSign [x] "unsign_i16" = Just $ toUnsigned I.Int16 x
    handleSign [x] "unsign_i32" = Just $ toUnsigned I.Int32 x
    handleSign [x] "unsign_i64" = Just $ toUnsigned I.Int64 x
    handleSign _ _ = Nothing

    handleOps [x] s
      | Just unop <- find ((== s) . prettyString) allUnOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp unop x'
    handleOps [TupLit [x, y] _] s
      | Just bop <- find ((== s) . prettyString) allBinOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          y' <- internaliseExp1 "y" y
          fmap pure $ letSubExp desc $ I.BasicOp $ I.BinOp bop x' y'
      | Just cmp <- find ((== s) . prettyString) allCmpOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          y' <- internaliseExp1 "y" y
          fmap pure $ letSubExp desc $ I.BasicOp $ I.CmpOp cmp x' y'
    handleOps [x] s
      | Just conv <- find ((== s) . prettyString) allConvOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.ConvOp conv x'
    handleOps _ _ = Nothing

    handleSOACs [k, lam, arr] "partition" = do
      k' <- fromIntegral <$> fromInt32 k
      Just $ \_desc -> do
        arrs <- internaliseExpToVars "partition_input" arr
        lam' <- internalisePartitionLambda internaliseLambda k' lam $ map I.Var arrs
        uncurry (++) <$> partitionWithSOACS (fromIntegral k') lam' arrs
      where
        fromInt32 (Literal (SignedValue (Int32Value k')) _) = Just k'
        fromInt32 (IntLit k' (Info (E.Scalar (E.Prim (E.Signed Int32)))) _) = Just $ fromInteger k'
        fromInt32 _ = Nothing
    handleSOACs [lam, ne, arr] "reduce" = Just $ \desc ->
      internaliseScanOrReduce desc "reduce" reduce (lam, ne, arr, loc)
      where
        reduce w red_lam nes arrs =
          I.Screma w arrs
            <$> I.reduceSOAC [Reduce Noncommutative red_lam nes]
    handleSOACs [lam, ne, arr] "reduce_comm" = Just $ \desc ->
      internaliseScanOrReduce desc "reduce" reduce (lam, ne, arr, loc)
      where
        reduce w red_lam nes arrs =
          I.Screma w arrs
            <$> I.reduceSOAC [Reduce Commutative red_lam nes]
    handleSOACs [lam, ne, arr] "scan" = Just $ \desc ->
      internaliseScanOrReduce desc "scan" reduce (lam, ne, arr, loc)
      where
        reduce w scan_lam nes arrs =
          I.Screma w arrs <$> I.scanSOAC [Scan scan_lam nes]
    handleSOACs [rf, dest, op, ne, buckets, img] "hist_1d" = Just $ \desc ->
      internaliseHist 1 desc rf dest op ne buckets img loc
    handleSOACs [rf, dest, op, ne, buckets, img] "hist_2d" = Just $ \desc ->
      internaliseHist 2 desc rf dest op ne buckets img loc
    handleSOACs [rf, dest, op, ne, buckets, img] "hist_3d" = Just $ \desc ->
      internaliseHist 3 desc rf dest op ne buckets img loc
    handleSOACs _ _ = Nothing

    handleAccs [dest, f, bs] "scatter_stream" = Just $ \desc ->
      internaliseStreamAcc desc dest Nothing f bs
    handleAccs [dest, op, ne, f, bs] "hist_stream" = Just $ \desc ->
      internaliseStreamAcc desc dest (Just (op, ne)) f bs
    handleAccs [acc, i, v] "acc_write" = Just $ \desc -> do
      acc' <- head <$> internaliseExpToVars "acc" acc
      i' <- internaliseExp1 "acc_i" i
      vs <- internaliseExp "acc_v" v
      fmap pure $ letSubExp desc $ BasicOp $ UpdateAcc acc' [i'] vs
    handleAccs _ _ = Nothing

    handleAD [f, x, v] fname
      | fname `elem` ["jvp2", "vjp2"] = Just $ \desc -> do
          x' <- internaliseExp "ad_x" x
          v' <- internaliseExp "ad_v" v
          lam <- internaliseLambdaCoerce f =<< mapM subExpType x'
          fmap (map I.Var) . letTupExp desc . Op $
            case fname of
              "jvp2" -> JVP lam x' v'
              _ -> VJP lam x' v'
    handleAD _ _ = Nothing

    handleRest [a, si, v] "scatter" = Just $ scatterF 1 a si v
    handleRest [a, si, v] "scatter_2d" = Just $ scatterF 2 a si v
    handleRest [a, si, v] "scatter_3d" = Just $ scatterF 3 a si v
    handleRest [n, m, arr] "unflatten" = Just $ \desc -> do
      arrs <- internaliseExpToVars "unflatten_arr" arr
      n' <- internaliseExp1 "n" n
      m' <- internaliseExp1 "m" m
      -- Each dimension must be nonnegative, and the unflattened
      -- dimension needs to have the same number of elements as the
      -- original dimension.
      old_dim <- I.arraysSize 0 <$> mapM lookupType arrs
      dim_ok <-
        letSubExp "dim_ok" <=< toExp $
          pe64 old_dim .==. pe64 n'
            * pe64 m'
              .&&. pe64 n'
              .>=. 0
              .&&. pe64 m'
              .>=. 0
      dim_ok_cert <-
        assert
          "dim_ok_cert"
          dim_ok
          ( ErrorMsg
              [ "Cannot unflatten array of shape [",
                ErrorVal int64 old_dim,
                "] to array of shape [",
                ErrorVal int64 n',
                "][",
                ErrorVal int64 m',
                "]"
              ]
          )
          loc
      certifying dim_ok_cert $
        forM arrs $ \arr' -> do
          arr_t <- lookupType arr'
          letSubExp desc . I.BasicOp $
            I.Reshape
              I.ReshapeArbitrary
              (reshapeOuter (I.Shape [n', m']) 1 $ I.arrayShape arr_t)
              arr'
    handleRest [arr] "manifest" = Just $ \desc -> do
      arrs <- internaliseExpToVars "flatten_arr" arr
      forM arrs $ \arr' -> do
        r <- I.arrayRank <$> lookupType arr'
        if r == 0
          then pure $ I.Var arr'
          else letSubExp desc $ I.BasicOp $ I.Manifest [0 .. r - 1] arr'
    handleRest [arr] "flatten" = Just $ \desc -> do
      arrs <- internaliseExpToVars "flatten_arr" arr
      forM arrs $ \arr' -> do
        arr_t <- lookupType arr'
        let n = arraySize 0 arr_t
            m = arraySize 1 arr_t
        k <- letSubExp "flat_dim" $ I.BasicOp $ I.BinOp (Mul Int64 I.OverflowUndef) n m
        letSubExp desc . I.BasicOp $
          I.Reshape
            I.ReshapeArbitrary
            (reshapeOuter (I.Shape [k]) 2 $ I.arrayShape arr_t)
            arr'
    handleRest [x, y] "concat" = Just $ \desc -> do
      xs <- internaliseExpToVars "concat_x" x
      ys <- internaliseExpToVars "concat_y" y
      outer_size <- arraysSize 0 <$> mapM lookupType xs
      let sumdims xsize ysize =
            letSubExp "conc_tmp" $
              I.BasicOp $
                I.BinOp (I.Add I.Int64 I.OverflowUndef) xsize ysize
      ressize <-
        foldM sumdims outer_size
          =<< mapM (fmap (arraysSize 0) . mapM lookupType) [ys]

      let conc xarr yarr =
            I.BasicOp $ I.Concat 0 (xarr :| [yarr]) ressize
      mapM (letSubExp desc) $ zipWith conc xs ys
    handleRest [e] "transpose" = Just $ \desc ->
      internaliseOperation desc e $ \v -> do
        r <- I.arrayRank <$> lookupType v
        pure $ I.Rearrange ([1, 0] ++ [2 .. r - 1]) v
    handleRest [x, y] "zip" = Just $ \desc ->
      mapM (letSubExp "zip_copy" . BasicOp . Replicate mempty . I.Var)
        =<< ( (++)
                <$> internaliseExpToVars (desc ++ "_zip_x") x
                <*> internaliseExpToVars (desc ++ "_zip_y") y
            )
    handleRest [x] "unzip" = Just $ \desc ->
      mapM (letSubExp desc . BasicOp . Replicate mempty . I.Var)
        =<< internaliseExpToVars desc x
    handleRest [arr, offset, n1, s1, n2, s2] "flat_index_2d" = Just $ \desc -> do
      flatIndexHelper desc loc arr offset [(n1, s1), (n2, s2)]
    handleRest [arr1, offset, s1, s2, arr2] "flat_update_2d" = Just $ \desc -> do
      flatUpdateHelper desc loc arr1 offset [s1, s2] arr2
    handleRest [arr, offset, n1, s1, n2, s2, n3, s3] "flat_index_3d" = Just $ \desc -> do
      flatIndexHelper desc loc arr offset [(n1, s1), (n2, s2), (n3, s3)]
    handleRest [arr1, offset, s1, s2, s3, arr2] "flat_update_3d" = Just $ \desc -> do
      flatUpdateHelper desc loc arr1 offset [s1, s2, s3] arr2
    handleRest [arr, offset, n1, s1, n2, s2, n3, s3, n4, s4] "flat_index_4d" = Just $ \desc -> do
      flatIndexHelper desc loc arr offset [(n1, s1), (n2, s2), (n3, s3), (n4, s4)]
    handleRest [arr1, offset, s1, s2, s3, s4, arr2] "flat_update_4d" = Just $ \desc -> do
      flatUpdateHelper desc loc arr1 offset [s1, s2, s3, s4] arr2
    handleRest _ _ = Nothing

    toSigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Scalar (E.Prim E.Bool) ->
          letTupExp' desc
            =<< eIf
              (eSubExp e')
              (resultBodyM [intConst int_to 1])
              (resultBodyM [intConst int_to 0])
        E.Scalar (E.Prim (E.Signed int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SExt int_from int_to) e'
        E.Scalar (E.Prim (E.Unsigned int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Scalar (E.Prim (E.FloatType float_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToSI float_from int_to) e'
        _ -> error "Futhark.Internalise: non-numeric type in ToSigned"

    toUnsigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Scalar (E.Prim E.Bool) ->
          letTupExp' desc
            =<< eIf
              (eSubExp e')
              (resultBodyM [intConst int_to 1])
              (resultBodyM [intConst int_to 0])
        E.Scalar (E.Prim (E.Signed int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Scalar (E.Prim (E.Unsigned int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Scalar (E.Prim (E.FloatType float_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToUI float_from int_to) e'
        _ -> error "Futhark.Internalise.internaliseExp: non-numeric type in ToUnsigned"

    scatterF dim a si v desc = do
      si' <- internaliseExpToVars "write_arg_i" si
      svs <- internaliseExpToVars "write_arg_v" v
      sas <- internaliseExpToVars "write_arg_a" a

      si_w <- I.arraysSize 0 <$> mapM lookupType si'
      sv_ts <- mapM lookupType svs

      svs' <- forM (zip svs sv_ts) $ \(sv, sv_t) -> do
        let sv_shape = I.arrayShape sv_t
            sv_w = arraySize 0 sv_t

        -- Generate an assertion and reshapes to ensure that sv and si' are the same
        -- size.
        cmp <-
          letSubExp "write_cmp" $
            I.BasicOp $
              I.CmpOp (I.CmpEq I.int64) si_w sv_w
        c <-
          assert
            "write_cert"
            cmp
            "length of index and value array does not match"
            loc
        certifying c $
          letExp (baseString sv ++ "_write_sv") . I.BasicOp $
            I.Reshape I.ReshapeCoerce (reshapeOuter (I.Shape [si_w]) 1 sv_shape) sv

      indexType <- fmap rowType <$> mapM lookupType si'
      indexName <- mapM (\_ -> newVName "write_index") indexType
      valueNames <- replicateM (length sv_ts) $ newVName "write_value"

      sa_ts <- mapM lookupType sas
      let bodyTypes = concat (replicate (length sv_ts) indexType) ++ map (I.stripArray dim) sa_ts
          paramTypes = indexType <> map rowType sv_ts
          bodyNames = indexName <> valueNames
          bodyParams = zipWith (I.Param mempty) bodyNames paramTypes

      -- This body is boring right now, as every input is exactly the output.
      -- But it can get funky later on if fused with something else.
      body <- localScope (scopeOfLParams bodyParams) . buildBody_ $ do
        let outs = concat (replicate (length valueNames) indexName) ++ valueNames
        results <- forM outs $ \name ->
          letSubExp "write_res" $ I.BasicOp $ I.SubExp $ I.Var name
        ensureResultShape
          "scatter value has wrong size"
          loc
          bodyTypes
          (subExpsRes results)

      let lam =
            I.Lambda
              { I.lambdaParams = bodyParams,
                I.lambdaReturnType = bodyTypes,
                I.lambdaBody = body
              }
          sivs = si' <> svs'

      let sa_ws = map (I.Shape . take dim . arrayDims) sa_ts
      letTupExp' desc $ I.Op $ I.Scatter si_w sivs lam $ zip3 sa_ws (repeat 1) sas

flatIndexHelper :: String -> SrcLoc -> E.Exp -> E.Exp -> [(E.Exp, E.Exp)] -> InternaliseM [SubExp]
flatIndexHelper desc loc arr offset slices = do
  arrs <- internaliseExpToVars "arr" arr
  offset' <- internaliseExp1 "offset" offset
  old_dim <- I.arraysSize 0 <$> mapM lookupType arrs
  offset_inbounds_down <- letSubExp "offset_inbounds_down" $ I.BasicOp $ I.CmpOp (I.CmpUle Int64) (intConst Int64 0) offset'
  offset_inbounds_up <- letSubExp "offset_inbounds_up" $ I.BasicOp $ I.CmpOp (I.CmpUlt Int64) offset' old_dim
  slices' <-
    mapM
      ( \(n, s) -> do
          n' <- internaliseExp1 "n" n
          s' <- internaliseExp1 "s" s
          pure (n', s')
      )
      slices
  (min_bound, max_bound) <-
    foldM
      ( \(lower, upper) (n, s) -> do
          n_m1 <- letSubExp "span" $ I.BasicOp $ I.BinOp (I.Sub Int64 I.OverflowUndef) n (intConst Int64 1)
          spn <- letSubExp "span" $ I.BasicOp $ I.BinOp (I.Mul Int64 I.OverflowUndef) n_m1 s

          span_and_lower <- letSubExp "span_and_lower" $ I.BasicOp $ I.BinOp (I.Add Int64 I.OverflowUndef) spn lower
          span_and_upper <- letSubExp "span_and_upper" $ I.BasicOp $ I.BinOp (I.Add Int64 I.OverflowUndef) spn upper

          lower' <- letSubExp "minimum" $ I.BasicOp $ I.BinOp (I.UMin Int64) span_and_lower lower
          upper' <- letSubExp "maximum" $ I.BasicOp $ I.BinOp (I.UMax Int64) span_and_upper upper

          pure (lower', upper')
      )
      (offset', offset')
      slices'
  min_in_bounds <- letSubExp "min_in_bounds" $ I.BasicOp $ I.CmpOp (I.CmpUle Int64) (intConst Int64 0) min_bound
  max_in_bounds <- letSubExp "max_in_bounds" $ I.BasicOp $ I.CmpOp (I.CmpUlt Int64) max_bound old_dim

  all_bounds <-
    foldM
      (\x y -> letSubExp "inBounds" $ I.BasicOp $ I.BinOp I.LogAnd x y)
      offset_inbounds_down
      [offset_inbounds_up, min_in_bounds, max_in_bounds]

  c <- assert "bounds_cert" all_bounds (ErrorMsg [ErrorString $ "Flat slice out of bounds: " <> prettyText old_dim <> " and " <> prettyText slices']) loc
  let slice = I.FlatSlice offset' $ map (uncurry FlatDimIndex) slices'
  certifying c $
    forM arrs $ \arr' ->
      letSubExp desc $ I.BasicOp $ I.FlatIndex arr' slice

flatUpdateHelper :: String -> SrcLoc -> E.Exp -> E.Exp -> [E.Exp] -> E.Exp -> InternaliseM [SubExp]
flatUpdateHelper desc loc arr1 offset slices arr2 = do
  arrs1 <- internaliseExpToVars "arr" arr1
  offset' <- internaliseExp1 "offset" offset
  old_dim <- I.arraysSize 0 <$> mapM lookupType arrs1
  offset_inbounds_down <- letSubExp "offset_inbounds_down" $ I.BasicOp $ I.CmpOp (I.CmpUle Int64) (intConst Int64 0) offset'
  offset_inbounds_up <- letSubExp "offset_inbounds_up" $ I.BasicOp $ I.CmpOp (I.CmpUlt Int64) offset' old_dim
  arrs2 <- internaliseExpToVars "arr" arr2
  ts <- mapM lookupType arrs2
  slices' <-
    mapM
      ( \(s, i) -> do
          s' <- internaliseExp1 "s" s
          let n = arraysSize i ts
          pure (n, s')
      )
      $ zip slices [0 ..]
  (min_bound, max_bound) <-
    foldM
      ( \(lower, upper) (n, s) -> do
          n_m1 <- letSubExp "span" $ I.BasicOp $ I.BinOp (I.Sub Int64 I.OverflowUndef) n (intConst Int64 1)
          spn <- letSubExp "span" $ I.BasicOp $ I.BinOp (I.Mul Int64 I.OverflowUndef) n_m1 s

          span_and_lower <- letSubExp "span_and_lower" $ I.BasicOp $ I.BinOp (I.Add Int64 I.OverflowUndef) spn lower
          span_and_upper <- letSubExp "span_and_upper" $ I.BasicOp $ I.BinOp (I.Add Int64 I.OverflowUndef) spn upper

          lower' <- letSubExp "minimum" $ I.BasicOp $ I.BinOp (I.UMin Int64) span_and_lower lower
          upper' <- letSubExp "maximum" $ I.BasicOp $ I.BinOp (I.UMax Int64) span_and_upper upper

          pure (lower', upper')
      )
      (offset', offset')
      slices'
  min_in_bounds <- letSubExp "min_in_bounds" $ I.BasicOp $ I.CmpOp (I.CmpUle Int64) (intConst Int64 0) min_bound
  max_in_bounds <- letSubExp "max_in_bounds" $ I.BasicOp $ I.CmpOp (I.CmpUlt Int64) max_bound old_dim

  all_bounds <-
    foldM
      (\x y -> letSubExp "inBounds" $ I.BasicOp $ I.BinOp I.LogAnd x y)
      offset_inbounds_down
      [offset_inbounds_up, min_in_bounds, max_in_bounds]

  c <- assert "bounds_cert" all_bounds (ErrorMsg [ErrorString $ "Flat slice out of bounds: " <> prettyText old_dim <> " and " <> prettyText slices']) loc
  let slice = I.FlatSlice offset' $ map (uncurry FlatDimIndex) slices'
  certifying c $
    forM (zip arrs1 arrs2) $ \(arr1', arr2') ->
      letSubExp desc $ I.BasicOp $ I.FlatUpdate arr1' slice arr2'

funcall ::
  String ->
  QualName VName ->
  [SubExp] ->
  SrcLoc ->
  InternaliseM [SubExp]
funcall desc (QualName _ fname) args loc = do
  (shapes, value_paramts, fun_params, rettype_fun) <- lookupFunction fname
  argts <- mapM subExpType args

  shapeargs <- argShapes shapes fun_params argts
  let diets =
        replicate (length shapeargs) I.ObservePrim
          ++ map I.diet value_paramts
  args' <-
    ensureArgShapes
      "function arguments of wrong shape"
      loc
      (map I.paramName fun_params)
      (map I.paramType fun_params)
      (shapeargs ++ args)
  argts' <- mapM subExpType args'
  case rettype_fun $ zip args' argts' of
    Nothing ->
      error $
        concat
          [ "Cannot apply ",
            prettyString fname,
            " to ",
            show (length args'),
            " arguments\n ",
            prettyString args',
            "\nof types\n ",
            prettyString argts',
            "\nFunction has ",
            show (length fun_params),
            " parameters\n ",
            prettyString fun_params
          ]
    Just ts -> do
      safety <- askSafety
      attrs <- asks envAttrs
      attributing attrs . letValExp' desc $
        I.Apply (internaliseFunName fname) (zip args' diets) ts (safety, loc, mempty)

-- Bind existential names defined by an expression, based on the
-- concrete values that expression evaluated to.  This most
-- importantly should be done after function calls, but also
-- everything else that can produce existentials in the source
-- language.
bindExtSizes :: AppRes -> [SubExp] -> InternaliseM ()
bindExtSizes (AppRes ret retext) ses = do
  let ts = foldMap toList $ internaliseType $ E.toStruct ret
  ses_ts <- mapM subExpType ses

  let combine t1 t2 =
        mconcat $ zipWith combine' (arrayExtDims t1) (arrayDims t2)
      combine' (I.Free (I.Var v)) se
        | v `elem` retext = M.singleton v se
      combine' _ _ = mempty

  forM_ (M.toList $ mconcat $ zipWith combine ts ses_ts) $ \(v, se) ->
    letBindNames [v] $ BasicOp $ SubExp se

askSafety :: InternaliseM Safety
askSafety = do
  check <- asks envDoBoundsChecks
  pure $ if check then I.Safe else I.Unsafe

-- Implement partitioning using maps, scans and writes.
partitionWithSOACS :: Int -> I.Lambda SOACS -> [I.VName] -> InternaliseM ([I.SubExp], [I.SubExp])
partitionWithSOACS k lam arrs = do
  arr_ts <- mapM lookupType arrs
  let w = arraysSize 0 arr_ts
  classes_and_increments <- letTupExp "increments" $ I.Op $ I.Screma w arrs (mapSOAC lam)
  (classes, increments) <- case classes_and_increments of
    classes : increments -> pure (classes, take k increments)
    _ -> error "partitionWithSOACS"

  add_lam_x_params <-
    replicateM k $ newParam "x" (I.Prim int64)
  add_lam_y_params <-
    replicateM k $ newParam "y" (I.Prim int64)
  add_lam_body <- runBodyBuilder $
    localScope (scopeOfLParams $ add_lam_x_params ++ add_lam_y_params) $
      fmap resultBody $
        forM (zip add_lam_x_params add_lam_y_params) $ \(x, y) ->
          letSubExp "z" $
            I.BasicOp $
              I.BinOp
                (I.Add Int64 I.OverflowUndef)
                (I.Var $ I.paramName x)
                (I.Var $ I.paramName y)
  let add_lam =
        I.Lambda
          { I.lambdaBody = add_lam_body,
            I.lambdaParams = add_lam_x_params ++ add_lam_y_params,
            I.lambdaReturnType = replicate k $ I.Prim int64
          }
      nes = replicate (length increments) $ intConst Int64 0

  scan <- I.scanSOAC [I.Scan add_lam nes]
  all_offsets <- letTupExp "offsets" $ I.Op $ I.Screma w increments scan

  -- We have the offsets for each of the partitions, but we also need
  -- the total sizes, which are the last elements in the offests.  We
  -- just have to be careful in case the array is empty.
  last_index <- letSubExp "last_index" $ I.BasicOp $ I.BinOp (I.Sub Int64 OverflowUndef) w $ constant (1 :: Int64)
  let nonempty_body = runBodyBuilder $
        fmap resultBody $
          forM all_offsets $ \offset_array ->
            letSubExp "last_offset" $ I.BasicOp $ I.Index offset_array $ Slice [I.DimFix last_index]
      empty_body = resultBodyM $ replicate k $ constant (0 :: Int64)
  is_empty <- letSubExp "is_empty" $ I.BasicOp $ I.CmpOp (CmpEq int64) w $ constant (0 :: Int64)
  sizes <-
    letTupExp "partition_size" =<< eIf (eSubExp is_empty) empty_body nonempty_body

  -- The total size of all partitions must necessarily be equal to the
  -- size of the input array.

  -- Create scratch arrays for the result.
  blanks <- forM arr_ts $ \arr_t ->
    letExp "partition_dest" $
      I.BasicOp $
        Scratch (I.elemType arr_t) (w : drop 1 (I.arrayDims arr_t))

  -- Now write into the result.
  write_lam <- do
    c_param <- newParam "c" (I.Prim int64)
    offset_params <- replicateM k $ newParam "offset" (I.Prim int64)
    value_params <- mapM (newParam "v" . I.rowType) arr_ts
    (offset, offset_stms) <-
      collectStms $
        mkOffsetLambdaBody
          (map I.Var sizes)
          (I.Var $ I.paramName c_param)
          0
          offset_params
    pure
      I.Lambda
        { I.lambdaParams = c_param : offset_params ++ value_params,
          I.lambdaReturnType =
            replicate (length arr_ts) (I.Prim int64)
              ++ map I.rowType arr_ts,
          I.lambdaBody =
            mkBody offset_stms $
              replicate (length arr_ts) (subExpRes offset)
                ++ I.varsRes (map I.paramName value_params)
        }
  results <-
    letTupExp "partition_res" . I.Op $
      I.Scatter w (classes : all_offsets ++ arrs) write_lam $
        zip3 (repeat $ I.Shape [w]) (repeat 1) blanks
  sizes' <-
    letSubExp "partition_sizes" $
      I.BasicOp $
        I.ArrayLit (map I.Var sizes) $
          I.Prim int64
  pure (map I.Var results, [sizes'])
  where
    mkOffsetLambdaBody ::
      [SubExp] ->
      SubExp ->
      Int ->
      [I.LParam SOACS] ->
      InternaliseM SubExp
    mkOffsetLambdaBody _ _ _ [] =
      pure $ constant (-1 :: Int64)
    mkOffsetLambdaBody sizes c i (p : ps) = do
      is_this_one <-
        letSubExp "is_this_one" $
          I.BasicOp $
            I.CmpOp (CmpEq int64) c $
              intConst Int64 $
                toInteger i
      next_one <- mkOffsetLambdaBody sizes c (i + 1) ps
      this_one <-
        letSubExp "this_offset"
          =<< foldBinOp
            (Add Int64 OverflowUndef)
            (constant (-1 :: Int64))
            (I.Var (I.paramName p) : take i sizes)
      letSubExp "total_res"
        =<< eIf
          (eSubExp is_this_one)
          (resultBodyM [this_one])
          (resultBodyM [next_one])

sizeExpForError :: E.Size -> InternaliseM [ErrorMsgPart SubExp]
sizeExpForError e = do
  e' <- internaliseExp1 "size" e
  pure ["[", ErrorVal int64 e', "]"]

typeExpForError :: E.TypeBase Size u -> InternaliseM [ErrorMsgPart SubExp]
typeExpForError (E.Scalar (E.Prim t)) = pure [ErrorString $ prettyText t]
typeExpForError (E.Scalar (E.TypeVar _ v args)) = do
  args' <- concat <$> mapM onArg args
  pure $ intersperse " " $ ErrorString (prettyText v) : args'
  where
    onArg (TypeArgDim d) = sizeExpForError d
    onArg (TypeArgType t) = typeExpForError t
typeExpForError (E.Scalar (E.Record fs))
  | Just ts <- E.areTupleFields fs = do
      ts' <- mapM typeExpForError ts
      pure $ ["("] ++ intercalate [", "] ts' ++ [")"]
  | otherwise = do
      fs' <- mapM onField $ M.toList fs
      pure $ ["{"] ++ intercalate [", "] fs' ++ ["}"]
  where
    onField (k, te) =
      (ErrorString (prettyText k <> ": ") :) <$> typeExpForError te
typeExpForError (E.Array _ shape et) = do
  shape' <- mconcat <$> mapM sizeExpForError (E.shapeDims shape)
  et' <- typeExpForError $ Scalar et
  pure $ shape' ++ et'
typeExpForError (E.Scalar (E.Sum cs)) = do
  cs' <- mapM onConstructor $ M.toList cs
  pure $ intercalate [" | "] cs'
  where
    onConstructor (c, ts) = do
      ts' <- mapM typeExpForError ts
      pure $ ErrorString ("#" <> prettyText c <> " ") : intercalate [" "] ts'
typeExpForError (E.Scalar Arrow {}) = pure ["#<fun>"]

-- A smart constructor that compacts neighbouring literals for easier
-- reading in the IR.
errorMsg :: [ErrorMsgPart a] -> ErrorMsg a
errorMsg = ErrorMsg . compact
  where
    compact [] = []
    compact (ErrorString x : ErrorString y : parts) =
      compact (ErrorString (x <> y) : parts)
    compact (x : y) = x : compact y
