{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

-- | Conversion of a monomorphic, first-order, defunctorised source
-- program to a core Futhark program.
module Futhark.Internalise.Exps (transformProg) where

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor
import Data.Foldable (toList)
import Data.List (elemIndex, find, intercalate, intersperse, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
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
internaliseValBind types fb@(E.ValBind entry fname _ (Info rettype) tparams params body _ attrs _) = do
  bindingFParams tparams params $ \shapeparams params' -> do
    let shapenames = map I.paramName shapeparams
        all_params = map pure shapeparams ++ concat params'
        msg =
          errorMsg
            [ "Internal runtime error.\n",
              "Return value of ",
              ErrorString (prettyText fname),
              " does not match type shape.\n",
              "This is a bug in the Futhark compiler. Please report this:\n",
              "  https://github.com/diku-dk/futhark/issues"
            ]

    (body', rettype') <- buildBody $ do
      body_res <- internaliseExp (baseName fname <> "_res") body
      (rettype', retals) <-
        first zeroExts . unzip . internaliseReturnType (map (fmap paramDeclType) all_params) rettype
          <$> mapM subExpType body_res

      when (null params') $
        bindExtSizes (E.AppRes (E.toStruct $ E.retType rettype) (E.retDims rettype)) body_res

      body_res' <-
        ensureResultExtShape msg (map I.fromDecl rettype') $ subExpsRes body_res
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
  let (E.ValBind _ ofname _ (Info rettype) tparams params _ _ attrs _) = vb
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
          funcall "entry_result" (E.qualName ofname) args
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

internaliseBody :: Name -> E.Exp -> InternaliseM (Body SOACS)
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
letValExp :: Name -> I.Exp SOACS -> InternaliseM [VName]
letValExp name e = do
  e_t <- expExtType e
  names <- replicateM (length e_t) $ newVName name
  letBindNames names e
  let ctx = shapeContext e_t
  pure $ map fst $ filter ((`S.notMember` ctx) . snd) $ zip names [0 ..]

letValExp' :: Name -> I.Exp SOACS -> InternaliseM [SubExp]
letValExp' _ (BasicOp (SubExp se)) = pure [se]
letValExp' name ses = map I.Var <$> letValExp name ses

internaliseAppExp :: Name -> E.AppRes -> E.AppExp -> InternaliseM [I.SubExp]
internaliseAppExp desc _ (E.Index e idxs _) = do
  vs <- internaliseExpToVars "indexed" e
  dims <- case vs of
    [] -> pure [] -- Will this happen?
    v : _ -> I.arrayDims <$> lookupType v
  (idxs', cs) <- internaliseSlice dims idxs
  let index v = do
        v_t <- lookupType v
        pure $ I.BasicOp $ I.Index v $ fullSlice v_t idxs'
  certifying cs $ mapM (letSubExp desc <=< index) vs
internaliseAppExp desc _ (E.Range start maybe_second end _) = do
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

  (it, lt_op) <-
    case E.typeOf start of
      E.Scalar (E.Prim (E.Signed it)) -> pure (it, CmpSlt it)
      E.Scalar (E.Prim (E.Unsigned it)) -> pure (it, CmpUlt it)
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
        I.CmpOp lt_op start' end'
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
  valid <- letSubExp "valid" $ I.BasicOp $ I.UnOp (I.Neg I.Bool) invalid
  cs <- assert "range_valid_c" valid errmsg

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
    (FunctionName qfname, args) -> do
      -- Argument evaluation is outermost-in so that any existential sizes
      -- created by function applications can be brought into scope.
      let fname = baseName $ qualLeaf qfname
          arg_desc = fname <> "_arg"

      -- Some functions are magical (overloaded) and we handle that here.
      case () of
        ()
          -- Short-circuiting operators are magical.
          | baseTag (qualLeaf qfname) <= maxIntrinsicTag,
            baseName (qualLeaf qfname) == "&&",
            [(x, _), (y, _)] <- args ->
              internaliseExp desc $
                E.AppExp
                  (E.If x y (E.Literal (E.BoolValue False) mempty) mempty)
                  (Info $ AppRes (E.Scalar $ E.Prim E.Bool) [])
          | baseTag (qualLeaf qfname) <= maxIntrinsicTag,
            baseName (qualLeaf qfname) == "||",
            [(x, _), (y, _)] <- args ->
              internaliseExp desc $
                E.AppExp
                  (E.If x (E.Literal (E.BoolValue True) mempty) y mempty)
                  (Info $ AppRes (E.Scalar $ E.Prim E.Bool) [])
          -- Overloaded and intrinsic functions never take array
          -- arguments (except equality, but those cannot be
          -- existential), so we can safely ignore the existential
          -- dimensions.
          | Just internalise <- isOverloadedFunction qfname desc -> do
              let prepareArg (arg, _) =
                    (E.toStruct (E.typeOf arg),) <$> internaliseExp "arg" arg
              internalise =<< mapM prepareArg args
          | Just internalise <- isIntrinsicFunction qfname (map fst args) ->
              internalise desc
          | baseTag (qualLeaf qfname) <= maxIntrinsicTag,
            Just (rettype, _) <- M.lookup fname I.builtInFunctions -> do
              let tag ses = [(se, I.Observe) | se <- ses]
              args' <- reverse <$> mapM (internaliseArg arg_desc) (reverse args)
              let args'' = concatMap tag args'
              letValExp' desc $ I.Apply fname args'' [(I.Prim rettype, mempty)] Safe
          | otherwise -> do
              args' <- concat . reverse <$> mapM (internaliseArg arg_desc) (reverse args)
              funcall desc qfname args'
internaliseAppExp desc _ (E.LetPat sizes pat e body _) =
  internalisePat desc sizes pat e $ internaliseExp desc body
internaliseAppExp _ _ (E.LetFun (ofname, _) _ _ _) =
  error $ "Unexpected LetFun " ++ prettyString ofname
internaliseAppExp desc _ (E.Loop sparams mergepat loopinit form loopbody _) = do
  ses <- internaliseExp "loop_init" $ loopInitExp loopinit
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

    -- Attributes that apply to loops.
    loopAttrs = oneAttr "unroll"
    -- Remove those attributes from the attribute set that apply to
    -- the loop itself.
    noLoopAttrs env = env {envAttrs = envAttrs env `withoutAttrs` loopAttrs}

    loopBody = local noLoopAttrs $ internaliseExp "loopres" loopbody

    forLoop mergepat' shapepat mergeinit i loopvars form' =
      bodyFromStms . localScope (scopeOfLoopForm form') $ do
        forM_ loopvars $ \(p, arr) ->
          letBindNames [I.paramName p] =<< eIndex arr [eSubExp (I.Var i)]
        ses <- loopBody
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
                case se of
                  I.Var v
                    | not $ primType $ paramType p ->
                        shapeCoerce (I.arrayDims $ paramType p) v
                  _ -> BasicOp $ SubExp se

          -- As the condition expression is inserted twice, we have to
          -- avoid shadowing (#1935).
          (cond_stms, cond') <-
            uncurry (flip renameStmsWith)
              =<< collectStms (internaliseExp1 "loop_cond" cond)
          addStms cond_stms
          pure cond'

        addStms init_loop_cond_stms

        bodyFromStms $ do
          ses <- loopBody
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
                  case se of
                    I.Var v
                      | not $ primType $ paramType p ->
                          shapeCoerce (I.arrayDims $ paramType p) v
                    _ -> BasicOp $ SubExp se
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
internaliseAppExp desc _ (E.Match e orig_cs _) = do
  ses <- internaliseExp (desc <> "_scrutinee") e
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
internaliseAppExp _ _ e@(E.LetWith {}) =
  error $ "internaliseAppExp: Unexpected LetWith at " ++ locStr (srclocOf e)

internaliseExp :: Name -> E.Exp -> InternaliseM [I.SubExp]
internaliseExp desc (E.Parens e _) =
  internaliseExp desc e
internaliseExp desc (E.Hole (Info t) loc) = do
  let msg = docText $ "Reached hole of type: " <> align (pretty t)
      ts = foldMap toList $ internaliseType (E.toStruct t)
  c <- assert "hole_c" (constant False) $ errorMsg [ErrorString msg]
  case mapM hasStaticShape ts of
    Nothing ->
      error $ "Hole at " <> locStr loc <> " has existential type:\n" <> show ts
    Just ts' ->
      -- Make sure we always generate a binding, even for primitives.
      certifying c $ mapM (fmap I.Var . letExp desc <=< eBlank . I.fromDecl) ts'
internaliseExp desc (E.QualParens _ e _) =
  internaliseExp desc e
internaliseExp desc (E.StringLit vs loc) =
  locating loc . fmap pure . letSubExp desc $
    I.BasicOp $
      I.ArrayLit (map constant vs) $
        I.Prim int8
internaliseExp _ (E.Var (E.QualName _ name) _ _) = do
  subst <- lookupSubst name
  case subst of
    Just substs -> pure substs
    Nothing -> pure [I.Var name]
internaliseExp desc (E.AppExp e (Info appres)) = do
  ses <- locating e $ internaliseAppExp desc appres e
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
    internaliseField (E.RecordFieldExplicit (L _ name) e _) =
      M.singleton name <$> internaliseExp desc e
    internaliseField (E.RecordFieldImplicit (L _ name) t loc) =
      internaliseField $
        E.RecordFieldExplicit
          (L noLoc (baseName name))
          (E.Var (E.qualName name) t loc)
          loc
internaliseExp desc (E.ArrayVal vs t loc) =
  locating loc $
    fmap pure . letSubExp desc . I.BasicOp $
      I.ArrayVal (map internalisePrimValue vs) (internalisePrimType t)
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
        letSubExp desc $ I.BasicOp $ I.Reshape flat_arr (reshapeAll (I.arrayShape flat_arr_t) new_shape')
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
internaliseExp desc (E.Coerce e _ (Info et) _) = do
  ses <- internaliseExp desc e
  ts <- internaliseCoerceType (E.toStruct et) <$> mapM subExpType ses
  dt' <- typeExpForError $ toStruct et
  forM (zip ses ts) $ \(e', t') -> do
    dims <- arrayDims <$> subExpType e'
    let parts =
          ["Value of (desugared) shape ["]
            ++ intersperse "][" (map (ErrorVal int64) dims)
            ++ ["] cannot match shape of type \""]
            ++ dt'
            ++ ["\"."]
    ensureExtShape (errorMsg parts) (I.fromDecl t') desc e'
internaliseExp desc (E.Negate e loc) = locating loc $ do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of
    I.Prim pt ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.Neg pt) e'
    _ -> error "Futhark.Internalise.internaliseExp: non-primitive type in Negate"
internaliseExp desc (E.Not e loc) = locating loc $ do
  e' <- internaliseExp1 "not_arg" e
  et <- subExpType e'
  case et of
    I.Prim (I.IntType t) ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.Complement t) e'
    I.Prim I.Bool ->
      letTupExp' desc $ I.BasicOp $ I.UnOp (I.Neg I.Bool) e'
    _ ->
      error "Futhark.Internalise.internaliseExp: non-int/bool type in Not"
internaliseExp desc (E.Update src slice ve loc) = locating loc $ do
  ves <- internaliseExp "lw_val" ve
  srcs <- internaliseExpToVars "src" src
  (src_dims, ve_dims) <- case (srcs, ves) of
    (src_v : _, ve_v : _) ->
      (,)
        <$> (I.arrayDims <$> lookupType src_v)
        <*> (I.arrayDims <$> subExpType ve_v)
    _ -> pure ([], []) -- Will this happen?
  (idxs', cs) <- internaliseSlice src_dims slice
  let src_dims' = sliceDims (Slice idxs')
      rank = length src_dims'
      errormsg =
        "Shape "
          <> errorShape src_dims'
          <> " of slice does not match shape "
          <> errorShape (take rank ve_dims)
          <> " of value."

  let comb sname ve' = do
        sname_t <- lookupType sname
        let full_slice = fullSlice sname_t idxs'
            rowtype = sname_t `setArrayDims` sliceDims full_slice
        ve'' <-
          ensureShape errormsg rowtype "lw_val_correct_shape" ve'
        letInPlace desc sname full_slice $ BasicOp $ SubExp ve''
  certifying cs $ map I.Var <$> zipWithM comb srcs ves
internaliseExp desc (E.RecordUpdate src fields ve _ loc) = locating loc $ do
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
    "scratch" -> do
      ts <- mapM subExpType e'
      forM (zip ts e') $ \(t, se) ->
        case t of
          I.Array pt shape _ ->
            letSubExp desc $ I.BasicOp $ I.Scratch pt $ I.shapeDims shape
          I.Prim pt ->
            pure $ constant $ blankPrimValue pt
          _ -> pure se
    "blank" -> do
      ts <- mapM subExpType e'
      forM (zip ts e') $ \(t, se) ->
        case t of
          I.Array pt shape _ ->
            letSubExp desc . I.BasicOp . I.Replicate shape . constant $
              blankPrimValue pt
          I.Prim pt ->
            pure $ constant $ blankPrimValue pt
          _ -> pure se
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
internaliseExp desc (E.Assert e1 e2 (Info check) loc) = locating loc $ do
  e1' <- internaliseExp1 "assert_cond" e1
  c <- assert "assert_c" e1' $ errorMsg [ErrorString $ "Assertion is false: " <> check]
  -- Make sure there are some bindings to certify.
  certifying c $ mapM rebind =<< internaliseExp desc e2
  where
    rebind v = do
      v' <- newVName "assert_res"
      letBindNames [v'] $ I.BasicOp $ I.SubExp v
      pure $ I.Var v'
internaliseExp _ (E.Constr c es (Info (E.Scalar (E.Sum fs))) loc) = locating loc $ do
  (ts, constr_map) <- internaliseSumType $ M.map (map E.toStruct) fs
  es' <- concat <$> mapM (internaliseExp "payload") es

  let noExt _ = pure $ intConst Int64 0
  ts' <- instantiateShapes noExt $ map fromDecl ts

  case lookupWithIndex c constr_map of
    Just (i, js)
      | length fs == 1 ->
          clauses 0 ts' (zip js es')
      | otherwise ->
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

internaliseArg :: Name -> (E.Exp, Maybe VName) -> InternaliseM [SubExp]
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
    compares (E.PatConstr c (Info (E.Scalar (E.Sum fs))) pats _) sum_ses = do
      -- Handle the unary sum type special case.
      let unary = length fs == 1
          ses = if unary then sum_ses else tail sum_ses
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
            ( [Just (I.IntValue $ intValue Int8 $ toInteger tag) | not unary]
                <> zipWith missingCmps [0 ..] payload_ses,
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
      comparesMany (map snd $ E.sortFields $ M.fromList $ map (first unLoc) fs) ses
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
  Name ->
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
      [v] -> baseName $ E.identName v
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
  [SubExp] ->
  [E.DimIndex] ->
  InternaliseM ([I.DimIndex SubExp], Certs)
internaliseSlice dims idxs = do
  (idxs', oks, parts) <- unzip3 <$> zipWithM internaliseDimIndex dims idxs
  ok <- letSubExp "index_ok" =<< eAll oks
  let msg =
        errorMsg $
          ["Index ["]
            ++ intercalate [", "] parts
            ++ ["] out of bounds for array of shape ["]
            ++ intersperse "][" (map (ErrorVal int64) $ take (length idxs) dims)
            ++ ["]."]
  c <- assert "index_certs" ok msg
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
  nonzero_stride <- letSubExp "nonzero_stride" $ I.BasicOp $ I.UnOp (I.Neg I.Bool) zero_stride

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
  Name ->
  Name ->
  (SubExp -> I.Lambda SOACS -> [SubExp] -> [VName] -> InternaliseM (SOAC SOACS)) ->
  (E.Exp, E.Exp, E.Exp) ->
  InternaliseM [SubExp]
internaliseScanOrReduce desc what f (lam, ne, arr) = do
  arrs <- internaliseExpToVars (what <> "_arr") arr
  nes <- internaliseExp (what <> "_ne") ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape
      "Row shape of input array does not match shape of neutral element"
      rowtype
      (what <> "_ne_right_shape")
      ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda lam nests arrts
  w <- arraysSize 0 <$> mapM lookupType arrs
  letValExp' desc . I.Op =<< f w lam' nes' arrs

internaliseHist ::
  Int ->
  Name ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  E.Exp ->
  InternaliseM [SubExp]
internaliseHist dim desc rf hist op ne buckets img = do
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
        rettype
        =<< bodyBind body

  -- get sizes of histogram and image arrays
  shape_hist <- I.Shape . take dim . I.arrayDims <$> lookupType (head hist')
  w_img <- I.arraySize 0 <$> lookupType (head img')

  letValExp' desc . I.Op $
    I.Hist w_img (buckets' ++ img') [HistOp shape_hist rf' hist' ne_shp op'] lam'

internaliseStreamAcc ::
  Name ->
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
    fmap subExpsRes
      . letValExp' "acc_res"
      . I.Op
      . I.Screma w (I.paramName acc_p : bs')
      =<< I.mapSOAC lam'

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

internaliseExp1 :: Name -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of
    [se] -> pure se
    _ -> error "Internalise.internaliseExp1: was passed not just a single subexpression"

-- | Promote to dimension type as appropriate for the original type.
-- Also return original type.
internaliseSizeExp :: Name -> E.Exp -> InternaliseM (I.SubExp, IntType)
internaliseSizeExp s e = do
  e' <- internaliseExp1 s e
  case E.typeOf e of
    E.Scalar (E.Prim (E.Signed it)) -> (,it) <$> asIntS Int64 e'
    _ -> error "internaliseSizeExp: bad type"

internaliseExpToVars :: Name -> E.Exp -> InternaliseM [I.VName]
internaliseExpToVars desc e =
  mapM asIdent =<< internaliseExp desc e
  where
    asIdent (I.Var v) = pure v
    asIdent se = letExp desc $ I.BasicOp $ I.SubExp se

internaliseOperation ::
  Name ->
  E.Exp ->
  (I.VName -> InternaliseM I.BasicOp) ->
  InternaliseM [I.SubExp]
internaliseOperation s e op = do
  vs <- internaliseExpToVars s e
  mapM (letSubExp s . I.BasicOp <=< op) vs

certifyingNonzero ::
  IntType ->
  SubExp ->
  InternaliseM a ->
  InternaliseM a
certifyingNonzero t x m = do
  zero <-
    letSubExp "zero" $
      I.BasicOp $
        CmpOp (CmpEq (IntType t)) x (intConst t 0)
  nonzero <- letSubExp "nonzero" $ I.BasicOp $ UnOp (I.Neg I.Bool) zero
  c <- assert "nonzero_cert" nonzero "division by zero"
  certifying c m

certifyingNonnegative ::
  IntType ->
  SubExp ->
  InternaliseM a ->
  InternaliseM a
certifyingNonnegative t x m = do
  nonnegative <-
    letSubExp "nonnegative" . I.BasicOp $
      CmpOp (CmpSle t) (intConst t 0) x
  c <- assert "nonzero_cert" nonnegative "negative exponent"
  certifying c m

internaliseBinOp ::
  Name ->
  E.BinOp ->
  I.SubExp ->
  I.SubExp ->
  E.PrimType ->
  E.PrimType ->
  InternaliseM [I.SubExp]
internaliseBinOp desc E.LogAnd x y E.Bool _ =
  simpleBinOp desc I.LogAnd x y
internaliseBinOp desc E.LogOr x y E.Bool _ =
  simpleBinOp desc I.LogOr x y
internaliseBinOp desc E.Plus x y (E.Signed t) _ =
  simpleBinOp desc (I.Add t I.OverflowWrap) x y
internaliseBinOp desc E.Plus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Add t I.OverflowWrap) x y
internaliseBinOp desc E.Plus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FAdd t) x y
internaliseBinOp desc E.Minus x y (E.Signed t) _ =
  simpleBinOp desc (I.Sub t I.OverflowWrap) x y
internaliseBinOp desc E.Minus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Sub t I.OverflowWrap) x y
internaliseBinOp desc E.Minus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FSub t) x y
internaliseBinOp desc E.Times x y (E.Signed t) _ =
  simpleBinOp desc (I.Mul t I.OverflowWrap) x y
internaliseBinOp desc E.Times x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Mul t I.OverflowWrap) x y
internaliseBinOp desc E.Times x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMul t) x y
internaliseBinOp desc E.Divide x y (E.Signed t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.SDiv t I.Unsafe) x y
internaliseBinOp desc E.Divide x y (E.Unsigned t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.UDiv t I.Unsafe) x y
internaliseBinOp desc E.Divide x y (E.FloatType t) _ =
  simpleBinOp desc (I.FDiv t) x y
internaliseBinOp desc E.Pow x y (E.FloatType t) _ =
  simpleBinOp desc (I.FPow t) x y
internaliseBinOp desc E.Pow x y (E.Signed t) _ =
  certifyingNonnegative t y $
    simpleBinOp desc (I.Pow t) x y
internaliseBinOp desc E.Pow x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Pow t) x y
internaliseBinOp desc E.Mod x y (E.Signed t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.SMod t I.Unsafe) x y
internaliseBinOp desc E.Mod x y (E.Unsigned t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.UMod t I.Unsafe) x y
internaliseBinOp desc E.Mod x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMod t) x y
internaliseBinOp desc E.Quot x y (E.Signed t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.SQuot t I.Unsafe) x y
internaliseBinOp desc E.Quot x y (E.Unsigned t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.UDiv t I.Unsafe) x y
internaliseBinOp desc E.Rem x y (E.Signed t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.SRem t I.Unsafe) x y
internaliseBinOp desc E.Rem x y (E.Unsigned t) _ =
  certifyingNonzero t y $
    simpleBinOp desc (I.UMod t I.Unsafe) x y
internaliseBinOp desc E.ShiftR x y (E.Signed t) _ =
  simpleBinOp desc (I.AShr t) x y
internaliseBinOp desc E.ShiftR x y (E.Unsigned t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp desc E.ShiftL x y (E.Signed t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp desc E.ShiftL x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Shl t) x y
internaliseBinOp desc E.Band x y (E.Signed t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp desc E.Band x y (E.Unsigned t) _ =
  simpleBinOp desc (I.And t) x y
internaliseBinOp desc E.Xor x y (E.Signed t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp desc E.Xor x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Xor t) x y
internaliseBinOp desc E.Bor x y (E.Signed t) _ =
  simpleBinOp desc (I.Or t) x y
internaliseBinOp desc E.Bor x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Or t) x y
internaliseBinOp desc E.Equal x y t _ =
  simpleCmpOp desc (I.CmpEq $ internalisePrimType t) x y
internaliseBinOp desc E.NotEqual x y t _ = do
  eq <- letSubExp (desc <> "true") $ I.BasicOp $ I.CmpOp (I.CmpEq $ internalisePrimType t) x y
  fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp (I.Neg I.Bool) eq
internaliseBinOp desc E.Less x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSlt t) x y
internaliseBinOp desc E.Less x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUlt t) x y
internaliseBinOp desc E.Leq x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSle t) x y
internaliseBinOp desc E.Leq x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUle t) x y
internaliseBinOp desc E.Greater x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSlt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Greater x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUlt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.Signed t) _ =
  simpleCmpOp desc (I.CmpSle t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.Unsigned t) _ =
  simpleCmpOp desc (I.CmpUle t) y x -- Note the swapped x and y
internaliseBinOp desc E.Less x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLt t) x y
internaliseBinOp desc E.Leq x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLe t) x y
internaliseBinOp desc E.Greater x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLt t) y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y (E.FloatType t) _ =
  simpleCmpOp desc (I.FCmpLe t) y x -- Note the swapped x and y

-- Relational operators for booleans.
internaliseBinOp desc E.Less x y E.Bool _ =
  simpleCmpOp desc I.CmpLlt x y
internaliseBinOp desc E.Leq x y E.Bool _ =
  simpleCmpOp desc I.CmpLle x y
internaliseBinOp desc E.Greater x y E.Bool _ =
  simpleCmpOp desc I.CmpLlt y x -- Note the swapped x and y
internaliseBinOp desc E.Geq x y E.Bool _ =
  simpleCmpOp desc I.CmpLle y x -- Note the swapped x and y
internaliseBinOp _ op _ _ t1 t2 =
  error $
    "Invalid binary operator "
      ++ prettyString op
      ++ " with operand types "
      ++ prettyString t1
      ++ ", "
      ++ prettyString t2

simpleBinOp ::
  Name ->
  I.BinOp ->
  I.SubExp ->
  I.SubExp ->
  InternaliseM [I.SubExp]
simpleBinOp desc bop x y =
  letTupExp' desc $ I.BasicOp $ I.BinOp bop x y

simpleCmpOp ::
  Name ->
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

findFuncall :: E.AppExp -> (Function, [(E.Exp, Maybe VName)])
findFuncall (E.Apply f args _)
  | E.Var fname _ _ <- f =
      (FunctionName fname, map onArg $ NE.toList args)
  | E.Hole (Info _) loc <- f =
      (FunctionHole loc, map onArg $ NE.toList args)
  where
    onArg (Info argext, e) = (e, argext)
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
      rettype
      =<< bodyBind body

-- | Overloaded operators are treated here.
isOverloadedFunction ::
  E.QualName VName ->
  Name ->
  Maybe ([(E.StructType, [SubExp])] -> InternaliseM [SubExp])
isOverloadedFunction qname desc = do
  guard $ baseTag (qualLeaf qname) <= maxIntrinsicTag
  handle $ baseName $ qualLeaf qname
  where
    -- Handle equality and inequality specially, to treat the case of
    -- arrays.
    handle op
      | Just cmp_f <- isEqlOp op = Just $ \[(_, xe'), (_, ye')] -> do
          rs <- zipWithM doComparison xe' ye'
          cmp_f =<< letSubExp "eq" =<< eAll rs
      where
        isEqlOp "!=" = Just $ \eq ->
          letTupExp' desc $ I.BasicOp $ I.UnOp (I.Neg I.Bool) eq
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
                      letExp "x_flat" . I.BasicOp $
                        I.Reshape x' (reshapeAll (I.arrayShape x_t) (I.Shape [x_num_elems]))
                    y_flat <-
                      letExp "y_flat" . I.BasicOp $
                        I.Reshape y' (reshapeAll (I.arrayShape x_t) (I.Shape [x_num_elems]))

                    -- Compare the elements.
                    cmp_lam <- cmpOpLambda $ I.CmpEq (elemType x_t)
                    cmps <-
                      letExp "cmps"
                        . I.Op
                        . I.Screma x_num_elems [x_flat, y_flat]
                        =<< I.mapSOAC cmp_lam

                    -- Check that all were equal.
                    and_lam <- binOpLambda I.LogAnd I.Bool
                    reduce <- I.reduceSOAC [Reduce Commutative and_lam [constant True]]
                    all_equal <- letSubExp "all_equal" $ I.Op $ I.Screma x_num_elems [cmps] reduce
                    pure $ subExpsRes [all_equal]

              letSubExp "arrays_equal"
                =<< eIf (eSubExp shapes_match) compare_elems_body (resultBodyM [constant False])
    handle name
      | Just bop <-
          find
            ((name ==) . nameFromText . prettyText)
            [minBound .. maxBound :: E.BinOp] =
          Just $ \[(x_t, [x']), (y_t, [y'])] ->
            case (x_t, y_t) of
              (E.Scalar (E.Prim t1), E.Scalar (E.Prim t2)) ->
                internaliseBinOp desc bop x' y' t1 t2
              _ -> error "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."
    handle _ = Nothing

scatterF :: Int -> E.Exp -> E.Exp -> E.Exp -> Name -> InternaliseM [SubExp]
scatterF rank dest is v desc = do
  is' <- internaliseExpToVars "write_arg_i" is
  v' <- internaliseExpToVars "write_arg_v" v
  dest' <- internaliseExpToVars "write_arg_a" dest
  map I.Var
    <$> doScatter desc rank dest' (is' <> v') (pure . map (I.Var . I.paramName))

-- | Handle intrinsic functions.  These are only allowed to be called
-- in the prelude, and their internalisation may involve inspecting
-- the AST.
isIntrinsicFunction ::
  E.QualName VName ->
  [E.Exp] ->
  Maybe (Name -> InternaliseM [SubExp])
isIntrinsicFunction qname args = do
  guard $ baseTag (qualLeaf qname) <= maxIntrinsicTag
  let handlers =
        [ handleSign,
          handleOps,
          handleSOACs,
          handleAccs,
          handleAD,
          handleRest
        ]
  msum [h args $ baseName $ qualLeaf qname | h <- handlers]
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
      | Just unop <- find ((== s) . nameFromText . prettyText) allUnOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp unop x'
    handleOps [TupLit [x, y] _] s
      | Just bop <- find ((== s) . nameFromText . prettyText) allBinOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          y' <- internaliseExp1 "y" y
          fmap pure $ letSubExp desc $ I.BasicOp $ I.BinOp bop x' y'
      | Just cmp <- find ((== s) . nameFromText . prettyText) allCmpOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          y' <- internaliseExp1 "y" y
          fmap pure $ letSubExp desc $ I.BasicOp $ I.CmpOp cmp x' y'
    handleOps [x] s
      | Just conv <- find ((== s) . nameFromText . prettyText) allConvOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.ConvOp conv x'
    handleOps _ _ = Nothing

    handleSOACs [lam, arr] "map" = Just $ \desc -> do
      arr' <- internaliseExpToVars "map_arr" arr
      arr_ts <- mapM lookupType arr'
      lam' <- internaliseLambdaCoerce lam $ map rowType arr_ts
      let w = arraysSize 0 arr_ts
      letTupExp' desc . I.Op . I.Screma w arr' =<< I.mapSOAC lam'
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
      internaliseScanOrReduce desc "reduce" reduce (lam, ne, arr)
      where
        reduce w red_lam nes arrs =
          I.Screma w arrs
            <$> I.reduceSOAC [Reduce Noncommutative red_lam nes]
    handleSOACs [lam, ne, arr] "reduce_comm" = Just $ \desc ->
      internaliseScanOrReduce desc "reduce" reduce (lam, ne, arr)
      where
        reduce w red_lam nes arrs =
          I.Screma w arrs
            <$> I.reduceSOAC [Reduce Commutative red_lam nes]
    handleSOACs [lam, ne, arr] "scan" = Just $ \desc ->
      internaliseScanOrReduce desc "scan" reduce (lam, ne, arr)
      where
        reduce w scan_lam nes arrs =
          I.Screma w arrs <$> I.scanSOAC [Scan scan_lam nes]
    handleSOACs [rf, dest, op, ne, buckets, img] "hist_1d" = Just $ \desc ->
      internaliseHist 1 desc rf dest op ne buckets img
    handleSOACs [rf, dest, op, ne, buckets, img] "hist_2d" = Just $ \desc ->
      internaliseHist 2 desc rf dest op ne buckets img
    handleSOACs [rf, dest, op, ne, buckets, img] "hist_3d" = Just $ \desc ->
      internaliseHist 3 desc rf dest op ne buckets img
    handleSOACs _ _ = Nothing

    handleAccs [dest, f, bs] "scatter_stream" = Just $ \desc ->
      internaliseStreamAcc desc dest Nothing f bs
    handleAccs [dest, op, ne, f, bs] "hist_stream" = Just $ \desc ->
      internaliseStreamAcc desc dest (Just (op, ne)) f bs
    handleAccs [acc, i, v] "acc_write" = Just $ \desc -> do
      acc' <- head <$> internaliseExpToVars "acc" acc
      i' <- internaliseExp1 "acc_i" i
      vs <- internaliseExp "acc_v" v
      fmap pure $ letSubExp desc $ BasicOp $ UpdateAcc Safe acc' [i'] vs
    handleAccs _ _ = Nothing

    handleAD [f, x, v] fname
      | fname `elem` ["jvp2", "vjp2"] = Just $ \desc -> do
          x' <- internaliseExp "ad_x" x
          v' <- internaliseExp "ad_v" v
          lam <- internaliseLambdaCoerce f =<< mapM subExpType x'
          fmap (map I.Var) . letTupExp desc . Op $
            case fname of
              "jvp2" -> JVP x' v' lam
              _ -> VJP x' v' lam
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
        assert "dim_ok_cert" dim_ok $
          ErrorMsg
            [ "Cannot unflatten array of shape [",
              ErrorVal int64 old_dim,
              "] to array of shape [",
              ErrorVal int64 n',
              "][",
              ErrorVal int64 m',
              "]"
            ]
      certifying dim_ok_cert $
        forM arrs $ \arr' -> do
          arr_t <- lookupType arr'
          letSubExp desc . I.BasicOp $
            I.Reshape arr' $
              reshapeAll (I.arrayShape arr_t) $
                reshapeOuter (I.Shape [n', m']) 1 $
                  I.arrayShape arr_t
    handleRest [arr] "manifest" = Just $ \desc -> do
      arrs <- internaliseExpToVars "flatten_arr" arr
      forM arrs $ \arr' -> do
        r <- I.arrayRank <$> lookupType arr'
        if r == 0
          then pure $ I.Var arr'
          else letSubExp desc $ I.BasicOp $ I.Manifest arr' [0 .. r - 1]
    handleRest [arr] "flatten" = Just $ \desc -> do
      arrs <- internaliseExpToVars "flatten_arr" arr
      forM arrs $ \arr' -> do
        arr_t <- lookupType arr'
        let n = arraySize 0 arr_t
            m = arraySize 1 arr_t
        k <- letSubExp "flat_dim" $ I.BasicOp $ I.BinOp (Mul Int64 I.OverflowUndef) n m
        letSubExp desc . I.BasicOp $
          I.Reshape arr' $
            reshapeAll (I.arrayShape arr_t) $
              reshapeOuter (I.Shape [k]) 2 $
                I.arrayShape arr_t
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
        pure $ I.Rearrange v ([1, 0] ++ [2 .. r - 1])
    handleRest [x, y] "zip" = Just $ \desc ->
      mapM (letSubExp "zip_copy" . BasicOp . Replicate mempty . I.Var)
        =<< ( (++)
                <$> internaliseExpToVars (desc <> "_zip_x") x
                <*> internaliseExpToVars (desc <> "_zip_y") y
            )
    handleRest [x] "unzip" = Just $ \desc ->
      mapM (letSubExp desc . BasicOp . Replicate mempty . I.Var)
        =<< internaliseExpToVars desc x
    handleRest [arr, offset, n1, s1, n2, s2] "flat_index_2d" = Just $ \desc -> do
      flatIndexHelper desc arr offset [(n1, s1), (n2, s2)]
    handleRest [arr1, offset, s1, s2, arr2] "flat_update_2d" = Just $ \desc -> do
      flatUpdateHelper desc arr1 offset [s1, s2] arr2
    handleRest [arr, offset, n1, s1, n2, s2, n3, s3] "flat_index_3d" = Just $ \desc -> do
      flatIndexHelper desc arr offset [(n1, s1), (n2, s2), (n3, s3)]
    handleRest [arr1, offset, s1, s2, s3, arr2] "flat_update_3d" = Just $ \desc -> do
      flatUpdateHelper desc arr1 offset [s1, s2, s3] arr2
    handleRest [arr, offset, n1, s1, n2, s2, n3, s3, n4, s4] "flat_index_4d" = Just $ \desc -> do
      flatIndexHelper desc arr offset [(n1, s1), (n2, s2), (n3, s3), (n4, s4)]
    handleRest [arr1, offset, s1, s2, s3, s4, arr2] "flat_update_4d" = Just $ \desc -> do
      flatUpdateHelper desc arr1 offset [s1, s2, s3, s4] arr2
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

flatIndexHelper :: Name -> E.Exp -> E.Exp -> [(E.Exp, E.Exp)] -> InternaliseM [SubExp]
flatIndexHelper desc arr offset slices = do
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

  c <-
    assert "bounds_cert" all_bounds $
      ErrorMsg [ErrorString $ "Flat slice out of bounds: " <> prettyText old_dim <> " and " <> prettyText slices']
  let slice = I.FlatSlice offset' $ map (uncurry FlatDimIndex) slices'
  certifying c $
    forM arrs $ \arr' ->
      letSubExp desc $ I.BasicOp $ I.FlatIndex arr' slice

flatUpdateHelper :: Name -> E.Exp -> E.Exp -> [E.Exp] -> E.Exp -> InternaliseM [SubExp]
flatUpdateHelper desc arr1 offset slices arr2 = do
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

  c <-
    assert "bounds_cert" all_bounds $
      ErrorMsg [ErrorString $ "Flat slice out of bounds: " <> prettyText old_dim <> " and " <> prettyText slices']
  let slice = I.FlatSlice offset' $ map (uncurry FlatDimIndex) slices'
  certifying c $
    forM (zip arrs1 arrs2) $ \(arr1', arr2') ->
      letSubExp desc $ I.BasicOp $ I.FlatUpdate arr1' slice arr2'

funcall ::
  Name ->
  QualName VName ->
  [SubExp] ->
  InternaliseM [SubExp]
funcall desc (QualName _ fname) args = do
  (shapes, value_paramts, fun_params, rettype_fun) <- lookupFunction fname
  argts <- mapM subExpType args

  shapeargs <- argShapes shapes fun_params argts
  let diets =
        replicate (length shapeargs) I.ObservePrim
          ++ map I.diet value_paramts
  args' <-
    ensureArgShapes
      "function arguments of wrong shape"
      (map I.paramName fun_params)
      (map I.paramType fun_params)
      (shapeargs ++ args)
  argts' <- mapM subExpType args'
  case rettype_fun $ zip args' argts' of
    Nothing ->
      error . unlines $
        [ "Cannot apply "
            <> prettyString fname
            <> " to "
            <> show (length args')
            <> " arguments",
          " " <> prettyString args',
          "of types",
          " " <> prettyString argts',
          "Function has " <> show (length fun_params) <> " parameters",
          " " <> prettyString fun_params
        ]
    Just ts -> do
      safety <- askSafety
      attrs <- asks envAttrs
      attributing attrs . letValExp' desc $
        I.Apply (internaliseFunName fname) (zip args' diets) ts safety

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
  classes_and_increments <-
    letTupExp "increments"
      . I.Op
      . I.Screma w arrs
      =<< mapSOAC lam
  (classes, increments) <- case classes_and_increments of
    classes : increments -> pure (classes, take k increments)
    _ -> error "partitionWithSOACS"

  add_lam_x_params <-
    replicateM k $ newParam "x" (I.Prim int64)
  add_lam_y_params <-
    replicateM k $ newParam "y" (I.Prim int64)
  add_lam_body <- runBodyBuilder $
    localScope (scopeOfLParams $ add_lam_x_params ++ add_lam_y_params) $
      fmap subExpsRes $
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
        fmap subExpsRes $
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
  results <-
    doScatter "partition_res" 1 blanks (classes : all_offsets ++ arrs) $ \params -> do
      let ([c_param], offset_params, value_params) =
            splitAt3 1 (length all_offsets) params
      offset <-
        mkOffsetLambdaBody
          (map I.Var sizes)
          (I.Var $ I.paramName c_param)
          0
          offset_params
      pure $ offset : map (I.Var . I.paramName) value_params
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
sizeExpForError e
  | e == anySize = pure ["[]"]
  | otherwise = do
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

errorShape :: [a] -> ErrorMsg a
errorShape dims =
  "["
    <> mconcat (intersperse "][" $ map (ErrorMsg . pure . ErrorVal int64) dims)
    <> "]"
