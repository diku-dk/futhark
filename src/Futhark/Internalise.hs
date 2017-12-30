{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
--
-- This module implements a transformation from source to core
-- Futhark.
--
module Futhark.Internalise
  ( internaliseProg
  , internaliseValue
  )
  where

import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import Data.List
import Data.Loc

import Language.Futhark as E hiding (TypeArg)
import Language.Futhark.TypeChecker as E (Imports)
import Futhark.Representation.SOACS as I hiding (stmPattern)
import Futhark.Transform.Rename as I
import Futhark.Transform.Substitute
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias

import Futhark.Internalise.Monad as I
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings
import Futhark.Internalise.Lambdas
import Futhark.Internalise.Modules as Modules
import Futhark.Util (dropAt)

-- | Convert a program in source Futhark to a program in the Futhark
-- core language.
internaliseProg :: MonadFreshNames m =>
                   E.Imports -> m (Either String I.Prog)
internaliseProg prog = do
  prog_decs <- Modules.transformProg prog
  prog' <- fmap (fmap I.Prog) $ runInternaliseM $ internaliseDecs prog_decs
  traverse I.renameProg prog'

internaliseDecs :: [E.Dec] -> InternaliseM ()
internaliseDecs ds =
  case ds of
    [] ->
      return ()
    ValDec vdec : ds' ->
      internaliseValBind vdec $ internaliseDecs ds'
    E.TypeDec tb : ds' ->
      bindingType (E.typeAlias tb)
      (E.typeParams tb, E.unInfo $ E.expandedType $ E.typeExp tb) $
      internaliseDecs ds'
    _:ds' ->
      internaliseDecs ds'

internaliseFunName :: VName -> [E.Pattern] -> InternaliseM Name
internaliseFunName ofname [] = return $ nameFromString $ pretty ofname ++ "f"
internaliseFunName ofname _  = nameFromString . pretty <$> newVName (baseString ofname)

internaliseValBind :: E.ValBind -> InternaliseM a -> InternaliseM a
internaliseValBind fb@(E.ValBind entry fname _ (Info rettype) tparams params body _ loc) m = do
  let specialise (e_ts,_) = do
        let param_ts = map (E.removeShapeAnnotations . E.patternStructType) params

        mapping <- fmap mconcat $ zipWithM mapTypeVariables param_ts $
                   map E.removeShapeAnnotations e_ts

        let mkEntry (tp, et) = (tp, ([], E.vacuousShapeAnnotations et))
            types = map mkEntry $ M.toList mapping
        bindingTypes types $ bindingParams tparams params $ \pcm shapeparams params' -> do
          (rettype_bad, _, rcm) <- internaliseReturnType rettype
          let rettype' = zeroExts rettype_bad

          let mkConstParam name = Param name $ I.Prim int32
              constparams = map (mkConstParam . snd) $ pcm<>rcm
              constnames = map I.paramName constparams
              constscope = M.fromList $ zip constnames $ repeat $
                           FParamInfo $ I.Prim $ IntType Int32

              shapenames = map I.paramName shapeparams
              normal_params = map I.paramName constparams ++ shapenames ++
                              map I.paramName (concat params')
              normal_param_names = S.fromList normal_params

          fname' <- internaliseFunName fname params
          when (null tparams) $
            maybeSpecialiseEarly fname fname' (concat params') rettype'
          body' <- localScope constscope $
                   ensureResultExtShape asserting
                   "shape of function result does not match shapes in return type"
                   loc (map I.fromDecl rettype')
                   =<< internaliseBody body

          let free_in_fun = freeInBody body' `S.difference` normal_param_names

          used_free_params <- forM (S.toList free_in_fun) $ \v -> do
            v_t <- lookupType v
            return $ Param v $ toDecl v_t Nonunique

          let free_shape_params = map (`Param` I.Prim int32) $
                                  concatMap (I.shapeVars . I.arrayShape . I.paramType) used_free_params
              free_params = nub $ free_shape_params ++ used_free_params
              all_params = constparams ++ free_params ++ shapeparams ++ concat params'

          addFunction $ I.FunDef Nothing fname' rettype' all_params body'

          return (fname',
                  pcm<>rcm,
                  map I.paramName free_params,
                  shapenames,
                  map declTypeOf $ concat params',
                  all_params,
                  applyRetType rettype' all_params)

  bindingFunction fname specialise $ do
    -- For any nullary function we force immediate generation.  Note
    -- that this means value declarations are also generated here - this
    -- is important!
    when (null params) $ void $ lookupFunction fname ([],[])

    when entry $ generateEntryPoint fb

    m
  where
    -- | Recompute existential sizes to start from zero.
    -- Necessary because some convoluted constructions will start
    -- them from somewhere else.
    zeroExts ts = generaliseExtTypes ts ts

generateEntryPoint :: E.ValBind -> InternaliseM ()
generateEntryPoint (E.ValBind _ ofname _ (Info rettype) _ orig_params _ _ loc) =
  -- We remove all shape annotations, so there should be no constant
  -- parameters here.
  bindingParams [] (map E.patternNoShapeAnnotations params) $
  \_ shapeparams params' -> do
    (entry_rettype, _, _) <- internaliseEntryReturnType $
                             E.vacuousShapeAnnotations rettype
    let entry' = entryPoint (zip params params') (rettype, entry_rettype)
        args = map (I.Var . I.paramName) $ concat params'
        e_ts = map (flip setAliases () . E.patternType) orig_params
        i_ts = staticShapes $ map I.paramType $ concat params'

    entry_body <- insertStmsM $ do
      vals <- fst <$> funcall "entry_result" (E.qualName ofname)
              (e_ts,map rankShaped i_ts) args loc
      ctx <- extractShapeContext (concat entry_rettype) <$>
             mapM (fmap I.arrayDims . subExpType) vals
      resultBodyM (ctx ++ vals)

    addFunction $
      I.FunDef (Just entry') (baseName ofname)
      (concat entry_rettype)
      (shapeparams ++ concat params') entry_body

  -- XXX: We massage the parameters a little bit to handle the case
  -- where there is just a single parameter that is a tuple.  This is
  -- wide-spread in existing Futhark code, although I'd like to get
  -- rid of it.
  where params = case orig_params of
          [TuplePattern ps _] -> ps
          _                   -> orig_params

entryPoint :: [(E.Pattern,[I.FParam])]
           -> (E.StructType,[[I.TypeBase ExtShape Uniqueness]])
           -> EntryPoint
entryPoint params (eret,crets) =
  (concatMap (uncurry entryPointType . preParam) params,
   case isTupleRecord eret of
     Just ts -> concat $ zipWith entryPointType ts crets
     _       -> entryPointType eret $ concat crets)
  where preParam = E.patternStructType *** staticShapes . map I.paramDeclType

        entryPointType :: E.StructType
                       -> [I.TypeBase ExtShape Uniqueness]
                       -> [EntryPointType]
        entryPointType (E.Prim E.Unsigned{}) _ =
          [I.TypeUnsigned]
        entryPointType (E.Array (PrimArray Unsigned{} _ _ _)) _ =
          [I.TypeUnsigned]
        entryPointType E.Prim{} _ =
          [I.TypeDirect]
        entryPointType (E.Array PrimArray{}) _ =
          [I.TypeDirect]
        entryPointType t ts =
          [I.TypeOpaque (pretty t') $ length ts]
          where t' = removeShapeAnnotations t `E.setUniqueness` Nonunique

internaliseIdent :: E.Ident -> InternaliseM I.VName
internaliseIdent (E.Ident name (Info tp) loc) =
  case tp of
    E.Prim{} -> return name
    _        -> fail $ "Futhark.Internalise.internaliseIdent: asked to internalise non-prim-typed ident '"
                       ++ pretty name ++ " of type " ++ pretty tp ++
                       " at " ++ locStr loc ++ "."

internaliseBody :: E.Exp -> InternaliseM Body
internaliseBody e = insertStmsM $ do
  ses <- internaliseExp "res" e
  return $ resultBody ses

internaliseBodyStms :: E.Exp -> ([SubExp] -> InternaliseM (Body, a))
                    -> InternaliseM (Body, a)
internaliseBodyStms e m = do
  ((Body _ bnds res,x), otherbnds) <-
    collectStms $ m =<< internaliseExp "res" e
  (,x) <$> mkBodyM (otherbnds <> bnds) res

internaliseExp :: String -> E.Exp -> InternaliseM [I.SubExp]

internaliseExp desc (E.Parens e _) =
  internaliseExp desc e

internaliseExp desc (E.QualParens _ e _) =
  internaliseExp desc e

internaliseExp _ (E.Var (E.QualName _ name) t loc) = do
  -- If this identifier is the name of a constant, we have to turn it
  -- into a call to the corresponding function.
  is_const <- lookupConstant loc name
  case is_const of
    Just ses ->
      return ses
    _ -> do
      subst <- asks $ M.lookup name . envSubsts
      case subst of
        Nothing     -> (:[]) . I.Var <$> internaliseIdent (E.Ident name (snd <$> t) loc)
        Just substs -> return substs

internaliseExp desc (E.Index e idxs loc) = do
  vs <- internaliseExpToVars "indexed" e
  dims <- case vs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', idx_cs) <- unzip <$> zipWithM (internaliseDimIndex loc) dims idxs
  let index v = do
        v_t <- lookupType v
        return $ I.BasicOp $ I.Index v $ fullSlice v_t idxs'
  certifying (mconcat idx_cs) $
    letSubExps desc =<< mapM index vs

internaliseExp desc (E.TupLit es _) =
  concat <$> mapM (internaliseExp desc) es

internaliseExp desc (E.RecordLit orig_fields _) =
  concatMap snd . sortFields . M.unions . reverse <$> mapM internaliseField orig_fields
  where internaliseField (E.RecordFieldExplicit name e _) =
          M.singleton name <$> internaliseExp desc e
        internaliseField (E.RecordFieldImplicit name t loc) =
          internaliseField $ E.RecordFieldExplicit (baseName name)
          (E.Var (E.qualName name) (([],) <$> t) loc) loc

internaliseExp desc (E.ArrayLit es (Info rowtype) loc)
  -- If this is a multidimensional array literal of primitives, we
  -- treat it specially by flattening it out followed by a reshape.
  -- This cuts down on the amount of statements that are produced, and
  -- thus allows us to efficiently handle huge array literals - a
  -- corner case, but an important one.
  | Just ((eshape,e'):es') <- mapM isArrayLiteral es,
    not $ null eshape,
    all ((eshape==) . fst) es',
    Just basetype <- E.peelArray (length eshape) rowtype =
      let flat_lit = E.ArrayLit (e' ++ concatMap snd es') (Info basetype) loc
          new_shape = E.TupLit [E.Literal (E.primValue k) loc
                               | k <- length es:eshape] loc
      in internaliseExp desc $ E.Reshape new_shape flat_lit loc

  | otherwise = do
  es' <- mapM (internaliseExp "arr_elem") es
  case es' of
    [] -> do
      rowtypes <- internaliseType (rowtype `setAliases` ())
      let arraylit rt = I.BasicOp $ I.ArrayLit [] rt
      letSubExps desc $ map (arraylit . zeroDim . fromDecl) rowtypes
    e' : _ -> do
      rowtypes <- mapM subExpType e'
      let arraylit ks rt = do
            ks' <- mapM (ensureShape asserting "shape of element differs from shape of first element"
                         loc rt "elem_reshaped") ks
            return $ I.BasicOp $ I.ArrayLit ks' rt
      letSubExps desc =<< zipWithM arraylit (transpose es') rowtypes
  where zeroDim t = t `I.setArrayShape`
                    I.Shape (replicate (I.arrayRank t) (constant (0::Int32)))

        isArrayLiteral :: E.Exp -> Maybe ([Int],[E.Exp])
        isArrayLiteral (E.ArrayLit inner_es _ _) = do
          (eshape,e):inner_es' <- mapM isArrayLiteral inner_es
          guard $ all ((eshape==) . fst) inner_es'
          return (length inner_es:eshape, e ++ concatMap snd inner_es')
        isArrayLiteral e =
          Just ([], [e])

internaliseExp desc (E.Range start maybe_second end _) = do
  start' <- internaliseExp1 "range_start" start
  end' <- internaliseExp1 "range_end" $ case end of
    DownToExclusive e -> e
    ToInclusive e -> e
    UpToExclusive e -> e

  (it, le_op, lt_op) <-
    case E.typeOf start of
      E.Prim (E.Signed it) -> return (it, CmpSle it, CmpSlt it)
      E.Prim (E.Unsigned it) -> return (it, CmpUle it, CmpUlt it)
      start_t -> fail $ "Start value in range has type " ++ pretty start_t

  let one = intConst it 1
      negone = intConst it (-1)
      default_step = case end of DownToExclusive{} -> negone
                                 ToInclusive{} -> one
                                 UpToExclusive{} -> one

  (step, step_zero) <- case maybe_second of
    Just second -> do
      second' <- internaliseExp1 "range_second" second
      subtracted_step <- letSubExp "subtracted_step" $ I.BasicOp $ I.BinOp (I.Sub it) second' start'
      step_zero <- letSubExp "step_zero" $ I.BasicOp $ I.CmpOp (I.CmpEq $ IntType it) start' second'
      return (subtracted_step, step_zero)
    Nothing ->
      return (default_step, constant False)

  step_sign <- letSubExp "s_sign" $ BasicOp $ I.UnOp (I.SSignum it) step
  step_sign_i32 <- asIntS Int32 step_sign

  bounds_invalid_downwards <- letSubExp "bounds_invalid_downwards" $
                              I.BasicOp $ I.CmpOp le_op start' end'
  bounds_invalid_upwards <- letSubExp "bounds_invalid_upwards" $
                            I.BasicOp $ I.CmpOp lt_op end' start'

  (distance, step_wrong_dir, bounds_invalid) <- case end of
    DownToExclusive{} -> do
      step_wrong_dir <- letSubExp "step_wrong_dir" $
                        I.BasicOp $ I.CmpOp (I.CmpEq $ IntType it) step_sign one
      distance <- letSubExp "distance" $
                  I.BasicOp $ I.BinOp (Sub it) start' end'
      distance_i32 <- asIntZ Int32 distance
      return (distance_i32, step_wrong_dir, bounds_invalid_downwards)
    UpToExclusive{} -> do
      step_wrong_dir <- letSubExp "step_wrong_dir" $
                        I.BasicOp $ I.CmpOp (I.CmpEq $ IntType it) step_sign negone
      distance <- letSubExp "distance" $ I.BasicOp $ I.BinOp (Sub it) end' start'
      distance_i32 <- asIntZ Int32 distance
      return (distance_i32, step_wrong_dir, bounds_invalid_upwards)
    ToInclusive{} -> do
      downwards <- letSubExp "downwards" $
                   I.BasicOp $ I.CmpOp (I.CmpEq $ IntType it) step_sign negone
      distance_downwards_exclusive <-
        letSubExp "distance_downwards_exclusive" $
        I.BasicOp $ I.BinOp (Sub it) start' end'
      distance_upwards_exclusive <-
        letSubExp "distance_upwards_exclusive" $
        I.BasicOp $ I.BinOp (Sub it) end' start'

      bounds_invalid <- letSubExp "bounds_invalid" $
                        I.If downwards
                        (resultBody [bounds_invalid_downwards])
                        (resultBody [bounds_invalid_upwards]) $
                        ifCommon [I.Prim I.Bool]
      distance_exclusive <- letSubExp "distance_exclusive" $
                            I.If downwards
                            (resultBody [distance_downwards_exclusive])
                            (resultBody [distance_upwards_exclusive]) $
                            ifCommon [I.Prim $ IntType it]
      distance_exclusive_i32 <- asIntZ Int32 distance_exclusive
      distance <- letSubExp "distance" $
                  I.BasicOp $ I.BinOp (Add Int32)
                  distance_exclusive_i32 (intConst Int32 1)
      return (distance, constant False, bounds_invalid)

  step_invalid <- letSubExp "step_invalid" $
                  I.BasicOp $ I.BinOp I.LogOr step_wrong_dir step_zero
  invalid <- letSubExp "range_invalid" $
             I.BasicOp $ I.BinOp I.LogOr step_invalid bounds_invalid

  step_i32 <- asIntS Int32 step
  pos_step <- letSubExp "pos_step" $
              I.BasicOp $ I.BinOp (Mul Int32) step_i32 step_sign_i32
  num_elems <- letSubExp "num_elems" =<<
               eIf (eSubExp invalid)
               (eBody [eSubExp $ intConst Int32 0])
               (eBody [eDivRoundingUp Int32 (eSubExp distance) (eSubExp pos_step)])
  pure <$> letSubExp desc (I.BasicOp $ I.Iota num_elems start' step it)

internaliseExp desc (E.Empty (TypeDecl _(Info et)) _) = do
  (ts, _, _) <- internaliseReturnType et
  let ts' = map (fromDecl . modifyArrayShape extToZero) ts
  letSubExps desc $ map (I.BasicOp . I.ArrayLit []) ts'
  where extToZero (I.Shape dims) = I.Shape $ map extDimToZero dims
        extDimToZero I.Ext{} = constant (0::Int32)
        extDimToZero (I.Free d) = d

internaliseExp desc (E.Ascript e (TypeDecl _ (Info et)) loc) = do
  es <- internaliseExp desc e
  (ts, _, cm) <- internaliseReturnType et
  mapM_ (uncurry (internaliseDimConstant loc)) cm
  forM (zip es ts) $ \(e',t') ->
    ensureExtShape asserting "value does not match shape in type"
    loc (I.fromDecl t') desc e'

internaliseExp desc (E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.Sub t) (I.intConst t 0) e'
             I.Prim (I.FloatType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.FSub t) (I.floatConst t 0) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in Negate"

internaliseExp desc e@E.Apply{} = do
  (qfname, args, _) <- findFuncall e
  let fname = nameFromString $ pretty $ baseName $ qualLeaf qfname
      loc = srclocOf e

  -- Some functions are magical (overloaded) and we handle that here.
  -- Note that polymorphic functions (which are not magical) are not
  -- handled here.
  case () of
    () | Just internalise <- isOverloadedFunction qfname args loc ->
           internalise desc
       | Just (rettype, _) <- M.lookup fname I.builtInFunctions -> do
           let tag ses = [ (se, I.Observe) | se <- ses ]
           args' <- mapM (internaliseExp "arg") args
           let args'' = concatMap tag args'
           letTupExp' desc $ I.Apply fname args'' [I.Prim rettype] (Safe, loc, [])
       | otherwise -> do
           args' <- concat <$> mapM (internaliseExp "arg") args
           i_ts <- staticShapes <$> mapM subExpType args'
           let e_ts = map (flip setAliases () . E.typeOf) args
           fst <$> funcall desc qfname (e_ts, map I.rankShaped i_ts) args' loc

internaliseExp desc (E.LetPat tparams pat e body loc) = do
  ses <- internaliseExp desc e
  t <- I.staticShapes <$> mapM I.subExpType ses
  stmPattern tparams pat t $ \cm pat_names match -> do
    mapM_ (uncurry (internaliseDimConstant loc)) cm
    ses' <- match loc ses
    forM_ (zip pat_names ses') $ \(v,se) ->
      letBindNames'_ [v] $ I.BasicOp $ I.SubExp se
    internaliseExp desc body

internaliseExp desc (E.LetFun ofname (tparams, params, retdecl, Info rettype, body) letbody loc) =
  internaliseValBind (E.ValBind False ofname retdecl (Info rettype) tparams params body Nothing loc) $
  internaliseExp desc letbody

internaliseExp desc (E.DoLoop tparams mergepat mergeexp form loopbody loc) = do
  -- We pretend that we saw a let-binding first to ensure that the
  -- initial values for the merge parameters match their annotated
  -- sizes
  ses <- internaliseExp "loop_init" mergeexp
  t <- I.staticShapes <$> mapM I.subExpType ses
  stmPattern tparams mergepat t $ \cm mergepat_names match -> do
    mapM_ (uncurry (internaliseDimConstant loc)) cm
    ses' <- match (srclocOf mergepat) ses
    forM_ (zip mergepat_names ses') $ \(v,se) ->
      letBindNames'_ [v] $ I.BasicOp $ I.SubExp se
    let mergeinit = map I.Var mergepat_names

    (loopbody', (form', shapepat, mergepat', mergeinit', pre_stms)) <-
      handleForm mergeinit form

    addStms pre_stms

    mergeinit_ts' <- mapM subExpType mergeinit'

    let ctxinit = argShapes
                  (map I.paramName shapepat)
                  (map I.paramType mergepat')
                  mergeinit_ts'
        ctxmerge = zip shapepat ctxinit
        valmerge = zip mergepat' mergeinit'
        dropCond = case form of E.While{} -> drop 1
                                _         -> id

    loop_res <- letTupExp desc $ I.DoLoop ctxmerge valmerge form' loopbody'
    return $ map I.Var $ dropCond loop_res

  where
    handleForm mergeinit (E.ForIn x arr) = do
      arr' <- internaliseExpToVars "for_in_arr" arr
      arr_ts <- mapM lookupType arr'
      let w = arraysSize 0 arr_ts

      i <- newVName "i"

      bindingIdentTypes [I.Ident i $ I.Prim $ IntType Int32] $
        bindingParams tparams [mergepat] $ \mergecm shapepat nested_mergepat ->
        bindingLambdaParams [] [x] (map rowType arr_ts) $ \x_cm x_params -> do
          mapM_ (uncurry (internaliseDimConstant loc)) x_cm
          mapM_ (uncurry (internaliseDimConstant loc)) mergecm
          let loopvars = zip x_params arr'
          internaliseBodyStms loopbody $ \ses -> do
            sets <- mapM subExpType ses
            let mergepat' = concat nested_mergepat
                shapeargs = argShapes
                            (map I.paramName shapepat)
                            (map I.paramType mergepat')
                            sets
            return (resultBody $ shapeargs ++ ses,
                    (I.ForLoop i Int32 w loopvars,
                     shapepat,
                     mergepat',
                     mergeinit,
                     mempty))

    handleForm mergeinit (E.For i num_iterations) = do
      num_iterations' <- internaliseExp1 "upper_bound" num_iterations
      i' <- internaliseIdent i
      num_iterations_t <- I.subExpType num_iterations'
      it <- case num_iterations_t of
              I.Prim (IntType it) -> return it
              _                   -> fail "internaliseExp DoLoop: invalid type"

      let i_ident = I.Ident i' $ I.Prim $ IntType it

      bindingIdentTypes [i_ident] $ bindingParams tparams [mergepat] $
        \mergecm shapepat nested_mergepat -> do
        mapM_ (uncurry (internaliseDimConstant loc)) mergecm
        internaliseBodyStms loopbody $ \ses -> do
          sets <- mapM subExpType ses
          let mergepat' = concat nested_mergepat
              shapeargs = argShapes
                          (map I.paramName shapepat)
                          (map I.paramType mergepat')
                          sets
          return (resultBody $ shapeargs ++ ses,
                  (I.ForLoop i' it num_iterations' [],
                   shapepat,
                   mergepat',
                   mergeinit,
                   mempty))

    handleForm mergeinit (E.While cond) =
      bindingParams tparams [mergepat] $ \mergecm shapepat nested_mergepat -> do
        mergeinit_ts <- mapM subExpType mergeinit
        mapM_ (uncurry (internaliseDimConstant loc)) mergecm
        let mergepat' = concat nested_mergepat
        -- We need to insert 'cond' twice - once for the initial
        -- condition (do we enter the loop at all?), and once with
        -- the result values of the loop (do we continue into the
        -- next iteration?).  This is safe, as the type rules for
        -- the external language guarantees that 'cond' does not
        -- consume anything.
        let shapeinit = argShapes
                        (map I.paramName shapepat)
                        (map I.paramType mergepat')
                        mergeinit_ts
            initsubst = [ (I.paramName mergeparam, initval)
                        | (mergeparam, initval) <-
                            zip (shapepat++mergepat') (shapeinit++mergeinit)
                        ]

        internaliseBodyStms loopbody $ \ses -> do
          sets <- mapM subExpType ses
          loop_while <- newParam "loop_while" $ I.Prim I.Bool
          (loop_cond, loop_cond_bnds) <-
            collectStms $ internaliseExp1 "loop_cond" cond
          let endsubst = [ (I.paramName mergeparam, endval)
                         | (mergeparam, endval) <-
                              zip (shapepat++mergepat') (shapeargs++ses)
                         ]
              shapeargs = argShapes
                          (map I.paramName shapepat)
                          (map I.paramType mergepat')
                          sets
          (loop_initial_cond, init_loop_cond_bnds) <-
            collectStms $
            shadowIdentsInExp initsubst loop_cond_bnds loop_cond
          (loop_end_cond, loop_end_cond_bnds) <-
            collectStms $ shadowIdentsInExp endsubst loop_cond_bnds loop_cond
          return (mkBody loop_end_cond_bnds $ shapeargs++[loop_end_cond]++ses,
                  (I.WhileLoop $ I.paramName loop_while,
                   shapepat,
                   loop_while : mergepat',
                   loop_initial_cond : mergeinit,
                   init_loop_cond_bnds))


internaliseExp desc (E.LetWith name src idxs ve body loc) = do
  srcs <- internaliseExpToVars "src" $
          E.Var (qualName (E.identName src)) (([],) <$> E.identType src) (srclocOf src)
  ves <- internaliseExp "lw_val" ve
  dims <- case srcs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', idx_cs) <- unzip <$> zipWithM (internaliseDimIndex loc) dims idxs
  let comb sname ve' = do
        sname_t <- lookupType sname
        let slice = fullSlice sname_t idxs'
            rowtype = sname_t `setArrayDims` sliceDims slice
        ve'' <- ensureShape asserting "shape of value does not match shape of source array"
                loc rowtype "lw_val_correct_shape" ve'
        certifying (mconcat idx_cs) $
          letInPlace "letwith_dst" sname (fullSlice sname_t idxs') $ BasicOp $ SubExp ve''
  dsts <- zipWithM comb srcs ves
  dstt <- I.staticShapes <$> mapM lookupType dsts
  let pat = E.Id (E.identName name)
            (E.vacuousShapeAnnotations <$> E.identType name)
            (srclocOf name)
  stmPattern [] pat dstt $ \cm pat_names match -> do
    mapM_ (uncurry (internaliseDimConstant loc)) cm
    dsts' <- match loc $ map I.Var dsts
    forM_ (zip pat_names dsts') $ \(v,dst) ->
      letBindNames'_ [v] $ I.BasicOp $ I.SubExp dst
    internaliseExp desc body

internaliseExp desc (E.Update src slice ve loc) = do
  src_name <- newVName "update_src"
  dest_name <- newVName "update_dest"
  let src_t = E.typeOf src
      src_ident = E.Ident src_name (E.Info src_t) loc
      dest_ident = E.Ident dest_name (E.Info src_t) loc

  internaliseExp desc $
    E.LetPat [] (E.Id src_name (E.Info $ E.vacuousShapeAnnotations src_t) loc) src
    (E.LetWith dest_ident src_ident slice ve
      (E.Var (E.qualName dest_name) (E.Info ([], src_t)) loc)
      loc)
    loc

internaliseExp desc (E.Unzip e _ _) =
  internaliseExp desc e

internaliseExp desc (E.Unsafe e _) =
  local (\env -> env { envDoBoundsChecks = False }) $
  internaliseExp desc e

internaliseExp _ (E.Zip _ e es loc) = do
  e' <- internaliseExpToVars "zip_arg" $ TupLit (e:es) loc
  case e' of
    e_key:es_unchecked -> do
      -- We will reshape all of es_unchecked' to have the same outer
      -- size as ts.  We will not change any of the inner dimensions.
      -- This will cause a runtime error if the outer sizes do not match,
      -- thus preserving the semantics of zip().
      w <- arraySize 0 <$> lookupType e_key
      let reshapeToOuter e_unchecked' = do
            unchecked_t <- lookupType e_unchecked'
            case I.arrayDims unchecked_t of
              []      -> return e_unchecked' -- Probably type error
              outer:inner -> do
                cmp <- letSubExp "zip_cmp" $ I.BasicOp $
                       I.CmpOp (I.CmpEq I.int32) w outer
                c   <- assertingOne $
                       letExp "zip_assert" $ I.BasicOp $
                       I.Assert cmp "arrays differ in length" (loc, mempty)
                certifying c $ letExp (postfix e_unchecked' "_zip_res") $
                  shapeCoerce (w:inner) e_unchecked'
      es' <- mapM reshapeToOuter es_unchecked
      return $ map I.Var $ e_key : es'
    [] -> return []

  where postfix i s = baseString i ++ s

internaliseExp _ (E.Rearrange perm e _) =
  internaliseOperation "rearrange" e $ \v ->
    return $ I.Rearrange perm v

internaliseExp _ (E.Rotate d offset e _) = do
  offset' <- internaliseExp1 "rotation_offset" offset
  internaliseOperation "rotate" e $ \v -> do
    r <- I.arrayRank <$> lookupType v
    let zero = constant (0::Int32)
        offsets = replicate d zero ++ [offset'] ++ replicate (r-d-1) zero
    return $ I.Rotate offsets v

internaliseExp _ (E.Reshape shape e loc) = do
  shape' <- internaliseShapeExp "shape" shape
  vs <- internaliseExpToVars "reshape_arg" e
  forM vs $ \v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    old_shape <- I.arrayShape <$> lookupType v
    let changed_dims = take orig_rank $ I.shapeDims old_shape
    shapeOk <- assertingOne $
               letExp "shape_ok" =<<
               eAssert (eCmpOp (I.CmpEq I.int32) (prod changed_dims) (prod shape'))
               "new shape has different number of elements than old shape" loc
    certifying shapeOk $ letSubExp "reshape" $
      I.BasicOp $ I.Reshape (reshapeOuter (DimNew <$> shape') orig_rank old_shape) v
  where prod = foldBinOp (I.Mul I.Int32) (constant (1 :: I.Int32))
        orig_rank = E.arrayRank $ E.typeOf e

internaliseExp _ (E.Split i splitexp arrexp loc) = do
  splits' <- internaliseExp "n" splitexp
  -- Note that @arrs@ is an array, because of array-of-tuples transformation
  arrs <- internaliseExpToVars "split_arr" arrexp
  split_dim <- arraysSize i <$> mapM lookupType arrs

  -- Assertions
  indexAsserts <- asserting $ do
    let indexConds = zipWith (\beg end -> BasicOp $ I.CmpOp (I.CmpSle I.Int32) beg end)
                     (I.constant (0 :: I.Int32):splits') (splits'++[split_dim])
    indexChecks <- mapM (letSubExp "split_index_cnd") indexConds
    fmap Certificates $ forM indexChecks $ \cnd ->
      letExp "split_index_assert" $ BasicOp $ I.Assert cnd "index out of bounds" (loc, mempty)

  -- Calculate diff between each split index
  let sizeExps = zipWith (\beg end -> BasicOp $ I.BinOp (I.Sub I.Int32) end beg)
                 (I.constant (0 :: I.Int32):splits') (splits'++[split_dim])
  sizeVars <- mapM (letSubExp "split_size") sizeExps
  splitExps <- certifying indexAsserts $ forM arrs $ \arr ->
    letTupExp' "split_res" $ BasicOp $ I.Split i sizeVars arr

  return $ concat $ transpose splitExps

internaliseExp desc (E.Concat i x ys loc) = do
  xs  <- internaliseExpToVars "concat_x" x
  yss <- mapM (internaliseExpToVars "concat_y") ys
  outer_size <- arraysSize i <$> mapM lookupType xs
  ressize <- foldM sumdims outer_size =<<
             mapM (fmap (arraysSize i) . mapM lookupType) yss

  let conc xarr yarrs = do
        -- All dimensions except for dimension 'i' must match.
        xt  <- lookupType xarr
        yts <- mapM lookupType yarrs
        let matches n m =
              letExp "match" =<<
              eAssert (pure $ I.BasicOp $ I.CmpOp (I.CmpEq I.int32) n m)
              "arguments do not have the same row shape" loc
            x_inner_dims  = dropAt i 1 $ I.arrayDims xt
            ys_inner_dims = map (dropAt i 1 . I.arrayDims) yts
            updims = zipWith3 updims' [0..] (I.arrayDims xt)
            updims' j xd yd | i == j    = yd
                            | otherwise = xd
        matchcs <- asserting $ Certificates . concat <$>
                   mapM (zipWithM matches x_inner_dims) ys_inner_dims
        yarrs'  <- forM yarrs $ \yarr -> do
          yt <- lookupType yarr
          certifying matchcs $ letExp "concat_y_reshaped" $
            shapeCoerce (updims $ I.arrayDims yt) yarr
        return $ I.BasicOp $ I.Concat i xarr yarrs' ressize
  letSubExps desc =<< zipWithM conc xs (transpose yss)

    where
        sumdims xsize ysize = letSubExp "conc_tmp" $ I.BasicOp $
                                        I.BinOp (I.Add I.Int32) xsize ysize

internaliseExp _ (E.Map _ [] _) = return []

internaliseExp desc (E.Map lam (arr:arrs) loc) = do
  -- Pretend the arrs were zipped to get the necessary reshapes in.
  -- This would be a type error in the source language, but it's the
  -- same in the core language.
  arrs' <- internaliseExpToVars "map_arr" (Zip 0 arr arrs loc)
  lam' <- internaliseMapLambda internaliseLambda lam $ map I.Var arrs'
  w <- arraysSize 0 <$> mapM lookupType arrs'
  letTupExp' desc $ I.Op $ I.Map w lam' arrs'

internaliseExp desc (E.Reduce comm lam ne arr loc) =
  internaliseScanOrReduce desc "reduce" (`I.Reduce` comm) (lam, ne, arr, loc)

internaliseExp desc (E.Scan lam ne arr loc) =
  internaliseScanOrReduce desc "scan" I.Scan (lam, ne, arr, loc)

internaliseExp desc (E.Filter lam arr _) = do
  arrs <- internaliseExpToVars "filter_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda [lam] $ map I.Var arrs
  (partition_sizes, partitioned) <- partitionWithSOACS 1 lam' arrs
  fmap (map I.Var . concat . transpose) $ forM partitioned $
    letTupExp desc . I.BasicOp . I.Split 0 partition_sizes

internaliseExp desc (E.Partition lams arr _) = do
  arrs <- internaliseExpToVars "partition_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda lams $ map I.Var arrs
  (partition_sizes, partitioned) <- partitionWithSOACS (k+1) lam' arrs
  fmap (map I.Var . concat . transpose) $ forM partitioned $
    letTupExp desc . I.BasicOp . I.Split 0 partition_sizes
  where k = length lams

internaliseExp desc (E.Stream form lam arr _) = do
  arrs <- internaliseExpToVars "stream_input" arr

  rowts <- mapM (fmap I.rowType . lookupType) arrs
  lam' <- internaliseStreamLambda internaliseLambda lam rowts

  -- If the stream form is a reduce, we also have to fiddle with the
  -- lambda to incorporate the reduce function.  FIXME: can't we just
  -- modify the internal representation of reduction streams?
  (form', lam'') <-
    case form of
      E.MapLike o -> do
        let (chunk_param, _, _) =
              partitionChunkedFoldParameters 0 $ I.extLambdaParams lam'
            ts = map (`setOuterSize` Free (I.Var $ I.paramName chunk_param)) $
                 extLambdaReturnType lam'

        body <- localScope (scopeOfLParams $ I.extLambdaParams lam') $
                ensureResultExtShape asserting
                "size of result not equal to chunk size" (srclocOf lam) ts $
                extLambdaBody lam'

        let lam'' = lam' { extLambdaReturnType = ts
                         , extLambdaBody = body
                         }
        return (I.Parallel o Commutative (I.Lambda [] (mkBody mempty []) []) [], lam'')
      E.RedLike o comm lam0 -> do
        -- Synthesize neutral elements by applying the fold function
        -- to an empty chunk.
        accs <- do
          let (chunk_param, _, lam_params) =
                partitionChunkedFoldParameters 0 $ I.extLambdaParams lam'
          letBindNames'_ [I.paramName chunk_param] $
            I.BasicOp $ I.SubExp $ constant (0::Int32)
          forM_ lam_params $ \p ->
            letBindNames'_ [I.paramName p] $
            I.BasicOp $ I.Scratch (I.elemType $ I.paramType p) $
            I.arrayDims $ I.paramType p
          bodyBind =<< renameBody (I.extLambdaBody lam')

        acctps <- mapM I.subExpType accs
        outsz  <- arraysSize 0 <$> mapM lookupType arrs
        let acc_arr_tps = [ I.arrayOf t (I.Shape [outsz]) NoUniqueness | t <- acctps ]
        lam0'  <- internaliseFoldLambda internaliseLambda lam0 acctps acc_arr_tps
        let lam0_acc_params = fst $ splitAt (length accs) $ I.lambdaParams lam0'
        acc_params <- forM lam0_acc_params $ \p -> do
          name <- newVName $ baseString $ I.paramName p
          return p { I.paramName = name }

        body_with_lam0 <-
          ensureResultShape asserting "shape of result does not match shape of initial value"
          (srclocOf lam) acctps <=< runBodyBinder $ do
            lam_res <- bodyBind $ extLambdaBody lam'

            let consumed = consumedByLambda $ Alias.analyseLambda lam0'
                copyIfConsumed p (I.Var v)
                  | I.paramName p `S.member` consumed =
                      letSubExp "acc_copy" $ I.BasicOp $ I.Copy v
                copyIfConsumed _ x = return x

            accs' <- zipWithM copyIfConsumed (I.lambdaParams lam0') accs
            new_lam_res <- eLambda lam0' $ accs' ++ lam_res
            return $ resultBody new_lam_res

        -- Make sure the chunk size parameter comes first.
        return (I.Parallel o comm lam0' accs,
                lam' { extLambdaParams = take 1 (extLambdaParams lam') <>
                                         acc_params <>
                                         drop 1 (extLambdaParams lam')
                     , extLambdaBody = body_with_lam0
                     , extLambdaReturnType = staticShapes acctps })
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ I.Stream w form' lam'' arrs

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp _ (E.Literal v _) =
  return [I.Constant $ internalisePrimValue v]

internaliseExp desc (E.If ce te fe _ _) =
  letTupExp' desc =<< eIf (BasicOp . SubExp <$> internaliseExp1 "cond" ce)
                          (internaliseBody te) (internaliseBody fe)

-- Builtin operators are handled specially because they are
-- overloaded.
internaliseExp desc (E.BinOp op (xe,_) (ye,_) _ loc)
  | Just internalise <- isOverloadedFunction op [xe, ye] loc =
      internalise desc

-- User-defined operators are just the same as a function call.
internaliseExp desc (E.BinOp op (xarg,xd) (yarg,yd) (Info ret) loc) =
  internaliseExp desc $
  E.Apply (E.Apply (E.Var op (Info ([], ret)) loc) xarg (Info xd) (Info ([], ret)) loc)
          yarg (Info yd) (Info ([], ret)) loc

internaliseExp desc (E.Project k e (Info rt) _) = do
  n <- internalisedTypeSize $ rt `setAliases` ()
  i' <- fmap sum $ mapM internalisedTypeSize $
        case E.typeOf e `setAliases` () of
               Record fs -> map snd $ filter ((<k) . fst) $ sortFields fs
               t         -> [t]
  take n . drop i' <$> internaliseExp desc e

internaliseExp _ e@E.Lambda{} =
  fail $ "internaliseExp: Unexpected lambda at " ++ locStr (srclocOf e)

internaliseExp _ e@E.OpSection{} =
  fail $ "internaliseExp: Unexpected operator section at " ++ locStr (srclocOf e)

internaliseExp _ e@E.OpSectionLeft{} =
  fail $ "internaliseExp: Unexpected left operator section at " ++ locStr (srclocOf e)

internaliseExp _ e@E.OpSectionRight{} =
  fail $ "internaliseExp: Unexpected right operator section at " ++ locStr (srclocOf e)

internaliseDimIndex :: SrcLoc -> SubExp -> E.DimIndex
                    -> InternaliseM (I.DimIndex SubExp, Certificates)
internaliseDimIndex loc w (E.DimFix i) = do
  (i', _) <- internaliseDimExp "i" i
  cs <- assertingOne $ boundsCheck loc w i'
  return (I.DimFix i', cs)
internaliseDimIndex loc w (E.DimSlice i j s) = do
  s' <- maybe (return one) (fmap fst . internaliseDimExp "s") s
  s_sign <- letSubExp "s_sign" $ BasicOp $ I.UnOp (I.SSignum Int32) s'
  backwards <- letSubExp "backwards" $ I.BasicOp $ I.CmpOp (I.CmpEq int32) s_sign negone
  w_minus_1 <- letSubExp "w_minus_1" $ BasicOp $ I.BinOp (Sub Int32) w one
  let i_def = letSubExp "i_def" $ I.If backwards
              (resultBody [w_minus_1])
              (resultBody [zero]) $ ifCommon [I.Prim int32]
      j_def = letSubExp "j_def" $ I.If backwards
              (resultBody [negone])
              (resultBody [w]) $ ifCommon [I.Prim int32]
  i' <- maybe i_def (fmap fst . internaliseDimExp "i") i
  j' <- maybe j_def (fmap fst . internaliseDimExp "j") j
  j_m_i <- letSubExp "j_m_i" $ BasicOp $ I.BinOp (Sub Int32) j' i'
  n <- letSubExp "n" =<< eDivRoundingUp Int32
       (pure $ BasicOp $ I.UnOp (I.Abs Int32) j_m_i)
       (pure $ I.BasicOp $ I.UnOp (I.Abs Int32) s')

  checked <- asserting $ fmap Certificates $ do
    -- Bounds checks depend on whether we are slicing forwards or
    -- backwards.  If forwards, we must check '0 <= i && i <= j'.  If
    -- backwards, '-1 <= j && j <= i'.  In both cases, we check '0 <=
    -- i+n*s && i+(n-1)*s < w'.  We only check if the slice is nonempty.
    empty_slice <- letSubExp "empty_slice" $ I.BasicOp $ I.CmpOp (CmpEq int32) n zero

    m <- letSubExp "m" $ I.BasicOp $ I.BinOp (Sub Int32) n one
    m_t_s <- letSubExp "m_t_s" $ I.BasicOp $ I.BinOp (Mul Int32) m s'
    i_p_m_t_s <- letSubExp "i_p_m_t_s" $ I.BasicOp $ I.BinOp (Add Int32) i' m_t_s
    zero_leq_i_p_m_t_s <- letSubExp "zero_leq_i_p_m_t_s" $
                          I.BasicOp $ I.CmpOp (I.CmpSle Int32) zero i_p_m_t_s
    i_p_m_t_s_leq_w <- letSubExp "i_p_m_t_s_leq_w" $
                       I.BasicOp $ I.CmpOp (I.CmpSle Int32) i_p_m_t_s w
    i_p_m_t_s_lth_w <- letSubExp "i_p_m_t_s_leq_w" $
                       I.BasicOp $ I.CmpOp (I.CmpSlt Int32) i_p_m_t_s w

    zero_lte_i <- letSubExp "zero_lte_i" $ I.BasicOp $ I.CmpOp (I.CmpSle Int32) zero i'
    i_lte_j <- letSubExp "i_lte_j" $ I.BasicOp $ I.CmpOp (I.CmpSle Int32) i' j'
    forwards_ok <- letSubExp "forwards_ok" =<<
                   foldBinOp I.LogAnd zero_lte_i
                   [zero_lte_i, i_lte_j, zero_leq_i_p_m_t_s, i_p_m_t_s_lth_w]

    negone_lte_j <- letSubExp "negone_lte_j" $ I.BasicOp $ I.CmpOp (I.CmpSle Int32) negone j'
    j_lte_i <- letSubExp "j_lte_i" $ I.BasicOp $ I.CmpOp (I.CmpSle Int32) j' i'
    backwards_ok <- letSubExp "backwards_ok" =<<
                    foldBinOp I.LogAnd negone_lte_j
                    [negone_lte_j, j_lte_i, zero_leq_i_p_m_t_s, i_p_m_t_s_leq_w]

    slice_ok <- letSubExp "slice_ok" $ I.If backwards
                (resultBody [backwards_ok])
                (resultBody [forwards_ok]) $
                ifCommon [I.Prim I.Bool]
    ok_or_empty <- letSubExp "ok_or_empty" $
                   I.BasicOp $ I.BinOp I.LogOr empty_slice slice_ok
    letTupExp "slice_cert" $ I.BasicOp $ I.Assert ok_or_empty "slice out of bounds" (loc, mempty)

  return (I.DimSlice i' n s', checked)
  where zero = constant (0::Int32)
        negone = constant (-1::Int32)
        one = constant (1::Int32)

internaliseScanOrReduce :: String -> String
                        -> (SubExp -> I.Lambda -> [(SubExp, VName)] -> SOAC SOACS)
                        -> (E.Exp, E.Exp, E.Exp, SrcLoc)
                        -> InternaliseM [SubExp]
internaliseScanOrReduce desc what f (lam, ne, arr, loc) = do
  arrs <- internaliseExpToVars (what++"_arr") arr
  nes <- internaliseExp (what++"_ne") ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape asserting
      "Row shape of input array does not match shape of neutral element"
      loc rowtype (what++"_ne_right_shape") ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda lam nests arrts
  let input = zip nes' arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ f w lam' input

internaliseExp1 :: String -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of [se] -> return se
             _ -> fail "Internalise.internaliseExp1: was passed not just a single subexpression"

-- | Promote to dimension type as appropriate for the original type.
-- Also return original type.
internaliseDimExp :: String -> E.Exp -> InternaliseM (I.SubExp, IntType)
internaliseDimExp s e = do
  e' <- internaliseExp1 s e
  case E.typeOf e of
    E.Prim (Signed it)   -> (,it) <$> asIntS Int32 e'
    E.Prim (Unsigned it) -> (,it) <$> asIntZ Int32 e'
    _                    -> fail "internaliseDimExp: bad type"

internaliseShapeExp :: String -> E.Exp -> InternaliseM [I.SubExp]
internaliseShapeExp s e =
  case E.typeOf e of
    t | Just ts <- isTupleRecord t ->
          zipWithM promote ts =<< internaliseExp s e
      | otherwise ->
          fmap pure . promote t =<< internaliseExp1 s e
  where promote (E.Prim Signed{}) se = asIntS Int32 se
        promote (E.Prim Unsigned{}) se = asIntZ Int32 se
        promote _ _ = fail "internaliseShapeExp.promote: bad type"

internaliseExpToVars :: String -> E.Exp -> InternaliseM [I.VName]
internaliseExpToVars desc e =
  mapM asIdent =<< internaliseExp desc e
  where asIdent (I.Var v) = return v
        asIdent se        = letExp desc $ I.BasicOp $ I.SubExp se

internaliseOperation :: String
                     -> E.Exp
                     -> (I.VName -> InternaliseM I.BasicOp)
                     -> InternaliseM [I.SubExp]
internaliseOperation s e op = do
  vs <- internaliseExpToVars s e
  letSubExps s =<< mapM (fmap I.BasicOp . op) vs

internaliseBinOp :: String
                 -> E.BinOp
                 -> I.SubExp -> I.SubExp
                 -> E.PrimType
                 -> E.PrimType
                 -> InternaliseM [I.SubExp]
internaliseBinOp desc E.Plus x y (E.Signed t) _ =
  simpleBinOp desc (I.Add t) x y
internaliseBinOp desc E.Plus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Add t) x y
internaliseBinOp desc E.Plus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FAdd t) x y
internaliseBinOp desc E.Minus x y (E.Signed t) _ =
  simpleBinOp desc (I.Sub t) x y
internaliseBinOp desc E.Minus x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Sub t) x y
internaliseBinOp desc E.Minus x y (E.FloatType t) _ =
  simpleBinOp desc (I.FSub t) x y
internaliseBinOp desc E.Times x y (E.Signed t) _ =
  simpleBinOp desc (I.Mul t) x y
internaliseBinOp desc E.Times x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Mul t) x y
internaliseBinOp desc E.Times x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMul t) x y
internaliseBinOp desc E.Divide x y (E.Signed t) _ =
  simpleBinOp desc (I.SDiv t) x y
internaliseBinOp desc E.Divide x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UDiv t) x y
internaliseBinOp desc E.Divide x y (E.FloatType t) _ =
  simpleBinOp desc (I.FDiv t) x y
internaliseBinOp desc E.Pow x y (E.FloatType t) _ =
  simpleBinOp desc (I.FPow t) x y
internaliseBinOp desc E.Pow x y (E.Signed t) _ =
  simpleBinOp desc (I.Pow t) x y
internaliseBinOp desc E.Pow x y (E.Unsigned t) _ =
  simpleBinOp desc (I.Pow t) x y
internaliseBinOp desc E.Mod x y (E.Signed t) _ =
  simpleBinOp desc (I.SMod t) x y
internaliseBinOp desc E.Mod x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UMod t) x y
internaliseBinOp desc E.Quot x y (E.Signed t) _ =
  simpleBinOp desc (I.SQuot t) x y
internaliseBinOp desc E.Quot x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UDiv t) x y
internaliseBinOp desc E.Rem x y (E.Signed t) _ =
  simpleBinOp desc (I.SRem t) x y
internaliseBinOp desc E.Rem x y (E.Unsigned t) _ =
  simpleBinOp desc (I.UMod t) x y
internaliseBinOp desc E.ShiftR x y (E.Signed t) _ =
  simpleBinOp desc (I.AShr t) x y
internaliseBinOp desc E.ShiftR x y (E.Unsigned t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp desc E.ZShiftR x y (E.Signed t) _ =
  simpleBinOp desc (I.LShr t) x y
internaliseBinOp desc E.ZShiftR x y (E.Unsigned t) _ =
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
  eq <- letSubExp (desc++"true") $ I.BasicOp $ I.CmpOp (I.CmpEq $ internalisePrimType t) x y
  fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp I.Not eq
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
internaliseBinOp _ op _ _ t1 t2 =
  fail $ "Invalid binary operator " ++ pretty op ++
  " with operand types " ++ pretty t1 ++ ", " ++ pretty t2

simpleBinOp :: String
            -> I.BinOp
            -> I.SubExp -> I.SubExp
            -> InternaliseM [I.SubExp]
simpleBinOp desc bop x y =
  letTupExp' desc $ I.BasicOp $ I.BinOp bop x y

simpleCmpOp :: String
            -> I.CmpOp
            -> I.SubExp -> I.SubExp
            -> InternaliseM [I.SubExp]
simpleCmpOp desc op x y =
  letTupExp' desc $ I.BasicOp $ I.CmpOp op x y

findFuncall :: E.Exp -> InternaliseM (E.QualName VName, [E.Exp], [E.StructType])
findFuncall (E.Var fname (Info (remaining, _)) _) =
  return (fname, [], remaining)
findFuncall (E.Apply f arg _ (Info (remaining, _)) _) = do
  (fname, args, _) <- findFuncall f
  return (fname, args ++ [arg], remaining)
findFuncall e =
  fail $ "Invalid function expression in application: " ++ pretty e

internaliseLambda :: InternaliseLambda

internaliseLambda (E.Parens e _) rowtypes =
  internaliseLambda e rowtypes

internaliseLambda (E.Lambda tparams params body _ (Info rettype) loc) rowtypes =
  bindingLambdaParams tparams params rowtypes $ \pcm params' -> do
    (rettype', _, rcm) <- internaliseReturnType rettype
    body' <- internaliseBody body
    mapM_ (uncurry (internaliseDimConstant loc)) $ pcm<>rcm
    return (params', body', map I.fromDecl rettype')

internaliseLambda (E.OpSection unop (Info xtype) (Info ytype) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpFunToLambda unop xtype ytype rettype
  internaliseLambda (E.Lambda [] params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.OpSectionLeft binop e (Info paramtype, Info _) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e $ uncurry $ flip (,)
  internaliseLambda (E.Lambda [] params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.OpSectionRight binop e (Info _, Info paramtype) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e id
  internaliseLambda (E.Lambda [] params body Nothing (Info rettype') loc) rowts

internaliseLambda e rowtypes = do
  (_, _, remaining_params_ts) <- findFuncall e
  (params, param_args) <- fmap unzip $ forM remaining_params_ts $ \et -> do
    name <- newVName "not_curried"
    return (E.Id name (Info $ E.vacuousShapeAnnotations $ et `setAliases` mempty) loc,
            E.Var (E.qualName name)
             (Info ([], E.removeShapeAnnotations $ et `setAliases` mempty)) loc)
  let rettype = E.typeOf e
      body = foldl (\f arg -> E.Apply f arg (Info E.Observe) (Info ([], rettype)) loc)
                   e
                   param_args
      rettype' = E.vacuousShapeAnnotations $ rettype `E.setAliases` ()
  internaliseLambda (E.Lambda [] params body Nothing (Info rettype') loc) rowtypes
  where loc = srclocOf e

binOpFunToLambda :: E.QualName VName
                 -> E.CompType -> E.CompType -> E.CompType
                 -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
binOpFunToLambda op xtype ytype rettype = do
  x_name <- newNameFromString "binop_param_x"
  y_name <- newNameFromString "binop_param_y"
  return ([E.Id x_name (Info $ E.vacuousShapeAnnotations xtype) noLoc,
           E.Id y_name (Info $ E.vacuousShapeAnnotations ytype) noLoc],
          E.BinOp op
           (E.Var (qualName x_name) (Info ([], xtype)) noLoc, E.Observe)
           (E.Var (qualName y_name) (Info ([], ytype)) noLoc, E.Observe)
           (Info rettype) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

binOpCurriedToLambda :: E.QualName VName
                     -> E.CompType -> E.CompType
                     -> E.Exp
                     -> ((E.Exp,E.Exp) -> (E.Exp,E.Exp))
                     -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
binOpCurriedToLambda op paramtype rettype e swap = do
  paramname <- newNameFromString "binop_param_noncurried"
  let (x', y') = swap (E.Var (qualName paramname) (Info ([], paramtype)) noLoc, e)
  return ([E.Id paramname (Info $ E.vacuousShapeAnnotations paramtype) noLoc],
          E.BinOp op (x',E.Observe) (y',E.Observe) (Info rettype) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

internaliseDimConstant :: SrcLoc -> Name -> VName -> InternaliseM ()
internaliseDimConstant loc fname name =
  letBind_ (basicPattern' [] [I.Ident name $ I.Prim I.int32]) $
  I.Apply fname [] [I.Prim I.int32] (Safe, loc, mempty)

-- | Some operators and functions are overloaded or otherwise special
-- - we detect and treat them here.
isOverloadedFunction :: E.QualName VName -> [E.Exp] -> SrcLoc
                     -> Maybe (String -> InternaliseM [SubExp])
isOverloadedFunction qname args loc = do
  guard $ baseTag (qualLeaf qname) <= maxIntrinsicTag
  handle args $ baseString $ qualLeaf qname
  where
    handle [x] "sign_i8"  = Just $ toSigned I.Int8 x
    handle [x] "sign_i16" = Just $ toSigned I.Int16 x
    handle [x] "sign_i32" = Just $ toSigned I.Int32 x
    handle [x] "sign_i64" = Just $ toSigned I.Int64 x

    handle [x] "unsign_i8"  = Just $ toUnsigned I.Int8 x
    handle [x] "unsign_i16" = Just $ toUnsigned I.Int16 x
    handle [x] "unsign_i32" = Just $ toUnsigned I.Int32 x
    handle [x] "unsign_i64" = Just $ toUnsigned I.Int64 x

    handle [x] "sgn" = Just $ signumF x
    handle [x] "abs" = Just $ absF x
    handle [x] "!" = Just $ notF x
    handle [x] "~" = Just $ complementF x

    handle [x] "opaque" = Just $ \desc ->
      mapM (letSubExp desc . BasicOp . Opaque) =<< internaliseExp "opaque_arg" x

    handle [x] s
      | Just unop <- find ((==s) . pretty) allUnOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp unop x'

    handle [x,y] s
      | Just bop <- find ((==s) . pretty) allBinOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          y' <- internaliseExp1 "y" y
          fmap pure $ letSubExp desc $ I.BasicOp $ I.BinOp bop x' y'
      | Just cmp <- find ((==s) . pretty) allCmpOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          y' <- internaliseExp1 "y" y
          fmap pure $ letSubExp desc $ I.BasicOp $ I.CmpOp cmp x' y'
    handle [x] s
      | Just conv <- find ((==s) . pretty) allConvOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.ConvOp conv x'

    -- Short-circuiting operators are magical.
    handle [x,y] "&&" = Just $ \desc ->
      internaliseExp desc $
      E.If x y (E.Literal (E.BoolValue False) noLoc) (Info (E.Prim E.Bool)) noLoc
    handle [x,y] "||" = Just $ \desc ->
        internaliseExp desc $
        E.If x (E.Literal (E.BoolValue True) noLoc) y (Info (E.Prim E.Bool)) noLoc

    -- Handle equality and inequality specially, to treat the case of
    -- arrays.
    handle [xe,ye] op
      | Just cmp_f <- isEqlOp op = Just $ \desc -> do
          xe' <- internaliseExp "x" xe
          ye' <- internaliseExp "y" ye
          rs <- zipWithM (doComparison desc) xe' ye'
          cmp_f desc =<< letSubExp "eq" =<< foldBinOp I.LogAnd (constant True) rs
        where isEqlOp "!=" = Just $ \desc eq ->
                letTupExp' desc $ I.BasicOp $ I.UnOp I.Not eq
              isEqlOp "==" = Just $ \_ eq ->
                return [eq]
              isEqlOp _ = Nothing

              doComparison desc x y = do
                x_t <- I.subExpType x
                y_t <- I.subExpType y
                case x_t of
                  I.Prim t -> letSubExp desc $ I.BasicOp $ I.CmpOp (I.CmpEq t) x y
                  _ -> do
                    let x_dims = I.arrayDims x_t
                        y_dims = I.arrayDims y_t
                    dims_match <- forM (zip x_dims y_dims) $ \(x_dim, y_dim) ->
                      letSubExp "dim_eq" $ I.BasicOp $ I.CmpOp (I.CmpEq int32) x_dim y_dim
                    shapes_match <- letSubExp "shapes_match" =<<
                                    foldBinOp I.LogAnd (constant True) dims_match
                    compare_elems_body <- runBodyBinder $ do
                      -- Flatten both x and y.
                      x_num_elems <- letSubExp "x_num_elems" =<<
                                     foldBinOp (I.Mul Int32) (constant (1::Int32)) x_dims
                      x' <- letExp "x" $ I.BasicOp $ I.SubExp x
                      y' <- letExp "x" $ I.BasicOp $ I.SubExp y
                      x_flat <- letExp "x_flat" $ I.BasicOp $ I.Reshape [I.DimNew x_num_elems] x'
                      y_flat <- letExp "y_flat" $ I.BasicOp $ I.Reshape [I.DimNew x_num_elems] y'

                      -- Compare the elements.
                      cmp_lam <- cmpOpLambda (I.CmpEq (elemType x_t)) (elemType x_t)
                      cmps <- letExp "cmps" $ I.Op $ I.Map x_num_elems cmp_lam [x_flat, y_flat]

                      -- Check that all were equal.
                      and_lam <- binOpLambda I.LogAnd I.Bool
                      all_equal <- letSubExp "all_equal" $ I.Op $
                                   I.Reduce x_num_elems Commutative and_lam [(constant True,cmps)]
                      return $ resultBody [all_equal]

                    letSubExp "arrays_equal" $
                      I.If shapes_match compare_elems_body (resultBody [constant False]) $
                      ifCommon [I.Prim I.Bool]

    handle [x,y] name
      | Just bop <- find ((name==) . pretty) [minBound..maxBound::E.BinOp] =
      Just $ \desc -> do
        x' <- internaliseExp1 "x" x
        y' <- internaliseExp1 "y" y
        case (E.typeOf x, E.typeOf y) of
          (E.Prim t1, E.Prim t2) ->
            internaliseBinOp desc bop x' y' t1 t2
          _ -> fail "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."

    handle [a, si, v] "scatter" = Just $ scatterF a si v

    handle _ _ = Nothing

    toSigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Prim E.Bool ->
          letTupExp' desc $ I.If e' (resultBody [intConst int_to 1])
                                    (resultBody [intConst int_to 0]) $
                                    ifCommon [I.Prim $ I.IntType int_to]
        E.Prim (E.Signed int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SExt int_from int_to) e'
        E.Prim (E.Unsigned int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Prim (E.FloatType float_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToSI float_from int_to) e'
        _ -> fail "Futhark.Internalise.handle: non-numeric type in ToSigned"

    toUnsigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Prim E.Bool ->
          letTupExp' desc $ I.If e' (resultBody [intConst int_to 1])
                                    (resultBody [intConst int_to 0]) $
                                    ifCommon [I.Prim $ I.IntType int_to]
        E.Prim (E.Signed int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Prim (E.Unsigned int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Prim (E.FloatType float_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToUI float_from int_to) e'
        _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToUnsigned"

    signumF e desc = do
      e' <- internaliseExp1 "signum_arg" e
      case E.typeOf e of
        E.Prim (E.Signed t) ->
          letTupExp' desc $ I.BasicOp $ I.UnOp (I.SSignum t) e'
        E.Prim (E.Unsigned t) ->
          letTupExp' desc $ I.BasicOp $ I.UnOp (I.USignum t) e'
        _ -> fail "Futhark.Internalise.internaliseExp: non-integer type in Signum"

    absF e desc = do
      e' <- internaliseExp1 "abs_arg" e
      case E.typeOf e of
        E.Prim (E.Signed t) ->
          letTupExp' desc $ I.BasicOp $ I.UnOp (I.Abs t) e'
        E.Prim (E.Unsigned _) ->
          return [e']
        E.Prim (E.FloatType t) ->
          letTupExp' desc $ I.BasicOp $ I.UnOp (I.FAbs t) e'
        _ -> fail "Futhark.Internalise.internaliseExp: non-integer type in Abs"

    notF e desc = do
      e' <- internaliseExp1 "not_arg" e
      letTupExp' desc $ I.BasicOp $ I.UnOp I.Not e'

    complementF e desc = do
      e' <- internaliseExp1 "complement_arg" e
      et <- subExpType e'
      case et of I.Prim (I.IntType t) ->
                   letTupExp' desc $ I.BasicOp $ I.UnOp (I.Complement t) e'
                 _ ->
                   fail "Futhark.Internalise.internaliseExp: non-integer type in Complement"

    scatterF a si v desc = do
      si' <- letExp "write_si" . BasicOp . SubExp =<< internaliseExp1 "write_arg_i" si
      svs <- internaliseExpToVars "write_arg_v" v
      sas <- internaliseExpToVars "write_arg_a" a

      si_shape <- I.arrayShape <$> lookupType si'
      let si_w = shapeSize 0 si_shape
      sv_ts <- mapM lookupType svs

      svs' <- forM (zip svs sv_ts) $ \(sv,sv_t) -> do
        let sv_shape = I.arrayShape sv_t
            sv_w = arraySize 0 sv_t

        -- Generate an assertion and reshapes to ensure that sv and si' are the same
        -- size.
        cmp <- letSubExp "write_cmp" $ I.BasicOp $
          I.CmpOp (I.CmpEq I.int32) si_w sv_w
        c   <- assertingOne $
          letExp "write_cert" $ I.BasicOp $
          I.Assert cmp "length of index and value array does not match" (loc, mempty)
        certifying c $ letExp (baseString sv ++ "_write_sv") $
          I.BasicOp $ I.Reshape (reshapeOuter [DimCoercion si_w] 1 sv_shape) sv

      indexType <- rowType <$> lookupType si'
      indexName <- newVName "write_index"
      valueNames <- replicateM (length sv_ts) $ newVName "write_value"

      sa_ts <- mapM lookupType sas
      let bodyTypes = replicate (length sv_ts) indexType ++ map rowType sa_ts
          paramTypes = indexType : map rowType sv_ts
          bodyNames = indexName : valueNames
          bodyParams = zipWith I.Param bodyNames paramTypes

      -- This body is pretty boring right now, as every input is exactly the output.
      -- But it can get funky later on if fused with something else.
      body <- localScope (scopeOfLParams bodyParams) $ insertStmsM $ do
        let outs = replicate (length valueNames) indexName ++ valueNames
        results <- forM outs $ \name ->
          letSubExp "write_res" $ I.BasicOp $ I.SubExp $ I.Var name
        ensureResultShape asserting "scatter value has wrong size" loc
          bodyTypes $ resultBody results

      let lam = I.Lambda { I.lambdaParams = bodyParams
                         , I.lambdaReturnType = bodyTypes
                         , I.lambdaBody = body
                         }
          sivs = si' : svs'

      let sa_ws = map (arraySize 0) sa_ts
      letTupExp' desc $ I.Op $ I.Scatter si_w lam sivs $ zip sa_ws sas

-- | Is the name a value constant?  If so, create the necessary
-- function call and return the corresponding subexpressions.
lookupConstant :: SrcLoc -> VName -> InternaliseM (Maybe [SubExp])
lookupConstant loc name = do
  is_const <- lookupFunction' name
  case is_const of
    Just (fname, constparams, _, _, _, _, mk_rettype) -> do
      (constargs, const_ds, const_ts) <- unzip3 <$> constFunctionArgs loc constparams
      safety <- askSafety
      case mk_rettype $ zip constargs $ map I.fromDecl const_ts of
        Nothing -> fail $ "lookupConstant: " ++ pretty name ++ " failed"
        Just rettype ->
          fmap (Just . map I.Var) $ letTupExp (baseString name) $
          I.Apply fname (zip constargs const_ds) rettype (safety, loc, mempty)
    Nothing -> return Nothing

constFunctionArgs :: SrcLoc -> ConstParams -> InternaliseM [(SubExp, I.Diet, I.DeclType)]
constFunctionArgs loc = mapM arg
  where arg (fname, name) = do
          safety <- askSafety
          se <- letSubExp (baseString name ++ "_arg") $
                I.Apply fname [] [I.Prim I.int32] (safety, loc, [])
          return (se, I.Observe, I.Prim I.int32)

funcall :: String -> QualName VName -> SpecArgs -> [SubExp] -> SrcLoc
        -> InternaliseM ([SubExp], [I.ExtType])
funcall desc (QualName _ fname) (e_ts, i_ts) args loc = do
  e_ts' <- mapM fullyApplyType e_ts
  (fname', constparams, closure, shapes, value_paramts, fun_params, rettype_fun) <-
    lookupFunction fname (e_ts', i_ts)
  (constargs, const_ds, _) <- unzip3 <$> constFunctionArgs loc constparams
  argts <- mapM subExpType args
  closure_ts <- mapM lookupType closure
  let shapeargs = argShapes shapes value_paramts argts
      diets = const_ds ++ replicate (length closure + length shapeargs) I.Observe ++
              map I.diet value_paramts
      constOrShape = const $ I.Prim int32
      paramts = map constOrShape constargs ++ closure_ts ++
                map constOrShape shapeargs ++ map I.fromDecl value_paramts
  args' <- ensureArgShapes asserting "function arguments of wrong shape"
           loc (map I.paramName fun_params)
           paramts (constargs ++ map I.Var closure ++ shapeargs ++ args)
  argts' <- mapM subExpType args'
  case rettype_fun $ zip args' argts' of
    Nothing -> fail $ "Cannot apply " ++ pretty fname ++ " to arguments\n " ++
               pretty args' ++ "\nof types\n " ++
               pretty argts' ++
               "\nFunction has parameters\n " ++ pretty fun_params
    Just ts -> do
      safety <- askSafety
      ses <- letTupExp' desc $ I.Apply fname' (zip args' diets) ts (safety, loc, mempty)
      return (ses, map I.fromDecl ts)

askSafety :: InternaliseM Safety
askSafety = do check <- asks envDoBoundsChecks
               return $ if check then I.Safe else I.Unsafe

boundsCheck :: SrcLoc -> I.SubExp -> I.SubExp -> InternaliseM I.VName
boundsCheck loc w e = do
  let check = eBinOp I.LogAnd (pure lowerBound) (pure upperBound)
      lowerBound = I.BasicOp $
                   I.CmpOp (I.CmpSle I.Int32) (I.constant (0 :: I.Int32)) e
      upperBound = I.BasicOp $
                   I.CmpOp (I.CmpSlt I.Int32) e w
  letExp "bounds_check" =<< eAssert check "index out of bounds" loc

shadowIdentsInExp :: [(VName, I.SubExp)] -> Stms SOACS -> I.SubExp
                  -> InternaliseM I.SubExp
shadowIdentsInExp substs bnds res = do
  body <- renameBody <=< insertStmsM $ do
    -- XXX: we have to substitute names to fix type annotations in the
    -- bindings.  This goes away once we get rid of these type
    -- annotations.
    let handleSubst nameSubsts (name, I.Var v)
          | v == name =
            return nameSubsts
          | otherwise =
            return $ M.insert name v nameSubsts
        handleSubst nameSubsts (name, se) = do
          letBindNames'_ [name] $ BasicOp $ SubExp se
          return nameSubsts
    nameSubsts <- foldM handleSubst M.empty substs
    addStms $ substituteNames nameSubsts bnds
    return $ resultBody [substituteNames nameSubsts res]
  res' <- bodyBind body
  case res' of
    [se] -> return se
    _    -> fail "Internalise.shadowIdentsInExp: something went very wrong"

-- Implement partitioning using maps, scans and writes.
partitionWithSOACS :: Int -> I.Lambda -> [I.VName] -> InternaliseM ([I.SubExp], [I.VName])
partitionWithSOACS k lam arrs = do
  arr_ts <- mapM lookupType arrs
  let w = arraysSize 0 arr_ts
  classes_and_increments <- letTupExp "increments" $ I.Op $ I.Map w lam arrs
  (classes, increments) <- case classes_and_increments of
                             classes : increments -> return (classes, take k increments)
                             _                    -> fail "partitionWithSOACS"

  add_lam_x_params <-
    replicateM k $ I.Param <$> newVName "x" <*> pure (I.Prim int32)
  add_lam_y_params <-
    replicateM k $ I.Param <$> newVName "y" <*> pure (I.Prim int32)
  add_lam_body <- runBodyBinder $
                  localScope (scopeOfLParams $ add_lam_x_params++add_lam_y_params) $
    fmap resultBody $ forM (zip add_lam_x_params add_lam_y_params) $ \(x,y) ->
      letSubExp "z" $ I.BasicOp $ I.BinOp (I.Add Int32)
      (I.Var $ I.paramName x) (I.Var $ I.paramName y)
  let add_lam = I.Lambda { I.lambdaBody = add_lam_body
                         , I.lambdaParams = add_lam_x_params ++ add_lam_y_params
                         , I.lambdaReturnType = replicate k $ I.Prim int32
                         }
      scan_input = zip (repeat $ constant (0::Int32)) increments

  all_offsets <- letTupExp "offsets" $ I.Op $ I.Scan w add_lam scan_input

  -- We have the offsets for each of the partitions, but we also need
  -- the total sizes, which are the last elements in the offests.  We
  -- just have to be careful in case the array is empty.
  last_index <- letSubExp "last_index" $ I.BasicOp $ I.BinOp (I.Sub Int32) w $ constant (1::Int32)
  nonempty_body <- runBodyBinder $ fmap resultBody $ forM all_offsets $ \offset_array ->
    letSubExp "last_offset" $ I.BasicOp $ I.Index offset_array [I.DimFix last_index]
  let empty_body = resultBody $ replicate k $ constant (0::Int32)
  is_empty <- letSubExp "is_empty" $ I.BasicOp $ I.CmpOp (CmpEq int32) w $ constant (0::Int32)
  sizes <- letTupExp "partition_size" $
           I.If is_empty empty_body nonempty_body $
           ifCommon $ replicate k $ I.Prim int32

  -- Compute total size of all partitions.
  sum_of_partition_sizes <- letSubExp "sum_of_partition_sizes" =<<
                            foldBinOp (Add Int32) (constant (0::Int32)) (map I.Var sizes)

  -- Create scratch arrays for the result.
  blanks <- forM arr_ts $ \arr_t ->
    letExp "partition_dest" $ I.BasicOp $
    Scratch (elemType arr_t) (sum_of_partition_sizes : drop 1 (I.arrayDims arr_t))

  -- Now write into the result.
  write_lam <- do
    c_param <- I.Param <$> newVName "c" <*> pure (I.Prim int32)
    offset_params <- replicateM k $ I.Param <$> newVName "offset" <*> pure (I.Prim int32)
    value_params <- forM arr_ts $ \arr_t ->
      I.Param <$> newVName "v" <*> pure (I.rowType arr_t)
    (offset, offset_stms) <- collectStms $ mkOffsetLambdaBody (map I.Var sizes)
                             (I.Var $ I.paramName c_param) 0 offset_params
    return I.Lambda { I.lambdaParams = c_param : offset_params ++ value_params
                    , I.lambdaReturnType = replicate (length arr_ts) (I.Prim int32) ++
                                           map I.rowType arr_ts
                    , I.lambdaBody = mkBody offset_stms $
                                     replicate (length arr_ts) offset ++
                                     map (I.Var . I.paramName) value_params
                    }
  results <- letTupExp "partition_res" $ I.Op $ I.Scatter w
             write_lam (classes : all_offsets ++ arrs) $ zip (repeat sum_of_partition_sizes) blanks
  return (map I.Var sizes, results)
  where
    mkOffsetLambdaBody :: [SubExp]
                       -> SubExp
                       -> Int
                       -> [I.LParam]
                       -> InternaliseM SubExp
    mkOffsetLambdaBody _ _ _ [] =
      return $ constant (-1::Int32)
    mkOffsetLambdaBody sizes c i (p:ps) = do
      is_this_one <- letSubExp "is_this_one" $ I.BasicOp $ I.CmpOp (CmpEq int32) c (constant i)
      next_one <- mkOffsetLambdaBody sizes c (i+1) ps
      this_one <- letSubExp "this_offset" =<<
                  foldBinOp (Add Int32) (constant (-1::Int32))
                  (I.Var (I.paramName p) : take i sizes)
      letSubExp "total_res" $ I.If is_this_one
        (resultBody [this_one]) (resultBody [next_one]) $ ifCommon [I.Prim int32]
