{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fmax-pmcheck-iterations=25000000#-}
-- |
--
-- This module implements a transformation from source to core
-- Futhark.
--
module Futhark.Internalise (internaliseProg) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Bitraversable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Char (chr)

import Language.Futhark as E hiding (TypeArg)
import Language.Futhark.Semantic (Imports)
import Futhark.Representation.SOACS as I hiding (stmPattern)
import Futhark.Transform.Rename as I
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Util (splitAt3)

import Futhark.Internalise.Monad as I
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings
import Futhark.Internalise.Lambdas
import Futhark.Internalise.Defunctorise as Defunctorise
import Futhark.Internalise.Defunctionalise as Defunctionalise
import Futhark.Internalise.Monomorphise as Monomorphise

-- | Convert a program in source Futhark to a program in the Futhark
-- core language.
internaliseProg :: MonadFreshNames m =>
                   Bool -> Imports -> m (I.Prog SOACS)
internaliseProg always_safe prog = do
  prog_decs <- Defunctorise.transformProg prog
  prog_decs' <- Monomorphise.transformProg prog_decs
  prog_decs'' <- Defunctionalise.transformProg prog_decs'
  prog' <- I.Prog <$> runInternaliseM always_safe (internaliseValBinds prog_decs'')
  I.renameProg prog'

internaliseValBinds :: [E.ValBind] -> InternaliseM ()
internaliseValBinds = mapM_ internaliseValBind

internaliseFunName :: VName -> [E.Pattern] -> InternaliseM Name
internaliseFunName ofname [] = return $ nameFromString $ pretty ofname ++ "f"
internaliseFunName ofname _  = do
  info <- lookupFunction' ofname
  -- In some rare cases involving local functions, the same function
  -- name may be re-used in multiple places.  We check whether the
  -- function name has already been used, and generate a new one if
  -- so.
  case info of
    Just _ -> nameFromString . pretty <$> newNameFromString (baseString ofname)
    Nothing -> return $ nameFromString $ pretty ofname

internaliseValBind :: E.ValBind -> InternaliseM ()
internaliseValBind fb@(E.ValBind entry fname retdecl (Info (rettype, retext)) tparams params body _ loc) = do
  bindingParams tparams params $ \pcm shapeparams params' -> do
    (rettype_bad, rcm) <- internaliseReturnType $ existentialiseRetExt rettype
    let rettype' = zeroExts rettype_bad

    let mkConstParam name = Param name $ I.Prim int32
        constparams = map (mkConstParam . snd) $ pcm<>rcm
        constnames = map I.paramName constparams
        constscope = M.fromList $ zip constnames $ repeat $
                     FParamInfo $ I.Prim $ IntType Int32

        shapenames = map I.paramName shapeparams
        normal_params = map I.paramName constparams ++ shapenames ++
                        map I.paramName (concat params')
        normal_param_names = namesFromList normal_params

    fname' <- internaliseFunName fname params

    body' <- localScope constscope $ do
      msg <- case retdecl of
               Just dt -> errorMsg .
                          ("Function return value does not match shape of type ":) <$>
                          typeExpForError rcm dt
               Nothing -> return $ errorMsg ["Function return value does not match shape of declared return type."]
      internaliseBody body >>=
        ensureResultExtShape asserting msg loc (map I.fromDecl rettype')

    let free_in_fun = freeIn body' `namesSubtract` normal_param_names

    used_free_params <- forM (namesToList free_in_fun) $ \v -> do
      v_t <- lookupType v
      return $ Param v $ toDecl v_t Nonunique

    let free_shape_params = map (`Param` I.Prim int32) $
                            concatMap (I.shapeVars . I.arrayShape . I.paramType) used_free_params
        free_params = nub $ free_shape_params ++ used_free_params
        all_params = constparams ++ free_params ++ shapeparams ++ concat params'

    addFunction $ I.FunDef Nothing fname' rettype' all_params body'

    if null params'
      then bindConstant fname (fname',
                               pcm<>rcm,
                               applyRetType rettype' constparams,
                               bindExtSizes rettype retext)
      else bindFunction fname (fname',
                               pcm<>rcm,
                               map I.paramName free_params,
                               shapenames,
                               map declTypeOf $ concat params',
                               all_params,
                               applyRetType rettype' all_params)

  case entry of Just (Info entry') -> generateEntryPoint entry' fb
                Nothing -> return ()

  where
    -- | Recompute existential sizes to start from zero.
    -- Necessary because some convoluted constructions will start
    -- them from somewhere else.
    zeroExts ts = generaliseExtTypes ts ts

    existentialiseRetExt = first onDim
      where onDim (NamedDim v)
              | qualLeaf v `elem` retext = AnyDim
            onDim d = d

allDimsFreshInType :: MonadFreshNames m => E.PatternType -> m E.PatternType
allDimsFreshInType = bitraverse onDim pure
  where onDim (E.NamedDim v) =
          E.NamedDim . E.qualName <$> newVName (baseString $ E.qualLeaf v)
        onDim _ = pure AnyDim

-- | Replace all named dimensions with a fresh name, and remove all
-- constant dimensions.  The point is to remove the constraints, but
-- keep the names around.  We use this for constructing the entry
-- point parameters.
allDimsFreshInPat :: MonadFreshNames m => E.Pattern -> m E.Pattern
allDimsFreshInPat (PatternAscription p _ _) =
  allDimsFreshInPat p
allDimsFreshInPat (PatternParens p _) =
  allDimsFreshInPat p
allDimsFreshInPat (Id v (Info t) loc) =
  Id v <$> (Info <$> allDimsFreshInType t) <*> pure loc
allDimsFreshInPat (TuplePattern ps loc) =
  TuplePattern <$> mapM allDimsFreshInPat ps <*> pure loc
allDimsFreshInPat (RecordPattern ps loc) =
  RecordPattern <$> mapM (traverse allDimsFreshInPat) ps <*> pure loc
allDimsFreshInPat (Wildcard (Info t) loc) =
  Wildcard <$> (Info <$> allDimsFreshInType t) <*> pure loc
allDimsFreshInPat (PatternLit e (Info t) loc) =
  PatternLit e <$> (Info <$> allDimsFreshInType t) <*> pure loc
allDimsFreshInPat (PatternConstr c (Info t) pats loc) =
  PatternConstr c <$> (Info <$> allDimsFreshInType t) <*>
  mapM allDimsFreshInPat pats <*> pure loc

generateEntryPoint :: E.StructType -> E.ValBind -> InternaliseM ()
generateEntryPoint ftype (E.ValBind _ ofname retdecl (Info (rettype, _)) _ params _ _ loc) = do
  -- We replace all shape annotations, so there should be no constant
  -- parameters here.
  params_fresh <- mapM allDimsFreshInPat params
  let tparams = map (`E.TypeParamDim` noLoc) $ S.toList $
                mconcat $ map E.patternDimNames params_fresh
  bindingParams tparams params_fresh $ \_ shapeparams params' -> do
    (entry_rettype, _) <- internaliseEntryReturnType $ anyDimShapeAnnotations rettype
    let (e_paramts, e_rettype) = E.unfoldFunType ftype
        entry' = entryPoint (zip3 params e_paramts params') (retdecl, e_rettype, entry_rettype)
        args = map (I.Var . I.paramName) $ concat params'

    entry_body <- insertStmsM $ do
      -- Special case the (rare) situation where the entry point is
      -- not a function.
      maybe_const <- internaliseIfConst loc ofname
      vals <- case maybe_const of
                Just ses ->
                  return ses
                Nothing ->
                  fst <$> funcall "entry_result" (E.qualName ofname) args loc
      ctx <- extractShapeContext (concat entry_rettype) <$>
             mapM (fmap I.arrayDims . subExpType) vals
      resultBodyM (ctx ++ vals)

    addFunction $
      I.FunDef (Just entry') (baseName ofname)
      (concat entry_rettype)
      (shapeparams ++ concat params') entry_body

entryPoint :: [(E.Pattern, E.StructType, [I.FParam])]
           -> (Maybe (E.TypeExp VName), E.StructType, [[I.TypeBase ExtShape Uniqueness]])
           -> EntryPoint
entryPoint params (retdecl, eret, crets) =
  (concatMap (entryPointType . preParam) params,
   case isTupleRecord eret of
     Just ts -> concatMap entryPointType $ zip3 retdecls ts crets
     _       -> entryPointType (retdecl, eret, concat crets))
  where preParam (p_pat, e_t, ps) = (paramOuterType p_pat,
                                     e_t,
                                     staticShapes $ map I.paramDeclType ps)
        paramOuterType (E.PatternAscription _ tdecl _) = Just $ declaredType tdecl
        paramOuterType (E.PatternParens p _) = paramOuterType p
        paramOuterType _ = Nothing

        retdecls = case retdecl of Just (TETuple tes _) -> map Just tes
                                   _                    -> repeat Nothing

        entryPointType :: (Maybe (E.TypeExp VName),
                           E.StructType,
                           [I.TypeBase ExtShape Uniqueness])
                       -> [EntryPointType]
        entryPointType (_, E.Scalar (E.Prim E.Unsigned{}), _) =
          [I.TypeUnsigned]
        entryPointType (_, E.Array _ _ (E.Prim E.Unsigned{}) _, _) =
          [I.TypeUnsigned]
        entryPointType (_, E.Scalar E.Prim{}, _) =
          [I.TypeDirect]
        entryPointType (_, E.Array _ _ E.Prim{} _, _) =
          [I.TypeDirect]
        entryPointType (te, t, ts) =
          [I.TypeOpaque desc $ length ts]
          where desc = maybe (pretty t') typeExpOpaqueName te
                t' = removeShapeAnnotations t `E.setUniqueness` Nonunique

        -- | We remove dimension arguments such that we hopefully end
        -- up with a simpler type name for the entry point.  The
        -- intend is that if an entry point uses a type 'nasty [w] [h]',
        -- then we should turn that into an opaque type just called
        -- 'nasty'.  Also, we try to give arrays of opaques a nicer name.
        typeExpOpaqueName (TEApply te TypeArgExpDim{} _) =
          typeExpOpaqueName te
        typeExpOpaqueName (TEArray te _ _) =
          let (d, te') = withoutDims te
          in "arr_" ++ typeExpOpaqueName te' ++
             "_" ++ show (1 + d) ++ "d"
        typeExpOpaqueName te = pretty te

        withoutDims (TEArray te _ _) =
          let (d, te') = withoutDims te
          in (d+1, te')
        withoutDims te = (0::Int, te)

internaliseIdent :: E.Ident -> InternaliseM I.VName
internaliseIdent (E.Ident name (Info tp) loc) =
  case tp of
    E.Scalar E.Prim{} -> return name
    _ -> fail $ "Futhark.Internalise.internaliseIdent: asked to internalise non-prim-typed ident '"
         ++ pretty name ++ " of type " ++ pretty tp ++
         " at " ++ locStr loc ++ "."

internaliseBody :: E.Exp -> InternaliseM Body
internaliseBody e = insertStmsM $ resultBody <$> internaliseExp "res" e

bodyFromStms :: InternaliseM (Result, a)
             -> InternaliseM (Body, a)
bodyFromStms m = do
  ((res, a), stms) <- collectStms m
  (,a) <$> mkBodyM stms res

internaliseExp :: String -> E.Exp -> InternaliseM [I.SubExp]

internaliseExp desc (E.Parens e _) =
  internaliseExp desc e

internaliseExp desc (E.QualParens _ e _) =
  internaliseExp desc e

internaliseExp desc (E.StringLit vs _) =
  fmap pure $ letSubExp desc $
  I.BasicOp $ I.ArrayLit (map constant vs) $ I.Prim int8

internaliseExp _ (E.Var (E.QualName _ name) (Info t) loc) = do
  subst <- asks $ M.lookup name . envSubsts
  case subst of
    Just substs -> return substs
    Nothing     -> do
      -- If this identifier is the name of a constant, we have to turn it
      -- into a call to the corresponding function.
      is_const <- internaliseIfConst loc name
      case is_const of
        Just ses -> return ses
        Nothing -> (:[]) . I.Var <$> internaliseIdent (E.Ident name (Info t) loc)

internaliseExp desc (E.Index e idxs (Info ret, Info retext) loc) = do
  vs <- internaliseExpToVars "indexed" e
  dims <- case vs of []  -> return [] -- Will this happen?
                     v:_ -> I.arrayDims <$> lookupType v
  (idxs', cs) <- internaliseSlice loc dims idxs
  let index v = do v_t <- lookupType v
                   return $ I.BasicOp $ I.Index v $ fullSlice v_t idxs'
  ses <- certifying cs $ letSubExps desc =<< mapM index vs
  bindExtSizes (E.toStruct ret) retext ses
  return ses

-- XXX: we map empty records and tuples to bools, because otherwise
-- arrays of unit will lose their sizes.
internaliseExp _ (E.TupLit [] _) =
  return [constant True]
internaliseExp _ (E.RecordLit [] _) =
  return [constant True]

internaliseExp desc (E.TupLit es _) = concat <$> mapM (internaliseExp desc) es

internaliseExp desc (E.RecordLit orig_fields _) =
  concatMap snd . sortFields . M.unions <$> mapM internaliseField orig_fields
  where internaliseField (E.RecordFieldExplicit name e _) =
          M.singleton name <$> internaliseExp desc e
        internaliseField (E.RecordFieldImplicit name t loc) =
          internaliseField $ E.RecordFieldExplicit (baseName name)
          (E.Var (E.qualName name) t loc) loc

internaliseExp desc (E.ArrayLit es (Info arr_t) loc)
  -- If this is a multidimensional array literal of primitives, we
  -- treat it specially by flattening it out followed by a reshape.
  -- This cuts down on the amount of statements that are produced, and
  -- thus allows us to efficiently handle huge array literals - a
  -- corner case, but an important one.
  | Just ((eshape,e'):es') <- mapM isArrayLiteral es,
    not $ null eshape,
    all ((eshape==) . fst) es',
    Just basetype <- E.peelArray (length eshape) arr_t = do
      let flat_lit = E.ArrayLit (e' ++ concatMap snd es') (Info basetype) loc
          new_shape = length es:eshape
      flat_arrs <- internaliseExpToVars "flat_literal" flat_lit
      forM flat_arrs $ \flat_arr -> do
        flat_arr_t <- lookupType flat_arr
        let new_shape' = reshapeOuter (map (DimNew . constant) new_shape)
                         1 $ I.arrayShape flat_arr_t
        letSubExp desc $ I.BasicOp $ I.Reshape new_shape' flat_arr

  | otherwise = do
      arr_t_ext <- internaliseType $ E.toStruct arr_t
      es' <- mapM (internaliseExp "arr_elem") es

      let typeFromElements =
            -- XXX: the monomorphiser may create single-element array
            -- literals with an unknown row type.  In those cases we
            -- need to look at the types of the actual elements.
            -- Fixing this in the monomorphiser is a lot more tricky
            -- than just working around it here.
            case es' of
              [] -> error $ "internaliseExp ArrayLit: existential type: " ++ pretty arr_t
              e':_ -> mapM subExpType e'

      rowtypes <-
        maybe typeFromElements pure $
        mapM (fmap rowType . hasStaticShape . I.fromDecl) arr_t_ext

      let arraylit ks rt =
            I.BasicOp $ I.ArrayLit ks rt

      letSubExps desc $
        if null es'
        then map (arraylit []) rowtypes
        else zipWith arraylit (transpose es') rowtypes

  where isArrayLiteral :: E.Exp -> Maybe ([Int],[E.Exp])
        isArrayLiteral (E.ArrayLit inner_es _ _) = do
          (eshape,e):inner_es' <- mapM isArrayLiteral inner_es
          guard $ all ((eshape==) . fst) inner_es'
          return (length inner_es:eshape, e ++ concatMap snd inner_es')
        isArrayLiteral e =
          Just ([], [e])

internaliseExp desc (E.Range start maybe_second end (Info ret, Info retext) loc) = do
  start' <- internaliseExp1 "range_start" start
  end' <- internaliseExp1 "range_end" $ case end of
    DownToExclusive e -> e
    ToInclusive e -> e
    UpToExclusive e -> e
  maybe_second' <-
    traverse (internaliseExp1 "range_second") maybe_second

  -- Construct an error message in case the range is invalid.
  let conv = case E.typeOf start of
               E.Scalar (E.Prim (E.Unsigned _)) -> asIntZ Int32
               _ -> asIntS Int32
  start'_i32 <- conv start'
  end'_i32 <- conv end'
  maybe_second'_i32 <- traverse conv maybe_second'
  let errmsg =
        errorMsg $
        ["Range "] ++
        [ErrorInt32 start'_i32] ++
        (case maybe_second'_i32 of
           Nothing -> []
           Just second_i32 -> ["..", ErrorInt32 second_i32]) ++
        (case end of
           DownToExclusive{} -> ["..>"]
           ToInclusive{} -> ["..."]
           UpToExclusive{} -> ["..<"]) ++
        [ErrorInt32 end'_i32, " is invalid."]

  (it, le_op, lt_op) <-
    case E.typeOf start of
      E.Scalar (E.Prim (E.Signed it)) -> return (it, CmpSle it, CmpSlt it)
      E.Scalar (E.Prim (E.Unsigned it)) -> return (it, CmpUle it, CmpUlt it)
      start_t -> fail $ "Start value in range has type " ++ pretty start_t

  let one = intConst it 1
      negone = intConst it (-1)
      default_step = case end of DownToExclusive{} -> negone
                                 ToInclusive{} -> one
                                 UpToExclusive{} -> one

  (step, step_zero) <- case maybe_second' of
    Just second' -> do
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

  cs <- assertingOne $ do
    invalid <- letSubExp "range_invalid" $
               I.BasicOp $ I.BinOp I.LogOr step_invalid bounds_invalid
    valid <- letSubExp "valid" $ I.BasicOp $ I.UnOp I.Not invalid

    letExp "range_valid_c" $
      I.BasicOp $ I.Assert valid errmsg (loc, mempty)

  step_i32 <- asIntS Int32 step
  pos_step <- letSubExp "pos_step" $
              I.BasicOp $ I.BinOp (Mul Int32) step_i32 step_sign_i32

  num_elems <- certifying cs $
               letSubExp "num_elems" =<<
               eDivRoundingUp Int32 (eSubExp distance) (eSubExp pos_step)

  se <- letSubExp desc (I.BasicOp $ I.Iota num_elems start' step it)
  bindExtSizes (E.toStruct ret) retext [se]
  return [se]

internaliseExp desc (E.Ascript e _ _) =
  internaliseExp desc e

internaliseExp desc (E.Coerce e (TypeDecl dt (Info et)) _ loc) = do
  es <- internaliseExp desc e
  (ts, cm) <- internaliseReturnType et
  mapM_ (uncurry (internaliseDimConstant loc)) cm
  dt' <- typeExpForError cm dt
  forM (zip es ts) $ \(e',t') -> do
    dims <- arrayDims <$> subExpType e'
    let parts = ["Value of (core language) shape ("] ++
                intersperse ", " (map ErrorInt32 dims) ++
                [") cannot match shape of type `"] ++ dt' ++ ["`."]
    ensureExtShape asserting (errorMsg parts) loc (I.fromDecl t') desc e'

internaliseExp desc (E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.Sub t) (I.intConst t 0) e'
             I.Prim (I.FloatType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.FSub t) (I.floatConst t 0) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in Negate"

internaliseExp desc e@E.Apply{} = do
  (qfname, args, ret, retext) <- findFuncall e
  let fname = nameFromString $ pretty $ baseName $ qualLeaf qfname
      loc = srclocOf e
      arg_desc = nameToString fname ++ "_arg"

  -- Some functions are magical (overloaded) and we handle that here.
  ses <-
    case () of
      -- Overloaded functions never take array arguments (except
      -- equality, but those cannot be existential), so we can safely
      -- ignore the existential dimensions.
      () | Just internalise <- isOverloadedFunction qfname (map fst args) loc ->
             internalise desc

         | Just (rettype, _) <- M.lookup fname I.builtInFunctions -> do
             let tag ses = [ (se, I.Observe) | se <- ses ]
             args' <- mapM (internaliseArg arg_desc) args
             let args'' = concatMap tag args'
             letTupExp' desc $ I.Apply fname args'' [I.Prim rettype] (Safe, loc, [])

         | otherwise -> do
             args' <- concat <$> mapM (internaliseArg arg_desc) args
             fst <$> funcall desc qfname args' loc

  bindExtSizes ret retext ses
  return ses

internaliseExp desc (E.LetPat pat e body (Info ret, Info retext) loc) = do
  ses <- internalisePat desc pat e body loc (internaliseExp desc)
  bindExtSizes (E.toStruct ret) retext ses
  return ses

internaliseExp desc (E.LetFun ofname (tparams, params, retdecl, Info rettype, body) letbody loc) = do
  internaliseValBind $ E.ValBind Nothing ofname retdecl (Info (rettype, [])) tparams params body Nothing loc
  internaliseExp desc letbody

internaliseExp desc (E.DoLoop sparams mergepat mergeexp form loopbody (Info (ret, retext)) loc) = do
  ses <- internaliseExp "loop_init" mergeexp
  ((loopbody', (form', shapepat, mergepat', mergeinit')), initstms) <-
    collectStms $ handleForm ses form

  addStms initstms
  mergeinit_ts' <- mapM subExpType mergeinit'

  let ctxinit = argShapes
                (map I.paramName shapepat)
                (map I.paramType mergepat')
                mergeinit_ts'
      ctxmerge = zip shapepat ctxinit
      valmerge = zip mergepat' mergeinit'
      dropCond = case form of E.While{} -> drop 1
                              _         -> id

  -- Ensure that the result of the loop matches the shapes of the
  -- merge parameters.  XXX: Ideally they should already match (by
  -- the source language type rules), but some of our
  -- transformations (esp. defunctionalisation) strips out some size
  -- information.  For a type-correct source program, these reshapes
  -- should simplify away.
  let merge = ctxmerge ++ valmerge
      merge_names = map (I.paramName . fst) merge
      merge_ts = existentialiseExtTypes merge_names $
                 staticShapes $ map (I.paramType . fst) merge
  loopbody'' <- localScope (scopeOfFParams $ map fst merge) $
                inScopeOf form' $
                ensureResultExtShapeNoCtx asserting
                "shape of loop result does not match shapes in loop parameter"
                loc merge_ts loopbody'

  loop_res <- map I.Var . dropCond <$>
              letTupExp desc (I.DoLoop ctxmerge valmerge form' loopbody'')
  bindExtSizes (E.toStruct ret) retext loop_res
  return loop_res

  where
    sparams' = map (`TypeParamDim` noLoc) sparams

    forLoop nested_mergepat shapepat mergeinit form' = do
      let mergepat' = concat nested_mergepat
      bodyFromStms $ inScopeOf form' $ do
        ses <- internaliseExp "loopres" loopbody
        sets <- mapM subExpType ses
        let shapeargs = argShapes
                        (map I.paramName shapepat)
                        (map I.paramType mergepat')
                        sets
        return (shapeargs ++ ses,
                (form',
                 shapepat,
                 mergepat',
                 mergeinit))

    handleForm mergeinit (E.ForIn x arr) = do
      arr' <- internaliseExpToVars "for_in_arr" arr
      arr_ts <- mapM lookupType arr'
      let w = arraysSize 0 arr_ts

      i <- newVName "i"

      bindingParams sparams' [mergepat] $
        \mergecm shapepat nested_mergepat ->
        bindingLambdaParams [x] (map rowType arr_ts) $ \x_cm x_params -> do
          mapM_ (uncurry (internaliseDimConstant loc)) x_cm
          mapM_ (uncurry (internaliseDimConstant loc)) mergecm
          let loopvars = zip x_params arr'
          forLoop nested_mergepat shapepat mergeinit $ I.ForLoop i Int32 w loopvars

    handleForm mergeinit (E.For i num_iterations) = do
      num_iterations' <- internaliseExp1 "upper_bound" num_iterations
      i' <- internaliseIdent i
      num_iterations_t <- I.subExpType num_iterations'
      it <- case num_iterations_t of
              I.Prim (IntType it) -> return it
              _                   -> fail "internaliseExp DoLoop: invalid type"

      bindingParams sparams' [mergepat] $
        \mergecm shapepat nested_mergepat -> do
          mapM_ (uncurry (internaliseDimConstant loc)) mergecm
          forLoop nested_mergepat shapepat mergeinit $ I.ForLoop i' it num_iterations' []

    handleForm mergeinit (E.While cond) =
      bindingParams sparams' [mergepat] $ \mergecm shapepat nested_mergepat -> do
        mergeinit_ts <- mapM subExpType mergeinit
        mapM_ (uncurry (internaliseDimConstant loc)) mergecm
        let mergepat' = concat nested_mergepat
        -- We need to insert 'cond' twice - once for the initial
        -- condition (do we enter the loop at all?), and once with the
        -- result values of the loop (do we continue into the next
        -- iteration?).  This is safe, as the type rules for the
        -- external language guarantees that 'cond' does not consume
        -- anything.
        let shapeinit = argShapes
                        (map I.paramName shapepat)
                        (map I.paramType mergepat')
                        mergeinit_ts

        (loop_initial_cond, init_loop_cond_bnds) <- collectStms $ do
          forM_ (zip shapepat shapeinit) $ \(p, se) ->
            letBindNames_ [paramName p] $ BasicOp $ SubExp se
          forM_ (zip (concat nested_mergepat) mergeinit) $ \(p, se) ->
            unless (se == I.Var (paramName p)) $
            letBindNames_ [paramName p] $ BasicOp $
            case se of I.Var v | not $ primType $ paramType p ->
                                   Reshape (map DimCoercion $ arrayDims $ paramType p) v
                       _ -> SubExp se
          internaliseExp1 "loop_cond" cond

        addStms init_loop_cond_bnds

        bodyFromStms $ do
          ses <- internaliseExp "loopres" loopbody
          sets <- mapM subExpType ses
          loop_while <- newParam "loop_while" $ I.Prim I.Bool
          let shapeargs = argShapes
                          (map I.paramName shapepat)
                          (map I.paramType mergepat')
                          sets

          -- Careful not to clobber anything.
          loop_end_cond_body <- renameBody <=< insertStmsM $ do
            forM_ (zip shapepat shapeargs) $ \(p, se) ->
              unless (se == I.Var (paramName p)) $
              letBindNames_ [paramName p] $ BasicOp $ SubExp se
            forM_ (zip (concat nested_mergepat) ses) $ \(p, se) ->
              unless (se == I.Var (paramName p)) $
              letBindNames_ [paramName p] $ BasicOp $
              case se of I.Var v | not $ primType $ paramType p ->
                                     Reshape (map DimCoercion $ arrayDims $ paramType p) v
                         _ -> SubExp se
            resultBody <$> internaliseExp "loop_cond" cond
          loop_end_cond <- bodyBind loop_end_cond_body

          return (shapeargs++loop_end_cond++ses,
                  (I.WhileLoop $ I.paramName loop_while,
                   shapepat,
                   loop_while : mergepat',
                   loop_initial_cond : mergeinit))

internaliseExp desc (E.LetWith name src idxs ve body t loc) = do
  let pat = E.Id (E.identName name) (E.identType name) loc
      src_t = E.fromStruct <$> E.identType src
      e = E.Update (E.Var (E.qualName $ E.identName src) src_t loc) idxs ve loc
  internaliseExp desc $ E.LetPat pat e body (t, Info []) loc

internaliseExp desc (E.Update src slice ve loc) = do
  ves <- internaliseExp "lw_val" ve
  srcs <- internaliseExpToVars "src" src
  dims <- case srcs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', cs) <- internaliseSlice loc dims slice

  let comb sname ve' = do
        sname_t <- lookupType sname
        let full_slice = fullSlice sname_t idxs'
            rowtype = sname_t `setArrayDims` sliceDims full_slice
        ve'' <- ensureShape asserting "shape of value does not match shape of source array"
                loc rowtype "lw_val_correct_shape" ve'
        letInPlace desc sname full_slice $ BasicOp $ SubExp ve''
  certifying cs $ map I.Var <$> zipWithM comb srcs ves

internaliseExp desc (E.RecordUpdate src fields ve _ _) = do
  src' <- internaliseExp desc src
  ve' <- internaliseExp desc ve
  replace (E.typeOf src `setAliases` ()) fields ve' src'
  where replace (E.Scalar (E.Record m)) (f:fs) ve' src'
          | Just t <- M.lookup f m = do
          i <- fmap sum $ mapM (internalisedTypeSize . snd) $
               takeWhile ((/=f) . fst) $ sortFields m
          k <- internalisedTypeSize t
          let (bef, to_update, aft) = splitAt3 i k src'
          src'' <- replace t fs ve' to_update
          return $ bef ++ src'' ++ aft
        replace _ _ ve' _ = return ve'

internaliseExp desc (E.Unsafe e _) =
  local (\env -> env { envDoBoundsChecks = False }) $
  internaliseExp desc e

internaliseExp desc (E.Assert e1 e2 (Info check) loc) = do
  e1' <- internaliseExp1 "assert_cond" e1
  c <- assertingOne $ letExp "assert_c" $
       I.BasicOp $ I.Assert e1' (errorMsg [ErrorString check]) (loc, mempty)
  -- Make sure there are some bindings to certify.
  certifying c $ mapM rebind =<< internaliseExp desc e2
  where rebind v = do
          v' <- newVName "assert_res"
          letBindNames_ [v'] $ I.BasicOp $ I.SubExp v
          return $ I.Var v'

internaliseExp _ (E.Constr c es (Info (E.Scalar (E.Sum fs))) loc) = do
  ((ts, constr_map), cm) <- internaliseSumType $ M.map (map E.toStruct) fs
  mapM_ (uncurry (internaliseDimConstant loc)) cm
  es' <- concat <$> mapM (internaliseExp "payload") es

  let noExt _ = return $ intConst Int32 0
  ts' <- instantiateShapes noExt $ map fromDecl ts

  case M.lookup c constr_map of
    Just (i, js) ->
      (intConst Int8 (toInteger i):) <$> clauses 0 ts' (zip js es')
    Nothing ->
      fail "internaliseExp Constr: missing constructor"

  where clauses j (t:ts) js_to_es
          | Just e <- j `lookup` js_to_es =
              (e:) <$> clauses (j+1) ts js_to_es
          | otherwise = do
              blank <- letSubExp "zero" =<< eBlank t
              (blank:) <$> clauses (j+1) ts js_to_es
        clauses _ [] _ =
          return []

internaliseExp _ (E.Constr _ _ (Info t) loc) =
  fail $ "internaliseExp: constructor with type " ++ pretty t ++ " at " ++ locStr loc

internaliseExp desc (E.Match e cs _ _) = do
  ses <- internaliseExp (desc ++ "_scrutinee") e
  case NE.uncons cs of
    (CasePat pCase eCase locCase, Nothing) -> do
      (_, pertinent) <- generateCond pCase ses
      internalisePat' pCase pertinent eCase locCase (internaliseExp desc)
    (c, Just cs') -> do
      let CasePat pLast eLast locLast = NE.last cs'
      bFalse <- do
        (_, pertinent) <- generateCond pLast ses
        eLast' <- internalisePat' pLast pertinent eLast locLast internaliseBody
        foldM (\bf c' -> eBody $ return $ generateCaseIf ses c' bf) eLast' $
          reverse $ NE.init cs'
      letTupExp' desc =<< generateCaseIf ses c bFalse

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp _ (E.Literal v _) =
  return [I.Constant $ internalisePrimValue v]

internaliseExp _ (E.IntLit v (Info t) _) =
  case t of
    E.Scalar (E.Prim (E.Signed it)) ->
      return [I.Constant $ I.IntValue $ intValue it v]
    E.Scalar (E.Prim (E.Unsigned it)) ->
      return [I.Constant $ I.IntValue $ intValue it v]
    E.Scalar (E.Prim (E.FloatType ft)) ->
      return [I.Constant $ I.FloatValue $ floatValue ft v]
    _ -> fail $ "internaliseExp: nonsensical type for integer literal: " ++ pretty t

internaliseExp _ (E.FloatLit v (Info t) _) =
  case t of
    E.Scalar (E.Prim (E.FloatType ft)) ->
      return [I.Constant $ I.FloatValue $ floatValue ft v]
    _ -> fail $ "internaliseExp: nonsensical type for float literal: " ++ pretty t

internaliseExp desc (E.If ce te fe (Info ret, Info retext) _) = do
  ses <- letTupExp' desc =<<
         eIf (BasicOp . SubExp <$> internaliseExp1 "cond" ce)
         (internaliseBody te) (internaliseBody fe)
  bindExtSizes (E.toStruct ret) retext ses
  return ses

-- Builtin operators are handled specially because they are
-- overloaded.
internaliseExp desc (E.BinOp (op, _) _ (xe,_) (ye,_) _ _ loc)
  | Just internalise <- isOverloadedFunction op [xe, ye] loc =
      internalise desc

-- User-defined operators are just the same as a function call.
internaliseExp desc (E.BinOp (op, oploc) (Info t)
                     (xarg, Info (xt,xext)) (yarg, Info (yt,yext))
                     _ (Info retext) loc) =
  internaliseExp desc $
  E.Apply (E.Apply (E.Var op (Info t) oploc) xarg (Info (E.diet xt, xext))
           (Info $ foldFunType [E.fromStruct yt] t, Info []) loc)
          yarg (Info (E.diet yt, yext)) (Info t, Info retext) loc

internaliseExp desc (E.Project k e (Info rt) _) = do
  n <- internalisedTypeSize $ rt `setAliases` ()
  i' <- fmap sum $ mapM internalisedTypeSize $
        case E.typeOf e `setAliases` () of
               E.Scalar (Record fs) ->
                 map snd $ takeWhile ((/=k) . fst) $ sortFields fs
               t -> [t]
  take n . drop i' <$> internaliseExp desc e

internaliseExp _ e@E.Lambda{} =
  fail $ "internaliseExp: Unexpected lambda at " ++ locStr (srclocOf e)

internaliseExp _ e@E.OpSection{} =
  fail $ "internaliseExp: Unexpected operator section at " ++ locStr (srclocOf e)

internaliseExp _ e@E.OpSectionLeft{} =
  fail $ "internaliseExp: Unexpected left operator section at " ++ locStr (srclocOf e)

internaliseExp _ e@E.OpSectionRight{} =
  fail $ "internaliseExp: Unexpected right operator section at " ++ locStr (srclocOf e)

internaliseExp _ e@E.ProjectSection{} =
  fail $ "internaliseExp: Unexpected projection section at " ++ locStr (srclocOf e)

internaliseExp _ e@E.IndexSection{} =
  fail $ "internaliseExp: Unexpected index section at " ++ locStr (srclocOf e)

internaliseArg :: String -> (E.Exp, Maybe VName) -> InternaliseM [SubExp]
internaliseArg desc (arg, argdim) = do
  arg' <- internaliseExp desc arg
  case (arg', argdim) of
    ([se], Just d) -> letBindNames_ [d] $ BasicOp $ SubExp se
    _ -> return ()
  return arg'

generateCond :: E.Pattern -> [I.SubExp] -> InternaliseM (I.SubExp, [I.SubExp])
generateCond orig_p orig_ses = do
  (cmps, pertinent, _) <- compares orig_p orig_ses
  cmp <- letSubExp "matches" =<< foldBinOp I.LogAnd (constant True) cmps
  return (cmp, pertinent)
  where
    -- Literals are always primitive values.
    compares (E.PatternLit e _ _) (se:ses) = do
      e' <- internaliseExp1 "constant" e
      I.Prim t' <- subExpType se
      cmp <- letSubExp "match_lit" $ I.BasicOp $ I.CmpOp (I.CmpEq t') e' se
      return ([cmp], [se], ses)

    compares (E.PatternConstr c (Info (E.Scalar (E.Sum fs))) pats _) (se:ses) = do
      ((payload_ts, m), _) <- internaliseSumType $ M.map (map toStruct) fs
      case M.lookup c m of
        Just (i, payload_is) -> do
          let i' = intConst Int8 $ toInteger i
          let (payload_ses, ses') = splitAt (length payload_ts) ses
          cmp <- letSubExp "match_constr" $ I.BasicOp $ I.CmpOp (I.CmpEq int8) i' se
          (cmps, pertinent, _) <- comparesMany pats $ map (payload_ses!!) payload_is
          return (cmp : cmps, pertinent, ses')
        Nothing ->
          fail "generateCond: missing constructor"

    compares (E.PatternConstr _ (Info t) _ _) _ =
      fail $ "generateCond: PatternConstr has nonsensical type: " ++ pretty t

    compares (E.Id _ t loc) ses =
      compares (E.Wildcard t loc) ses

    compares (E.Wildcard (Info t) _) ses = do
      n <- internalisedTypeSize $ E.toStruct t
      let (id_ses, rest_ses) = splitAt n ses
      return ([], id_ses, rest_ses)

    compares (E.PatternParens pat _) ses =
      compares pat ses

    compares (E.TuplePattern pats _) ses =
      comparesMany pats ses

    compares (E.RecordPattern fs _) ses =
      comparesMany (map snd $ E.sortFields $ M.fromList fs) ses

    compares (E.PatternAscription pat _ _) ses =
      compares pat ses

    compares pat [] =
      error $ "generateCond: No values left for pattern " ++ pretty pat

    comparesMany [] ses = return ([], [], ses)
    comparesMany (pat:pats) ses = do
      (cmps1, pertinent1, ses') <- compares pat ses
      (cmps2, pertinent2, ses'') <- comparesMany pats ses'
      return (cmps1 <> cmps2,
              pertinent1 <> pertinent2,
              ses'')

generateCaseIf :: [I.SubExp] -> Case -> I.Body -> InternaliseM I.Exp
generateCaseIf ses (CasePat p eCase loc) bFail = do
  (cond, pertinent) <- generateCond p ses
  eCase' <- internalisePat' p pertinent eCase loc internaliseBody
  eIf (eSubExp cond) (return eCase') (return bFail)

internalisePat :: String -> E.Pattern -> E.Exp
               -> E.Exp -> SrcLoc -> (E.Exp -> InternaliseM a)
               -> InternaliseM a
internalisePat desc p e body loc m = do
  ses <- internaliseExp desc' e
  internalisePat' p ses body loc m
  where desc' = case S.toList $ E.patternIdents p of
                  [v] -> baseString $ E.identName v
                  _ -> desc

internalisePat' :: E.Pattern -> [I.SubExp]
                -> E.Exp -> SrcLoc -> (E.Exp -> InternaliseM a)
                -> InternaliseM a
internalisePat' p ses body loc m = do
  t <- I.staticShapes <$> mapM I.subExpType ses
  stmPattern p t $ \cm pat_names match -> do
    mapM_ (uncurry (internaliseDimConstant loc)) cm
    ses' <- match loc ses
    forM_ (zip pat_names ses') $ \(v,se) ->
      letBindNames_ [v] $ I.BasicOp $ I.SubExp se
    m body

internaliseSlice :: SrcLoc
                 -> [SubExp]
                 -> [E.DimIndex]
                 -> InternaliseM ([I.DimIndex SubExp], Certificates)
internaliseSlice loc dims idxs = do
 (idxs', oks, parts) <- unzip3 <$> zipWithM internaliseDimIndex dims idxs
 c <- assertingOne $ do
   ok <- letSubExp "index_ok" =<< foldBinOp I.LogAnd (constant True) oks
   let msg = errorMsg $ ["Index ["] ++ intercalate [", "] parts ++
             ["] out of bounds for array of shape ["] ++
             intersperse "][" (map ErrorInt32 $ take (length idxs) dims) ++ ["]."]
   letExp "index_certs" $ I.BasicOp $ I.Assert ok msg (loc, mempty)
 return (idxs', c)

internaliseDimIndex :: SubExp -> E.DimIndex
                    -> InternaliseM (I.DimIndex SubExp, SubExp, [ErrorMsgPart SubExp])
internaliseDimIndex w (E.DimFix i) = do
  (i', _) <- internaliseDimExp "i" i
  let lowerBound = I.BasicOp $
                   I.CmpOp (I.CmpSle I.Int32) (I.constant (0 :: I.Int32)) i'
      upperBound = I.BasicOp $
                   I.CmpOp (I.CmpSlt I.Int32) i' w
  ok <- letSubExp "bounds_check" =<< eBinOp I.LogAnd (pure lowerBound) (pure upperBound)
  return (I.DimFix i', ok, [ErrorInt32 i'])
internaliseDimIndex w (E.DimSlice i j s) = do
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
  -- Something like a division-rounding-up, but accomodating negative
  -- operands.
  let divRounding x y =
        eBinOp (SQuot Int32) (eBinOp (Add Int32) x (eBinOp (Sub Int32) y (eSignum $ toExp s'))) y
  n <- letSubExp "n" =<< divRounding (toExp j_m_i) (toExp s')

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

  let parts = case (i, j, s) of
                (_, _, Just{}) ->
                  [maybe "" (const $ ErrorInt32 i') i, ":",
                   maybe "" (const $ ErrorInt32 j') j, ":",
                   ErrorInt32 s']
                (_, Just{}, _) ->
                  [maybe "" (const $ ErrorInt32 i') i, ":",
                   ErrorInt32 j'] ++
                   maybe mempty (const [":", ErrorInt32 s']) s
                (_, Nothing, Nothing) ->
                  [ErrorInt32 i']
  return (I.DimSlice i' n s', ok_or_empty, parts)
  where zero = constant (0::Int32)
        negone = constant (-1::Int32)
        one = constant (1::Int32)

internaliseScanOrReduce :: String -> String
                        -> (SubExp -> I.Lambda -> [SubExp] -> [VName] -> InternaliseM (SOAC SOACS))
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
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc . I.Op =<< f w lam' nes' arrs

internaliseHist :: String
                -> E.Exp -> E.Exp -> E.Exp -> E.Exp -> E.Exp -> E.Exp -> SrcLoc
                -> InternaliseM [SubExp]
internaliseHist desc rf hist op ne buckets img loc = do
  rf' <- internaliseExp1 "hist_rf" rf
  ne' <- internaliseExp "hist_ne" ne
  hist' <- internaliseExpToVars "hist_hist" hist
  buckets' <- letExp "hist_buckets" . BasicOp . SubExp =<<
              internaliseExp1 "hist_buckets" buckets
  img' <- internaliseExpToVars "hist_img" img

  -- reshape neutral element to have same size as the destination array
  ne_shp <- forM (zip ne' hist') $ \(n, h) -> do
    rowtype <- I.stripArray 1 <$> lookupType h
    ensureShape asserting
      "Row shape of destination array does not match shape of neutral element"
      loc rowtype "hist_ne_right_shape" n
  ne_ts <- mapM I.subExpType ne_shp
  his_ts <- mapM lookupType hist'
  op' <- internaliseFoldLambda internaliseLambda op ne_ts his_ts

  -- reshape return type of bucket function to have same size as neutral element
  -- (modulo the index)
  bucket_param <- newParam "bucket_p" $ I.Prim int32
  img_params <- mapM (newParam "img_p" . rowType) =<< mapM lookupType img'
  let params = bucket_param : img_params
      rettype = I.Prim int32 : ne_ts
      body = mkBody mempty $ map (I.Var . paramName) params
  body' <- localScope (scopeOfLParams params) $
           ensureResultShape asserting
           "Row shape of value array does not match row shape of hist target"
           (srclocOf img) rettype body

  -- get sizes of histogram and image arrays
  w_hist <- arraysSize 0 <$> mapM lookupType hist'
  w_img <- arraysSize 0 <$> mapM lookupType img'

  -- Generate an assertion and reshapes to ensure that buckets' and
  -- img' are the same size.
  b_shape <- I.arrayShape <$> lookupType buckets'
  let b_w = shapeSize 0 b_shape
  cmp <- letSubExp "bucket_cmp" $ I.BasicOp $ I.CmpOp (I.CmpEq I.int32) b_w w_img
  c <- assertingOne $
    letExp "bucket_cert" $ I.BasicOp $
    I.Assert cmp "length of index and value array does not match" (loc, mempty)
  buckets'' <- certifying c $ letExp (baseString buckets') $
    I.BasicOp $ I.Reshape (reshapeOuter [DimCoercion w_img] 1 b_shape) buckets'

  letTupExp' desc $ I.Op $
    I.Hist w_img [HistOp w_hist rf' hist' ne_shp op'] (I.Lambda params body' rettype) $ buckets'' : img'

internaliseStreamMap :: String -> StreamOrd -> E.Exp -> E.Exp
                     -> InternaliseM [SubExp]
internaliseStreamMap desc o lam arr = do
  arrs <- internaliseExpToVars "stream_input" arr
  lam' <- internaliseStreamMapLambda internaliseLambda lam $ map I.Var arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  let form = I.Parallel o Commutative (I.Lambda [] (mkBody mempty []) []) []
  letTupExp' desc $ I.Op $ I.Stream w form lam' arrs

internaliseStreamRed :: String -> StreamOrd -> Commutativity
                     -> E.Exp -> E.Exp -> E.Exp
                     -> InternaliseM [SubExp]
internaliseStreamRed desc o comm lam0 lam arr = do
  arrs <- internaliseExpToVars "stream_input" arr
  rowts <- mapM (fmap I.rowType . lookupType) arrs
  (lam_params, lam_body) <-
    internaliseStreamLambda internaliseLambda lam rowts
  let (chunk_param, _, lam_val_params) =
        partitionChunkedFoldParameters 0 lam_params

  -- Synthesize neutral elements by applying the fold function
  -- to an empty chunk.
  letBindNames_ [I.paramName chunk_param] $
    I.BasicOp $ I.SubExp $ constant (0::Int32)
  forM_ lam_val_params $ \p ->
    letBindNames_ [I.paramName p] $
    I.BasicOp $ I.Scratch (I.elemType $ I.paramType p) $
    I.arrayDims $ I.paramType p
  accs <- bodyBind =<< renameBody lam_body

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
    (srclocOf lam0) acctps <=< insertStmsM $ do
      lam_res <- bodyBind lam_body

      let consumed = consumedByLambda $ Alias.analyseLambda lam0'
          copyIfConsumed p (I.Var v)
            | I.paramName p `nameIn` consumed =
                letSubExp "acc_copy" $ I.BasicOp $ I.Copy v
          copyIfConsumed _ x = return x

      accs' <- zipWithM copyIfConsumed (I.lambdaParams lam0') accs
      lam_res' <- ensureArgShapes asserting
                  "shape of chunk function result does not match shape of initial value"
                  (srclocOf lam) [] (map I.typeOf $ I.lambdaParams lam0') lam_res
      new_lam_res <- eLambda lam0' $ map eSubExp $ accs' ++ lam_res'
      return $ resultBody new_lam_res

  -- Make sure the chunk size parameter comes first.
  let form = I.Parallel o comm lam0' accs
      lam' = I.Lambda { lambdaParams = chunk_param : acc_params ++ lam_val_params
                      , lambdaBody = body_with_lam0
                      , lambdaReturnType = acctps }
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ I.Stream w form lam' arrs

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
    E.Scalar (E.Prim (Signed it))   -> (,it) <$> asIntS Int32 e'
    E.Scalar (E.Prim (Unsigned it)) -> (,it) <$> asIntZ Int32 e'
    _                               -> fail "internaliseDimExp: bad type"

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
internaliseBinOp desc E.Mod x y (E.FloatType t) _ =
  simpleBinOp desc (I.FMod t) x y
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

findFuncall :: E.Exp -> InternaliseM (E.QualName VName,
                                      [(E.Exp, Maybe VName)],
                                      E.StructType,
                                      [VName])
findFuncall (E.Var fname (Info t) _) =
  return (fname, [], E.toStruct t, [])
findFuncall (E.Apply f arg (Info (_, argext)) (Info ret, Info retext) _) = do
  (fname, args, _, _) <- findFuncall f
  return (fname, args ++ [(arg, argext)], E.toStruct ret, retext)
findFuncall e =
  fail $ "Invalid function expression in application: " ++ pretty e

internaliseLambda :: InternaliseLambda

internaliseLambda (E.Parens e _) rowtypes =
  internaliseLambda e rowtypes

internaliseLambda (E.Lambda params body _ (Info (_, rettype)) loc) rowtypes =
  bindingLambdaParams params rowtypes $ \pcm params' -> do
    (rettype', rcm) <- internaliseReturnType rettype
    body' <- internaliseBody body
    mapM_ (uncurry (internaliseDimConstant loc)) $ pcm<>rcm
    return (params', body', map I.fromDecl rettype')

internaliseLambda e _ = fail $ "internaliseLambda: unexpected expression:\n" ++ pretty e

internaliseDimConstant :: SrcLoc -> Name -> VName -> InternaliseM ()
internaliseDimConstant loc fname name =
  letBind_ (basicPattern [] [I.Ident name $ I.Prim I.int32]) $
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

    handle [x] "!" = Just $ complementF x

    handle [x] "opaque" = Just $ \desc ->
      mapM (letSubExp desc . BasicOp . Opaque) =<< internaliseExp "opaque_arg" x

    handle [x] s
      | Just unop <- find ((==s) . pretty) allUnOps = Just $ \desc -> do
          x' <- internaliseExp1 "x" x
          fmap pure $ letSubExp desc $ I.BasicOp $ I.UnOp unop x'

    handle [TupLit [x,y] _] s
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
      E.If x y (E.Literal (E.BoolValue False) noLoc) (Info $ E.Scalar $ E.Prim E.Bool, Info []) noLoc
    handle [x,y] "||" = Just $ \desc ->
        internaliseExp desc $
        E.If x (E.Literal (E.BoolValue True) noLoc) y (Info $ E.Scalar $ E.Prim E.Bool, Info []) noLoc

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
                      cmps <- letExp "cmps" $ I.Op $
                              I.Screma x_num_elems (I.mapSOAC cmp_lam) [x_flat, y_flat]

                      -- Check that all were equal.
                      and_lam <- binOpLambda I.LogAnd I.Bool
                      reduce <- I.reduceSOAC [Reduce Commutative and_lam [constant True]]
                      all_equal <- letSubExp "all_equal" $ I.Op $ I.Screma x_num_elems reduce [cmps]
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
          (E.Scalar (E.Prim t1), E.Scalar (E.Prim t2)) ->
            internaliseBinOp desc bop x' y' t1 t2
          _ -> fail "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."

    handle [E.TupLit [a, si, v] _] "scatter" = Just $ scatterF a si v

    handle [E.TupLit [e, E.ArrayLit vs _ _] _] "cmp_threshold" = do
      s <- mapM isCharLit vs
      Just $ \desc -> do
        x <- internaliseExp1 "threshold_x" e
        pure <$> letSubExp desc (Op $ CmpThreshold x s)
      where isCharLit (Literal (SignedValue iv) _) = Just $ chr $ fromIntegral $ intToInt64 iv
            isCharLit _                            = Nothing

    handle [E.TupLit [n, m, arr] _] "unflatten" = Just $ \desc -> do
      arrs <- internaliseExpToVars "unflatten_arr" arr
      n' <- internaliseExp1 "n" n
      m' <- internaliseExp1 "m" m
      -- The unflattened dimension needs to have the same number of elements
      -- as the original dimension.
      old_dim <- I.arraysSize 0 <$> mapM lookupType arrs
      dim_ok <- assertingOne $ letExp "dim_ok" =<<
                eAssert (eCmpOp (I.CmpEq I.int32)
                         (eBinOp (I.Mul Int32) (eSubExp n') (eSubExp m'))
                         (eSubExp old_dim))
                "new shape has different number of elements than old shape" loc
      certifying dim_ok $ forM arrs $ \arr' -> do
        arr_t <- lookupType arr'
        letSubExp desc $ I.BasicOp $
          I.Reshape (reshapeOuter [DimNew n', DimNew m'] 1 $ I.arrayShape arr_t) arr'

    handle [arr] "flatten" = Just $ \desc -> do
      arrs <- internaliseExpToVars "flatten_arr" arr
      forM arrs $ \arr' -> do
        arr_t <- lookupType arr'
        let n = arraySize 0 arr_t
            m = arraySize 1 arr_t
        k <- letSubExp "flat_dim" $ I.BasicOp $ I.BinOp (Mul Int32) n m
        letSubExp desc $ I.BasicOp $
          I.Reshape (reshapeOuter [DimNew k] 2 $ I.arrayShape arr_t) arr'

    handle [TupLit [x, y] _] "concat" = Just $ \desc -> do
      xs <- internaliseExpToVars "concat_x" x
      ys <- internaliseExpToVars "concat_y" y
      outer_size <- arraysSize 0 <$> mapM lookupType xs
      let sumdims xsize ysize = letSubExp "conc_tmp" $ I.BasicOp $
                                I.BinOp (I.Add I.Int32) xsize ysize
      ressize <- foldM sumdims outer_size =<<
                 mapM (fmap (arraysSize 0) . mapM lookupType) [ys]

      let conc xarr yarr =
            I.BasicOp $ I.Concat 0 xarr [yarr] ressize
      letSubExps desc $ zipWith conc xs ys

    handle [TupLit [offset, e] _] "rotate" = Just $ \desc -> do
      offset' <- internaliseExp1 "rotation_offset" offset
      internaliseOperation desc e $ \v -> do
        r <- I.arrayRank <$> lookupType v
        let zero = intConst Int32 0
            offsets = offset' : replicate (r-1) zero
        return $ I.Rotate offsets v

    handle [e] "transpose" = Just $ \desc ->
      internaliseOperation desc e $ \v -> do
        r <- I.arrayRank <$> lookupType v
        return $ I.Rearrange ([1,0] ++ [2..r-1]) v

    handle [TupLit [x, y] _] "zip" = Just $ \desc ->
      (++) <$> internaliseExp (desc ++ "_zip_x") x
           <*> internaliseExp (desc ++ "_zip_y") y

    handle [TupLit [lam, arr] _] "map" = Just $ \desc -> do
      arr' <- internaliseExpToVars "map_arr" arr
      lam' <- internaliseMapLambda internaliseLambda lam $ map I.Var arr'
      w <- arraysSize 0 <$> mapM lookupType arr'
      letTupExp' desc $ I.Op $
        I.Screma w (I.mapSOAC lam') arr'

    handle [TupLit [lam, arr] _] "filter" = Just $ \_desc -> do
      arrs <- internaliseExpToVars "filter_input" arr
      lam' <- internalisePartitionLambda internaliseLambda 1 lam $ map I.Var arrs
      uncurry (++) <$> partitionWithSOACS 1 lam' arrs

    handle [TupLit [k, lam, arr] _] "partition" = do
      k' <- fromIntegral <$> isInt32 k
      Just $ \_desc -> do
        arrs <- internaliseExpToVars "partition_input" arr
        lam' <- internalisePartitionLambda internaliseLambda k' lam $ map I.Var arrs
        uncurry (++) <$> partitionWithSOACS k' lam' arrs
        where isInt32 (Literal (SignedValue (Int32Value k')) _) = Just k'
              isInt32 (IntLit k' (Info (E.Scalar (E.Prim (Signed Int32)))) _) = Just $ fromInteger k'
              isInt32 _ = Nothing

    handle [TupLit [lam, ne, arr] _] "reduce" = Just $ \desc ->
      internaliseScanOrReduce desc "reduce" reduce (lam, ne, arr, loc)
      where reduce w red_lam nes arrs =
              I.Screma w <$>
              I.reduceSOAC [Reduce Noncommutative red_lam nes] <*> pure arrs

    handle [TupLit [lam, ne, arr] _] "reduce_comm" = Just $ \desc ->
      internaliseScanOrReduce desc "reduce" reduce (lam, ne, arr, loc)
      where reduce w red_lam nes arrs =
              I.Screma w <$>
              I.reduceSOAC [Reduce Commutative red_lam nes] <*> pure arrs

    handle [TupLit [lam, ne, arr] _] "scan" = Just $ \desc ->
      internaliseScanOrReduce desc "scan" reduce (lam, ne, arr, loc)
      where reduce w scan_lam nes arrs =
              I.Screma w <$> I.scanSOAC scan_lam nes <*> pure arrs

    handle [TupLit [op, f, arr] _] "reduce_stream" = Just $ \desc ->
      internaliseStreamRed desc InOrder Noncommutative op f arr

    handle [TupLit [op, f, arr] _] "reduce_stream_per" = Just $ \desc ->
      internaliseStreamRed desc Disorder Commutative op f arr

    handle [TupLit [f, arr] _] "map_stream" = Just $ \desc ->
      internaliseStreamMap desc InOrder f arr

    handle [TupLit [f, arr] _] "map_stream_per" = Just $ \desc ->
      internaliseStreamMap desc Disorder f arr

    handle [TupLit [rf, dest, op, ne, buckets, img] _] "hist" = Just $ \desc ->
      internaliseHist desc rf dest op ne buckets img loc

    handle [x] "unzip" = Just $ flip internaliseExp x
    handle [x] "trace" = Just $ flip internaliseExp x
    handle [x] "break" = Just $ flip internaliseExp x

    handle _ _ = Nothing

    toSigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Scalar (E.Prim E.Bool) ->
          letTupExp' desc $ I.If e' (resultBody [intConst int_to 1])
                                    (resultBody [intConst int_to 0]) $
                                    ifCommon [I.Prim $ I.IntType int_to]
        E.Scalar (E.Prim (E.Signed int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SExt int_from int_to) e'
        E.Scalar (E.Prim (E.Unsigned int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Scalar (E.Prim (E.FloatType float_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToSI float_from int_to) e'
        _ -> fail "Futhark.Internalise.handle: non-numeric type in ToSigned"

    toUnsigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Scalar (E.Prim E.Bool) ->
          letTupExp' desc $ I.If e' (resultBody [intConst int_to 1])
                                    (resultBody [intConst int_to 0]) $
                                    ifCommon [I.Prim $ I.IntType int_to]
        E.Scalar (E.Prim (E.Signed int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Scalar (E.Prim (E.Unsigned int_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Scalar (E.Prim (E.FloatType float_from)) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToUI float_from int_to) e'
        _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToUnsigned"

    complementF e desc = do
      e' <- internaliseExp1 "complement_arg" e
      et <- subExpType e'
      case et of I.Prim (I.IntType t) ->
                   letTupExp' desc $ I.BasicOp $ I.UnOp (I.Complement t) e'
                 I.Prim I.Bool ->
                   letTupExp' desc $ I.BasicOp $ I.UnOp I.Not e'
                 _ ->
                   fail "Futhark.Internalise.internaliseExp: non-int/bool type in Complement"

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
      letTupExp' desc $ I.Op $ I.Scatter si_w lam sivs $ zip3 sa_ws (repeat 1) sas

-- | Is the name a value constant?  If so, create the necessary
-- function call and return the corresponding subexpressions.
internaliseIfConst :: SrcLoc -> VName -> InternaliseM (Maybe [SubExp])
internaliseIfConst loc name = do
  is_const <- lookupConst name
  scope <- askScope
  case is_const of
    Just (fname, constparams, mk_rettype, bind_ext)
      | name `M.notMember` scope -> do
      (constargs, const_ds, const_ts) <- unzip3 <$> constFunctionArgs loc constparams
      safety <- askSafety
      case mk_rettype $ zip constargs $ map I.fromDecl const_ts of
        Nothing -> fail $ "internaliseIfConst: " ++
                   unwords (pretty name : zipWith (curry pretty) constargs const_ts) ++
                   " failed"
        Just rettype -> do
          ses <- fmap (map I.Var) $ letTupExp (baseString name) $
                 I.Apply fname (zip constargs const_ds) rettype (safety, loc, mempty)
          bind_ext ses
          return $ Just ses
    _ -> return Nothing

constFunctionArgs :: SrcLoc -> ConstParams -> InternaliseM [(SubExp, I.Diet, I.DeclType)]
constFunctionArgs loc = mapM arg
  where arg (fname, name) = do
          safety <- askSafety
          se <- letSubExp (baseString name ++ "_arg") $
                I.Apply fname [] [I.Prim I.int32] (safety, loc, [])
          return (se, I.ObservePrim, I.Prim I.int32)

funcall :: String -> QualName VName -> [SubExp] -> SrcLoc
        -> InternaliseM ([SubExp], [I.ExtType])
funcall desc (QualName _ fname) args loc = do
  (fname', constparams, closure, shapes, value_paramts, fun_params, rettype_fun) <-
    lookupFunction fname
  (constargs, const_ds, _) <- unzip3 <$> constFunctionArgs loc constparams
  argts <- mapM subExpType args
  closure_ts <- mapM lookupType closure
  let shapeargs = argShapes shapes value_paramts argts
      diets = const_ds ++ replicate (length closure + length shapeargs) I.ObservePrim ++
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

-- Bind existential names defined by an expression, based on the
-- concrete values that expression evaluated to.  This most
-- importantly should be done after function calls, but also
-- everything else that can produce existentials in the source
-- language.
bindExtSizes :: E.StructType -> [VName] -> [SubExp] -> InternaliseM ()
bindExtSizes ret retext ses = do
  ts <- concat . fst <$>
        internaliseParamTypes mempty (M.fromList $ zip retext retext) [ret]
  ses_ts <- mapM subExpType ses

  let combine t1 t2 =
        mconcat $ zipWith combine' (arrayExtDims t1) (arrayDims t2)
      combine' (I.Free (I.Var v)) se
        | v `elem` retext = M.singleton v se
      combine' _ _ = mempty

  forM_ (M.toList $ mconcat $ zipWith combine ts ses_ts) $ \(v, se) ->
    letBindNames_ [v] $ BasicOp $ SubExp se

askSafety :: InternaliseM Safety
askSafety = do check <- asks envDoBoundsChecks
               safe <- asks envSafe
               return $ if check || safe then I.Safe else I.Unsafe

-- Implement partitioning using maps, scans and writes.
partitionWithSOACS :: Int -> I.Lambda -> [I.VName] -> InternaliseM ([I.SubExp], [I.SubExp])
partitionWithSOACS k lam arrs = do
  arr_ts <- mapM lookupType arrs
  let w = arraysSize 0 arr_ts
  classes_and_increments <- letTupExp "increments" $ I.Op $ I.Screma w (mapSOAC lam) arrs
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
      nes = replicate (length increments) $ constant (0::Int32)

  scan <- I.scanSOAC add_lam nes
  all_offsets <- letTupExp "offsets" $ I.Op $ I.Screma w scan increments

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
             write_lam (classes : all_offsets ++ arrs) $
             zip3 (repeat sum_of_partition_sizes) (repeat 1) blanks
  sizes' <- letSubExp "partition_sizes" $ I.BasicOp $
            I.ArrayLit (map I.Var sizes) $ I.Prim int32
  return (map I.Var results, [sizes'])
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

typeExpForError :: ConstParams -> E.TypeExp VName -> InternaliseM [ErrorMsgPart SubExp]
typeExpForError _ (E.TEVar qn _) =
  return [ErrorString $ pretty qn]
typeExpForError cm (E.TEUnique te _) = ("*":) <$> typeExpForError cm te
typeExpForError cm (E.TEArray te d _) = do
  d' <- dimExpForError cm d
  te' <- typeExpForError cm te
  return $ ["[", d', "]"] ++ te'
typeExpForError cm (E.TETuple tes _) = do
  tes' <- mapM (typeExpForError cm) tes
  return $ ["("] ++ intercalate [", "] tes' ++ [")"]
typeExpForError cm (E.TERecord fields _) = do
  fields' <- mapM onField fields
  return $ ["{"] ++ intercalate [", "] fields' ++ ["}"]
  where onField (k, te) = (ErrorString (pretty k ++ ": "):) <$> typeExpForError cm te
typeExpForError cm (E.TEArrow _ t1 t2 _) = do
  t1' <- typeExpForError cm t1
  t2' <- typeExpForError cm t2
  return $ t1' ++ [" -> "] ++ t2'
typeExpForError cm (E.TEApply t arg _) = do
  t' <- typeExpForError cm t
  arg' <- case arg of TypeArgExpType argt -> typeExpForError cm argt
                      TypeArgExpDim d _   -> pure <$> dimExpForError cm d
  return $ t' ++ [" "] ++ arg'
typeExpForError cm (E.TESum cs _) = do
  cs' <- mapM (onClause . snd) cs
  return $ intercalate [" | "] cs'
  where onClause c = do
          c' <- mapM (typeExpForError cm) c
          return $ intercalate [" "] c'

dimExpForError :: ConstParams -> E.DimExp VName -> InternaliseM (ErrorMsgPart SubExp)
dimExpForError cm (DimExpNamed d _) = do
  substs <- asks $ M.lookup (E.qualLeaf d) . envSubsts
  let fname = nameFromString $ pretty (E.qualLeaf d) ++ "f"
  d' <- case (substs, lookup fname cm) of
          (Just [v], _) -> return v
          (_, Just v)   -> return $ I.Var v
          _             -> return $ I.Var $ E.qualLeaf d
  return $ ErrorInt32 d'
dimExpForError _ (DimExpConst d _) =
  return $ ErrorString $ pretty d
dimExpForError _ DimExpAny = return ""

-- A smart constructor that compacts neighbouring literals for easier
-- reading in the IR.
errorMsg :: [ErrorMsgPart a] -> ErrorMsg a
errorMsg = ErrorMsg . compact
  where compact [] = []
        compact (ErrorString x : ErrorString y : parts) =
          compact (ErrorString (x++y) : parts)
        compact (x:y) = x : compact y
