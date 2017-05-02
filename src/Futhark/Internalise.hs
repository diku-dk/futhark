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
import Control.Applicative
import Control.Monad.State  hiding (mapM, sequence)
import Control.Monad.Reader hiding (mapM, sequence)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Ord
import Data.Traversable (mapM, sequence)
import Data.Loc

import Prelude hiding (mapM, sequence, mod)

import Language.Futhark as E hiding (TypeArg)
import Futhark.Representation.SOACS as I hiding (bindingPattern)
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
import Futhark.Util (chunks, dropAt)

-- | Convert a program in source Futhark to a program in the Futhark
-- core language.
internaliseProg :: MonadFreshNames m =>
                   E.Prog -> m (Either String I.Prog)
internaliseProg prog = do
  prog' <- fmap (fmap I.Prog) $ runInternaliseM $ do
    addBuiltinFunctions
    internaliseDecs $ progDecs prog
  sequence $ fmap I.renameProg prog'

addBuiltinFunctions :: InternaliseM ()
addBuiltinFunctions = mapM_ addBuiltin $ M.toList E.intrinsics
  where addBuiltin (name, E.IntrinsicMonoFun paramts t) =
          noteFunction name $ const $
           return (baseName name,
                   [], [], [], map (I.Prim . internalisePrimType) paramts,
                   params,
                   const $ Just $ ExtRetType [I.Prim $ internalisePrimType t])
          where params =
                  [Param (VName (nameFromString "x") i) (I.Prim $ internalisePrimType pt)
                  | (i,pt) <- zip [0..] paramts]
        addBuiltin _ =
          return ()

internaliseDecs :: [E.Dec] -> InternaliseM ()
internaliseDecs ds =
  case ds of
    [] ->
      return ()
    ValDec vdec : ds' -> do
      internaliseValBind vdec
      internaliseDecs ds'
    FunDec fdec : ds' -> do
      internaliseFunBind fdec
      internaliseDecs ds'
    E.ModDec mb : ds' | null (modParams mb) -> do
      let me = maybeAscript (srclocOf mb) (E.modSignature mb) $ modExp mb
      internaliseModExp me
      v <- lookupSubst $ E.qualName $ E.modName mb
      noteMod v mempty me
      internaliseDecs ds'
    E.ModDec mb : ds' -> do
      v <- fulfillingPromise $ E.modName mb
      substs <- asks envFunctorSubsts
      let addParam p me = E.ModLambda p Nothing me $ srclocOf me
      noteMod v substs $
        foldr addParam (maybeAscript (srclocOf mb) (E.modSignature mb) $ E.modExp mb) $
        modParams mb
      internaliseDecs ds'
    E.TypeDec tb : ds' -> do
      v <- fulfillingPromise $ E.typeAlias tb
      substs <- allSubsts
      noteType v (substs, E.typeParams tb,
                  E.unInfo $ E.expandedType $ E.typeExp tb)
      internaliseDecs ds'
    E.OpenDec e es (Info added) _ : ds' -> do
      mapM_ internaliseModExp (e:es)
      mapM_ openedName added
      internaliseDecs ds'
    _ :ds' ->
      internaliseDecs ds'

maybeAscript :: SrcLoc -> Maybe (SigExp, Info (M.Map VName VName)) -> ModExp
             -> ModExp
maybeAscript loc (Just (mtye, substs)) me = ModAscript me mtye substs loc
maybeAscript _ Nothing me = me

internaliseModExp :: E.ModExp
                  -> InternaliseM ()
internaliseModExp E.ModVar{} =
  return ()
internaliseModExp E.ModLambda{} =
  return ()
internaliseModExp (E.ModParens e _) =
  internaliseModExp e
internaliseModExp E.ModImport{} = return ()
internaliseModExp (E.ModDecs ds _) =
  internaliseDecs ds
internaliseModExp (E.ModAscript me _ (Info substs) _) = do
  noteDecSubsts substs
  morePromises substs $ internaliseModExp me
internaliseModExp (E.ModApply orig_f orig_arg (Info orig_p_substs) (Info orig_b_substs) _) = do
  internaliseModExp orig_arg
  generatingFunctor orig_p_substs orig_b_substs $ do
    f_e <- evalModExp orig_f
    case f_e of
      Just (p, substs, body) -> do
        noteMod p mempty orig_arg
        withDecSubstitutions substs $
          internaliseModExp body
      Nothing ->
        fail $ "Cannot apply " ++ pretty orig_f ++ " to " ++ pretty orig_arg
  where evalModExp (E.ModVar v _) = do
          ModBinding v_substs me <- lookupMod =<< lookupSubst v
          f_e <- evalModExp me
          case f_e of
            Just (p, me_substs, body) ->
              return $ Just (p, me_substs `M.union` v_substs, body)
            _ ->
              return Nothing
        evalModExp (E.ModLambda (ModParam p _ _) sig me loc) = do
          substs <- asks envFunctorSubsts
          return $ Just (p, substs, maybeAscript loc sig me)
        evalModExp (E.ModParens e _) =
          evalModExp e
        evalModExp (E.ModAscript me _ (Info substs) _) = do
          noteDecSubsts substs
          morePromises substs $ evalModExp me
        evalModExp (E.ModApply f arg (Info p_substs) (Info b_substs) _) = do
          f_e <- evalModExp f
          internaliseModExp arg
          case f_e of
            Just (p, substs, body) -> do
              noteMod p mempty arg
              generatingFunctor p_substs b_substs $
                withDecSubstitutions substs $
                evalModExp body
            Nothing ->
              fail $ "Cannot apply " ++ pretty f ++ " to " ++ pretty arg
        evalModExp (E.ModDecs ds _) = do
          internaliseDecs ds
          return Nothing
        evalModExp E.ModImport{} =
          return Nothing

internaliseValBind :: E.ValBind -> InternaliseM ()
internaliseValBind (E.ValBind entry name _ t e loc) =
  internaliseFunBind $ E.FunBind entry name Nothing t [] [] e loc

internaliseFunName :: VName -> [E.Pattern] -> InternaliseM Name
internaliseFunName ofname [] = return $ nameFromString $ pretty ofname ++ "f"
internaliseFunName ofname _  = nameFromString . pretty <$> newVName (baseString ofname)

internaliseFunBind :: E.FunBind -> InternaliseM ()
internaliseFunBind fb@(E.FunBind entry ofname _ (Info rettype) tparams params body loc) = do
  fname <- fulfillingPromise ofname
  substs <- allSubsts
  noteFunction fname $ \(e_ts, _) -> withDecSubstitutions substs $
    generatingFunctor mempty mempty $ do
      let param_ts = map (E.removeShapeAnnotations . E.patternStructType) params

      mapping <- fmap mconcat $ zipWithM mapTypeVariables param_ts $
                 map E.removeShapeAnnotations e_ts

      let mkEntry (tp, et) = (tp, (substs, [], E.vacuousShapeAnnotations et))
          types = map mkEntry $ M.toList mapping
      notingTypes types $ bindingParams tparams params $ \pcm shapeparams params' -> do
        (rettype', _, rcm) <- internaliseReturnType rettype
        fname' <- internaliseFunName fname params
        body' <- ensureResultExtShape asserting loc (map I.fromDecl rettype')
                 =<< internaliseBody body

        let mkConstParam name = Param name $ I.Prim int32
            constparams = map (mkConstParam . snd) $ pcm<>rcm
            shapenames = map I.paramName shapeparams
            normal_params = map paramName constparams ++ shapenames ++ map paramName (concat params')
            normal_param_names = S.fromList normal_params
            free_in_fun = freeInBody body' `S.difference` normal_param_names

        used_free_params <- forM (S.toList free_in_fun) $ \v -> do
          v_t <- lookupType v
          return $ Param v $ toDecl v_t Nonunique

        let free_shape_params = map (`Param` I.Prim int32) $
                                concatMap (I.shapeVars . I.arrayShape . paramType) used_free_params
            free_params = nub $ free_shape_params ++ used_free_params
            all_params = constparams ++ free_params ++ shapeparams ++ concat params'

        addFunction $ I.FunDef Nothing fname' (ExtRetType rettype') all_params body'

        return (fname',
                pcm<>rcm,
                map I.paramName free_params,
                shapenames,
                map declTypeOf $ concat params',
                all_params,
                applyRetType (ExtRetType rettype') all_params)

  -- For any nullary function we force immediate generation.  Note
  -- that this means value declarations are also generated here - this
  -- is important!
  when (null params) $ void $ lookupFunction fname ([],[])

  when entry $ generateEntryPoint fb

generateEntryPoint :: E.FunBind -> InternaliseM ()
generateEntryPoint (E.FunBind _ ofname _ (Info rettype) _ orig_params _ loc) =
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

    entry_body <- insertStmsM $
      resultBody . fst <$> funcall "entry_result" (E.qualName ofname) (e_ts,i_ts) args loc

    addFunction $
      I.FunDef (Just entry') (baseName ofname)
      (ExtRetType $ concat entry_rettype)
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
          [I.TypeOpaque (pretty t) $ length ts]

internaliseIdent :: E.Ident -> InternaliseM I.VName
internaliseIdent (E.Ident name (Info tp) loc) =
  case tp of
    E.Prim{} -> return name
    _        -> fail $ "Futhark.Internalise.internaliseIdent: asked to internalise non-prim-typed ident '"
                       ++ pretty name ++ "' at " ++ locStr loc ++ "."

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

extraBodyStms :: [Stm]
                  -> InternaliseM (Body, a)
                  -> InternaliseM (Body, a)
extraBodyStms bnds m = do
  (body, x) <- m
  return (insertStms bnds body, x)

internaliseExp :: String -> E.Exp -> InternaliseM [I.SubExp]

internaliseExp desc (E.Parens e _) =
  internaliseExp desc e

internaliseExp _ (E.Var v t loc) = do
  name <- lookupSubst v
  -- If this identifier is the name of a constant, we have to turn it
  -- into a call to the corresponding function.
  is_const <- lookupConstant name
  case is_const of
    Just ses ->
      return ses
    _ -> do
      subst <- asks $ M.lookup name . envSubsts
      case subst of
        Nothing     -> (:[]) . I.Var <$> internaliseIdent (E.Ident name t loc)
        Just substs -> return substs

internaliseExp desc (E.Index e idxs loc) = do
  vs <- internaliseExpToVars "indexed" e
  dims <- case vs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', idx_cs) <- unzip <$> zipWithM (internaliseDimIndex loc) dims idxs
  let index v = do
        v_t <- lookupType v
        return $ I.BasicOp $ I.Index (concat idx_cs) v $ fullSlice v_t idxs'
  letSubExps desc =<< mapM index vs

internaliseExp desc (E.TupLit es _) =
  concat <$> mapM (internaliseExp desc) es

internaliseExp desc (E.RecordLit orig_fields _) =
  concatMap snd . sortFields . M.unions . reverse <$> mapM internaliseField orig_fields
  where internaliseField (E.RecordField name e _) = do
          e' <- internaliseExp desc e
          return $ M.singleton name e'
        internaliseField (E.RecordRecord e) = do
          (field_names, field_types) <-
            case E.typeOf e of
              Record fs -> return $ unzip $ sortFields fs
              _         -> fail $ "Type of " ++ pretty e ++ " is not record."
          e' <- internaliseExp desc e
          lens <- mapM (internalisedTypeSize . flip setAliases ()) field_types
          return $ M.fromList $ zip field_names $ chunks lens e'

internaliseExp desc (E.ArrayLit es (Info rowtype) loc) = do
  es' <- mapM (internaliseExp "arr_elem") es
  case es' of
    [] -> do
      rowtypes <- internaliseType (rowtype `setAliases` ())
      let arraylit rt = I.BasicOp $ I.ArrayLit [] rt
      letSubExps desc $ map (arraylit . zeroDim . fromDecl) rowtypes
    e' : _ -> do
      rowtypes <- mapM subExpType e'
      let arraylit ks rt = do
            ks' <- mapM (ensureShape asserting loc rt "elem_reshaped") ks
            return $ I.BasicOp $ I.ArrayLit ks' rt
      letSubExps desc =<< zipWithM arraylit (transpose es') rowtypes
  where zeroDim t = t `I.setArrayShape`
                    I.Shape (replicate (I.arrayRank t) (constant (0::Int32)))

internaliseExp desc (E.Empty (TypeDecl _(Info et)) _) = do
  (ts, _, _) <- internaliseReturnType et
  let ts' = map (fromDecl . modifyArrayShape extToZero) ts
  letSubExps desc $ map (I.BasicOp . I.ArrayLit []) ts'
  where extToZero (I.ExtShape dims) = I.Shape $ map extDimToZero dims
        extDimToZero I.Ext{} = constant (0::Int32)
        extDimToZero (I.Free d) = d

internaliseExp desc (E.Ascript e (TypeDecl _ (Info et)) loc) = do
  es <- internaliseExp desc e
  (ts, _, cm) <- internaliseReturnType et
  mapM_ (uncurry internaliseDimConstant) cm
  forM (zip es ts) $ \(e',t') ->
    ensureExtShape asserting loc (I.fromDecl t') desc e'

internaliseExp desc (E.Negate e _) = do
  e' <- internaliseExp1 "negate_arg" e
  et <- subExpType e'
  case et of I.Prim (I.IntType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.Sub t) (I.intConst t 0) e'
             I.Prim (I.FloatType t) ->
               letTupExp' desc $ I.BasicOp $ I.BinOp (I.FSub t) (I.floatConst t 0) e'
             _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in Negate"

-- Some functions are magical (overloaded) and we handle that here.
-- Note that polymorphic functions (which are not magical) are not
-- handled here.
internaliseExp desc (E.Apply fname args _ _)
  | Just internalise <- isOverloadedFunction fname $ map fst args =
      internalise desc

internaliseExp desc (E.Apply fname args _ _)
  | Just (rettype, _) <- M.lookup fname' I.builtInFunctions = do
  args' <- mapM (internaliseExp "arg" . fst) args
  let args'' = concatMap tag args'
  letTupExp' desc $ I.Apply fname' args'' (ExtRetType [I.Prim rettype])
  where tag ses = [ (se, I.Observe) | se <- ses ]
        -- Builtin functions are special anyway, so it is OK to not
        -- use lookupSubst here.
        fname' = nameFromString $ pretty $ baseName $ qualLeaf fname

internaliseExp desc (E.Apply qfname args _ loc) = do
  args' <- concat <$> mapM (internaliseExp "arg" . fst) args
  i_ts <- staticShapes <$> mapM subExpType args'
  let e_ts = map (flip setAliases () . E.typeOf . fst) args
  fst <$> funcall desc qfname (e_ts, i_ts) args' loc

internaliseExp desc (E.LetPat tparams pat e body loc) = do
  ses <- internaliseExp desc e
  t <- I.staticShapes <$> mapM I.subExpType ses
  bindingPattern tparams pat t $ \cm pat_names match -> do
    mapM_ (uncurry internaliseDimConstant) cm
    ses' <- match loc ses
    forM_ (zip pat_names ses') $ \(v,se) ->
      letBindNames'_ [v] $ I.BasicOp $ I.SubExp se
    internaliseExp desc body

internaliseExp desc (E.LetFun ofname (tparams, params, retdecl, Info rettype, body) letbody loc) = do
  internaliseFunBind $ E.FunBind False ofname retdecl (Info rettype) tparams params body loc
  internaliseExp desc letbody

internaliseExp desc (E.DoLoop tparams mergepat mergeexp form loopbody letbody loc) = do
  mergeinit <- internaliseExp "loop_init" mergeexp
  mergeinit_ts <- mapM subExpType mergeinit

  (wrap, form_contents) <- case form of
    E.For dir lbound i ubound -> do
      ubound' <- internaliseExp1 "upper_bound" ubound
      i' <- internaliseIdent i
      ubound_t <- I.subExpType ubound'
      it <- case ubound_t of
              I.Prim (IntType it) -> return it
              _                   -> fail "internaliseExp DoLoop: invalid type"
      lbound' <- case lbound of
                   ZeroBound -> return $ I.intConst it 0
                   ExpBound e -> internaliseExp1 "lower_bound" e
      num_iterations <- letSubExp "num_iterations" $
                        BasicOp $ I.BinOp (I.Sub it) ubound' lbound'

      j <- newVName $ baseString i'
      let i_ident = I.Ident i' $ I.Prim $ IntType it
      i_bnds <- case dir of
        E.FromUpTo ->
          return [mkLet' [] [i_ident] $
                  I.BasicOp $ I.BinOp (I.Add it) lbound' (I.Var j)]
        E.FromDownTo -> do
          upper_bound_less_one <-
            letSubExp "upper_bound_less_one" $
            BasicOp $ I.BinOp (I.Sub it) ubound' (intConst it 1)
          return [mkLet' [] [i_ident] $
                  I.BasicOp $ I.BinOp (I.Sub it) upper_bound_less_one (I.Var j)]
      return ( bindingIdentTypes [I.Ident j $ I.Prim $ IntType it, i_ident] .
               extraBodyStms i_bnds
             , Left (j, num_iterations))
    E.While cond ->
      return (id, Right cond)

  (loopbody', (form', shapepat, mergepat', frob, mergeinit', pre_bnds)) <-
    wrap $ bindingParams tparams [mergepat] $ \mergecm shapepat nested_mergepat -> do
    mapM_ (uncurry internaliseDimConstant) mergecm
    internaliseBodyStms loopbody $ \ses -> do
      sets <- mapM subExpType ses
      let mergepat' = concat nested_mergepat
          shapeinit = argShapes
                      (map I.paramName shapepat)
                      (map I.paramType mergepat')
                      mergeinit_ts
          shapeargs = argShapes
                      (map I.paramName shapepat)
                      (map I.paramType mergepat')
                      sets
      case form_contents of
        Left (i', bound) -> do
          bound_t <- I.subExpType bound
          case bound_t of
            I.Prim (IntType it) ->
              return (resultBody $ shapeargs ++ ses,
                      (I.ForLoop i' it bound,
                       shapepat,
                       mergepat',
                       id,
                       mergeinit,
                       []))
            t ->
              fail $ "internaliseExp DoLoop: bound has unexpected type: " ++ pretty t
        Right cond -> do
          -- We need to insert 'cond' twice - once for the initial
          -- condition (do we enter the loop at all?), and once with
          -- the result values of the loop (do we continue into the
          -- next iteration?).  This is safe, as the type rules for
          -- the external language guarantees that 'cond' does not
          -- consume anything.
          loop_while <- newParam "loop_while" $ I.Prim I.Bool
          (loop_cond, loop_cond_bnds) <-
            collectStms $ internaliseExp1 "loop_cond" cond
          let initsubst = [ (I.paramName mergeparam, initval)
                            | (mergeparam, initval) <-
                               zip (shapepat++mergepat') (shapeinit++mergeinit)
                            ]
              endsubst = [ (I.paramName mergeparam, endval)
                         | (mergeparam, endval) <-
                              zip (shapepat++mergepat') (shapeargs++ses)
                         ]
          (loop_initial_cond, init_loop_cond_bnds) <-
            collectStms $
            shadowIdentsInExp initsubst loop_cond_bnds loop_cond
          (loop_end_cond, loop_end_cond_bnds) <-
            collectStms $
            shadowIdentsInExp endsubst loop_cond_bnds loop_cond
          return (mkBody loop_end_cond_bnds $
                  shapeargs++[loop_end_cond]++ses,
                  (I.WhileLoop $ I.paramName loop_while,
                   shapepat,
                   loop_while : mergepat',
                   addAnother,
                   loop_initial_cond : mergeinit,
                   init_loop_cond_bnds))

  mapM_ addStm pre_bnds

  mergeinit_ts' <- mapM subExpType mergeinit'

  let ctxinit = argShapes
                (map I.paramName shapepat)
                (map I.paramType mergepat')
                mergeinit_ts'
      ctxmerge = zip shapepat ctxinit
      valmerge = zip mergepat' mergeinit'
      loop = I.DoLoop ctxmerge valmerge form' loopbody'
  loopt <- I.expExtType loop
  bindingPattern tparams (frob mergepat) loopt $ \cm mergepat_names match -> do
    mapM_ (uncurry internaliseDimConstant) cm
    loop_res <- match loc . map I.Var =<< letTupExp "loop_res" loop
    forM_ (zip mergepat_names loop_res) $ \(v,se) ->
      letBindNames'_ [v] $ I.BasicOp $ I.SubExp se
    internaliseExp desc letbody

  where addAnother t =
          TuplePattern [E.Wildcard (Info $ E.Prim $ E.Signed E.Int32) (srclocOf t), t] noLoc

internaliseExp desc (E.LetWith name src idxs ve body loc) = do
  srcs <- internaliseExpToVars "src" $
          E.Var (qualName (E.identName src)) (E.identType src) (srclocOf src)
  ves <- internaliseExp "lw_val" ve
  dims <- case srcs of
            [] -> return [] -- Will this happen?
            v:_ -> I.arrayDims <$> lookupType v
  (idxs', idx_cs) <- unzip <$> zipWithM (internaliseDimIndex loc) dims idxs
  let comb sname ve' = do
        sname_t <- lookupType sname
        let slice = fullSlice sname_t idxs'
            rowtype = sname_t `setArrayDims` sliceDims slice
        ve'' <- ensureShape asserting loc rowtype "lw_val_correct_shape" ve'
        letInPlace "letwith_dst" (concat idx_cs) sname (fullSlice sname_t idxs') $
          BasicOp $ SubExp ve''
  dsts <- zipWithM comb srcs ves
  dstt <- I.staticShapes <$> mapM lookupType dsts
  bindingPattern [] (E.Id name) dstt $ \cm pat_names match -> do
    mapM_ (uncurry internaliseDimConstant) cm
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
    E.LetPat [] (E.Id src_ident) src
    (E.LetWith dest_ident src_ident slice ve
      (E.Var (E.qualName dest_name) (E.Info src_t) loc)
      loc)
    loc

internaliseExp desc (E.Shape e _) = do
  ks <- internaliseExp (desc<>"_shape") e
  case ks of
    (k:_) -> do kt <- I.subExpType k
                letSubExps desc [I.BasicOp $ I.ArrayLit (I.arrayDims kt) $ I.Prim int32]
    _     -> return [I.constant (0 :: I.Int32)] -- Will this ever happen?

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
                       I.Assert cmp loc
                letExp (postfix e_unchecked' "_zip_res") $
                  shapeCoerce c (w:inner) e_unchecked'
      es' <- mapM reshapeToOuter es_unchecked
      return $ map I.Var $ e_key : es'
    [] -> return []

  where postfix i s = baseString i ++ s

internaliseExp _ (E.Rearrange perm e _) =
  internaliseOperation "rearrange" e $ \v ->
    return $ I.Rearrange [] perm v

internaliseExp _ (E.Rotate d offset e _) = do
  offset' <- internaliseExp1 "rotation_offset" offset
  internaliseOperation "rotate" e $ \v -> do
    rank <- I.arrayRank <$> lookupType v
    let zero = constant (0::Int32)
        offsets = replicate d zero ++ [offset'] ++ replicate (rank-d-1) zero
    return $ I.Rotate [] offsets v

internaliseExp _ (E.Reshape shape e loc) = do
  shape' <- internaliseShapeExp "shape" shape
  internaliseOperation "reshape" e $ \v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    dims <- I.arrayDims <$> lookupType v
    shapeOk <- assertingOne $
               letExp "shape_ok" =<<
               eAssert (eCmpOp (I.CmpEq I.int32) (prod dims) (prod shape'))
               loc
    return $ I.Reshape shapeOk (DimNew <$> shape') v
  where prod = foldBinOp (I.Mul I.Int32) (constant (1 :: I.Int32))

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
    forM indexChecks$ \cnd ->
      letExp "split_index_assert" $ BasicOp $ I.Assert cnd loc

  -- Calculate diff between each split index
  let sizeExps = zipWith (\beg end -> BasicOp $ I.BinOp (I.Sub I.Int32) end beg)
                 (I.constant (0 :: I.Int32):splits') (splits'++[split_dim])
  sizeVars <- mapM (letSubExp "split_size") sizeExps
  splitExps <- forM arrs $ \arr -> letTupExp' "split_res" $
                                   BasicOp $ I.Split indexAsserts i sizeVars arr

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
              eAssert (pure $ I.BasicOp $ I.CmpOp (I.CmpEq I.int32) n m) loc
            x_inner_dims  = dropAt i 1 $ I.arrayDims xt
            ys_inner_dims = map (dropAt i 1 . I.arrayDims) yts
            updims = zipWith3 updims' [0..] (I.arrayDims xt)
            updims' j xd yd | i == j    = yd
                            | otherwise = xd
        matchcs <- asserting $
                   concat <$> mapM (zipWithM matches x_inner_dims) ys_inner_dims
        yarrs'  <- forM yarrs $ \yarr -> do
          yt <- lookupType yarr
          letExp "concat_y_reshaped" $
            shapeCoerce matchcs (updims $ I.arrayDims yt) yarr
        return $ I.BasicOp $ I.Concat [] i xarr yarrs' ressize
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
  letTupExp' desc $ I.Op $ I.Map [] w lam' arrs'

internaliseExp desc (E.Reduce comm lam ne arr loc) =
  internaliseScanOrReduce desc "reduce"
    (\cs w -> I.Reduce cs w comm) (lam, ne, arr, loc)

internaliseExp desc (E.Scan lam ne arr loc) =
  internaliseScanOrReduce desc "scan" I.Scan (lam, ne, arr, loc)

internaliseExp desc (E.Filter lam arr _) = do
  arrs <- internaliseExpToVars "filter_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda [lam] $ map I.Var arrs
  (partition_sizes, partitioned) <- partitionWithSOACS 1 lam' arrs
  fmap (map I.Var . concat . transpose) $ forM partitioned $
    letTupExp desc . I.BasicOp . I.Split [] 0 partition_sizes

internaliseExp desc (E.Partition lams arr _) = do
  arrs <- internaliseExpToVars "partition_input" arr
  lam' <- internalisePartitionLambdas internaliseLambda lams $ map I.Var arrs
  (partition_sizes, partitioned) <- partitionWithSOACS (k+1) lam' arrs
  fmap (map I.Var . concat . transpose) $ forM partitioned $
    letTupExp desc . I.BasicOp . I.Split [] 0 partition_sizes
  where k = length lams

internaliseExp desc (E.Stream form lam arr _) = do
  arrs <- internaliseExpToVars "stream_input" arr
  (lam_accs, lam_acc_param_ts) <-
    case form of E.MapLike{} -> return ([], [])
                 E.RedLike{} -> return ([], [])
                 E.Sequential acc -> do
                   accs <- internaliseExp "stream_acc" acc
                   acc_ts <- mapM I.subExpType accs
                   return (accs, acc_ts)

  rowts <- mapM (fmap I.rowType . lookupType) arrs
  lam' <- internaliseStreamLambda internaliseLambda lam lam_acc_param_ts rowts

  -- If the stream form is a reduce, we also have to fiddle with the
  -- lambda to incorporate the reduce function.  FIXME: can't we just
  -- modify the internal representation of reduction streams?
  (form', lam'') <-
    case form of
      E.MapLike o -> return (I.MapLike o, lam')
      E.Sequential _ -> return (I.Sequential lam_accs, lam')
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
          name <- newVName $ baseString $ paramName p
          return p { I.paramName = name }

        body_with_lam0 <-
          ensureResultShape asserting (srclocOf lam) acctps <=< runBodyBinder $ do
            lam_res <- bodyBind $ extLambdaBody lam'

            let consumed = consumedByLambda $ Alias.analyseLambda lam0'
                copyIfConsumed p (I.Var v)
                  | paramName p `S.member` consumed =
                      letSubExp "acc_copy" $ I.BasicOp $ I.Copy v
                copyIfConsumed _ x = return x

            accs' <- zipWithM copyIfConsumed (I.lambdaParams lam0') accs
            new_lam_res <- eLambda lam0' $ accs' ++ lam_res
            return $ resultBody new_lam_res

        -- Make sure the chunk size parameter comes first.
        return (I.RedLike o comm lam0' accs,
                lam' { extLambdaParams = take 1 (extLambdaParams lam') <>
                                         acc_params <>
                                         drop 1 (extLambdaParams lam')
                     , extLambdaBody = body_with_lam0
                     , extLambdaReturnType = staticShapes acctps })
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ I.Stream [] w form' lam'' arrs

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp desc (E.Iota e _) = do
  (e', it) <- internaliseDimExp "n" e
  letTupExp' desc $ I.BasicOp $ I.Iota e' (intConst it 0) (intConst it 1) it

internaliseExp desc (E.Replicate ne ve _) = do
  (ne', _) <- internaliseDimExp "n" ne
  ves <- internaliseExp "replicate_v" ve
  letSubExps desc $ I.BasicOp . I.Replicate (I.Shape [ne']) <$> ves

internaliseExp _ (E.Literal v _) =
  return [I.Constant $ internalisePrimValue v]

internaliseExp desc (E.If ce te fe (Info t) _) = do
  ce' <- internaliseExp1 "cond" ce
  te' <- internaliseBody te
  fe' <- internaliseBody fe
  (t', _, _) <- internaliseReturnType $ E.vacuousShapeAnnotations t `setAliases` ()
  letTupExp' desc $ I.If ce' te' fe' $ map I.fromDecl t'

-- Builtin operators are handled specially because they are
-- overloaded.
internaliseExp desc (E.BinOp op (xe,_) (ye,_) _ _)
  | Just internalise <- isOverloadedFunction op [xe, ye] =
      internalise desc

-- User-defined operators are just the same as a function call.
internaliseExp desc (E.BinOp op xarg yarg ret loc) =
  internaliseExp desc $ E.Apply op [xarg,yarg] ret loc

internaliseExp desc (E.Project k e (Info rt) _) = do
  n <- internalisedTypeSize $ rt `setAliases` ()
  i' <- fmap sum $ mapM internalisedTypeSize $
        case E.typeOf e `setAliases` () of
               Record fs -> map snd $ filter ((<k) . fst) $ sortFields fs
               t         -> [t]
  take n . drop i' <$> internaliseExp desc e

internaliseExp desc (E.Scatter a si v loc) = do
  si' <- letExp "write_si" . BasicOp . SubExp =<< internaliseExp1 "write_arg_i" si
  svs <- internaliseExpToVars "write_arg_v" v
  sas <- internaliseExpToVars "write_arg_a" a

  si_shape <- I.arrayShape <$> lookupType si'
  let si_w = shapeSize 0 si_shape

  (tvs, svs') <- fmap unzip $ forM svs $ \sv -> do
    tv <- rowType <$> lookupType sv -- the element type
    sv_shape <- I.arrayShape <$> lookupType sv
    let sv_w = shapeSize 0 sv_shape

    -- Generate an assertion and reshapes to ensure that sv and si' are the same
    -- size.
    cmp <- letSubExp "write_cmp" $ I.BasicOp $
      I.CmpOp (I.CmpEq I.int32) si_w sv_w
    c   <- assertingOne $
      letExp "write_cert" $ I.BasicOp $
      I.Assert cmp loc
    sv' <- letExp (baseString sv ++ "_write_sv") $
      I.BasicOp $ I.Reshape c (reshapeOuter [DimCoercion si_w] 1 sv_shape) sv

    return (tv, sv')

  indexType <- rowType <$> lookupType si'
  let bodyTypes = replicate (length tvs) indexType ++ tvs
      paramTypes = indexType : tvs

  indexName <- newVName "write_index"
  valueNames <- replicateM (length tvs) $ newVName "write_value"

  let bodyNames = indexName : valueNames
  let bodyParams = zipWith I.Param bodyNames paramTypes

  -- This body is pretty boring right now, as every input is exactly the output.
  -- But it can get funky later on if fused with something else.
  (body, _) <- runBinderEmptyEnv $ insertStmsM $ do
    let outs = replicate (length valueNames) indexName ++ valueNames
    results <- forM outs $ \name ->
      letSubExp "write_res" $ I.BasicOp $ I.SubExp $ I.Var name
    return $ resultBody results

  let lam = Lambda { I.lambdaParams = bodyParams
                   , I.lambdaReturnType = bodyTypes
                   , I.lambdaBody = body
                   }
      sivs = si' : svs'
  aws <- mapM (fmap (arraySize 0) . lookupType) sas
  letTupExp' desc $ I.Op $ I.Scatter [] si_w lam sivs $ zip aws sas

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
              (resultBody [zero]) [I.Prim int32]
      j_def = letSubExp "j_def" $ I.If backwards
              (resultBody [negone])
              (resultBody [w]) [I.Prim int32]
  i' <- maybe i_def (fmap fst . internaliseDimExp "i") i
  j' <- maybe j_def (fmap fst . internaliseDimExp "j") j
  j_m_i <- letSubExp "j_m_i" $ BasicOp $ I.BinOp (Sub Int32) j' i'
  n <- letSubExp "n" =<< eDivRoundingUp Int32
       (pure $ BasicOp $ I.UnOp (I.Abs Int32) j_m_i)
       (pure $ I.BasicOp $ I.UnOp (I.Abs Int32) s')

  checked <- asserting $ do
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
                (resultBody [forwards_ok])
                [I.Prim I.Bool]
    ok_or_empty <- letSubExp "ok_or_empty" $
                   I.BasicOp $ I.BinOp I.LogOr empty_slice slice_ok
    letTupExp "slice_cert" $ I.BasicOp $ I.Assert ok_or_empty loc

  return (I.DimSlice i' n s', checked)
  where zero = constant (0::Int32)
        negone = constant (-1::Int32)
        one = constant (1::Int32)

internaliseScanOrReduce :: String -> String
                        -> (Certificates -> SubExp -> I.Lambda -> [(SubExp, VName)] -> SOAC SOACS)
                        -> (E.Lambda, E.Exp, E.Exp, SrcLoc)
                        -> InternaliseM [SubExp]
internaliseScanOrReduce desc what f (lam, ne, arr, loc) = do
  arrs <- internaliseExpToVars (what++"_arr") arr
  nes <- internaliseExp (what++"_ne") ne
  nes' <- forM (zip nes arrs) $ \(ne', arr') -> do
    rowtype <- I.stripArray 1 <$> lookupType arr'
    ensureShape asserting loc rowtype (what++"_ne_right_shape") ne'
  nests <- mapM I.subExpType nes'
  arrts <- mapM lookupType arrs
  lam' <- internaliseFoldLambda internaliseLambda lam nests arrts
  let input = zip nes' arrs
  w <- arraysSize 0 <$> mapM lookupType arrs
  letTupExp' desc $ I.Op $ f [] w lam' input

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


internaliseLambda :: InternaliseLambda

internaliseLambda (E.AnonymFun tparams params body _ (Info rettype) _) rowtypes =
  bindingLambdaParams tparams params rowtypes $ \pcm params' -> do
    (rettype', _, rcm) <- internaliseReturnType rettype
    body' <- internaliseBody body
    mapM_ (uncurry internaliseDimConstant) $ pcm<>rcm
    return (params', body', map I.fromDecl rettype')

-- All overloaded functions are unary, so if they're here, they have
-- no curried arguments and only one real argument.
internaliseLambda (E.CurryFun qfname [] (Info ([et], _)) loc) [it] = do
  param <- newParam "not_curried" it
  let arg = E.Var (E.qualName (paramName param)) (Info et) loc
  case isOverloadedFunction qfname [arg] of
    Just special -> do
      (res, stms) <- localScope (scopeOfLParams [param]) $
                     collectStms $ special "curried_result"
      let body = mkBody stms res
      body_t <- bodyExtType body
      return ([param], body, body_t)
    Nothing ->
      internaliseCurrying qfname [] loc ([et], [it])

internaliseLambda (E.CurryFun qfname curargs (Info (paramts, _)) loc) rowtypes =
  internaliseCurrying qfname curargs loc (paramts, rowtypes)

internaliseLambda (E.BinOpFun unop (Info xtype) (Info ytype) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpFunToLambda unop xtype ytype rettype
  internaliseLambda (AnonymFun [] params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.CurryBinOpLeft binop e (Info paramtype, Info _) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e $ uncurry $ flip (,)
  internaliseLambda (AnonymFun [] params body Nothing (Info rettype') loc) rowts

internaliseLambda (E.CurryBinOpRight binop e (Info _, Info paramtype) (Info rettype) loc) rowts = do
  (params, body, rettype') <-
    binOpCurriedToLambda binop paramtype rettype e id
  internaliseLambda (AnonymFun [] params body Nothing (Info rettype') loc) rowts

internaliseCurrying :: QualName VName
                    -> [E.Exp]
                    -> SrcLoc
                    -> ([E.Type], [I.Type])
                    -> InternaliseM ([I.LParam], I.Body, [I.ExtType])
internaliseCurrying qfname curargs loc (row_ets, row_its) = do
  curargs' <- concat <$> mapM (internaliseExp "curried") curargs
  i_ts <- staticShapes <$> mapM subExpType curargs'
  params <- mapM (newParam "not_curried") row_its
  let args = curargs' ++ map (I.Var . I.paramName) params
      e_ts = map (`setAliases` ()) $ map E.typeOf curargs ++ row_ets
  ((res, ts), fun_bnds) <- localScope (scopeOfLParams params) $ collectStms $
    funcall "curry_result" qfname (e_ts, i_ts) args loc
  return (params, mkBody fun_bnds res, ts)

binOpFunToLambda :: E.QualName VName
                 -> E.Type -> E.Type -> E.Type
                 -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
binOpFunToLambda op xtype ytype rettype = do
  x_name <- newNameFromString "binop_param_x"
  y_name <- newNameFromString "binop_param_y"
  let ident_x = E.Ident x_name (Info xtype) noLoc
      ident_y = E.Ident y_name (Info ytype) noLoc
  return ([E.Id ident_x, E.Id ident_y],
          E.BinOp op
           (E.Var (qualName x_name) (Info xtype) noLoc, E.Observe)
           (E.Var (qualName y_name) (Info ytype) noLoc, E.Observe)
           (Info rettype) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

binOpCurriedToLambda :: E.QualName VName
                     -> E.Type -> E.Type
                     -> E.Exp
                     -> ((E.Exp,E.Exp) -> (E.Exp,E.Exp))
                     -> InternaliseM ([E.Pattern], E.Exp, E.StructType)
binOpCurriedToLambda op paramtype rettype e swap = do
  paramname <- newNameFromString "binop_param_noncurried"
  let ident = E.Ident paramname (Info paramtype) noLoc
      (x', y') = swap (E.Var (qualName paramname) (Info paramtype) noLoc, e)
  return ([E.Id ident],
          E.BinOp op (x',E.Observe) (y',E.Observe) (Info rettype) noLoc,
          E.vacuousShapeAnnotations $ E.toStruct rettype)

internaliseDimConstant :: Name -> VName -> InternaliseM ()
internaliseDimConstant fname name =
  letBind_ (basicPattern' [] [I.Ident name $ I.Prim I.int32]) $
  I.Apply fname [] $ primRetType I.int32

-- | Some operators and functions are overloaded or otherwise special
-- - we detect and treat them here.
isOverloadedFunction :: E.QualName VName -> [E.Exp]
                     -> Maybe (String -> InternaliseM [SubExp])
isOverloadedFunction qname args = do
  guard $ baseTag (qualLeaf qname) <= maxIntrinsicTag
  handle args $ baseString $ qualLeaf qname
  where
    handle [x] "i8"  = Just $ toSigned I.Int8 x
    handle [x] "i16" = Just $ toSigned I.Int16 x
    handle [x] "i32" = Just $ toSigned I.Int32 x
    handle [x] "i64" = Just $ toSigned I.Int64 x

    handle [x] "u8"  = Just $ toUnsigned I.Int8 x
    handle [x] "u16" = Just $ toUnsigned I.Int16 x
    handle [x] "u32" = Just $ toUnsigned I.Int32 x
    handle [x] "u64" = Just $ toUnsigned I.Int64 x

    handle [x] "f32" = Just $ toFloat I.Float32 x
    handle [x] "f64" = Just $ toFloat I.Float64 x

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
          rs <- zipWithM (doComparison (cmp_f desc)) xe' ye'
          letTupExp' desc =<< foldBinOp I.LogAnd (constant True) rs
        where isEqlOp "!=" = Just $ \desc t x y -> do
                eq <- letSubExp (desc++"true") $ I.BasicOp $ I.CmpOp (I.CmpEq t) x y
                letSubExp desc $ I.BasicOp $ I.UnOp I.Not eq
              isEqlOp "==" = Just $ \desc t x y ->
                letSubExp desc $ I.BasicOp $ I.CmpOp (I.CmpEq t) x y
              isEqlOp _ = Nothing

              doComparison cmp_f x y = do
                x_t <- I.subExpType x
                y_t <- I.subExpType y
                case x_t of
                  I.Prim t -> cmp_f t x y
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
                      x_flat <- letExp "x_flat" $ I.BasicOp $ I.Reshape [] [I.DimNew x_num_elems] x'
                      y_flat <- letExp "y_flat" $ I.BasicOp $ I.Reshape [] [I.DimNew x_num_elems] y'

                      -- Compare the elements.
                      cmp_lam <- cmpOpLambda (I.CmpEq (elemType x_t)) (elemType x_t)
                      cmps <- letExp "cmps" $ I.Op $ I.Map [] x_num_elems cmp_lam [x_flat, y_flat]

                      -- Check that all were equal.
                      and_lam <- binOpLambda I.LogAnd I.Bool
                      all_equal <- letSubExp "all_equal" $ I.Op $
                                   I.Reduce [] x_num_elems Commutative and_lam [(constant True,cmps)]
                      return $ resultBody [all_equal]

                    letSubExp "arrays_equal" $
                      I.If shapes_match compare_elems_body (resultBody [constant False]) [I.Prim I.Bool]

    handle [x,y] name = do
      bop <- find ((name==) . pretty) [minBound..maxBound::E.BinOp]
      Just $ \desc -> do
        x' <- internaliseExp1 "x" x
        y' <- internaliseExp1 "y" y
        case (E.typeOf x, E.typeOf y) of
          (E.Prim t1, E.Prim t2) ->
            internaliseBinOp desc bop x' y' t1 t2
          _ -> fail "Futhark.Internalise.internaliseExp: non-primitive type in BinOp."

    handle _ _ = Nothing

    toSigned int_to e desc = do
      e' <- internaliseExp1 "trunc_arg" e
      case E.typeOf e of
        E.Prim E.Bool ->
          letTupExp' desc $ I.If e' (resultBody [intConst int_to 1])
                                    (resultBody [intConst int_to 0])
                                    [I.Prim $ I.IntType int_to]
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
                                    (resultBody [intConst int_to 0])
                                    [I.Prim $ I.IntType int_to]
        E.Prim (E.Signed int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Prim (E.Unsigned int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.ZExt int_from int_to) e'
        E.Prim (E.FloatType float_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.FPToUI float_from int_to) e'
        _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToUnsigned"

    toFloat float_to e desc = do
      e' <- internaliseExp1 "tofloat_arg" e
      case E.typeOf e of
        E.Prim E.Bool ->
          letTupExp' desc $ I.If e' (resultBody [floatConst float_to 1])
                                    (resultBody [floatConst float_to 0])
                                    [I.Prim $ I.FloatType float_to]
        E.Prim (E.Signed int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.SIToFP int_from float_to) e'
        E.Prim (E.Unsigned int_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (I.UIToFP int_from float_to) e'
        E.Prim (E.FloatType float_from) ->
          letTupExp' desc $ I.BasicOp $ I.ConvOp (FPConv float_from float_to) e'
        _ -> fail "Futhark.Internalise.internaliseExp: non-numeric type in ToFloat"

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

-- | Is the name a value constant?  If so, create the necessary
-- function call and return the corresponding subexpressions.
lookupConstant :: VName -> InternaliseM (Maybe [SubExp])
lookupConstant name = do
  is_const <- lookupFunction' name
  case is_const of
    Just (fname, constparams, _, _, _, _, mk_rettype) -> do
      (constargs, const_ds, const_ts) <- unzip3 <$> constFunctionArgs constparams
      case mk_rettype $ zip constargs $ map I.fromDecl const_ts of
        Nothing -> fail $ "lookupConstant: " ++ pretty name ++ " failed"
        Just rettype ->
          fmap (Just . map I.Var) $
          letTupExp (baseString name) $ I.Apply fname (zip constargs const_ds) rettype
    Nothing -> return Nothing

constFunctionArgs :: ConstParams -> InternaliseM [(SubExp, I.Diet, I.DeclType)]
constFunctionArgs = mapM arg
  where arg (fname, name) = do
          se <- letSubExp (baseString name ++ "_arg") $
                I.Apply fname [] $ primRetType I.int32
          return (se, I.Observe, I.Prim I.int32)

funcall :: String -> QualName VName -> SpecArgs -> [SubExp] -> SrcLoc
        -> InternaliseM ([SubExp], [I.ExtType])
funcall desc qfname (e_ts, i_ts) args loc = do
  fname <- lookupSubst qfname
  e_ts' <- mapM fullyApplyType e_ts
  (fname', constparams, closure, shapes, value_paramts, fun_params, rettype_fun) <-
    lookupFunction fname (e_ts', i_ts)
  (constargs, const_ds, _) <- unzip3 <$> constFunctionArgs constparams
  argts <- mapM subExpType args
  closure_ts <- mapM lookupType closure
  let shapeargs = argShapes shapes value_paramts argts
      diets = const_ds ++ replicate (length closure + length shapeargs) I.Observe ++
              map I.diet value_paramts
      constOrShape = const $ I.Prim int32
      paramts = map constOrShape constargs ++ closure_ts ++
                map constOrShape shapeargs ++ map I.fromDecl value_paramts
  args' <- ensureArgShapes asserting loc (map I.paramName fun_params)
           paramts (constargs ++ map I.Var closure ++ shapeargs ++ args)
  argts' <- mapM subExpType args'
  case rettype_fun $ zip args' argts' of
    Nothing -> fail $ "Cannot apply " ++ pretty fname ++ " to arguments\n " ++
               pretty args' ++ "\nof types\n " ++
               pretty argts' ++
               "\nFunction has parameters\n " ++ pretty fun_params
    Just (ExtRetType ts) -> do
      ses <- letTupExp' desc $ I.Apply fname' (zip args' diets) (ExtRetType ts)
      return (ses, map I.fromDecl ts)


boundsCheck :: SrcLoc -> I.SubExp -> I.SubExp -> InternaliseM I.VName
boundsCheck loc w e = do
  let check = eBinOp I.LogAnd (pure lowerBound) (pure upperBound)
      lowerBound = I.BasicOp $
                   I.CmpOp (I.CmpSle I.Int32) (I.constant (0 :: I.Int32)) e
      upperBound = I.BasicOp $
                   I.CmpOp (I.CmpSlt I.Int32) e w
  letExp "bounds_check" =<< eAssert check loc

shadowIdentsInExp :: [(VName, I.SubExp)] -> [Stm] -> I.SubExp
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
    mapM_ addStm $ substituteNames nameSubsts bnds
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
  classes_and_increments <- letTupExp "increments" $ I.Op $ I.Map [] w lam arrs
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
      (I.Var $ paramName x) (I.Var $ paramName y)
  let add_lam = I.Lambda { I.lambdaBody = add_lam_body
                         , I.lambdaParams = add_lam_x_params ++ add_lam_y_params
                         , I.lambdaReturnType = replicate k $ I.Prim int32
                         }
      scan_input = zip (repeat $ constant (0::Int32)) increments

  all_offsets <- letTupExp "offsets" $ I.Op $ I.Scan [] w add_lam scan_input

  -- We have the offsets for each of the partitions, but we also need
  -- the total sizes, which are the last elements in the offests.  We
  -- just have to be careful in case the array is empty.
  last_index <- letSubExp "last_index" $ I.BasicOp $ I.BinOp (I.Sub Int32) w $ constant (1::Int32)
  nonempty_body <- runBodyBinder $ fmap resultBody $ forM all_offsets $ \offset_array ->
    letSubExp "last_offset" $ I.BasicOp $ I.Index [] offset_array [I.DimFix last_index]
  let empty_body = resultBody $ replicate k $ constant (0::Int32)
  is_empty <- letSubExp "is_empty" $ I.BasicOp $ I.CmpOp (CmpEq int32) w $ constant (0::Int32)
  sizes <- letTupExp "partition_size" $
           I.If is_empty empty_body nonempty_body $ replicate k $ I.Prim int32

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
                             (I.Var $ paramName c_param) 0 offset_params
    return I.Lambda { I.lambdaParams = c_param : offset_params ++ value_params
                    , I.lambdaReturnType = replicate (length arr_ts) (I.Prim int32) ++
                                           map I.rowType arr_ts
                    , I.lambdaBody = mkBody offset_stms $
                                     replicate (length arr_ts) offset ++
                                     map (I.Var . paramName) value_params
                    }
  results <- letTupExp "partition_res" $ I.Op $ I.Scatter [] w
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
                  (I.Var (paramName p) : take i sizes)
      letSubExp "total_res" $ I.If is_this_one
        (resultBody [this_one]) (resultBody [next_one]) [I.Prim int32]
