{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for public API types.
module Futhark.CodeGen.Backends.GenericC.Types
  ( generateAPITypes,
    valueTypeToCType,
    opaqueToCType,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Char (isDigit)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Monad
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.ImpCode
import Futhark.Manifest qualified as Manifest
import Futhark.Util (chunks, mapAccumLM, zEncodeText)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

opaqueToCType :: Name -> CompilerM op s C.Type
opaqueToCType desc = do
  name <- publicName $ opaqueName desc
  pure [C.cty|struct $id:name|]

valueTypeToCType :: Publicness -> ValueType -> CompilerM op s C.Type
valueTypeToCType _ (ValueType signed (Rank 0) pt) =
  pure $ primAPIType signed pt
valueTypeToCType pub (ValueType signed (Rank rank) pt) = do
  name <- publicName $ arrayName pt signed rank
  let add = M.insertWith max (signed, pt, rank) pub
  modify $ \s -> s {compArrayTypes = add $ compArrayTypes s}
  pure [C.cty|struct $id:name|]

arrayLibraryFunctions ::
  Publicness ->
  Space ->
  PrimType ->
  Signedness ->
  Int ->
  CompilerM op s Manifest.ArrayOps
arrayLibraryFunctions pub space pt signed rank = do
  let pt' = primAPIType signed pt
      name = arrayName pt signed rank
      arr_name = "futhark_" <> name
      array_type = [C.cty|struct $id:arr_name|]

  new_array <- publicName $ "new_" <> name
  new_raw_array <- publicName $ "new_raw_" <> name
  free_array <- publicName $ "free_" <> name
  values_array <- publicName $ "values_" <> name
  values_raw_array <- publicName $ "values_raw_" <> name
  shape_array <- publicName $ "shape_" <> name

  let shape_names = ["dim" <> prettyText i | i <- [0 .. rank - 1]]
      shape_params = [[C.cparam|typename int64_t $id:k|] | k <- shape_names]
      arr_size = cproduct [[C.cexp|$id:k|] | k <- shape_names]
      arr_size_array = cproduct [[C.cexp|arr->shape[$int:i]|] | i <- [0 .. rank - 1]]
  copy <- asks $ opsCopy . envOperations

  memty <- rawMemCType space

  let prepare_new = do
        resetMem [C.cexp|arr->mem|] space
        allocMem
          [C.cexp|arr->mem|]
          [C.cexp|$exp:arr_size * $int:(primByteSize pt::Int)|]
          space
          [C.cstm|return NULL;|]
        forM_ [0 .. rank - 1] $ \i ->
          let dim_s = "dim" ++ show i
           in stm [C.cstm|arr->shape[$int:i] = $id:dim_s;|]

  new_body <- collect $ do
    prepare_new
    copy
      CopyNoBarrier
      [C.cexp|arr->mem.mem|]
      [C.cexp|0|]
      space
      [C.cexp|(const unsigned char*)data|]
      [C.cexp|0|]
      DefaultSpace
      [C.cexp|((size_t)$exp:arr_size) * $int:(primByteSize pt::Int)|]

  new_raw_body <- collect $ do
    prepare_new
    copy
      CopyNoBarrier
      [C.cexp|arr->mem.mem|]
      [C.cexp|0|]
      space
      [C.cexp|data|]
      [C.cexp|offset|]
      space
      [C.cexp|((size_t)$exp:arr_size) * $int:(primByteSize pt::Int)|]

  free_body <- collect $ unRefMem [C.cexp|arr->mem|] space

  values_body <-
    collect $
      copy
        CopyNoBarrier
        [C.cexp|(unsigned char*)data|]
        [C.cexp|0|]
        DefaultSpace
        [C.cexp|arr->mem.mem|]
        [C.cexp|0|]
        space
        [C.cexp|((size_t)$exp:arr_size_array) * $int:(primByteSize pt::Int)|]

  ctx_ty <- contextType
  ops <- asks envOperations

  let proto = case pub of
        Public -> headerDecl (ArrayDecl (nameFromText name))
        Private -> libDecl

  proto
    [C.cedecl|struct $id:arr_name;|]
  proto
    [C.cedecl|$ty:array_type* $id:new_array($ty:ctx_ty *ctx, const $ty:pt' *data, $params:shape_params);|]
  proto
    [C.cedecl|$ty:array_type* $id:new_raw_array($ty:ctx_ty *ctx, $ty:memty data, typename int64_t offset, $params:shape_params);|]
  proto
    [C.cedecl|int $id:free_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]
  proto
    [C.cedecl|int $id:values_array($ty:ctx_ty *ctx, $ty:array_type *arr, $ty:pt' *data);|]
  proto
    [C.cedecl|$ty:memty $id:values_raw_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]
  proto
    [C.cedecl|const typename int64_t* $id:shape_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]

  mapM_
    libDecl
    [C.cunit|
          $ty:array_type* $id:new_array($ty:ctx_ty *ctx, const $ty:pt' *data, $params:shape_params) {
            int err = 0;
            $ty:array_type* bad = NULL;
            $ty:array_type *arr = ($ty:array_type*) malloc(sizeof($ty:array_type));
            if (arr == NULL) {
              return bad;
            }
            $items:(criticalSection ops new_body)
            if (err != 0) {
              free(arr);
              return bad;
            }
            return arr;
          }

          $ty:array_type* $id:new_raw_array($ty:ctx_ty *ctx, $ty:memty data, typename int64_t offset, $params:shape_params) {
            int err = 0;
            $ty:array_type* bad = NULL;
            $ty:array_type *arr = ($ty:array_type*) malloc(sizeof($ty:array_type));
            if (arr == NULL) {
              return bad;
            }
            $items:(criticalSection ops new_raw_body)
            return arr;
          }

          int $id:free_array($ty:ctx_ty *ctx, $ty:array_type *arr) {
            $items:(criticalSection ops free_body)
            free(arr);
            return 0;
          }

          int $id:values_array($ty:ctx_ty *ctx, $ty:array_type *arr, $ty:pt' *data) {
            int err = 0;
            $items:(criticalSection ops values_body)
            return err;
          }

          $ty:memty $id:values_raw_array($ty:ctx_ty *ctx, $ty:array_type *arr) {
            (void)ctx;
            return arr->mem.mem;
          }

          const typename int64_t* $id:shape_array($ty:ctx_ty *ctx, $ty:array_type *arr) {
            (void)ctx;
            return arr->shape;
          }
          |]

  pure $
    Manifest.ArrayOps
      { Manifest.arrayFree = free_array,
        Manifest.arrayShape = shape_array,
        Manifest.arrayValues = values_array,
        Manifest.arrayNew = new_array
      }

lookupOpaqueType :: Name -> OpaqueTypes -> OpaqueType
lookupOpaqueType v (OpaqueTypes types) =
  case lookup v types of
    Just t -> t
    Nothing -> error $ "Unknown opaque type: " ++ show v

opaquePayload :: OpaqueTypes -> OpaqueType -> [ValueType]
opaquePayload _ (OpaqueType ts) = ts
opaquePayload _ (OpaqueSum ts _) = ts
opaquePayload types (OpaqueRecord fs) = concatMap f fs
  where
    f (_, TypeOpaque s) = opaquePayload types $ lookupOpaqueType s types
    f (_, TypeTransparent v) = [v]

entryPointTypeToCType :: Publicness -> EntryPointType -> CompilerM op s C.Type
entryPointTypeToCType _ (TypeOpaque desc) = opaqueToCType desc
entryPointTypeToCType pub (TypeTransparent vt) = valueTypeToCType pub vt

entryTypeName :: EntryPointType -> Manifest.TypeName
entryTypeName (TypeOpaque desc) = nameToText desc
entryTypeName (TypeTransparent vt) = prettyText vt

-- | Figure out which of the members of an opaque type corresponds to
-- which fields.
recordFieldPayloads :: OpaqueTypes -> [EntryPointType] -> [a] -> [[a]]
recordFieldPayloads types = chunks . map typeLength
  where
    typeLength (TypeTransparent _) = 1
    typeLength (TypeOpaque desc) =
      length $ opaquePayload types $ lookupOpaqueType desc types

projectField ::
  Operations op s ->
  EntryPointType ->
  [(Int, ValueType)] ->
  CompilerM op s (C.Type, [C.BlockItem])
projectField _ (TypeTransparent (ValueType sign (Rank 0) pt)) [(i, _)] = do
  pure
    ( primAPIType sign pt,
      [C.citems|v = obj->$id:(tupleField i);|]
    )
projectField ops (TypeTransparent vt) [(i, _)] = do
  ct <- valueTypeToCType Public vt
  pure
    ( [C.cty|$ty:ct *|],
      criticalSection
        ops
        [C.citems|v = malloc(sizeof($ty:ct));
                  memcpy(v, obj->$id:(tupleField i), sizeof($ty:ct));
                  (void)(*(v->mem.references))++;|]
    )
projectField _ (TypeTransparent _) rep =
  error $ "projectField: invalid representation of transparent type: " ++ show rep
projectField ops (TypeOpaque f_desc) components = do
  ct <- opaqueToCType f_desc
  let setField j (i, ValueType _ (Rank r) _) =
        if r == 0
          then [C.citems|v->$id:(tupleField j) = obj->$id:(tupleField i);|]
          else
            [C.citems|v->$id:(tupleField j) = malloc(sizeof(*v->$id:(tupleField j)));
                      *v->$id:(tupleField j) = *obj->$id:(tupleField i);
                      (void)(*(v->$id:(tupleField j)->mem.references))++;|]
  pure
    ( [C.cty|$ty:ct *|],
      criticalSection
        ops
        [C.citems|v = malloc(sizeof($ty:ct));
                  $items:(concat (zipWith setField [0..] components))|]
    )

recordProjectFunctions ::
  OpaqueTypes ->
  Name ->
  [(Name, EntryPointType)] ->
  [ValueType] ->
  CompilerM op s [Manifest.RecordField]
recordProjectFunctions types desc fs vds = do
  opaque_type <- opaqueToCType desc
  ctx_ty <- contextType
  ops <- asks envOperations
  let onField ((f, et), elems) = do
        let f' =
              if isValidCName $ opaqueName desc <> "_" <> nameToText f
                then nameToText f
                else zEncodeText (nameToText f)
        project <- publicName $ "project_" <> opaqueName desc <> "_" <> f'
        (et_ty, project_items) <- projectField ops et elems
        headerDecl
          (OpaqueDecl desc)
          [C.cedecl|int $id:project($ty:ctx_ty *ctx, $ty:et_ty *out, const $ty:opaque_type *obj);|]
        libDecl
          [C.cedecl|int $id:project($ty:ctx_ty *ctx, $ty:et_ty *out, const $ty:opaque_type *obj) {
                      (void)ctx;
                      $ty:et_ty v;
                      $items:project_items
                      *out = v;
                      return 0;
                    }|]
        pure $ Manifest.RecordField (nameToText f) (entryTypeName et) project

  mapM onField . zip fs . recordFieldPayloads types (map snd fs) $
    zip [0 ..] vds

setFieldField :: (C.ToExp a) => Int -> a -> ValueType -> C.Stm
setFieldField i e (ValueType _ (Rank r) _)
  | r == 0 =
      [C.cstm|v->$id:(tupleField i) = $exp:e;|]
  | otherwise =
      [C.cstm|{v->$id:(tupleField i) = malloc(sizeof(*$exp:e));
               *v->$id:(tupleField i) = *$exp:e;
               (void)(*(v->$id:(tupleField i)->mem.references))++;}|]

recordNewFunctions ::
  OpaqueTypes ->
  Name ->
  [(Name, EntryPointType)] ->
  [ValueType] ->
  CompilerM op s Manifest.CFuncName
recordNewFunctions types desc fs vds = do
  opaque_type <- opaqueToCType desc
  ctx_ty <- contextType
  ops <- asks envOperations
  new <- publicName $ "new_" <> opaqueName desc

  (params, new_stms) <-
    fmap (unzip . snd)
      . mapAccumLM onField 0
      . zip fs
      . recordFieldPayloads types (map snd fs)
      $ vds

  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:new($ty:ctx_ty *ctx, $ty:opaque_type** out, $params:params);|]
  libDecl
    [C.cedecl|int $id:new($ty:ctx_ty *ctx, $ty:opaque_type** out, $params:params) {
                $ty:opaque_type* v = malloc(sizeof($ty:opaque_type));
                $items:(criticalSection ops new_stms)
                *out = v;
                return FUTHARK_SUCCESS;
              }|]
  pure new
  where
    onField offset ((f, et), f_vts) = do
      let param_name =
            if T.all isDigit (nameToText f)
              then C.toIdent ("v" <> f) mempty
              else C.toIdent ("f_" <> f) mempty
      case et of
        TypeTransparent (ValueType sign (Rank 0) pt) -> do
          let ct = primAPIType sign pt
          pure
            ( offset + 1,
              ( [C.cparam|const $ty:ct $id:param_name|],
                [C.citem|v->$id:(tupleField offset) = $id:param_name;|]
              )
            )
        TypeTransparent vt -> do
          ct <- valueTypeToCType Public vt
          pure
            ( offset + 1,
              ( [C.cparam|const $ty:ct* $id:param_name|],
                [C.citem|{v->$id:(tupleField offset) = malloc(sizeof($ty:ct));
                          *v->$id:(tupleField offset) = *$id:param_name;
                          (void)(*(v->$id:(tupleField offset)->mem.references))++;}|]
              )
            )
        TypeOpaque f_desc -> do
          ct <- opaqueToCType f_desc
          let param_fields = do
                i <- [0 ..]
                pure [C.cexp|$id:param_name->$id:(tupleField i)|]
          pure
            ( offset + length f_vts,
              ( [C.cparam|const $ty:ct* $id:param_name|],
                [C.citem|{$stms:(zipWith3 setFieldField [offset ..] param_fields f_vts)}|]
              )
            )

sumVariants ::
  Name ->
  [(Name, [(EntryPointType, [Int])])] ->
  [ValueType] ->
  CompilerM op s [Manifest.SumVariant]
sumVariants desc variants vds = do
  opaque_ty <- opaqueToCType desc
  ctx_ty <- contextType
  ops <- asks envOperations

  let onVariant i (name, payload) = do
        construct <- publicName $ "new_" <> opaqueName desc <> "_" <> nameToText name
        destruct <- publicName $ "destruct_" <> opaqueName desc <> "_" <> nameToText name

        constructFunction ops ctx_ty opaque_ty i construct payload
        destructFunction ops ctx_ty opaque_ty i destruct payload

        pure $
          Manifest.SumVariant
            { Manifest.sumVariantName = nameToText name,
              Manifest.sumVariantPayload = map (entryTypeName . fst) payload,
              Manifest.sumVariantConstruct = construct,
              Manifest.sumVariantDestruct = destruct
            }

  zipWithM onVariant [0 :: Int ..] variants
  where
    constructFunction ops ctx_ty opaque_ty i fname payload = do
      (params, new_stms) <- unzip <$> zipWithM constructPayload [0 ..] payload

      let used = concatMap snd payload
      set_unused_stms <-
        mapM setUnused $ filter ((`notElem` used) . fst) (zip [0 ..] vds)

      headerDecl
        (OpaqueDecl desc)
        [C.cedecl|int $id:fname($ty:ctx_ty *ctx,
                                $ty:opaque_ty **out,
                                $params:params);|]

      libDecl
        [C.cedecl|int $id:fname($ty:ctx_ty *ctx,
                                $ty:opaque_ty **out,
                                $params:params) {
                    (void)ctx;
                    $ty:opaque_ty* v = malloc(sizeof($ty:opaque_ty));
                    v->$id:(tupleField 0) = $int:i;
                    { $items:(criticalSection ops new_stms) }
                    // Set other fields
                    { $items:set_unused_stms }
                    *out = v;
                    return FUTHARK_SUCCESS;
                  }|]

    -- We must initialise some of the fields that are unused in this
    -- variant; specifically the ones corresponding to arrays. This
    -- has the unfortunate effect that all arrays in the nonused
    -- constructor are set to have size 0.
    setUnused (_, ValueType _ (Rank 0) _) =
      pure [C.citem|{}|]
    setUnused (i, ValueType signed (Rank rank) pt) = do
      new_array <- publicName $ "new_" <> arrayName pt signed rank
      let dims = replicate rank [C.cexp|0|]
      pure [C.citem|v->$id:(tupleField i) = $id:new_array(ctx, NULL, $args:dims);|]

    constructPayload j (et, is) = do
      let param_name = "v" <> show (j :: Int)
      case et of
        TypeTransparent (ValueType sign (Rank 0) pt) -> do
          let ct = primAPIType sign pt
              i = head is
          pure
            ( [C.cparam|const $ty:ct $id:param_name|],
              [C.citem|v->$id:(tupleField i) = $id:param_name;|]
            )
        TypeTransparent vt -> do
          ct <- valueTypeToCType Public vt
          let i = head is
          pure
            ( [C.cparam|const $ty:ct* $id:param_name|],
              [C.citem|{v->$id:(tupleField i) = malloc(sizeof($ty:ct));
                        memcpy(v->$id:(tupleField i), $id:param_name, sizeof(const $ty:ct));
                        (void)(*(v->$id:(tupleField i)->mem.references))++;}|]
            )
        TypeOpaque f_desc -> do
          ct <- opaqueToCType f_desc
          let param_fields = do
                i <- [0 ..]
                pure [C.cexp|$id:param_name->$id:(tupleField i)|]
              vts = map (vds !!) is
          pure
            ( [C.cparam|const $ty:ct* $id:param_name|],
              [C.citem|{$stms:(zipWith3 setFieldField is param_fields vts)}|]
            )

    destructFunction ops ctx_ty opaque_ty i fname payload = do
      (params, destruct_stms) <- unzip <$> zipWithM (destructPayload ops) [0 ..] payload
      headerDecl
        (OpaqueDecl desc)
        [C.cedecl|int $id:fname($ty:ctx_ty *ctx,
                                $params:params,
                                const $ty:opaque_ty *obj);|]

      libDecl
        [C.cedecl|int $id:fname($ty:ctx_ty *ctx,
                                $params:params,
                                const $ty:opaque_ty *obj) {
                    (void)ctx;
                    assert(obj->$id:(tupleField 0) == $int:i);
                    $stms:destruct_stms
                    return FUTHARK_SUCCESS;
                  }|]

    destructPayload ops j (et, is) = do
      let param_name = "v" <> show (j :: Int)
      (ct, project_items) <- projectField ops et $ zip is $ map (vds !!) is
      pure
        ( [C.cparam|$ty:ct* $id:param_name|],
          [C.cstm|{$ty:ct v;
                   $items:project_items
                   *$id:param_name = v;
                  }|]
        )

sumVariantFunction :: Name -> CompilerM op s Manifest.CFuncName
sumVariantFunction desc = do
  opaque_ty <- opaqueToCType desc
  ctx_ty <- contextType
  variant <- publicName $ "variant_" <> opaqueName desc
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:variant($ty:ctx_ty *ctx, const $ty:opaque_ty* v);|]
  -- This depends on the assumption that the first value always
  -- encodes the variant.
  libDecl
    [C.cedecl|int $id:variant($ty:ctx_ty *ctx, const $ty:opaque_ty* v) {
                (void)ctx;
                return v->$id:(tupleField 0);
              }|]
  pure variant

processOpaqueRecord ::
  OpaqueTypes ->
  Name ->
  OpaqueType ->
  [ValueType] ->
  CompilerM op s (Maybe Manifest.RecordOps, Maybe Manifest.SumOps)
processOpaqueRecord _ _ (OpaqueType _) _ =
  pure (Nothing, Nothing)
processOpaqueRecord _types desc (OpaqueSum _ cs) vds =
  (Nothing,) . Just
    <$> ( Manifest.SumOps
            <$> sumVariants desc cs vds
            <*> sumVariantFunction desc
        )
processOpaqueRecord types desc (OpaqueRecord fs) vds =
  (,Nothing) . Just
    <$> ( Manifest.RecordOps
            <$> recordProjectFunctions types desc fs vds
            <*> recordNewFunctions types desc fs vds
        )

opaqueLibraryFunctions ::
  OpaqueTypes ->
  Name ->
  OpaqueType ->
  CompilerM op s (Manifest.OpaqueOps, Maybe Manifest.RecordOps, Maybe Manifest.SumOps)
opaqueLibraryFunctions types desc ot = do
  name <- publicName $ opaqueName desc
  free_opaque <- publicName $ "free_" <> opaqueName desc
  store_opaque <- publicName $ "store_" <> opaqueName desc
  restore_opaque <- publicName $ "restore_" <> opaqueName desc

  let opaque_type = [C.cty|struct $id:name|]

      freeComponent i (ValueType signed (Rank rank) pt) = unless (rank == 0) $ do
        let field = tupleField i
        free_array <- publicName $ "free_" <> arrayName pt signed rank
        -- Protect against NULL here, because we also want to use this
        -- to free partially loaded opaques.
        stm
          [C.cstm|if (obj->$id:field != NULL && (tmp = $id:free_array(ctx, obj->$id:field)) != 0) {
                ret = tmp;
             }|]

      storeComponent i (ValueType sign (Rank 0) pt) =
        let field = tupleField i
         in ( storageSize pt 0 [C.cexp|NULL|],
              storeValueHeader sign pt 0 [C.cexp|NULL|] [C.cexp|out|]
                ++ [C.cstms|memcpy(out, &obj->$id:field, sizeof(obj->$id:field));
                            out += sizeof(obj->$id:field);|]
            )
      storeComponent i (ValueType sign (Rank rank) pt) =
        let arr_name = arrayName pt sign rank
            field = tupleField i
            shape_array = "futhark_shape_" <> arr_name
            values_array = "futhark_values_" <> arr_name
            shape' = [C.cexp|$id:shape_array(ctx, obj->$id:field)|]
            num_elems = cproduct [[C.cexp|$exp:shape'[$int:j]|] | j <- [0 .. rank - 1]]
         in ( storageSize pt rank shape',
              storeValueHeader sign pt rank shape' [C.cexp|out|]
                ++ [C.cstms|ret |= $id:values_array(ctx, obj->$id:field, (void*)out);
                            out += $exp:num_elems * sizeof($ty:(primStorageType pt));|]
            )

  ctx_ty <- contextType

  let vds = opaquePayload types ot
  free_body <- collect $ zipWithM_ freeComponent [0 ..] vds

  store_body <- collect $ do
    let (sizes, stores) = unzip $ zipWith storeComponent [0 ..] vds
        size_vars = map (("size_" ++) . show) [0 .. length sizes - 1]
        size_sum = csum [[C.cexp|$id:size|] | size <- size_vars]
    forM_ (zip size_vars sizes) $ \(v, e) ->
      item [C.citem|typename int64_t $id:v = $exp:e;|]
    stm [C.cstm|*n = $exp:size_sum;|]
    stm [C.cstm|if (p != NULL && *p == NULL) { *p = malloc(*n); }|]
    stm [C.cstm|if (p != NULL) { unsigned char *out = *p; $stms:(concat stores) }|]

  let restoreComponent i (ValueType sign (Rank 0) pt) = do
        let field = tupleField i
            dataptr = "data_" ++ show i
        stms $ loadValueHeader sign pt 0 [C.cexp|NULL|] [C.cexp|src|]
        item [C.citem|const void* $id:dataptr = src;|]
        stm [C.cstm|src += sizeof(obj->$id:field);|]
        pure [C.cstms|memcpy(&obj->$id:field, $id:dataptr, sizeof(obj->$id:field));|]
      restoreComponent i (ValueType sign (Rank rank) pt) = do
        let field = tupleField i
            arr_name = arrayName pt sign rank
            new_array = "futhark_new_" <> arr_name
            dataptr = "data_" <> prettyText i
            shapearr = "shape_" <> prettyText i
            dims = [[C.cexp|$id:shapearr[$int:j]|] | j <- [0 .. rank - 1]]
            num_elems = cproduct dims
        item [C.citem|typename int64_t $id:shapearr[$int:rank] = {0};|]
        stms $ loadValueHeader sign pt rank [C.cexp|$id:shapearr|] [C.cexp|src|]
        item [C.citem|const void* $id:dataptr = src;|]
        stm [C.cstm|obj->$id:field = NULL;|]
        stm [C.cstm|src += $exp:num_elems * sizeof($ty:(primStorageType pt));|]
        pure
          [C.cstms|
             obj->$id:field = $id:new_array(ctx, $id:dataptr, $args:dims);
             if (obj->$id:field == NULL) { err = 1; }|]

  load_body <- collect $ do
    loads <- concat <$> zipWithM restoreComponent [0 ..] (opaquePayload types ot)
    stm
      [C.cstm|if (err == 0) {
                $stms:loads
              }|]

  headerDecl
    (OpaqueTypeDecl desc)
    [C.cedecl|struct $id:name;|]
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:free_opaque($ty:ctx_ty *ctx, $ty:opaque_type *obj);|]
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:store_opaque($ty:ctx_ty *ctx, const $ty:opaque_type *obj, void **p, size_t *n);|]
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|$ty:opaque_type* $id:restore_opaque($ty:ctx_ty *ctx, const void *p);|]

  (record, sumops) <- processOpaqueRecord types desc ot vds

  -- We do not need to enclose most bodies in a critical section,
  -- because when we operate on the components of the opaque, we are
  -- calling public API functions that do their own locking.  The
  -- exception is projection, where we fiddle with reference counts.
  mapM_
    libDecl
    [C.cunit|
          int $id:free_opaque($ty:ctx_ty *ctx, $ty:opaque_type *obj) {
            (void)ctx;
            int ret = 0, tmp;
            $items:free_body
            free(obj);
            return ret;
          }

          int $id:store_opaque($ty:ctx_ty *ctx,
                               const $ty:opaque_type *obj, void **p, size_t *n) {
            (void)ctx;
            int ret = 0;
            $items:store_body
            return ret;
          }

          $ty:opaque_type* $id:restore_opaque($ty:ctx_ty *ctx,
                                              const void *p) {
            (void)ctx;
            int err = 0;
            const unsigned char *src = p;
            $ty:opaque_type* obj = malloc(sizeof($ty:opaque_type));
            $items:load_body
            if (err != 0) {
              int ret = 0, tmp;
              $items:free_body
              free(obj);
              obj = NULL;
            }
            return obj;
          }
    |]

  pure
    ( Manifest.OpaqueOps
        { Manifest.opaqueFree = free_opaque,
          Manifest.opaqueStore = store_opaque,
          Manifest.opaqueRestore = restore_opaque
        },
      record,
      sumops
    )

generateArray ::
  Space ->
  ((Signedness, PrimType, Int), Publicness) ->
  CompilerM op s (Maybe (T.Text, Manifest.Type))
generateArray space ((signed, pt, rank), pub) = do
  name <- publicName $ arrayName pt signed rank
  let memty = fatMemType space
  libDecl [C.cedecl|struct $id:name { $ty:memty mem; typename int64_t shape[$int:rank]; };|]
  ops <- arrayLibraryFunctions pub space pt signed rank
  let pt_name = prettySigned (signed == Unsigned) pt
      pretty_name = mconcat (replicate rank "[]") <> pt_name
      arr_type = [C.cty|struct $id:name*|]
  case pub of
    Public ->
      pure $
        Just
          ( pretty_name,
            Manifest.TypeArray (typeText arr_type) pt_name rank ops
          )
    Private ->
      pure Nothing

generateOpaque ::
  OpaqueTypes ->
  (Name, OpaqueType) ->
  CompilerM op s (T.Text, Manifest.Type)
generateOpaque types (desc, ot) = do
  name <- publicName $ opaqueName desc
  members <- zipWithM field (opaquePayload types ot) [(0 :: Int) ..]
  libDecl [C.cedecl|struct $id:name { $sdecls:members };|]
  (ops, record, sumops) <- opaqueLibraryFunctions types desc ot
  let opaque_type = [C.cty|struct $id:name*|]
  pure (nameToText desc, Manifest.TypeOpaque (typeText opaque_type) ops record sumops)
  where
    field vt@(ValueType _ (Rank r) _) i = do
      ct <- valueTypeToCType Private vt
      pure $
        if r == 0
          then [C.csdecl|$ty:ct $id:(tupleField i);|]
          else [C.csdecl|$ty:ct *$id:(tupleField i);|]

generateAPITypes :: Space -> OpaqueTypes -> CompilerM op s (M.Map T.Text Manifest.Type)
generateAPITypes arr_space types@(OpaqueTypes opaques) = do
  mapM_ (findNecessaryArrays . snd) opaques
  array_ts <- mapM (generateArray arr_space) . M.toList =<< gets compArrayTypes
  opaque_ts <- mapM (generateOpaque types) opaques
  pure $ M.fromList $ catMaybes array_ts <> opaque_ts
  where
    -- Ensure that array types will be generated before the opaque
    -- types that allow projection of them.  This is because the
    -- projection functions somewhat uglily directly poke around in
    -- the innards to increment reference counts.
    findNecessaryArrays (OpaqueType _) =
      pure ()
    findNecessaryArrays (OpaqueSum _ variants) =
      mapM_ (mapM_ (entryPointTypeToCType Public . fst) . snd) variants
    findNecessaryArrays (OpaqueRecord fs) =
      mapM_ (entryPointTypeToCType Public . snd) fs
