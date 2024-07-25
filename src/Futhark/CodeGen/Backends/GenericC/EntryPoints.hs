{-# LANGUAGE QuasiQuotes #-}

-- | Generate the entry point packing/unpacking code.
module Futhark.CodeGen.Backends.GenericC.EntryPoints
  ( onEntryPoint,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Monad
import Futhark.CodeGen.Backends.GenericC.Types (opaqueToCType, valueTypeToCType)
import Futhark.CodeGen.ImpCode
import Futhark.Manifest qualified as Manifest
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

valueDescToType :: ValueDesc -> ValueType
valueDescToType (ScalarValue pt signed _) =
  ValueType signed (Rank 0) pt
valueDescToType (ArrayValue _ _ pt signed shape) =
  ValueType signed (Rank (length shape)) pt

prepareEntryInputs ::
  [ExternalValue] ->
  CompilerM op s ([(C.Param, Maybe C.Exp)], [C.BlockItem])
prepareEntryInputs args = collect' $ zipWithM prepare [(0 :: Int) ..] args
  where
    arg_names = namesFromList $ concatMap evNames args
    evNames (OpaqueValue _ vds) = map vdName vds
    evNames (TransparentValue vd) = [vdName vd]
    vdName (ArrayValue v _ _ _ _) = v
    vdName (ScalarValue _ _ v) = v

    prepare pno (TransparentValue vd) = do
      let pname = "in" ++ show pno
      (ty, check) <- prepareValue Public [C.cexp|$id:pname|] vd
      pure
        ( [C.cparam|const $ty:ty $id:pname|],
          if null check then Nothing else Just $ allTrue check
        )
    prepare pno (OpaqueValue desc vds) = do
      ty <- opaqueToCType desc
      let pname = "in" ++ show pno
          field i ScalarValue {} = [C.cexp|$id:pname->$id:(tupleField i)|]
          field i ArrayValue {} = [C.cexp|$id:pname->$id:(tupleField i)|]
      checks <- map snd <$> zipWithM (prepareValue Private) (zipWith field [0 ..] vds) vds
      pure
        ( [C.cparam|const $ty:ty *$id:pname|],
          if all null checks
            then Nothing
            else Just $ allTrue $ concat checks
        )

    prepareValue _ src (ScalarValue pt signed name) = do
      let pt' = primAPIType signed pt
          src' = fromStorage pt $ C.toExp src mempty
      stm [C.cstm|$id:name = $exp:src';|]
      pure (pt', [])
    prepareValue pub src vd@(ArrayValue mem _ _ _ shape) = do
      ty <- valueTypeToCType pub $ valueDescToType vd

      stm [C.cstm|$exp:mem = $exp:src->mem;|]

      let rank = length shape
          maybeCopyDim (Var d) i
            | d `notNameIn` arg_names =
                ( Just [C.cstm|$id:d = $exp:src->shape[$int:i];|],
                  [C.cexp|$id:d == $exp:src->shape[$int:i]|]
                )
          maybeCopyDim x i =
            ( Nothing,
              [C.cexp|$exp:x == $exp:src->shape[$int:i]|]
            )

      let (sets, checks) =
            unzip $ zipWith maybeCopyDim shape [0 .. rank - 1]
      stms $ catMaybes sets

      pure ([C.cty|$ty:ty*|], checks)

prepareEntryOutputs :: [ExternalValue] -> CompilerM op s ([C.Param], [C.BlockItem])
prepareEntryOutputs = collect' . zipWithM prepare [(0 :: Int) ..]
  where
    prepare pno (TransparentValue vd) = do
      let pname = "out" ++ show pno
      ty <- valueTypeToCType Public $ valueDescToType vd

      case vd of
        ArrayValue {} -> do
          stm [C.cstm|assert((*$id:pname = ($ty:ty*) malloc(sizeof($ty:ty))) != NULL);|]
          prepareValue [C.cexp|*$id:pname|] vd
          pure [C.cparam|$ty:ty **$id:pname|]
        ScalarValue {} -> do
          prepareValue [C.cexp|*$id:pname|] vd
          pure [C.cparam|$ty:ty *$id:pname|]
    prepare pno (OpaqueValue desc vds) = do
      let pname = "out" ++ show pno
      ty <- opaqueToCType desc
      vd_ts <- mapM (valueTypeToCType Private . valueDescToType) vds

      stm [C.cstm|assert((*$id:pname = ($ty:ty*) malloc(sizeof($ty:ty))) != NULL);|]

      forM_ (zip3 [0 ..] vd_ts vds) $ \(i, ct, vd) -> do
        let field = [C.cexp|((*$id:pname)->$id:(tupleField i))|]
        case vd of
          ScalarValue {} -> pure ()
          ArrayValue {} -> do
            stm [C.cstm|assert(($exp:field = ($ty:ct*) malloc(sizeof($ty:ct))) != NULL);|]
        prepareValue field vd

      pure [C.cparam|$ty:ty **$id:pname|]

    prepareValue dest (ScalarValue t _ name) =
      let name' = toStorage t $ C.toExp name mempty
       in stm [C.cstm|$exp:dest = $exp:name';|]
    prepareValue dest (ArrayValue mem _ _ _ shape) = do
      stm [C.cstm|$exp:dest->mem = $id:mem;|]

      let rank = length shape
          maybeCopyDim (Constant x) i =
            [C.cstm|$exp:dest->shape[$int:i] = $exp:x;|]
          maybeCopyDim (Var d) i =
            [C.cstm|$exp:dest->shape[$int:i] = $id:d;|]
      stms $ zipWith maybeCopyDim shape [0 .. rank - 1]

entryName :: Name -> T.Text
entryName = ("entry_" <>) . escapeName . nameToText

onEntryPoint ::
  [C.BlockItem] ->
  [Name] ->
  Name ->
  Function op ->
  CompilerM op s (Maybe (C.Definition, (T.Text, Manifest.EntryPoint)))
onEntryPoint _ _ _ (Function Nothing _ _ _) = pure Nothing
onEntryPoint get_consts relevant_params fname (Function (Just (EntryPoint ename results args)) outputs inputs _) = inNewFunction $ do
  let out_args = map (\p -> [C.cexp|&$id:(paramName p)|]) outputs
      in_args = map (\p -> [C.cexp|$id:(paramName p)|]) inputs

  inputdecls <- collect $ mapM_ stubParam inputs
  outputdecls <- collect $ mapM_ stubParam outputs
  decl_mem <- declAllocatedMem

  entry_point_function_name <- publicName $ entryName ename

  (inputs', unpack_entry_inputs) <- prepareEntryInputs $ map snd args
  let (entry_point_input_params, entry_point_input_checks) = unzip inputs'

  (entry_point_output_params, pack_entry_outputs) <-
    prepareEntryOutputs $ map snd results

  ctx_ty <- contextType

  headerDecl
    EntryDecl
    [C.cedecl|int $id:entry_point_function_name
                                     ($ty:ctx_ty *ctx,
                                      $params:entry_point_output_params,
                                      $params:entry_point_input_params);|]

  let checks = catMaybes entry_point_input_checks
      check_input =
        if null checks
          then []
          else
            [C.citems|
         if (!($exp:(allTrue (catMaybes entry_point_input_checks)))) {
           ret = 1;
           set_error(ctx, msgprintf("Error: entry point arguments have invalid sizes.\n"));
         }|]

      critical =
        [C.citems|
         $items:decl_mem
         $items:unpack_entry_inputs
         $items:check_input
         if (ret == 0) {
           ret = $id:(funName fname)(ctx, $args:out_args, $args:in_args);
           if (ret == 0) {
             $items:get_consts

             $items:pack_entry_outputs
           }
         }
        |]

  ops <- asks envOperations

  let cdef =
        [C.cedecl|
       int $id:entry_point_function_name
           ($ty:ctx_ty *ctx,
            $params:entry_point_output_params,
            $params:entry_point_input_params) {
         $items:inputdecls
         $items:outputdecls

         int ret = 0;

         $items:(criticalSection ops critical)

         return ret;
       }
       |]

      manifest =
        Manifest.EntryPoint
          { Manifest.entryPointCFun = entry_point_function_name,
            Manifest.entryPointTuningParams = map nameToText relevant_params,
            -- Note that our convention about what is "input/output"
            -- and what is "results/args" is different between the
            -- manifest and ImpCode.
            Manifest.entryPointOutputs = map outputManifest results,
            Manifest.entryPointInputs = map inputManifest args
          }

  pure $ Just (cdef, (nameToText ename, manifest))
  where
    stubParam (MemParam name space) =
      declMem name space
    stubParam (ScalarParam name ty) = do
      let ty' = primTypeToCType ty
      decl [C.cdecl|$ty:ty' $id:name = $exp:(blankPrimValue ty);|]

    vdType (TransparentValue (ScalarValue pt signed _)) =
      prettySigned (signed == Unsigned) pt
    vdType (TransparentValue (ArrayValue _ _ pt signed shape)) =
      mconcat (replicate (length shape) "[]")
        <> prettySigned (signed == Unsigned) pt
    vdType (OpaqueValue name _) =
      nameToText name

    outputManifest (u, vd) =
      Manifest.Output
        { Manifest.outputType = vdType vd,
          Manifest.outputUnique = u == Unique
        }
    inputManifest ((v, u), vd) =
      Manifest.Input
        { Manifest.inputName = nameToText v,
          Manifest.inputType = vdType vd,
          Manifest.inputUnique = u == Unique
        }
