{-# LANGUAGE QuasiQuotes #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent ISPC program.
module Futhark.CodeGen.Backends.MulticoreISPC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
    operations,
    ISPCState,
  )
where

import Control.Lens (each, over)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.DList qualified as DL
import Data.List (unzip4)
import Data.Loc (noLoc)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.MulticoreC qualified as MC
import Futhark.CodeGen.Backends.MulticoreC.Boilerplate (generateBoilerplate)
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode.Multicore
import Futhark.CodeGen.ImpGen.Multicore qualified as ImpGen
import Futhark.CodeGen.RTS.C (errorsH, ispcUtilH, uniformH)
import Futhark.IR.MCMem (MCMem, Prog)
import Futhark.IR.Prop (isBuiltInFunction)
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)

type ISPCCompilerM a = GC.CompilerM Multicore ISPCState a

-- | Transient state tracked by the ISPC backend.
data ISPCState = ISPCState
  { sDefs :: DL.DList C.Definition,
    sUniform :: Names
  }

uniform :: C.TypeQual
uniform = C.EscTypeQual "uniform" noLoc

unmasked :: C.TypeQual
unmasked = C.EscTypeQual "unmasked" noLoc

export :: C.TypeQual
export = C.EscTypeQual "export" noLoc

varying :: C.TypeQual
varying = C.EscTypeQual "varying" noLoc

-- | Compile the program to C and ISPC code using multicore operations.
compileProg ::
  (MonadFreshNames m) => T.Text -> Prog MCMem -> m (ImpGen.Warnings, (GC.CParts, T.Text))
compileProg version prog = do
  -- Dynamic scheduling seems completely broken currently, so we disable it.
  (ws, defs) <- ImpGen.compileProg prog
  let Functions funs = defFuns defs

  (ws', (cparts, endstate)) <-
    traverse
      ( GC.compileProg'
          "ispc"
          version
          mempty
          operations
          (ISPCState mempty mempty)
          ( do
              generateBoilerplate
              mapM_ compileBuiltinFun funs
          )
          mempty
          (DefaultSpace, [DefaultSpace])
          MC.cliOptions
      )
      (ws, defs)

  let ispc_decls = definitionsText $ DL.toList $ sDefs $ GC.compUserState endstate

  -- The bool #define is a workaround around an ISPC bug, stdbool doesn't get included.
  let ispcdefs =
        [untrimming|
#define bool uint8
typedef int64 int64_t;
typedef int32 int32_t;
typedef int16 int16_t;
typedef int8 int8_t;
typedef int8 char;
typedef unsigned int64 uint64_t;
typedef unsigned int32 uint32_t;
typedef unsigned int16 uint16_t;
typedef unsigned int8 uint8_t;
#define volatile
#define SCALAR_FUN_ATTR static inline

$errorsH

#define INFINITY (floatbits((uniform int)0x7f800000))
#define NAN (floatbits((uniform int)0x7fc00000))
#define fabs(x) abs(x)
#define FUTHARK_F64_ENABLED
$cScalarDefs

$uniformH

$ispcUtilH

$ispc_decls|]

  pure (ws', (cparts, ispcdefs))

-- | Compiler operations specific to the ISPC multicore backend.
operations :: GC.Operations Multicore ISPCState
operations =
  MC.operations
    { GC.opsCompiler = compileOp,
      -- FIXME: the default codegen for LMAD copies does not work for ISPC.
      GC.opsCopies = mempty
    }

ispcDecl :: C.Definition -> ISPCCompilerM ()
ispcDecl def =
  GC.modifyUserState (\s -> s {sDefs = sDefs s <> DL.singleton def})

ispcEarlyDecl :: C.Definition -> ISPCCompilerM ()
ispcEarlyDecl def =
  GC.modifyUserState (\s -> s {sDefs = DL.singleton def <> sDefs s})

ispcDef :: MC.DefSpecifier ISPCState
ispcDef s f = do
  s' <- MC.multicoreName s
  ispcDecl =<< f s'
  pure s'

-- | Expose a struct to both ISPC and C.
sharedDef :: MC.DefSpecifier ISPCState
sharedDef s f = do
  s' <- MC.multicoreName s
  ispcDecl =<< f s'
  GC.earlyDecl =<< f s'
  pure s'

-- | ISPC has no string literals, so this makes one in C and exposes it via an
-- external function, returning the name.
makeStringLiteral :: String -> ISPCCompilerM Name
makeStringLiteral str = do
  name <- MC.multicoreDef "strlit_shim" $ \s ->
    pure [C.cedecl|char* $id:s() { return $string:str; }|]
  ispcDecl
    [C.cedecl|extern "C" $tyqual:unmasked $tyqual:uniform char* $tyqual:uniform $id:name();|]
  pure name

-- | Set memory in ISPC
setMem :: (C.ToExp a, C.ToExp b) => a -> b -> Space -> ISPCCompilerM ()
setMem dest src space = do
  let src_s = T.unpack $ expText $ C.toExp src noLoc
  strlit <- makeStringLiteral src_s
  GC.stm
    [C.cstm|if ($id:(GC.fatMemSet space)(ctx, &$exp:dest, &$exp:src,
                                            $id:strlit()) != 0) {
                    $escstm:("unmasked { return 1; }")
                  }|]

-- | Unref memory in ISPC
unRefMem :: (C.ToExp a) => a -> Space -> ISPCCompilerM ()
unRefMem mem space = do
  cached <- isJust <$> GC.cacheMem mem
  let mem_s = T.unpack $ expText $ C.toExp mem noLoc
  strlit <- makeStringLiteral mem_s
  unless cached $
    GC.stm
      [C.cstm|if ($id:(GC.fatMemUnRef space)(ctx, &$exp:mem, $id:strlit()) != 0) {
                  $escstm:("unmasked { return 1; }")
                }|]

-- | Allocate memory in ISPC
allocMem ::
  (C.ToExp a, C.ToExp b) =>
  a ->
  b ->
  Space ->
  C.Stm ->
  ISPCCompilerM ()
allocMem mem size space on_failure = do
  let mem_s = T.unpack $ expText $ C.toExp mem noLoc
  strlit <- makeStringLiteral mem_s
  GC.stm
    [C.cstm|if ($id:(GC.fatMemAlloc space)(ctx, &$exp:mem, $exp:size, $id:strlit())) {
                    $stm:on_failure
            }|]

-- | Free memory in ISPC
freeAllocatedMem :: ISPCCompilerM [C.BlockItem]
freeAllocatedMem = GC.collect $ mapM_ (uncurry unRefMem) =<< gets GC.compDeclaredMem

-- | Given a ImpCode function, generate all the required machinery for calling
-- it in ISPC, both in a varying or uniform context. This involves handling
-- for the fact that ISPC cannot pass structs by value to external functions.
compileBuiltinFun :: (Name, Function op) -> ISPCCompilerM ()
compileBuiltinFun (fname, func@(Function _ outputs inputs _))
  | isNothing $ functionEntry func = do
      let extra = [[C.cparam|$tyqual:uniform struct futhark_context * $tyqual:uniform ctx|]]
          extra_c = [[C.cparam|struct futhark_context * ctx|]]
          extra_exp = [[C.cexp|$id:p|] | C.Param (Just p) _ _ _ <- extra]

      (inparams_c, in_args_c) <- mapAndUnzipM (compileInputsExtern []) inputs
      (outparams_c, out_args_c) <- mapAndUnzipM (compileOutputsExtern []) outputs

      (inparams_extern, _) <- mapAndUnzipM (compileInputsExtern [C.ctyquals|$tyqual:uniform|]) inputs
      (outparams_extern, _) <- mapAndUnzipM (compileOutputsExtern [C.ctyquals|$tyqual:uniform|]) outputs

      (inparams_uni, in_args_noderef) <- mapAndUnzipM compileInputsUniform inputs
      (outparams_uni, out_args_noderef) <- mapAndUnzipM compileOutputsUniform outputs

      (inparams_varying, in_args_vary, prebody_in') <- unzip3 <$> mapM compileInputsVarying inputs
      (outparams_varying, out_args_vary, prebody_out', postbody_out') <- unzip4 <$> mapM compileOutputsVarying outputs
      let (prebody_in, prebody_out, postbody_out) = over each concat (prebody_in', prebody_out', postbody_out')

      GC.libDecl
        [C.cedecl|int $id:(funName fname <> "_extern")($params:extra_c, $params:outparams_c, $params:inparams_c) {
                  return $id:(funName fname)($args:extra_exp, $args:out_args_c, $args:in_args_c);
                }|]

      let ispc_extern =
            [C.cedecl|extern "C" $tyqual:unmasked $tyqual:uniform int $id:((funName fname) <> "_extern")
                      ($params:extra, $params:outparams_extern, $params:inparams_extern);|]

          ispc_uniform =
            [C.cedecl|$tyqual:uniform int $id:(funName fname)
                    ($params:extra, $params:outparams_uni, $params:inparams_uni) {
                      return $id:(funName (fname<>"_extern"))(
                        $args:extra_exp,
                        $args:out_args_noderef,
                        $args:in_args_noderef);
                    }|]

          ispc_varying =
            [C.cedecl|$tyqual:uniform int $id:(funName fname)
                    ($params:extra, $params:outparams_varying, $params:inparams_varying) {
                        $tyqual:uniform int err = 0;
                        $items:prebody_in
                        $items:prebody_out
                        $escstm:("foreach_active (i)")
                        {
                          err |= $id:(funName $ fname<>"_extern")(
                            $args:extra_exp,
                            $args:out_args_vary,
                            $args:in_args_vary);
                        }
                        $items:postbody_out
                        return err;
                    }|]

      mapM_ ispcEarlyDecl [ispc_varying, ispc_uniform, ispc_extern]
  | otherwise = pure ()
  where
    compileInputsExtern vari (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      pure ([C.cparam|$tyquals:vari $ty:ctp $id:name|], [C.cexp|$id:name|])
    compileInputsExtern vari (MemParam name space) = do
      ty <- GC.memToCType name space
      pure ([C.cparam|$tyquals:vari $ty:ty * $tyquals:vari $id:name|], [C.cexp|*$id:name|])

    compileOutputsExtern vari (ScalarParam name bt) = do
      p_name <- newVName $ "out_" ++ baseString name
      let ctp = GC.primTypeToCType bt
      pure ([C.cparam|$tyquals:vari $ty:ctp * $tyquals:vari $id:p_name|], [C.cexp|$id:p_name|])
    compileOutputsExtern vari (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      pure ([C.cparam|$tyquals:vari $ty:ty * $tyquals:vari $id:p_name|], [C.cexp|$id:p_name|])

    compileInputsUniform (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
          params = [C.cparam|$tyqual:uniform $ty:ctp $id:name|]
          args = [C.cexp|$id:name|]
      pure (params, args)
    compileInputsUniform (MemParam name space) = do
      ty <- GC.memToCType name space
      let params = [C.cparam|$tyqual:uniform $ty:ty $id:name|]
          args = [C.cexp|&$id:name|]
      pure (params, args)

    compileOutputsUniform (ScalarParam name bt) = do
      p_name <- newVName $ "out_" ++ baseString name
      let ctp = GC.primTypeToCType bt
          params = [C.cparam|$tyqual:uniform $ty:ctp *$tyqual:uniform $id:p_name|]
          args = [C.cexp|$id:p_name|]
      pure (params, args)
    compileOutputsUniform (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      let params = [C.cparam|$tyqual:uniform $ty:ty $id:p_name|]
          args = [C.cexp|&$id:p_name|]
      pure (params, args)

    compileInputsVarying (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
          params = [C.cparam|$ty:ctp $id:name|]
          args = [C.cexp|extract($id:name,i)|]
          pre_body = []
      pure (params, args, pre_body)
    compileInputsVarying (MemParam name space) = do
      typ <- GC.memToCType name space
      newvn <- newVName $ "aos_" <> baseString name
      let params = [C.cparam|$ty:typ $id:name|]
          args = [C.cexp|&$id:(newvn)[i]|]
          pre_body =
            [C.citems|$tyqual:uniform $ty:typ $id:(newvn)[programCount];
                               $id:(newvn)[programIndex] = $id:name;|]
      pure (params, args, pre_body)

    compileOutputsVarying (ScalarParam name bt) = do
      p_name <- newVName $ "out_" ++ baseString name
      deref_name <- newVName $ "aos_" ++ baseString name
      vari_p_name <- newVName $ "convert_" ++ baseString name
      let ctp = GC.primTypeToCType bt
          pre_body =
            [C.citems|$tyqual:varying $ty:ctp $id:vari_p_name = *$id:p_name;
                                $tyqual:uniform $ty:ctp $id:deref_name[programCount];
                                $id:deref_name[programIndex] = $id:vari_p_name;|]
          post_body = [C.citems|*$id:p_name = $id:(deref_name)[programIndex];|]
          params = [C.cparam|$tyqual:varying $ty:ctp * $tyqual:uniform $id:p_name|]
          args = [C.cexp|&$id:(deref_name)[i]|]
      pure (params, args, pre_body, post_body)
    compileOutputsVarying (MemParam name space) = do
      typ <- GC.memToCType name space
      newvn <- newVName $ "aos_" <> baseString name
      let params = [C.cparam|$ty:typ $id:name|]
          args = [C.cexp|&$id:(newvn)[i]|]
          pre_body =
            [C.citems|$tyqual:uniform $ty:typ $id:(newvn)[programCount];
                       $id:(newvn)[programIndex] = $id:name;|]
      pure (params, args, pre_body, [])

-- | Handle logging an error message in ISPC.
handleError :: ErrorMsg Exp -> String -> ISPCCompilerM ()
handleError msg stacktrace = do
  -- Get format sting
  (formatstr, formatargs) <- GC.errorMsgString msg
  let formatstr' = "Error: " <> formatstr <> "\n\nBacktrace:\n%s"
  -- Get args types and names for shim
  let arg_types = errorMsgArgTypes msg
  arg_names <- mapM (newVName . const "arg") arg_types
  let params = zipWith (\ty name -> [C.cparam|$ty:(GC.primTypeToCType ty) $id:name|]) arg_types arg_names
  let params_uni = zipWith (\ty name -> [C.cparam|$tyqual:uniform $ty:(GC.primTypeToCType ty) $id:name|]) arg_types arg_names
  -- Make shim
  let formatargs' = mapArgNames msg formatargs arg_names
  shim <- MC.multicoreDef "assert_shim" $ \s -> do
    pure
      [C.cedecl|void $id:s(struct futhark_context* ctx, $params:params) {
          set_error(ctx, msgprintf($string:formatstr', $args:formatargs', $string:stacktrace));
      }|]
  ispcDecl
    [C.cedecl|extern "C" $tyqual:unmasked void $id:shim($tyqual:uniform struct futhark_context* $tyqual:uniform, $params:params_uni);|]
  -- Call the shim
  args <- getErrorValExps msg
  uni <- newVName "uni"
  let args' = map (\x -> [C.cexp|extract($exp:x, $id:uni)|]) args
  GC.items
    [C.citems|
      $escstm:("foreach_active(" <> prettyString uni <> ")")
      {
        $id:shim(ctx, $args:args');
        err = FUTHARK_PROGRAM_ERROR;
      }
      $escstm:("unmasked { return err; }")|]
  where
    getErrorVal (ErrorString _) = Nothing
    getErrorVal (ErrorVal _ v) = Just v

    getErrorValExps (ErrorMsg m) = mapM compileExp $ mapMaybe getErrorVal m

    mapArgNames' (x : xs) (y : ys) (t : ts)
      | isJust $ getErrorVal x = [C.cexp|$id:t|] : mapArgNames' xs ys ts
      | otherwise = y : mapArgNames' xs ys (t : ts)
    mapArgNames' _ ys [] = ys
    mapArgNames' _ _ _ = []

    mapArgNames (ErrorMsg parts) = mapArgNames' parts

-- | Given the name and type of a parameter, return the C type used to
-- represent it. We use uniform pointers to varying values for lexical
-- memory blocks, as this generally results in less gathers/scatters.
getMemType :: VName -> PrimType -> ISPCCompilerM C.Type
getMemType dest elemtype = do
  cached <- isJust <$> GC.cacheMem dest
  if cached
    then pure [C.cty|$tyqual:varying $ty:(primStorageType elemtype)* uniform|]
    else pure [C.cty|$ty:(primStorageType elemtype)*|]

compileExp :: Exp -> ISPCCompilerM C.Exp
compileExp e@(ValueExp (FloatValue (Float64Value v))) =
  if isInfinite v || isNaN v
    then GC.compileExp e
    else pure [C.cexp|$esc:(prettyString v <> "d")|]
compileExp e@(ValueExp (FloatValue (Float16Value v))) =
  if isInfinite v || isNaN v
    then GC.compileExp e
    else pure [C.cexp|$esc:(prettyString v <> "f16")|]
compileExp (ValueExp val) =
  pure $ C.toExp val mempty
compileExp (LeafExp v _) =
  pure [C.cexp|$id:v|]
compileExp (UnOpExp Complement {} x) = do
  x' <- compileExp x
  pure [C.cexp|~$exp:x'|]
compileExp (UnOpExp Not {} x) = do
  x' <- compileExp x
  pure [C.cexp|!$exp:x'|]
compileExp (UnOpExp (FAbs Float32) x) = do
  x' <- compileExp x
  pure [C.cexp|(float)fabs($exp:x')|]
compileExp (UnOpExp (FAbs Float64) x) = do
  x' <- compileExp x
  pure [C.cexp|fabs($exp:x')|]
compileExp (UnOpExp SSignum {} x) = do
  x' <- compileExp x
  pure [C.cexp|($exp:x' > 0 ? 1 : 0) - ($exp:x' < 0 ? 1 : 0)|]
compileExp (UnOpExp USignum {} x) = do
  x' <- compileExp x
  pure [C.cexp|($exp:x' > 0 ? 1 : 0) - ($exp:x' < 0 ? 1 : 0) != 0|]
compileExp (UnOpExp op x) = do
  x' <- compileExp x
  pure [C.cexp|$id:(prettyString op)($exp:x')|]
compileExp (CmpOpExp cmp x y) = do
  x' <- compileExp x
  y' <- compileExp y
  pure $ case cmp of
    CmpEq {} -> [C.cexp|$exp:x' == $exp:y'|]
    FCmpLt {} -> [C.cexp|$exp:x' < $exp:y'|]
    FCmpLe {} -> [C.cexp|$exp:x' <= $exp:y'|]
    CmpLlt {} -> [C.cexp|$exp:x' < $exp:y'|]
    CmpLle {} -> [C.cexp|$exp:x' <= $exp:y'|]
    _ -> [C.cexp|$id:(prettyString cmp)($exp:x', $exp:y')|]
compileExp (ConvOpExp conv x) = do
  x' <- compileExp x
  pure [C.cexp|$id:(prettyString conv)($exp:x')|]
compileExp (BinOpExp bop x y) = do
  x' <- compileExp x
  y' <- compileExp y
  pure $ case bop of
    Add _ OverflowUndef -> [C.cexp|$exp:x' + $exp:y'|]
    Sub _ OverflowUndef -> [C.cexp|$exp:x' - $exp:y'|]
    Mul _ OverflowUndef -> [C.cexp|$exp:x' * $exp:y'|]
    FAdd {} -> [C.cexp|$exp:x' + $exp:y'|]
    FSub {} -> [C.cexp|$exp:x' - $exp:y'|]
    FMul {} -> [C.cexp|$exp:x' * $exp:y'|]
    FDiv {} -> [C.cexp|$exp:x' / $exp:y'|]
    Xor {} -> [C.cexp|$exp:x' ^ $exp:y'|]
    And {} -> [C.cexp|$exp:x' & $exp:y'|]
    Or {} -> [C.cexp|$exp:x' | $exp:y'|]
    LogAnd {} -> [C.cexp|$exp:x' && $exp:y'|]
    LogOr {} -> [C.cexp|$exp:x' || $exp:y'|]
    _ -> [C.cexp|$id:(prettyString bop)($exp:x', $exp:y')|]
compileExp (FunExp h args _) = do
  args' <- mapM compileExp args
  pure [C.cexp|$id:(funName (nameFromString h))($args:args')|]

-- | Compile a block of code with ISPC specific semantics, falling back
-- to generic C when this semantics is not needed.
-- All recursive constructors are duplicated here, since not doing so
-- would cause use to enter regular generic C codegen with no escape.
compileCode :: MCCode -> ISPCCompilerM ()
compileCode (Comment s code) = do
  xs <- GC.collect $ compileCode code
  let comment = "// " ++ T.unpack s
  GC.stm
    [C.cstm|$comment:comment
              { $items:xs }
             |]
compileCode (DeclareScalar name _ t) = do
  let ct = GC.primTypeToCType t
  quals <- getVariabilityQuals name
  GC.decl [C.cdecl|$tyquals:quals $ty:ct $id:name;|]
compileCode (DeclareArray name t vs) = do
  name_realtype <- newVName $ baseString name ++ "_realtype"
  let ct = GC.primTypeToCType t
  case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- vs']
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs')] = {$inits:vs''};|]
    ArrayZeros n ->
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
  -- Make an exported C shim to access a faked memory block.
  shim <- MC.multicoreDef "get_static_array_shim" $ \f ->
    pure
      [C.cedecl|struct memblock $id:f(struct futhark_context* ctx) {
                  return (struct memblock){NULL,(unsigned char*)$id:name_realtype,0};
                }|]
  ispcDecl
    [C.cedecl|extern "C" $tyqual:unmasked $tyqual:uniform struct memblock $tyqual:uniform
                        $id:shim($tyqual:uniform struct futhark_context* $tyqual:uniform ctx);|]
  -- Call it
  GC.item [C.citem|$tyqual:uniform struct memblock $id:name = $id:shim(ctx);|]
compileCode (c1 :>>: c2) = go (GC.linearCode (c1 :>>: c2))
  where
    go (DeclareScalar name _ t : SetScalar dest e : code)
      | name == dest = do
          let ct = GC.primTypeToCType t
          e' <- compileExp e
          quals <- getVariabilityQuals name
          GC.item [C.citem|$tyquals:quals $ty:ct $id:name = $exp:e';|]
          go code
    go (x : xs) = compileCode x >> go xs
    go [] = pure ()
compileCode (Allocate name (Count (TPrimExp e)) space) = do
  size <- compileExp e
  cached <- GC.cacheMem name
  case cached of
    Just cur_size ->
      GC.stm
        [C.cstm|if ($exp:cur_size < $exp:size) {
                  err = lexical_realloc(ctx, &$exp:name, &$exp:cur_size, $exp:size);
                  if (err != FUTHARK_SUCCESS) {
                    $escstm:("unmasked { return err; }")
                  }
                }|]
    _ ->
      allocMem name size space [C.cstm|$escstm:("unmasked { return 1; }")|]
compileCode (SetMem dest src space) =
  setMem dest src space
compileCode (Write dest (Count idx) elemtype DefaultSpace _ elemexp)
  | isConstExp (untyped idx) = do
      dest' <- GC.rawMem dest
      idxexp <- compileExp $ constFoldPrimExp $ untyped idx
      deref <-
        GC.derefPointer
          dest'
          [C.cexp|($tyquals:([varying]) typename int64_t)$exp:idxexp|]
          <$> getMemType dest elemtype
      elemexp' <- toStorage elemtype <$> compileExp elemexp
      GC.stm [C.cstm|$exp:deref = $exp:elemexp';|]
  | otherwise = do
      dest' <- GC.rawMem dest
      idxexp <- compileExp $ untyped idx
      deref <-
        GC.derefPointer
          dest'
          [C.cexp|($tyquals:([varying]) typename int64_t)$exp:idxexp|]
          <$> getMemType dest elemtype
      elemexp' <- toStorage elemtype <$> compileExp elemexp
      GC.stm [C.cstm|$exp:deref = $exp:elemexp';|]
  where
    isConstExp = isSimple . constFoldPrimExp
    isSimple (ValueExp _) = True
    isSimple _ = False
compileCode (Read x src (Count iexp) restype DefaultSpace _) = do
  src' <- GC.rawMem src
  e <-
    fmap (fromStorage restype) $
      GC.derefPointer src'
        <$> compileExp (untyped iexp)
        <*> getMemType src restype
  GC.stm [C.cstm|$id:x = $exp:e;|]
compileCode (Copy t shape (dst, DefaultSpace) dst_lmad (src, DefaultSpace) src_lmad) = do
  dst' <- GC.rawMem dst
  src' <- GC.rawMem src
  let doWrite dst_i ve = do
        deref <-
          GC.derefPointer
            dst'
            [C.cexp|($tyquals:([varying]) typename int64_t)$exp:dst_i|]
            <$> getMemType dst t
        GC.stm [C.cstm|$exp:deref = $exp:(toStorage t ve);|]
      doRead src_i =
        fromStorage t . GC.derefPointer src' src_i <$> getMemType src t
  GC.compileCopyWith shape doWrite dst_lmad doRead src_lmad
compileCode (Free name space) = do
  cached <- isJust <$> GC.cacheMem name
  unless cached $ unRefMem name space
compileCode (For i bound body)
  -- The special-case here is to avoid certain pathological/contrived
  -- programs that construct statically known zero-element arrays.
  -- Due to the way we do constant-fold index functions, this produces
  -- code that looks like it has uniform/varying mismatches (i.e. race
  -- conditions) to ISPC, even though that code is never actually run.
  | isZero bound = pure ()
  | otherwise = do
      let i' = C.toIdent i
          t = GC.primTypeToCType $ primExpType bound
      bound' <- compileExp bound
      body' <- GC.collect $ compileCode body
      quals <- getVariabilityQuals i
      GC.stm
        [C.cstm|for ($tyquals:quals $ty:t $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
  where
    isZero (ValueExp v) = zeroIsh v
    isZero _ = False
compileCode (While cond body) = do
  cond' <- compileExp $ untyped cond
  body' <- GC.collect $ compileCode body
  GC.stm
    [C.cstm|while ($exp:cond') {
            $items:body'
          }|]
compileCode (If cond tbranch fbranch) = do
  cond' <- compileExp $ untyped cond
  tbranch' <- GC.collect $ compileCode tbranch
  fbranch' <- GC.collect $ compileCode fbranch
  GC.stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCode (Call dests fname args) = do
  (dests', unpack_dest) <- mapAndUnzipM GC.compileDest dests
  defCallIspc dests' fname =<< mapM GC.compileArg args
  GC.stms $ mconcat unpack_dest
  where
    defCallIspc dests' fname' args' = do
      let out_args = [[C.cexp|&$id:d|] | d <- dests']
          args''
            | isBuiltInFunction fname' = args'
            | otherwise = [C.cexp|ctx|] : out_args ++ args'
      case dests' of
        [d]
          | isBuiltInFunction fname' ->
              GC.stm [C.cstm|$id:d = $id:(funName fname')($args:args'');|]
        _ ->
          GC.item
            [C.citem|
            if ($id:(funName fname')($args:args'') != 0) {
              $escstm:("unmasked { return 1; }")
            }|]
compileCode (Assert e msg (loc, locs)) = do
  e' <- compileExp e
  err <- GC.collect $ handleError msg stacktrace
  GC.stm [C.cstm|if (!$exp:e') { $items:err }|]
  where
    stacktrace = T.unpack $ prettyStacktrace 0 $ map locText $ loc : locs
compileCode code =
  GC.compileCode code

-- | Prepare a struct with memory allocted in the scope and populate
-- its fields with values
prepareMemStruct :: [(VName, VName)] -> [VName] -> ISPCCompilerM Name
prepareMemStruct lexmems fatmems = do
  let lex_defs = concatMap lexMemDef lexmems
  let fat_defs = map fatMemDef fatmems
  name <- ispcDef "mem_struct" $ \s -> do
    pure
      [C.cedecl|struct $id:s {
        $sdecls:lex_defs
        $sdecls:fat_defs
      };|]
  let name' = name <> "_"
  GC.decl [C.cdecl|$tyqual:uniform struct $id:name $id:name';|]
  forM_ (concatMap (\(a, b) -> [a, b]) lexmems) $ \m ->
    GC.stm [C.cstm|$id:name'.$id:m = $id:m;|]
  forM_ fatmems $ \m ->
    GC.stm [C.cstm|$id:name'.$id:m = &$id:m;|]
  pure name
  where
    lexMemDef (name, size) =
      [ [C.csdecl|$tyqual:varying unsigned char * $tyqual:uniform $id:name;|],
        [C.csdecl|$tyqual:varying size_t $id:size;|]
      ]
    fatMemDef name =
      [C.csdecl|$tyqual:varying struct memblock * $tyqual:uniform $id:name;|]

-- | Get memory from the memory struct into local variables
compileGetMemStructVals :: Name -> [(VName, VName)] -> [VName] -> ISPCCompilerM ()
compileGetMemStructVals struct lexmems fatmems = do
  forM_ fatmems $ \m ->
    GC.decl [C.cdecl|struct memblock $id:m = *$id:struct->$id:m;|]
  forM_ lexmems $ \(m, s) -> do
    GC.decl [C.cdecl|$tyqual:varying unsigned char * $tyqual:uniform $id:m = $id:struct->$id:m;|]
    GC.decl [C.cdecl|size_t $id:s = $id:struct->$id:s;|]

-- | Write back potentially changed memory addresses and sizes to the memory struct
compileWritebackMemStructVals :: Name -> [(VName, VName)] -> [VName] -> ISPCCompilerM ()
compileWritebackMemStructVals struct lexmems fatmems = do
  forM_ fatmems $ \m ->
    GC.stm [C.cstm|*$id:struct->$id:m = $id:m;|]
  forM_ lexmems $ \(m, s) -> do
    GC.stm [C.cstm|$id:struct->$id:m = $id:m;|]
    GC.stm [C.cstm|$id:struct->$id:s = $id:s;|]

-- | Read back potentially changed memory addresses and sizes to the memory struct into local variables
compileReadbackMemStructVals :: Name -> [(VName, VName)] -> [VName] -> ISPCCompilerM ()
compileReadbackMemStructVals struct lexmems fatmems = do
  forM_ fatmems $ \m ->
    GC.stm [C.cstm|$id:m = *$id:struct.$id:m;|]
  forM_ lexmems $ \(m, s) -> do
    GC.stm [C.cstm|$id:m = $id:struct.$id:m;|]
    GC.stm [C.cstm|$id:s = $id:struct.$id:s;|]

compileGetStructVals ::
  Name ->
  [VName] ->
  [(C.Type, MC.ValueType)] ->
  ISPCCompilerM [C.BlockItem]
compileGetStructVals struct a b = concat <$> zipWithM field a b
  where
    struct' = struct <> "_"
    field name (ty, MC.Prim pt) = do
      let inner = [C.cexp|$id:struct'->$id:(MC.closureFreeStructField name)|]
      pure [C.citems|$tyqual:uniform $ty:ty $id:name = $exp:(fromStorage pt inner);|]
    field name (_, _) = do
      strlit <- makeStringLiteral $ prettyString name
      pure
        [C.citems|$tyqual:uniform struct memblock $id:name;
                     $id:name.desc = $id:strlit();
                     $id:name.mem = $id:struct'->$id:(MC.closureFreeStructField name);
                     $id:name.size = 0;
                     $id:name.references = NULL;|]

-- | Can the given code produce an error? If so, we can't use foreach
-- loops, since they don't allow for early-outs in error handling.
mayProduceError :: MCCode -> Bool
mayProduceError (x :>>: y) = mayProduceError x || mayProduceError y
mayProduceError (If _ x y) = mayProduceError x || mayProduceError y
mayProduceError (For _ _ x) = mayProduceError x
mayProduceError (While _ x) = mayProduceError x
mayProduceError (Comment _ x) = mayProduceError x
mayProduceError (Op (ForEachActive _ body)) = mayProduceError body
mayProduceError (Op (ForEach _ _ _ body)) = mayProduceError body
mayProduceError (Op SegOp {}) = True
mayProduceError Allocate {} = True
mayProduceError Assert {} = True
mayProduceError SetMem {} = True
mayProduceError Free {} = True
mayProduceError Call {} = True
mayProduceError _ = False

-- Generate a segop function for top_level and potentially nested SegOp code
compileOp :: GC.OpCompiler Multicore ISPCState
compileOp (SegOp name params seq_task par_task retvals (SchedulerInfo e sched)) = do
  let (ParallelTask seq_code) = seq_task
  free_ctypes <- mapM MC.paramToCType params
  retval_ctypes <- mapM MC.paramToCType retvals
  let free_args = map paramName params
      retval_args = map paramName retvals
      free = zip free_args free_ctypes
      retval = zip retval_args retval_ctypes

  e' <- compileExp e

  let lexical = lexicalMemoryUsageMC OpaqueKernels $ Function Nothing [] params seq_code

  fstruct <-
    MC.prepareTaskStruct sharedDef "task" free_args free_ctypes retval_args retval_ctypes

  fpar_task <- MC.generateParLoopFn lexical (name ++ "_task") seq_code fstruct free retval
  MC.addTimingFields fpar_task

  let ftask_name = fstruct <> "_task"

  to_c <- GC.collect $ do
    GC.decl [C.cdecl|struct scheduler_segop $id:ftask_name;|]
    GC.stm [C.cstm|$id:ftask_name.args = args;|]
    GC.stm [C.cstm|$id:ftask_name.top_level_fn = $id:fpar_task;|]
    GC.stm [C.cstm|$id:ftask_name.name = $string:(nameToString fpar_task);|]
    GC.stm [C.cstm|$id:ftask_name.iterations = iterations;|]
    -- Create the timing fields for the task
    GC.stm [C.cstm|$id:ftask_name.task_time = &ctx->program->$id:(MC.functionTiming fpar_task);|]
    GC.stm [C.cstm|$id:ftask_name.task_iter = &ctx->program->$id:(MC.functionIterations fpar_task);|]

    case sched of
      Dynamic -> GC.stm [C.cstm|$id:ftask_name.sched = DYNAMIC;|]
      Static -> GC.stm [C.cstm|$id:ftask_name.sched = STATIC;|]

    -- Generate the nested segop function if available
    case par_task of
      Just (ParallelTask nested_code) -> do
        let lexical_nested = lexicalMemoryUsageMC OpaqueKernels $ Function Nothing [] params nested_code
        fnpar_task <- MC.generateParLoopFn lexical_nested (name ++ "_nested_task") nested_code fstruct free retval
        GC.stm [C.cstm|$id:ftask_name.nested_fn = $id:fnpar_task;|]
      Nothing ->
        GC.stm [C.cstm|$id:ftask_name.nested_fn=NULL;|]

    GC.stm [C.cstm|return scheduler_prepare_task(&ctx->scheduler, &$id:ftask_name);|]

  schedn <- MC.multicoreDef "schedule_shim" $ \s ->
    pure
      [C.cedecl|int $id:s(struct futhark_context* ctx, void* args, typename int64_t iterations) {
        $items:to_c
    }|]

  ispcDecl
    [C.cedecl|extern "C" $tyqual:unmasked $tyqual:uniform int $id:schedn
                        (struct futhark_context $tyqual:uniform * $tyqual:uniform ctx,
                        struct $id:fstruct $tyqual:uniform * $tyqual:uniform args,
                        $tyqual:uniform int iterations);|]

  aos_name <- newVName "aos"
  GC.items
    [C.citems|
    $escstm:("#if ISPC")
    $tyqual:uniform struct $id:fstruct $id:aos_name[programCount];
    $id:aos_name[programIndex] = $id:(fstruct <> "_");
    $escstm:("foreach_active (i)")
    {
      if (err == 0) {
        err = $id:schedn(ctx, &$id:aos_name[i], extract($exp:e', i));
      }
    }
    if (err != 0) {
      $escstm:("unmasked { return err; }")
    }
    $escstm:("#else")
    err = $id:schedn(ctx, &$id:(fstruct <> "_"), $exp:e');
    if (err != 0) {
      goto cleanup;
    }
    $escstm:("#endif")|]
compileOp (ISPCKernel body free) = do
  free_ctypes <- mapM MC.paramToCType free
  let free_args = map paramName free

  let lexical = lexicalMemoryUsageMC OpaqueKernels $ Function Nothing [] free body
  -- Generate ISPC kernel
  fstruct <- MC.prepareTaskStruct sharedDef "param_struct" free_args free_ctypes [] []
  let fstruct' = fstruct <> "_"

  ispcShim <- ispcDef "loop_ispc" $ \s -> do
    mainBody <- GC.inNewFunction $
      analyzeVariability body $
        cachingMemory lexical $ \decl_cached free_cached lexmems ->
          GC.collect $ do
            GC.decl [C.cdecl|$tyqual:uniform struct futhark_context * $tyqual:uniform ctx = $id:fstruct'->ctx;|]
            GC.items =<< compileGetStructVals fstruct free_args free_ctypes
            body' <- GC.collect $ compileCode body
            mapM_ GC.item decl_cached
            mapM_ GC.item =<< GC.declAllocatedMem

            -- Make inner kernel for error handling, if needed
            if mayProduceError body
              then do
                fatmems <- gets (map fst . GC.compDeclaredMem)
                mstruct <- prepareMemStruct lexmems fatmems
                let mstruct' = mstruct <> "_"
                innerShim <- ispcDef "inner_ispc" $ \t -> do
                  innerBody <- GC.collect $ do
                    GC.decl [C.cdecl|$tyqual:uniform struct futhark_context * $tyqual:uniform ctx = $id:fstruct'->ctx;|]
                    GC.items =<< compileGetStructVals fstruct free_args free_ctypes
                    compileGetMemStructVals mstruct' lexmems fatmems
                    GC.decl [C.cdecl|$tyqual:uniform int err = 0;|]
                    mapM_ GC.item body'
                    compileWritebackMemStructVals mstruct' lexmems fatmems
                    GC.stm [C.cstm|return err;|]
                  pure
                    [C.cedecl|
                static $tyqual:unmasked inline $tyqual:uniform int $id:t(
                  $tyqual:uniform typename int64_t start,
                  $tyqual:uniform typename int64_t end,
                  struct $id:fstruct $tyqual:uniform * $tyqual:uniform $id:fstruct',
                  struct $id:mstruct $tyqual:uniform * $tyqual:uniform $id:mstruct') {
                  $items:innerBody
                }|]
                -- Call the kernel and read back potentially changed memory
                GC.decl [C.cdecl|$tyqual:uniform int err = $id:innerShim(start, end, $id:fstruct', &$id:mstruct');|]
                compileReadbackMemStructVals mstruct' lexmems fatmems
              else do
                GC.decl [C.cdecl|$tyqual:uniform int err = 0;|]
                mapM_ GC.item body'

            free_mem <- freeAllocatedMem
            GC.stm [C.cstm|cleanup: {$stms:free_cached $items:free_mem}|]
            GC.stm [C.cstm|return err;|]
    GC.earlyDecl
      [C.cedecl|int $id:s(typename int64_t start,
                                  typename int64_t end,
                                  struct $id:fstruct * $id:fstruct');|]
    pure
      [C.cedecl|
        $tyqual:export $tyqual:uniform int $id:s($tyqual:uniform typename int64_t start,
                                                 $tyqual:uniform typename int64_t end,
                                                 struct $id:fstruct $tyqual:uniform * $tyqual:uniform $id:fstruct' ) {
          $items:mainBody
        }|]

  -- Generate C code to call into ISPC kernel
  GC.items
    [C.citems|
    err = $id:ispcShim(start, end, & $id:fstruct');
    if (err != 0) {
      goto cleanup;
    }|]
compileOp (ForEach i from bound body) = do
  from' <- compileExp from
  bound' <- compileExp bound
  body' <- GC.collect $ compileCode body
  if mayProduceError body
    then
      GC.stms
        [C.cstms|
      for ($tyqual:uniform typename int64_t i = 0; i < (($exp:bound' - $exp:from') / programCount); i++) {
        typename int64_t $id:i = $exp:from' + programIndex + i * programCount;
        $items:body'
      }
      if (programIndex < (($exp:bound' - $exp:from') % programCount)) {
        typename int64_t $id:i = $exp:from' + programIndex + ((($exp:bound' - $exp:from') / programCount) * programCount);
        $items:body'
      }|]
    else
      GC.stms
        [C.cstms|
      $escstm:(T.unpack ("foreach (" <> prettyText i <> " = " <> expText from' <> " ... " <> expText bound' <> ")")) {
        $items:body'
      }|]
compileOp (ForEachActive name body) = do
  body' <- GC.collect $ compileCode body
  GC.stms
    [C.cstms|
    for ($tyqual:uniform unsigned int $id:name = 0; $id:name < programCount; $id:name++) {
      if (programIndex == $id:name) {
        $items:body'
      }
    }|]
compileOp (ExtractLane dest (ValueExp v) _) =
  -- extract() on constants is not allowed (type is uniform, not
  -- varying), so just turn them into an assignment.
  GC.stm [C.cstm|$id:dest = $exp:v;|]
compileOp (ExtractLane dest tar lane) = do
  tar' <- compileExp tar
  lane' <- compileExp lane
  GC.stm [C.cstm|$id:dest = extract($exp:tar', $exp:lane');|]
compileOp (Atomic aop) =
  MC.atomicOps aop $ \ty arr -> do
    cached <- isJust <$> GC.cacheMem arr
    if cached
      then pure [C.cty|$tyqual:varying $ty:ty* $tyqual:uniform|]
      else pure [C.cty|$ty:ty*|]
compileOp op = MC.compileOp op

-- | Like @GenericC.cachingMemory@, but adapted for ISPC codegen.
cachingMemory ::
  M.Map VName Space ->
  ([C.BlockItem] -> [C.Stm] -> [(VName, VName)] -> GC.CompilerM op s a) ->
  GC.CompilerM op s a
cachingMemory lexical f = do
  let cached = M.keys $ M.filter (== DefaultSpace) lexical

  cached' <- forM cached $ \mem -> do
    size <- newVName $ prettyString mem <> "_cached_size"
    pure (mem, size)

  let lexMem env =
        env
          { GC.envCachedMem =
              M.fromList (map (first (`C.toExp` noLoc)) cached')
                <> GC.envCachedMem env
          }

      declCached (mem, size) =
        [ [C.citem|size_t $id:size = 0;|],
          [C.citem|$tyqual:varying unsigned char * $tyqual:uniform $id:mem = NULL;|]
        ]

      freeCached (mem, _) =
        [C.cstm|free($id:mem);|]

  local lexMem $ f (concatMap declCached cached') (map freeCached cached') cached'

-- Variability analysis
type Dependencies = M.Map VName Names

data Variability = Uniform | Varying
  deriving (Eq, Ord, Show)

newtype VariabilityM a
  = VariabilityM (ReaderT Names (State Dependencies) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Dependencies,
      MonadReader Names
    )

execVariabilityM :: VariabilityM a -> Dependencies
execVariabilityM (VariabilityM m) =
  execState (runReaderT m mempty) mempty

-- | Extend the set of dependencies with a new one
addDeps :: VName -> Names -> VariabilityM ()
addDeps v ns = do
  deps <- get
  env <- ask
  case M.lookup v deps of
    Nothing -> put $ M.insert v (ns <> env) deps
    Just ns' -> put $ M.insert v (ns <> ns') deps

-- | Find all the dependencies in a body of code
findDeps :: MCCode -> VariabilityM ()
findDeps (x :>>: y) = do
  findDeps x
  findDeps y
findDeps (If cond x y) =
  local (<> freeIn cond) $ do
    findDeps x
    findDeps y
findDeps (For idx bound x) = do
  addDeps idx free
  local (<> free) $ findDeps x
  where
    free = freeIn bound
findDeps (While cond x) = do
  local (<> freeIn cond) $ findDeps x
findDeps (Comment _ x) =
  findDeps x
findDeps (Op (SegOp _ free _ _ retvals _)) =
  mapM_
    ( \x ->
        addDeps (paramName x) $
          namesFromList $
            map paramName free
    )
    retvals
findDeps (Op (ForEach _ _ _ body)) =
  findDeps body
findDeps (Op (ForEachActive _ body)) =
  findDeps body
findDeps (SetScalar name e) =
  addDeps name $ freeIn e
findDeps (Call tars _ args) =
  mapM_ (\x -> addDeps x $ freeIn args) tars
findDeps (Read x arr (Count iexp) _ DefaultSpace _) = do
  addDeps x $ freeIn (untyped iexp)
  addDeps x $ oneName arr
findDeps (Op (GetLoopBounds x y)) = do
  addDeps x mempty
  addDeps y mempty
findDeps (Op (ExtractLane x _ _)) = do
  addDeps x mempty
findDeps (Op (Atomic (AtomicCmpXchg _ old arr ind res val))) = do
  addDeps res $ freeIn arr <> freeIn ind <> freeIn val
  addDeps old $ freeIn arr <> freeIn ind <> freeIn val
findDeps _ = pure ()

-- | Take a list of dependencies and iterate them to a fixed point.
depsFixedPoint :: Dependencies -> Dependencies
depsFixedPoint deps =
  if deps == deps'
    then deps
    else depsFixedPoint deps'
  where
    grow names =
      names <> foldMap (\n -> M.findWithDefault mempty n deps) (namesIntMap names)
    deps' = M.map grow deps

-- | Find roots of variance. These are memory blocks declared in
-- the current scope as well as loop indices of foreach loops.
findVarying :: MCCode -> [VName]
findVarying (x :>>: y) = findVarying x ++ findVarying y
findVarying (If _ x y) = findVarying x ++ findVarying y
findVarying (For _ _ x) = findVarying x
findVarying (While _ x) = findVarying x
findVarying (Comment _ x) = findVarying x
findVarying (Op (ForEachActive _ body)) = findVarying body
findVarying (Op (ForEach idx _ _ body)) = idx : findVarying body
findVarying (DeclareMem mem _) = [mem]
findVarying _ = []

-- | Analyze variability in a body of code and run an action with
-- info about that variability in the compiler state.
analyzeVariability :: MCCode -> ISPCCompilerM a -> ISPCCompilerM a
analyzeVariability code m = do
  let roots = findVarying code
  let deps = depsFixedPoint $ execVariabilityM $ findDeps code
  let safelist = M.filter (\b -> all (`notNameIn` b) roots) deps
  let safe = namesFromList $ M.keys safelist
  pre_state <- GC.getUserState
  GC.modifyUserState (\s -> s {sUniform = safe})
  a <- m
  GC.modifyUserState (\s -> s {sUniform = sUniform pre_state})
  pure a

-- | Get the variability of a variable
getVariability :: VName -> ISPCCompilerM Variability
getVariability name = do
  uniforms <- sUniform <$> GC.getUserState
  pure $
    if name `nameIn` uniforms
      then Uniform
      else Varying

-- | Get the variability qualifiers of a variable
getVariabilityQuals :: VName -> ISPCCompilerM [C.TypeQual]
getVariabilityQuals name = variQuals <$> getVariability name
  where
    variQuals Uniform = [C.ctyquals|$tyqual:uniform|]
    variQuals Varying = []
