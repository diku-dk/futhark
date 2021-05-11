{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

--

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.  This C program is expected to
-- be converted to WebAssembly, so we also produce the intended
-- JavaScript wrapper.
module Futhark.CodeGen.Backends.MulticoreWASM
  ( compileProg,
    runServer,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

--import Language.Futhark.Core (nameToString) -- TODO (see if might be useful later)

import Data.FileEmbed
import Data.List (intercalate, nub)
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.Backends.MulticoreC as MC
import Futhark.CodeGen.Backends.SequentialC.Boilerplate
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGen
import Futhark.IR.Primitive
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import NeatInterpolation (text)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

-- | Compile the program to sequential C with a JavaScript wrapper.
--compileProg :: MonadFreshNames m => Prog MCMem -> m (ImpGen.Warnings, (GC.CParts, String, [String]))
--compileProg prog = do
--  (ws, prog') <- MC.compileProg prog
--
--  prog'' <-
--    GC.compileProg
--      "multicore-wasm"
--      operations
--      generateBoilerplate
--      ""
--      [DefaultSpace]
--      []
--      prog'
--  pure (ws, (prog'', javascriptWrapper (fRepMyRep prog'), emccExportNames (fRepMyRep prog')))
--  where
--    -- pure (ws, (prog'', undefined))
--
--    operations :: GC.Operations Imp.Multicore ()
--    operations =
--      GC.defaultOperations
--        { GC.opsCompiler = const $ return ()
--        }
--

compileProg :: MonadFreshNames m => Prog MCMem -> m (ImpGen.Warnings, (GC.CParts, String, [String]))
compileProg prog = do
  (ws, prog') <- ImpGen.compileProg prog

  prog'' <-
    GC.compileProg
      "wasm-multicore"
      MC.operations
      generateContext
      ""
      [DefaultSpace]
      MC.cliOptions
      prog'
    
  pure (ws, (prog'', javascriptWrapper (fRepMyRep prog'), emccExportNames (fRepMyRep prog')))
  where
    generateContext = do
      let scheduler_h = $(embedStringFile "rts/c/scheduler.h")
      mapM_ GC.earlyDecl [C.cunit|$esc:scheduler_h|]

      cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:s;|],
          [C.cedecl|struct $id:s { int debugging;
                                   int profiling;
                                   int num_threads;
                                 };|]
        )

      GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:cfg* $id:s(void);|],
          [C.cedecl|struct $id:cfg* $id:s(void) {
                                 struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                                 if (cfg == NULL) {
                                   return NULL;
                                 }
                                 cfg->debugging = 0;
                                 cfg->profiling = 0;
                                 cfg->num_threads = 0;
                                 return cfg;
                               }|]
        )

      GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
          [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                                 free(cfg);
                               }|]
        )

      GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
          [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                          cfg->debugging = detail;
                        }|]
        )

      GC.publicDef_ "context_config_set_profiling" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
          [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                          cfg->profiling = flag;
                        }|]
        )

      GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
          [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                                 /* Does nothing for this backend. */
                                 (void)cfg; (void)detail;
                               }|]
        )

      GC.publicDef_ "context_config_set_num_threads" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg *cfg, int n);|],
          [C.cedecl|void $id:s(struct $id:cfg *cfg, int n) {
                                 cfg->num_threads = n;
                               }|]
        )

      (fields, init_fields) <- GC.contextContents

      ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:s;|],
          [C.cedecl|struct $id:s {
                          struct scheduler scheduler;
                          int detail_memory;
                          int debugging;
                          int profiling;
                          int profiling_paused;
                          int logging;
                          typename lock_t lock;
                          char *error;
                          typename FILE *log;
                          int total_runs;
                          long int total_runtime;
                          $sdecls:fields

                          // Tuning parameters
                          typename int64_t tuning_timing;
                          typename int64_t tuning_iter;
                        };|]
        )

      GC.publicDef_ "context_new" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
          [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }

                 // Initialize rand()
                 fast_srand(time(0));
                 ctx->detail_memory = cfg->debugging;
                 ctx->debugging = cfg->debugging;
                 ctx->profiling = cfg->profiling;
                 ctx->profiling_paused = 0;
                 ctx->logging = 0;
                 ctx->error = NULL;
                 ctx->log = stderr;
                 create_lock(&ctx->lock);

                 int tune_kappa = 0;
                 double kappa = 5.1f * 1000;

                 if (tune_kappa) {
                   if (determine_kappa(&kappa) != 0) {
                     return NULL;
                   }
                 }

                 if (scheduler_init(&ctx->scheduler,
                                    cfg->num_threads > 0 ?
                                    cfg->num_threads : num_processors(),
                                    kappa) != 0) {
                   return NULL;
                 }

                 $stms:init_fields

                 init_constants(ctx);

                 return ctx;
              }|]
        )

      GC.publicDef_ "context_free" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
          [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                 free_constants(ctx);
                 (void)scheduler_destroy(&ctx->scheduler);
                 free_lock(&ctx->lock);
                 free(ctx);
               }|]
        )

      GC.publicDef_ "context_sync" GC.InitDecl $ \s ->
        ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
          [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                                 (void)ctx;
                                 return 0;
                               }|]
        )

      GC.earlyDecl [C.cedecl|static const char *size_names[0];|]
      GC.earlyDecl [C.cedecl|static const char *size_vars[0];|]
      GC.earlyDecl [C.cedecl|static const char *size_classes[0];|]

      GC.publicDef_ "context_config_set_size" GC.InitDecl $ \s ->
        ( [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value);|],
          [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value) {
                         (void)cfg; (void)size_name; (void)size_value;
                         return 1;
                       }|]
        )

fRepMyRep :: Imp.Definitions Imp.Multicore -> [JSEntryPoint]
fRepMyRep prog =
  let Imp.Definitions _ (Imp.Functions fs) = prog
      entries = filter (\(_, Imp.Function isEntry _ _ _ _ _) -> isEntry) fs
      result = map (\(n, Imp.Function _ _ _ _ res args) -> JSEntryPoint {name = nameToString n, parameters = map extToString args, ret = map extToString res}) entries
   in -- TODO take futhark_entry from nameToString n
      result

extToString :: Imp.ExternalValue -> String
extToString (Imp.TransparentValue (Imp.ArrayValue vn _ pt s dimSize)) = concat (replicate (length dimSize) "[]") ++ extToString (Imp.TransparentValue (Imp.ScalarValue pt s vn))
extToString (Imp.TransparentValue (Imp.ScalarValue (FloatType Float32) _ _)) = "f32"
extToString (Imp.TransparentValue (Imp.ScalarValue (FloatType Float64) _ _)) = "f64"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int8) Imp.TypeDirect _)) = "i8"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int16) Imp.TypeDirect _)) = "i16"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int32) Imp.TypeDirect _)) = "i32"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int64) Imp.TypeDirect _)) = "i64"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int8) Imp.TypeUnsigned _)) = "u8"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int16) Imp.TypeUnsigned _)) = "u16"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int32) Imp.TypeUnsigned _)) = "u32"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int64) Imp.TypeUnsigned _)) = "u64"
extToString (Imp.TransparentValue (Imp.ScalarValue Bool _ _)) = "bool"
extToString (Imp.OpaqueValue _ _) = "opaq"
extToString _ = "Not Reached"

-- TODO 32
-- Handle Booleans
-- Hhandle OpaqueTypes

--type EntryPointName = String
type EntryPointTyp = String

data JSEntryPoint = JSEntryPoint
  { name :: String,
    parameters :: [EntryPointTyp],
    ret :: [EntryPointTyp]
  }

jsServer :: String
jsServer = $(embedStringFile "rts/javascript/server.js")

jsValues :: String
jsValues = $(embedStringFile "rts/javascript/values.js")

initFunc :: String
initFunc =
  T.unpack
    [text|
   function initData(data) {
      var nDataBytes = data.length * data.BYTES_PER_ELEMENT;
      var dataPtr = Module._malloc(nDataBytes);
      var dataHeap = new Uint8Array(Module.HEAPU8.buffer, dataPtr, nDataBytes);
      dataHeap.set(new Uint8Array(data.buffer));
      return dataHeap
  }
  |]

-- initDataHeap :: Int -> String -> String
-- initDataHeap idx arrType = "    var dataHeap" ++ show idx ++ " = initData(new " ++ arrType ++ "(1));"

dataHeapConv :: String -> String -> String
dataHeapConv size arrType =
  "    var dataHeapRes = new " ++ arrType ++ "(dataHeap.buffer, res, " ++ size ++ ");"

resDataHeap :: Int -> String -> String
resDataHeap idx arrType =
  "    var res" ++ show idx ++ " = new " ++ arrType ++ "(dataHeap" ++ show idx ++ ".buffer,"
    ++ " dataHeap"
    ++ show idx
    ++ ".byteOffset, 1);"

javascriptWrapper :: [JSEntryPoint] -> String
javascriptWrapper entryPoints =
  unlines
    [ jsServer,
      jsValues,
      cwraps,
      cwrapsJSE entryPoints,
      unlines $ map cwrapEntryPoint entryPoints,
      initFunc,
      ptrFromWrap,
      arrWrapper,
      classDef,
      constructor entryPoints,
      getEntryPointsFun,
      --entryDic entryPoints,
      unlines $ concatMap (\jse -> map toFutharkArray (nub (ret jse ++ parameters jse))) entryPoints,
      unlines $ concatMap (\jse -> map fromFutharkArrayShape (nub (ret jse ++ parameters jse))) entryPoints,
      --unlines $ concatMap (\jse -> map fromFutharkArrayRawValues (ret jse)) entryPoints,
      unlines $ concatMap (\jse -> map fromFutharkArrayValues (nub (ret jse ++ parameters jse))) entryPoints,
      unlines $ map jsWrapEntryPoint entryPoints,
      endClassDef
    ]

--TODO Figure out if this needs arguements
arrWrapper :: String
arrWrapper =
  T.unpack
    [text|
    function padTyp(typ) {
      if (typ.length == 3) {
        return " " + typ;
      } else if (typ.length == 2) {
        return "  " + typ;
      } else {
        return typ;
      }
    }

    class ArrayWrapper {
      // array has one element if scalar
      // else it has 4
      constructor(arr) {
        if (arr.length == 1) {
          this.scalar_constructor(arr[0]);
        } else if (arr.length == 2) {
          this.bool_constructor(arr[0], arr[1]);
        } else {
          this.arr_constructor(arr[0], arr[1], arr[2], arr[3]);
        }
      }


      arr_constructor(fc, ptr, typ, dim) {
        this.dim = dim;
        this.typ = typ;
        this.ptr = ptr;
        this.fc = fc;
        this.arr = this.fc[this.func_name('values')].apply(this.fc, [this.ptr]);
        this.shapey = this.fc[this.func_name('shape')].apply(this.fc, [this.ptr]);
        this.arr_init = false;
        this.shape_init = true;
        this.is_scalar = false;
        this.is_arr = false;
        this.value = this.arr;
      }

      // should only be called with one element array
      scalar_constructor(arr) {
        this.shape_init = true;
        this.shapey = [];
        this.is_scalar = true;
        this.arr = arr;
      }

      bool_constructor(v1, v2) {
        this.typ = "bool";
        this.scalar_constructor(v1);
      }



      func_name(fname) {
        return 'from_futhark_' + fname + '_' + this.typ + '_' + this.dim + 'd_arr';
      }

      shape() {
        if (this.shape_init) {
          return this.shapey;
        } else {
          this.shapey = this.fc[this.func_name('shape')].apply(this.fc, [this.ptr]);
          this.shape_init = true;
          return this.shapey;
       }
     }

      values() {
        return this.arr;
      }

      bytes_per_element() {
        if (this.typ == "bool") {
          return 1;
        } else {
          return this.arr.BYTES_PER_ELEMENT;
        }
      }

      str_type() {
        if (this.typ == "bool") {
          return "bool";
        }
        if (this.is_scalar) {
          return padTyp(type_strs[this.arr.constructor.name]);
        } else {
          return padTyp(this.typ);
        }
      }

    }
  |]

cwraps :: String
cwraps =
  unlines
    [ cwrapFun "futhark_context_config_new" 0,
      cwrapFun "futhark_context_new" 1,
      cwrapFun "futhark_context_sync" 1,
      cwrapFun "futhark_context_get_error" 1
    ]

-- TODO Only wrap functions for first params
cwrapsJSE :: [JSEntryPoint] -> String
cwrapsJSE jses =
  unlines $
    map (\arg -> cwrapFun (gfn "new" arg) (dim arg + 2)) jses'
      ++ map (\arg -> cwrapFun (gfn "shape" arg) (dim arg + 2)) jses'
      ++ map (\arg -> cwrapFun (gfn "values_raw" arg) (dim arg + 2)) jses'
      ++ map (\arg -> cwrapFun (gfn "values" arg) 2) jses'
  where
    jses' = filter (\t -> dim t > 0) $ nub $ concatMap (\jse -> parameters jse ++ ret jse) jses
    gfn typ str = "futhark_" ++ typ ++ "_" ++ baseType str ++ "_" ++ show (dim str) ++ "d"

emccExportNames :: [JSEntryPoint] -> [String]
emccExportNames jses =
  map (\jse -> "'_futhark_entry_" ++ name jse ++ "'") jses
    ++ map (\arg -> "'" ++ gfn "new" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "shape" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "values_raw" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "values" arg ++ "'") jses'
    ++ ["_futhark_context_config_new", "_futhark_context_new", "_futhark_context_sync", "_futhark_context_get_error"]
  where
    jses' = filter (\t -> dim t > 0) $ nub $ concatMap (\jse -> parameters jse ++ ret jse) jses
    gfn typ str = "_futhark_" ++ typ ++ "_" ++ baseType str ++ "_" ++ show (dim str) ++ "d"

cwrapFun :: String -> Int -> String
cwrapFun fname numArgs =
  T.unpack
    [text|
  ${fn} =
  Module.cwrap(
      '${fn}', 'number', [${args}],
    );
  |]
  where
    fn = T.pack fname
    args = T.pack $ intercalate ", " $ replicate numArgs "'number'"

classDef :: String
classDef = "class FutharkContext {"

endClassDef :: String
endClassDef = "}"

constructor :: [JSEntryPoint] -> String
constructor jses =
  T.unpack
    [text|
    constructor() {
      this.cfg = futhark_context_config_new();
      this.ctx = futhark_context_new(this.cfg);
      this.entry_points = {
        ${entries}
      };
    }
  |]
  where
    entries = T.pack $ intercalate "," $ map dicEntry jses

getEntryPointsFun :: String
getEntryPointsFun =
  T.unpack
    [text|
    get_entry_points() {
      return this.entry_points;
    }
  |]

dicEntry :: JSEntryPoint -> String
dicEntry jse =
  T.unpack
    [text|
  '${ename}' : [${params}, ${rets}]
  |]
  where
    ename = T.pack $ name jse
    params = T.pack $ show $ parameters jse
    rets = T.pack $ show $ ret jse

jsWrapEntryPoint :: JSEntryPoint -> String
jsWrapEntryPoint jse =
  unlines
    [ "  " ++ func_name ++ "(" ++ args1 ++ ") {",
      initss,
      paramsToPtr,
      "    if (futhark_entry_" ++ func_name ++ "(this.ctx, " ++ rets ++ ", " ++ args1 ++ ") > 0) {",
      "     var error_ptr = futhark_context_get_error(this.ctx);",
      "     var error_msg = '';",
      "     var next_char;",
      "     for (var i=0; 0 != (next_char=getValue(i + error_ptr, 'i8')); i++) {",
      "       error_msg = error_msg + String.fromCharCode(next_char);",
      "     }",
      "     throw error_msg;",
      "    }",
      results,
      "    futhark_context_sync(this.ctx);",
      "    return [" ++ res ++ "];",
      "  }"
    ]
  where
    func_name = name jse
    alr = [0 .. length (ret jse) - 1]
    alp = [0 .. length (parameters jse) - 1]
    convTypes = map typeConversion $ ret jse
    initss = unlines $ map (\i -> inits i (ret jse !! i)) alr
    results = unlines $ map (\i -> if head (ret jse !! i) == '[' then "" else resDataHeap i (convTypes !! i)) alr
    rets = intercalate ", " [retPtrOrOther i jse "dataHeap" ".byteOffset" ptrRes | i <- alr]
    args1 = intercalate ", " ["in" ++ show i | i <- alp]
    paramsToPtr = unlines ["  in" ++ show i ++ " = ptrFromWrap(in" ++ show i ++ ")" | i <- alp]
    res = intercalate ", " [retPtrOrOtherBool i jse "new ArrayWrapper([res" "])" ptrResValue | i <- alr]
    ptrRes i _ = "res" ++ show i
    ptrResValue i _ =
      "new ArrayWrapper([this, getValue(res" ++ show i ++ ", 'i32'), '"
        ++ baseType (ret jse !! i)
        ++ "', "
        ++ show (dim (ret jse !! i))
        ++ "])"
    retPtrOrOther i jse' pre post f =
      if head (ret jse' !! i) == '['
        then f i $ ret jse' !! i
        else --else "new ArrayWrapper([res" ++ show i ++ "])"
          pre ++ show i ++ post
    retPtrOrOtherBool i jse' pre post f
      | head (ret jse' !! i) == '[' = f i $ ret jse' !! i
      | head (ret jse' !! i) == 'b' = pre ++ show i ++ ", 'bool'" ++ post
      | otherwise = pre ++ show i ++ post

ptrFromWrap :: String
ptrFromWrap =
  T.unpack
    [text|
    function ptrFromWrap(x) {
      if (typeof x == 'number' || typeof x == 'bigint') {
        return x;
      }
      if (x.constructor.name == "ArrayWrapper") {
        return x.ptr;
      }
    }
  |]

cwrapEntryPoint :: JSEntryPoint -> String
cwrapEntryPoint jse =
  T.unpack
    [text|
    futhark_entry_${ename} =
      Module.cwrap(
        'futhark_entry_${ename}', 'number', ${args}
      );
   |]
  where
    ename = T.pack $ name jse
    arg_length = length (parameters jse) + length (ret jse)
    args = T.pack $ "['number'" ++ concat (replicate arg_length ", 'number'") ++ "]"

inits :: Int -> EntryPointTyp -> String
inits argNum ep =
  let (i, typ) = retType ep
   in if i == 0
        then initNotPtr argNum typ
        else makePtr argNum

initNotPtr :: Int -> String -> String
initNotPtr i typ =
  "var dataHeap" ++ show i ++ " = initData(new " ++ typeConversion typ ++ "(1));"

makePtr :: Int -> String
makePtr i = "    var res" ++ show i ++ " = Module._malloc(8);"

retType :: String -> (Integer, String)
retType ('[' : ']' : end) =
  let (val, typ) = retType end
   in (val + 1, typ)
retType typ = (0, typeConversion typ)

baseType :: String -> String
baseType ('[' : ']' : end) = baseType end
baseType typ = typ

dim :: String -> Int
dim ('[' : ']' : end) = dim end + 1
dim _ = 0

jsType :: String -> String
jsType = snd . retType

typeConversion :: String -> String
typeConversion typ =
  case typ of
    "i8" -> "Int8Array"
    "i16" -> "Int16Array"
    "i32" -> "Int32Array"
    "i64" -> "BigInt64Array"
    "u8" -> "Uint8Array"
    "u16" -> "Uint16Array"
    "u32" -> "Uint32Array"
    "u64" -> "BigUint64Array"
    "f32" -> "Float32Array"
    "f64" -> "Float64Array"
    "bool" -> "Int8Array"
    _ -> typ

toFutharkArray :: String -> String
toFutharkArray str =
  if dim str == 0
    then ""
    else
      unlines
        [ "to_futhark_" ++ ftype ++ "_" ++ show i ++ "d_arr(" ++ intercalate ", " args1 ++ ") {",
          -- "  // Possibly do sanity check that dimensions dim0 * dimn == arr.length",
          "  var dataHeap = initData(arr);",
          "  var fut_arr = futhark_new_" ++ ftype ++ "_" ++ show i ++ "d(" ++ intercalate ", " args2 ++ ");",
          "  return fut_arr;",
          "}"
        ]
  where
    ctx = "this.ctx"
    arr = "arr"
    ofs = "dataHeap.byteOffset"
    i = dim str
    dims = map (\j -> "dim" ++ show j) [0 .. i -1]
    --cast = map (\d -> "Number(" ++ d ++ ")") dims
    args1 = arr : dims
    args2 = [ctx, ofs] ++ dims
    ftype = baseType str

fromFutharkArrayShape :: String -> String
fromFutharkArrayShape str =
  if dim str == 0
    then ""
    else
      unlines
        [ "from_futhark_shape_" ++ ftype ++ "_" ++ show i ++ "d_arr(futhark_arr) {",
          "  var ptr = futhark_shape_" ++ ftype ++ "_" ++ show i ++ "d(" ++ intercalate ", " args1 ++ ");",
          "  var dataHeap = new Uint8Array(Module.HEAPU8.buffer, ptr, 8 * " ++ show i ++ ");",
          " var result = new BigUint64Array(dataHeap.buffer, dataHeap.byteOffset, " ++ show i ++ ");",
          " return result;",
          "}"
        ]
  where
    ctx = "this.ctx"
    arr = "futhark_arr"
    args1 = [ctx, arr]
    i = dim str
    ftype = baseType str

fromFutharkArrayValues :: String -> String
fromFutharkArrayValues str =
  if dim str == 0
    then ""
    else
      unlines
        [ "from_futhark_values_" ++ ftype ++ "_" ++ show i ++ "d_arr(" ++ intercalate ", " args1 ++ ") {",
          -- Possibly do sanity check that dimensions dim0 * dimn == arr.length",
          "  var dims = this.from_futhark_shape_" ++ ftype ++ "_" ++ show i ++ "d_arr(fut_arr);",
          "  var length = Number(dims.reduce((a, b) => a * b));",
          "  var dataHeap = initData(new " ++ arrType ++ "(length));",
          "  var res = futhark_values_raw_" ++ ftype ++ "_" ++ show i ++ "d(" ++ intercalate ", " args2 ++ ");",
          dataHeapConv "length" arrType,
          "  return dataHeapRes;",
          "}"
        ]
  where
    ctx = "this.ctx"
    fut_arr = "fut_arr"
    -- ofs = "dataHeap.byteOffset"
    args1 = [fut_arr]
    args2 = [ctx, fut_arr]
    i = dim str
    ftype = baseType str
    arrType = jsType str

runServer :: String
runServer =
  T.unpack
    [text|
   Module.onRuntimeInitialized = () => {
     var context = new FutharkContext();
     var server = new Server(context);
     server.run();
   }
  |]
