{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Futhark.CodeGen.Backends.GenericWASM
  ( GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
    JSEntryPoint (..),
    emccExportNames,
    javascriptWrapper,
    extToString,
    runServer,
    libraryExports,
  )
where

import Data.FileEmbed
import Data.List (intercalate, nub)
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.SimpleRep (opaqueName)
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import Futhark.IR.Primitive
import NeatInterpolation (text)

extToString :: Imp.ExternalValue -> String
extToString (Imp.TransparentValue u (Imp.ArrayValue vn _ pt s dimSize)) =
  concat (replicate (length dimSize) "[]") ++ extToString (Imp.TransparentValue u (Imp.ScalarValue pt s vn))
extToString (Imp.TransparentValue _ (Imp.ScalarValue (FloatType Float32) _ _)) = "f32"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (FloatType Float64) _ _)) = "f64"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int8) Imp.TypeDirect _)) = "i8"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int16) Imp.TypeDirect _)) = "i16"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int32) Imp.TypeDirect _)) = "i32"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int64) Imp.TypeDirect _)) = "i64"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int8) Imp.TypeUnsigned _)) = "u8"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int16) Imp.TypeUnsigned _)) = "u16"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int32) Imp.TypeUnsigned _)) = "u32"
extToString (Imp.TransparentValue _ (Imp.ScalarValue (IntType Int64) Imp.TypeUnsigned _)) = "u64"
extToString (Imp.TransparentValue _ (Imp.ScalarValue Bool _ _)) = "bool"
extToString (Imp.OpaqueValue _ oname vds) = opaqueName oname vds
extToString ev = error $ "extToString: missing case: " ++ show ev

type EntryPointType = String

data JSEntryPoint = JSEntryPoint
  { name :: String,
    parameters :: [EntryPointType],
    ret :: [EntryPointType]
  }

emccExportNames :: [JSEntryPoint] -> [String]
emccExportNames jses =
  map (\jse -> "'_futhark_entry_" ++ name jse ++ "'") jses
    ++ map (\arg -> "'" ++ gfn "new" arg ++ "'") arrays
    ++ map (\arg -> "'" ++ gfn "free" arg ++ "'") arrays
    ++ map (\arg -> "'" ++ gfn "shape" arg ++ "'") arrays
    ++ map (\arg -> "'" ++ gfn "values_raw" arg ++ "'") arrays
    ++ map (\arg -> "'" ++ gfn "values" arg ++ "'") arrays
    ++ map (\arg -> "'" ++ "_futhark_free_" ++ arg ++ "'") opaques
    ++ [ "_futhark_context_config_new",
         "_futhark_context_config_free",
         "_futhark_context_new",
         "_futhark_context_free",
         "_futhark_context_get_error"
       ]
  where
    arrays = filter isArray typs
    opaques = filter isOpaque typs
    typs = nub $ concatMap (\jse -> parameters jse ++ ret jse) jses
    gfn typ str = "_futhark_" ++ typ ++ "_" ++ baseType str ++ "_" ++ show (dim str) ++ "d"

javascriptWrapper :: [JSEntryPoint] -> String
javascriptWrapper entryPoints =
  unlines
    [ jsServer,
      jsValues,
      jsClasses,
      classFutharkContext entryPoints
    ]

jsServer :: String
jsServer = $(embedStringFile "rts/javascript/server.js")

jsValues :: String
jsValues = $(embedStringFile "rts/javascript/values.js")

jsClasses :: String
jsClasses = $(embedStringFile "rts/javascript/wrapperclasses.js")

classFutharkContext :: [JSEntryPoint] -> String
classFutharkContext entryPoints =
  unlines
    [ "class FutharkContext {",
      constructor entryPoints,
      getFreeFun,
      getEntryPointsFun,
      getErrorFun,
      unlines $ map toFutharkArray arrays,
      unlines $ map jsWrapEntryPoint entryPoints,
      "}",
      T.unpack
        [text|
      async function newFutharkContext() {
        var wasm = await loadWASM();
        return new FutharkContext(wasm);
      }
      |]
    ]
  where
    arrays = filter isArray typs
    typs = nub $ concatMap (\jse -> parameters jse ++ ret jse) entryPoints

constructor :: [JSEntryPoint] -> String
constructor jses =
  T.unpack
    [text|
  constructor(wasm, num_threads) {
    this.wasm = wasm;
    this.cfg = this.wasm._futhark_context_config_new();
    if (num_threads) this.wasm._futhark_context_config_set_num_threads(this.cfg, num_threads);
    this.ctx = this.wasm._futhark_context_new(this.cfg);
    this.entry_points = {
      ${entries}
    };
  }
  |]
  where
    entries = T.pack $ intercalate "," $ map dicEntry jses

getFreeFun :: String
getFreeFun =
  T.unpack
    [text|
  free() {
    this.wasm._futhark_context_free(this.ctx);
    this.wasm._futhark_context_config_free(this.cfg);
  }
  |]

getEntryPointsFun :: String
getEntryPointsFun =
  T.unpack
    [text|
  get_entry_points() {
    return this.entry_points;
  }
  |]

getErrorFun :: String
getErrorFun =
  T.unpack
    [text|
  get_error() {
    var ptr = this.wasm._futhark_context_get_error(this.ctx);
    var len = HEAP8.subarray(ptr).indexOf(0);
    var str = String.fromCharCode(...HEAP8.subarray(ptr, ptr + len));
    this.wasm._free(ptr);
    return str;
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
  T.unpack
    [text|
  ${func_name}(${inparams}) {
    var out = [${outparams}].map(n => this.wasm._malloc(n));
    var to_free = [];
    var do_free = () => { out.forEach(this.wasm._free); to_free.forEach(f => f.free()); };
    ${paramsToPtr}
    if (this.wasm._futhark_entry_${func_name}(this.ctx, ...out, ${ins}) > 0) {
      do_free();
      throw this.get_error();
    }
    ${results}
    do_free();
    return ${res};
  }
  |]
  where
    func_name = T.pack $ name jse

    alp = [0 .. length (parameters jse) - 1]
    inparams = T.pack $ intercalate ", " ["in" ++ show i | i <- alp]
    ins = T.pack $ intercalate ", " [maybeDerefence ("in" ++ show i) $ parameters jse !! i | i <- alp]
    paramsToPtr = T.pack $ unlines $ filter ("" /=) [arrayPointer ("in" ++ show i) $ parameters jse !! i | i <- alp]

    alr = [0 .. length (ret jse) - 1]
    outparams = T.pack $ intercalate ", " [show $ typeSize $ ret jse !! i | i <- alr]
    results = T.pack $ unlines [makeResult i $ ret jse !! i | i <- alr]
    res_array = intercalate ", " ["result" ++ show i | i <- alr]
    res = T.pack $ if length (ret jse) == 1 then "result0" else "[" ++ res_array ++ "]"

maybeDerefence :: String -> String -> String
maybeDerefence arg typ =
  if isScalar typ then arg else arg ++ ".ptr"

arrayPointer :: String -> String -> String
arrayPointer arg typ =
  if isArray typ
    then "  if (" ++ arg ++ " instanceof Array) { " ++ reassign ++ "; to_free.push(" ++ arg ++ "); }"
    else ""
  where
    reassign = arg ++ " = this.new_" ++ signature ++ "_from_jsarray(" ++ arg ++ ")"
    signature = baseType typ ++ "_" ++ show (dim typ) ++ "d"

makeResult :: Int -> String -> String
makeResult i typ =
  "  var result" ++ show i ++ " = "
    ++ if isArray typ
      then "this.new_" ++ signature ++ "_from_ptr(" ++ readout ++ ");"
      else
        if isOpaque typ
          then "new FutharkOpaque(this, " ++ readout ++ ", this.wasm._futhark_free_" ++ typ ++ ");"
          else readout ++ if typ == "bool" then "!==0;" else ";"
  where
    res = "out[" ++ show i ++ "]"
    readout = typeHeap typ ++ "[" ++ res ++ " >> " ++ show (typeShift typ) ++ "]"
    signature = baseType typ ++ "_" ++ show (dim typ) ++ "d"

baseType :: String -> String
baseType ('[' : ']' : end) = baseType end
baseType typ = typ

dim :: String -> Int
dim ('[' : ']' : end) = dim end + 1
dim _ = 0

isArray :: String -> Bool
isArray typ = take 2 typ == "[]"

isOpaque :: String -> Bool
isOpaque typ = take 6 typ == "opaque"

isScalar :: String -> Bool
isScalar typ = not (isArray typ || isOpaque typ)

typeSize :: String -> Integer
typeSize typ =
  case typ of
    "i8" -> 1
    "i16" -> 2
    "i32" -> 4
    "i64" -> 8
    "u8" -> 1
    "u16" -> 2
    "u32" -> 4
    "u64" -> 8
    "f32" -> 4
    "f64" -> 8
    "bool" -> 1
    _ -> 4

typeShift :: String -> Integer
typeShift typ =
  case typ of
    "i8" -> 0
    "i16" -> 1
    "i32" -> 2
    "i64" -> 3
    "u8" -> 0
    "u16" -> 1
    "u32" -> 2
    "u64" -> 3
    "f32" -> 2
    "f64" -> 3
    "bool" -> 0
    _ -> 2

typeHeap :: String -> String
typeHeap typ =
  case typ of
    "i8" -> "this.wasm.HEAP8"
    "i16" -> "this.wasm.HEAP16"
    "i32" -> "this.wasm.HEAP32"
    "i64" -> "this.wasm.HEAP64"
    "u8" -> "this.wasm.HEAPU8"
    "u16" -> "this.wasm.HEAPU16"
    "u32" -> "this.wasm.HEAPU32"
    "u64" -> "(new BigUint64Array(this.wasm.HEAP64.buffer))"
    "f32" -> "this.wasm.HEAPF32"
    "f64" -> "this.wasm.HEAPF64"
    "bool" -> "this.wasm.HEAP8"
    _ -> "this.wasm.HEAP32"

toFutharkArray :: String -> String
toFutharkArray typ =
  T.unpack
    [text|
  ${new}_from_jsarray(${arraynd_p}) {
    return this.${new}(${arraynd_flat_p}, ${arraynd_dims_p});
  }
  ${new}(array, ${dims}) {
    console.assert(array.length === ${dims_multiplied}, 'len=%s,dims=%s', array.length, [${dims}].toString());
      var copy = this.wasm._malloc(array.length << ${shift});
      ${heapType}.set(array, copy >> ${shift});
      var ptr = ${fnew}(this.ctx, copy, ${bigint_dims});
      this.wasm._free(copy);
      return this.${new}_from_ptr(ptr);
    }

    ${new}_from_ptr(ptr) {
      return new FutharkArray(this, ptr, ${args});
    }
    |]
  where
    d = dim typ
    ftype = baseType typ
    heap = typeHeap ftype
    signature = ftype ++ "_" ++ show d ++ "d"
    new = T.pack $ "new_" ++ signature
    fnew = T.pack $ "_futhark_new_" ++ signature
    fshape = "this.wasm._futhark_shape_" ++ signature
    fvalues = "this.wasm._futhark_values_raw_" ++ signature
    ffree = "this.wasm._futhark_free_" ++ signature
    arraynd = "array" ++ show d ++ "d"
    shift = T.pack $ show (typeShift ftype)
    heapType = T.pack heap
    arraynd_flat = if d > 1 then arraynd ++ ".flat()" else arraynd
    arraynd_dims = intercalate ", " [arraynd ++ mult i "[0]" ++ ".length" | i <- [0 .. d -1]]
    dims = T.pack $ intercalate ", " ["d" ++ show i | i <- [0 .. d -1]]
    dims_multiplied = T.pack $ intercalate "*" ["d" ++ show i | i <- [0 .. d -1]]
    bigint_dims = T.pack $ intercalate ", " ["BigInt(d" ++ show i ++ ")" | i <- [0 .. d -1]]
    mult i s = concat $ replicate i s
    (arraynd_p, arraynd_flat_p, arraynd_dims_p) = (T.pack arraynd, T.pack arraynd_flat, T.pack arraynd_dims)
    args = T.pack $ intercalate ", " ["'" ++ ftype ++ "'", show d, heap, fshape, fvalues, ffree]

runServer :: String
runServer =
  T.unpack
    [text|
   Module.onRuntimeInitialized = () => {
     var context = new FutharkContext(Module);
     var server = new Server(context);
     server.run();
   }
  |]

libraryExports :: String
libraryExports = "export {newFutharkContext, FutharkContext, FutharkArray, FutharkOpaque};"
